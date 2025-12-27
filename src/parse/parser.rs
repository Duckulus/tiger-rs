use crate::parse::ast::{Dec, EField, Exp, Field, FunDec, Oper, TypeDecl, Var};
use crate::parse::lexer::{Span, Spanned, Token};
use chumsky::input::ValueInput;
use chumsky::prelude::*;

pub fn var_parser<'a, I>(
    expr: impl Parser<'a, I, Spanned<Exp>, extra::Err<Rich<'a, Token, Span>>> + Clone,
) -> impl Parser<'a, I, Spanned<Var>, extra::Err<Rich<'a, Token, Span>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span>,
{
    let simple = select! {Token::ID(s) => s}
        .map_with(|id, extra| (id, extra.span()))
        .map(|(s, span)| (Var::simple((s, span)), span));

    let field = just(Token::DOT)
        .ignore_then(select! {Token::ID(s) => s})
        .map_with(|f, extra| (f, extra.span()))
        .map(|s: Spanned<String>| {
            Box::new(move |var: Spanned<Var>| {
                (
                    Var::field(var.0, s.clone()),
                    SimpleSpan::from(var.1.start..s.1.end),
                )
            }) as Box<dyn Fn(Spanned<Var>) -> Spanned<Var>>
        });

    let subscript = just(Token::LBRACK)
        .ignore_then(expr)
        .then_ignore(just(Token::RBRACK))
        .map_with(|f, extra| (f, extra.span()))
        .map(|exp| {
            Box::new(move |var: Spanned<Var>| {
                (
                    Var::subscript(var.0, exp.0.clone()),
                    SimpleSpan::from(var.1.start..exp.1.end),
                )
            }) as Box<dyn Fn(Spanned<Var>) -> Spanned<Var>>
        });

    simple.foldl(choice((field, subscript)).repeated(), |var, op| op(var))
}

pub fn decs_parser<'a, I>(
    expr: impl Parser<'a, I, Spanned<Exp>, extra::Err<Rich<'a, Token, Span>>> + Clone + 'a,
) -> impl Parser<'a, I, Vec<Dec>, extra::Err<Rich<'a, Token, Span>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span>,
{
    let var_dec = just(Token::VAR)
        .ignore_then(select! {Token::ID(s) => s})
        .map_with(|s, extra| (s, extra.span()))
        .then(
            just(Token::COLON)
                .ignore_then(select! {Token::ID(s) => s}.map_with(|s, extra| (s, extra.span())))
                .or_not(),
        )
        .then_ignore(just(Token::ASSIGN))
        .then(expr.clone())
        .map(|((id, typ), exp)| Dec::var(id, typ, exp))
        .boxed();
    let typed_fields = select! {Token::ID(s)=> s}
        .then_ignore(just(Token::COLON))
        .then(select! {Token::ID(s)=> s}.map_with(|typ, extra| (typ, extra.span())))
        .map(|(name, typ)| Field { name, typ })
        .separated_by(just(Token::COMMA))
        .collect();
    let ty = select! {Token::ID(s) => s}
        .map_with(|id, extra| TypeDecl::Name((id, extra.span())))
        .or(typed_fields
            .clone()
            .delimited_by(just(Token::LBRACE), just(Token::RBRACE))
            .map(TypeDecl::Record))
        .or(just(Token::ARRAY)
            .ignore_then(just(Token::OF))
            .ignore_then(select! {Token::ID(s) => s}
                .map_with(|id, extra| (id, extra.span())))
            .map(TypeDecl::Array))
        .boxed();
    let ty_dec = just(Token::TYPE)
        .ignore_then(select! {Token::ID(s) => s})
        .then_ignore(just(Token::EQ))
        .then(ty)
        .repeated()
        .at_least(1)
        .collect()
        .map(Dec::typee);

    let fun_dec = just(Token::FUNCTION)
        .ignore_then(select! {Token::ID(s) => s})
        .then_ignore(just(Token::LPAREN))
        .then(typed_fields)
        .then_ignore(just(Token::RPAREN))
        .then(
            just(Token::COLON)
                .ignore_then(select! {Token::ID(s) => s})
                .map_with(|typ, extra| (typ, extra.span()))
                .or_not(),
        )
        .then_ignore(just(Token::EQ))
        .then(expr)
        .map(|(((name, params), result), body)| FunDec {
            name,
            params,
            result,
            body,
        })
        .repeated()
        .at_least(1)
        .collect()
        .map(Dec::function);

    choice((var_dec, ty_dec, fun_dec)).repeated().collect()
}

pub fn exp_parser<'a, I>()
-> impl Parser<'a, I, Spanned<Exp>, extra::Err<Rich<'a, Token, Span>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let var = var_parser(expr.clone())
            .boxed()
            .map(|(var, span)| Exp::var(var));
        let nil = just(Token::NIL).to(Exp::Nil);
        let int = select! {Token::INT(n) => Exp::Int(n)};
        let string = select! {Token::STRING(s) =>Exp::String(s)};
        let call = select! {Token::ID(s) => s}
            .map_with(|id, extra| (id, extra.span()))
            .then_ignore(just(Token::LPAREN))
            .then(expr.clone().separated_by(just(Token::COMMA)).collect())
            .then_ignore(just(Token::RPAREN))
            .map(|(s, args)| Exp::call(s, args));
        let seq = just(Token::LPAREN)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::SEMICOLON))
                    .at_least(2)
                    .collect(),
            )
            .then_ignore(just(Token::RPAREN))
            .map(Exp::seq);
        let parens = expr
            .clone()
            .delimited_by(just(Token::LPAREN), just(Token::RPAREN));

        let atom = choice((call, var, nil, int, string, seq))
            .map_with(|exp, e| (exp, e.span()))
            .or(parens)
            .boxed();

        let unary = just(Token::MINUS)
            .map_with(|op, extra| (Oper::Minus, extra.span()))
            .repeated()
            .foldr_with(atom, |op, exp, e| {
                (
                    Exp::op(op, (Exp::int(0), SimpleSpan::from(0..0)), exp),
                    e.span(),
                )
            })
            .boxed();

        let product = unary
            .clone()
            .foldl_with(
                choice((
                    just(Token::TIMES).map_with(|_,extra| (Oper::Times, extra.span())),
                    just(Token::DIVIDE).map_with(|_, extra| (Oper::Divide, extra.span())),
                ))
                .then(unary)
                .repeated(),
                |lhs, (op, rhs), e| (Exp::op(op, lhs, rhs), e.span()),
            )
            .boxed();

        let sum = product
            .clone()
            .foldl_with(
                choice((
                    just(Token::PLUS).map_with(|_,extra| (Oper::Plus, extra.span())),
                    just(Token::MINUS).map_with(|_, extra| (Oper::Minus, extra.span())),
                ))
                .then(product)
                .repeated(),
                |lhs, (op, rhs), e| (Exp::op(op, lhs, rhs), e.span()),
            )
            .boxed();

        let comparison = sum
            .clone()
            .then(choice((
                just(Token::EQ).map_with(|_,extra| (Oper::Eq, extra.span())),
                just(Token::NEQ).map_with(|_,extra| (Oper::Neq, extra.span())),
                just(Token::GT).map_with(|_,extra| (Oper::Gt, extra.span())),
                just(Token::GE).map_with(|_,extra| (Oper::Ge, extra.span())),
                just(Token::LT).map_with(|_,extra| (Oper::Lt, extra.span())),
                just(Token::LE).map_with(|_,extra| (Oper::Le, extra.span())),
            )))
            .then(sum.clone())
            .map_with(|((lhs, op), rhs), e| (Exp::op(op, lhs, rhs), e.span()))
            .boxed();

        let comparison = comparison.or(sum);

        let and = comparison
            .clone()
            .foldl_with(
                just(Token::AND).ignore_then(comparison).repeated(),
                |lhs, rhs, e| {
                    (
                        Exp::iff(lhs, rhs, Some((Exp::Int(0), SimpleSpan::from(0..0)))),
                        e.span(),
                    )
                },
            )
            .boxed();

        let or = and
            .clone()
            .foldl_with(
                just(Token::OR).ignore_then(and).repeated(),
                |lhs, rhs, e| {
                    (
                        Exp::iff(lhs, (Exp::Int(1), SimpleSpan::from(0..0)), Some(rhs)),
                        e.span(),
                    )
                },
            )
            .boxed();

        let record = select! {Token::ID(s) => s}
            .map_with(|id, extra| (id, extra.span()))
            .then_ignore(just(Token::LBRACE))
            .then(
                select! {Token::ID(s) => s}
                    .map_with(|id, extra| (id, extra.span()))
                    .then_ignore(just(Token::EQ))
                    .then(expr.clone())
                    .map(|(name, value)| EField { name, value })
                    .separated_by(just(Token::COMMA))
                    .collect(),
            )
            .then_ignore(just(Token::RBRACE))
            .map_with(|(typ, fields), e| (Exp::record(typ, fields), e.span()))
            .boxed();

        let assign = var_parser(expr.clone())
            .then_ignore(just(Token::ASSIGN))
            .then(expr.clone())
            .map_with(|((var, span), exp), e| (Exp::assign(var, exp), e.span()))
            .boxed();
        let iff = just(Token::IF)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::THEN))
            .then(expr.clone())
            .then(just(Token::ELSE).ignore_then(expr.clone()).or_not())
            .map_with(|((cond, then), elsee), e| (Exp::iff(cond, then, elsee), e.span()))
            .boxed();
        let whilee = just(Token::WHILE)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::DO))
            .then(expr.clone())
            .map_with(|(cond, body), e| (Exp::whilee(cond, body), e.span()))
            .boxed();
        let forr = just(Token::FOR)
            .ignore_then(select! {Token::ID(s) => s})
            .then_ignore(just(Token::ASSIGN))
            .then(expr.clone())
            .then_ignore(just(Token::TO))
            .then(expr.clone())
            .then_ignore(just(Token::DO))
            .then(expr.clone())
            .map_with(|(((s, lo), hi), body), e| (Exp::forr(s, lo, hi, body), e.span()))
            .boxed();
        let breakk = just(Token::BREAK)
            .to(Exp::Break)
            .map_with(|exp, e| (exp, e.span()));
        let array = select! {Token::ID(s) => s}
            .map_with(|id, extra| (id, extra.span()))
            .then_ignore(just(Token::LBRACK))
            .then(expr.clone())
            .then_ignore(just(Token::RBRACK))
            .then_ignore(just(Token::OF))
            .then(expr.clone())
            .map_with(|((s, n), i), e| (Exp::array(s, n, i), e.span()))
            .boxed();

        let lett = just(Token::LET)
            .ignore_then(decs_parser(expr.clone()))
            .then_ignore(just(Token::IN))
            .then(expr.clone().separated_by(just(Token::SEMICOLON)).collect())
            .then_ignore(just(Token::END))
            .map_with(|(decs, exps), e| (Exp::Let(decs, exps), e.span()))
            .boxed();

        choice((lett, array, breakk, forr, whilee, iff, assign, record, or)).boxed()
    })
}
