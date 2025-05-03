use crate::ast::{Dec, EField, Exp, Field, FunDec, Oper, Type, Var};
use crate::lexer::{Span, Token};
use chumsky::input::ValueInput;
use chumsky::prelude::*;

pub fn var_parser<'a, I>(
    expr: impl Parser<'a, I, Exp, extra::Err<Rich<'a, Token, Span>>> + Clone,
) -> impl Parser<'a, I, Var, extra::Err<Rich<'a, Token, Span>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span>,
{
    let simple = select! {Token::ID(s) => Var::simple(s)};
    let field = just(Token::DOT)
        .ignore_then(select! {Token::ID(s) => s})
        .map(|s| Box::new(move |var| Var::field(var, s.clone())) as Box<dyn Fn(Var) -> Var>);

    let subscript = just(Token::LBRACK)
        .ignore_then(expr)
        .then_ignore(just(Token::RBRACK))
        .map(|exp| {
            Box::new(move |var| Var::subscript(var, exp.clone())) as Box<dyn Fn(Var) -> Var>
        });

    simple
        .foldl(choice((field, subscript)).repeated(), |var, op| op(var))
}

pub fn decs_parser<'a, I>(
    expr: impl Parser<'a, I, Exp, extra::Err<Rich<'a, Token, Span>>> + Clone + 'a,
) -> impl Parser<'a, I, Vec<Dec>, extra::Err<Rich<'a, Token, Span>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span>,
{
    let var_dec = just(Token::VAR)
        .ignore_then(select! {Token::ID(s) => s})
        .then(
            just(Token::COLON)
                .ignore_then(select! {Token::ID(s) => s})
                .or_not(),
        )
        .then_ignore(just(Token::ASSIGN))
        .then(expr.clone())
        .map(|((id, typ), exp)| Dec::var(id, typ, exp))
        .boxed();
    let typed_fields = select! {Token::ID(s)=> s}
        .then_ignore(just(Token::COLON))
        .then(select! {Token::ID(s)=> s})
        .map(|(name, typ)| Field { name, typ })
        .separated_by(just(Token::COMMA))
        .collect();
    let ty = select! {Token::ID(s) => Type::Name(s)}
        .or(typed_fields
            .clone()
            .delimited_by(just(Token::LBRACE), just(Token::RBRACE))
            .map(Type::Record))
        .or(just(Token::ARRAY)
            .ignore_then(just(Token::OF))
            .ignore_then(select! {Token::ID(s) => s})
            .map(Type::Array))
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

pub fn exp_parser<'a, I>() -> impl Parser<'a, I, Exp, extra::Err<Rich<'a, Token, Span>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let var = var_parser(expr.clone()).map(Exp::var).boxed();
        let nil = just(Token::NIL).to(Exp::Nil);
        let int = select! {Token::INT(n) => Exp::Int(n)};
        let string = select! {Token::STRING(s) =>Exp::String(s)};
        let call = select! {Token::ID(s) => s}
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

        let atom = choice((call, var, nil, int, string, seq, parens)).boxed();

        let unary = just(Token::MINUS)
            .repeated()
            .foldr(atom, |_, e| Exp::op(Oper::Minus, Exp::int(0), e))
            .boxed();

        let product = unary
            .clone()
            .foldl(
                choice((
                    just(Token::TIMES).map(|_| Oper::Times),
                    just(Token::DIVIDE).map(|_| Oper::Divide),
                ))
                .then(unary)
                .repeated(),
                |lhs, (op, rhs)| Exp::op(op, lhs, rhs),
            )
            .boxed();

        let sum = product
            .clone()
            .foldl(
                choice((
                    just(Token::PLUS).map(|_| Oper::Plus),
                    just(Token::MINUS).map(|_| Oper::Minus),
                ))
                .then(product)
                .repeated(),
                |lhs, (op, rhs)| Exp::op(op, lhs, rhs),
            )
            .boxed();

        let comparison = sum
            .clone()
            .then(choice((
                just(Token::EQ).to(Oper::Eq),
                just(Token::NEQ).to(Oper::Neq),
                just(Token::GT).to(Oper::Gt),
                just(Token::GE).to(Oper::Ge),
                just(Token::LT).to(Oper::Lt),
                just(Token::LE).to(Oper::Le),
            )))
            .then(sum.clone())
            .map(|((lhs, op), rhs)| Exp::op(op, lhs, rhs))
            .boxed();

        let comparison = comparison.or(sum);

        let and = comparison
            .clone()
            .foldl(
                just(Token::AND).ignore_then(comparison).repeated(),
                |lhs, rhs| Exp::iff(lhs, rhs, Some(Exp::Int(0))),
            )
            .boxed();

        let or = and
            .clone()
            .foldl(just(Token::OR).ignore_then(and).repeated(), |lhs, rhs| {
                Exp::iff(lhs, Exp::Int(1), Some(rhs))
            })
            .boxed();

        let record = select! {Token::ID(s) => s}
            .then_ignore(just(Token::LBRACE))
            .then(
                select! {Token::ID(s) => s}
                    .then_ignore(just(Token::EQ))
                    .then(expr.clone())
                    .map(|(name, value)| EField { name, value })
                    .separated_by(just(Token::COMMA))
                    .collect(),
            )
            .then_ignore(just(Token::RBRACE))
            .map(|(typ, fields)| Exp::record(typ, fields))
            .boxed();

        let assign = var_parser(expr.clone())
            .then_ignore(just(Token::ASSIGN))
            .then(expr.clone())
            .map(|(var, exp)| Exp::assign(var, exp))
            .boxed();
        let iff = just(Token::IF)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::THEN))
            .then(expr.clone())
            .then(just(Token::ELSE).ignore_then(expr.clone()).or_not())
            .map(|((cond, then), elsee)| Exp::iff(cond, then, elsee))
            .boxed();
        let whilee = just(Token::WHILE)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::DO))
            .then(expr.clone())
            .map(|(cond, body)| Exp::whilee(cond, body))
            .boxed();
        let forr = just(Token::FOR)
            .ignore_then(select! {Token::ID(s) => s})
            .then_ignore(just(Token::ASSIGN))
            .then(expr.clone())
            .then_ignore(just(Token::TO))
            .then(expr.clone())
            .then_ignore(just(Token::DO))
            .then(expr.clone())
            .map(|(((s, lo), hi), body)| Exp::forr(s, lo, hi, body))
            .boxed();
        let breakk = just(Token::BREAK).to(Exp::Break);
        let array = select! {Token::ID(s) => s}
            .then_ignore(just(Token::LBRACK))
            .then(expr.clone())
            .then_ignore(just(Token::RBRACK))
            .then_ignore(just(Token::OF))
            .then(expr.clone())
            .map(|((s, n), i)| Exp::array(s, n, i))
            .boxed();

        let lett = just(Token::LET)
            .ignore_then(decs_parser(expr.clone()))
            .then_ignore(just(Token::IN))
            .then(expr.clone().separated_by(just(Token::SEMICOLON)).collect())
            .then_ignore(just(Token::END))
            .map(|(decs, exps)| Exp::Let(decs, exps))
            .boxed();

        choice((lett, array, breakk, forr, whilee, iff, assign, record, or)).boxed()
    })
}
