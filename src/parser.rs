use crate::ast::{Dec, EField, Exp, Field, FunDec, Oper, Program, Type, Var};
use crate::lexer::{lexer, Span, Token};
use chumsky::input::{Stream, ValueInput};
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

    let complex = simple
        .clone()
        .foldl(choice((field, subscript)).repeated(), |var, op| op(var));
    complex
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
            .map(|fields| Type::Record(fields)))
        .or(just(Token::ARRAY)
            .ignore_then(just(Token::OF))
            .ignore_then(select! {Token::ID(s) => s})
            .map(|s| Type::Array(s)))
        .boxed();
    let ty_dec = just(Token::TYPE)
        .ignore_then(select! {Token::ID(s) => s})
        .then_ignore(just(Token::EQ))
        .then(ty)
        .repeated()
        .at_least(1)
        .collect()
        .map(|types| Dec::typee(types));

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
        .map(|funs| Dec::function(funs));

    choice((var_dec, ty_dec, fun_dec)).repeated().collect()
}

pub fn exp_parser<'a, I>() -> impl Parser<'a, I, Exp, extra::Err<Rich<'a, Token, Span>>> + Clone
where
    I: ValueInput<'a, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let var = var_parser(expr.clone()).map(|var| Exp::var(var)).boxed();
        let nil = just(Token::NIL).to(Exp::Nil);
        let int = select! {Token::INT(n) => Exp::Int(n)};
        let string = select! {Token::STRING(s) =>Exp::String(s)};
        let call = select! {Token::ID(s) => s}
            .clone()
            .then_ignore(just(Token::LPAREN))
            .then(expr.clone().separated_by(just(Token::COMMA)).collect())
            .then_ignore(just(Token::RPAREN))
            .map(|(s, args)| Exp::call(s, args));
        let seq = just(Token::LPAREN)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::COMMA))
                    .at_least(2)
                    .collect(),
            )
            .then_ignore(just(Token::RPAREN))
            .map(|exps| Exp::seq(exps));
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
                just(Token::LT).to(Oper::Le),
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

#[test]
pub fn test_parser() {
    let parse = move |input: &str| -> ParseResult<Exp, Rich<Token>> {
        let lexer = lexer();
        let tokens = lexer
            .parse(input)
            .unwrap()
            .into_iter()
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>();
        exp_parser().parse(Stream::from_iter(tokens.into_iter()))
    };
    let parse_unwrap = move |input: &str| -> Program { parse(input).unwrap() };
    assert_eq!(
        Exp::var(Var::simple("foo".to_string())),
        parse_unwrap("foo")
    );
    assert_eq!(
        Exp::var(Var::simple("foo".to_string())),
        parse_unwrap("(foo)")
    );
    assert_eq!(
        Exp::var(Var::field(
            Var::simple("foo".to_string()),
            "bar".to_string()
        )),
        parse_unwrap("foo.bar")
    );
    assert_eq!(
        Exp::var(Var::subscript(Var::simple("foo".to_string()), Exp::int(42))),
        parse_unwrap("foo[42]")
    );
    assert_eq!(
        Exp::var(Var::subscript(
            Var::field(Var::simple("foo".to_string()), "bar".to_string()),
            Exp::int(42)
        )),
        parse_unwrap("foo.bar[42]")
    );
    assert_eq!(Exp::nil(), parse_unwrap("nil"));
    assert_eq!(Exp::int(42), parse_unwrap("42"));
    assert_eq!(Exp::string("foo".to_string()), parse_unwrap("\"foo\""));
    assert_eq!(
        Exp::call("foo".to_string(), vec![Exp::int(1), Exp::int(2)]),
        parse_unwrap("foo(1,2)")
    );
    assert_eq!(
        Exp::seq(vec![Exp::int(1), Exp::int(2)]),
        parse_unwrap("(1,2)")
    );
    assert_eq!(
        Exp::op(Oper::Minus, Exp::int(0), Exp::int(1)),
        parse_unwrap("-1")
    );
    assert_eq!(
        Exp::op(Oper::Times, Exp::int(1), Exp::int(1)),
        parse_unwrap("1*1")
    );
    assert_eq!(
        Exp::op(
            Oper::Divide,
            Exp::op(Oper::Divide, Exp::int(1), Exp::int(2)),
            Exp::int(3)
        ),
        parse_unwrap("1/2/3")
    );
    assert_eq!(
        Exp::op(
            Oper::Plus,
            Exp::op(Oper::Times, Exp::int(1), Exp::int(2)),
            Exp::op(Oper::Times, Exp::int(3), Exp::int(4))
        ),
        parse_unwrap("1*2+3*4")
    );
    assert_eq!(
        Exp::op(
            Oper::Plus,
            Exp::Int(1),
            Exp::op(Oper::Times, Exp::Int(2), Exp::Int(3))
        ),
        parse_unwrap("1+2*3")
    );
    assert_eq!(
        Exp::op(
            Oper::Times,
            Exp::op(Oper::Plus, Exp::Int(1), Exp::Int(2)),
            Exp::Int(3)
        ),
        parse_unwrap("(1+2)*3")
    );
    assert_eq!(
        Exp::op(
            Oper::Eq,
            Exp::op(Oper::Plus, Exp::int(1), Exp::int(2)),
            Exp::op(Oper::Plus, Exp::int(3), Exp::int(4))
        ),
        parse_unwrap("1+2=3+4")
    );
    assert!(parse("a=b=c").has_errors());
    assert_eq!(
        Exp::iff(
            Exp::var(Var::simple("a".to_string())),
            Exp::var(Var::simple("b".to_string())),
            Some(Exp::int(0))
        ),
        parse_unwrap("a&b")
    );
    assert_eq!(
        Exp::iff(
            Exp::var(Var::simple("a".to_string())),
            Exp::int(1),
            Some(Exp::var(Var::simple("b".to_string())))
        ),
        parse_unwrap("a|b")
    );
    assert_eq!(
        Exp::iff(
            Exp::var(Var::simple("a".to_string())),
            Exp::int(1),
            Some(Exp::iff(
                Exp::var(Var::simple("b".to_string())),
                Exp::var(Var::simple("c".to_string())),
                Some(Exp::int(0))
            ))
        ),
        parse_unwrap("a|b&c")
    );
    assert_eq!(
        Exp::record(
            "foo".to_string(),
            vec![
                EField {
                    name: "a".to_string(),
                    value: Exp::int(1)
                },
                EField {
                    name: "b".to_string(),
                    value: Exp::int(2)
                }
            ]
        ),
        parse_unwrap("foo{a=1,b=2}")
    );
    assert_eq!(
        Exp::assign(
            Var::simple("foo".to_string()),
            Exp::record(
                "bar".to_string(),
                vec![
                    EField {
                        name: "a".to_string(),
                        value: Exp::int(1)
                    },
                    EField {
                        name: "b".to_string(),
                        value: Exp::int(2)
                    }
                ]
            )
        ),
        parse_unwrap("foo:=bar{a=1,b=2}")
    );
    assert_eq!(
        Exp::iff(Exp::Int(1), Exp::Int(2), None),
        parse_unwrap("if 1 then 2")
    );
    assert_eq!(
        Exp::iff(Exp::Int(1), Exp::Int(2), Some(Exp::Int(3))),
        parse_unwrap("if 1 then 2 else 3")
    );
    assert_eq!(
        Exp::iff(
            Exp::Int(1),
            Exp::iff(Exp::Int(2), Exp::Int(3), Some(Exp::Int(4))),
            None
        ),
        parse_unwrap("if 1 then if 2 then 3 else 4")
    );
    assert_eq!(
        Exp::whilee(Exp::Int(1), Exp::Int(2)),
        parse_unwrap("while 1 do 2")
    );
    assert_eq!(
        Exp::forr("i".to_string(), Exp::Int(1), Exp::Int(10), Exp::Int(11)),
        parse_unwrap("for i:=1 to 10 do 11")
    );
    assert_eq!(
        Exp::array("foo".to_string(), Exp::Int(10), Exp::Int(11)),
        parse_unwrap("foo[10] of 11")
    );
    assert_eq!(
        Exp::lett(
            vec![
                Dec::var("a".to_string(), None, Exp::Int(1)),
                Dec::var("b".to_string(), Some("int".to_string()), Exp::Int(2))
            ],
            vec![
                Exp::op(
                    Oper::Plus,
                    Exp::var(Var::simple("a".to_string())),
                    Exp::var(Var::simple("b".to_string()))
                ),
                Exp::Nil
            ]
        ),
        parse_unwrap("let var a := 1 var b: int := 2 in a + b;nil end")
    );
    assert_eq!(
        Exp::lett(
            vec![Dec::Type(vec![
                ("foo".to_string(), Type::Name("int".to_string())),
                ("bar".to_string(), Type::Array("int".to_string())),
                (
                    "baz".to_string(),
                    Type::Record(vec![
                        Field {
                            name: "a".to_string(),
                            typ: "int".to_string()
                        },
                        Field {
                            name: "b".to_string(),
                            typ: "int".to_string()
                        }
                    ])
                ),
            ])],
            vec![Exp::Nil]
        ),
        parse_unwrap(
            "let type foo = int type bar = array of int type baz = {a: int, b: int} in nil end"
        )
    );
    assert_eq!(
        Exp::lett(
            vec![Dec::function(vec![
                FunDec {
                    name: "foo".to_string(),
                    params: vec![
                        Field {
                            name: "a".to_string(),
                            typ: "int".to_string()
                        },
                        Field {
                            name: "b".to_string(),
                            typ: "int".to_string()
                        },
                    ],
                    result: None,
                    body: Exp::Nil
                },
                FunDec {
                    name: "bar".to_string(),
                    params: vec![],
                    result: Some("int".to_string()),
                    body: Exp::Int(2)
                },
            ])],
            vec![Exp::call(
                "foo".to_string(),
                vec![
                    Exp::call("bar".to_string(), vec![]),
                    Exp::call("bar".to_string(), vec![])
                ]
            )]
        ),
        parse_unwrap(
            "let function foo(a: int, b:int) = nil function bar(): int = 2 in foo(bar(), bar()) end"
        )
    );
}
