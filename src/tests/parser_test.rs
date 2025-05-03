use chumsky::{ParseResult, Parser};
use chumsky::error::Rich;
use chumsky::input::Stream;
use crate::ast::{Dec, EField, Exp, Field, FunDec, Oper, Program, Type, Var};
use crate::lexer::{lexer, Token};
use crate::parser;

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
        parser::exp_parser().parse(Stream::from_iter(tokens.into_iter()))
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
        parse_unwrap("(1;2)")
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