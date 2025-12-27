#![allow(unused)]
use crate::parse::ast::Var::Simple;
use crate::parse::ast::{
    Dec, EField, Exp, Field, FunDec, Oper, Program, Symbol, TypSymbol, TypeDecl, Var,
};
use crate::parse::lexer::{Spanned, Token, lexer};
use crate::parse::parser;
use chumsky::error::Rich;
use chumsky::input::Stream;
use chumsky::prelude::SimpleSpan;
use chumsky::{ParseResult, Parser};

fn remove_spans_var(var: Var) -> Var {
    match var {
        Var::Simple((s, _)) => Var::Simple((s, SimpleSpan::from(0..0))),
        Var::Field(v, s) => Var::Field(Box::new(remove_spans_var(*v)), s),
        Var::Subscript(v, e) => {
            Var::Subscript(Box::new(remove_spans_var(*v)), Box::new(remove_spans(*e)))
        }
    }
}

fn remove_spans_fields(fields: Vec<Field>) -> Vec<Field> {
    fields
        .into_iter()
        .map(|field| Field {
            name: field.name,
            typ: (field.typ.0, SimpleSpan::from(0..0)),
        })
        .collect()
}

fn remove_spans_dec(dec: Dec) -> Dec {
    match dec {
        Dec::Var((var, _), typ, exp) => Dec::Var(
            (var, SimpleSpan::from(0..0)),
            typ.map(|(typ, _)| (typ, SimpleSpan::from(0..0))),
            Box::new(remove_spans(*exp)),
        ),
        Dec::Type(types) => Dec::Type(
            types
                .into_iter()
                .map(|named_type| match named_type.1 {
                    TypeDecl::Name((symb, _)) => {
                        (named_type.0, TypeDecl::Name((symb, SimpleSpan::from(0..0))))
                    }
                    TypeDecl::Record(fields) => {
                        (named_type.0, TypeDecl::Record(remove_spans_fields(fields)))
                    }
                    TypeDecl::Array((symb, _)) => (
                        named_type.0,
                        TypeDecl::Array((symb, SimpleSpan::from(0..0))),
                    ),
                })
                .collect::<Vec<_>>(),
        ),
        Dec::Function(fundecs) => Dec::Function(
            fundecs
                .into_iter()
                .map(|dec| FunDec {
                    name: dec.name,
                    params: remove_spans_fields(dec.params),
                    result: dec.result.map(|(symb, _)| (symb, SimpleSpan::from(0..0))),
                    body: remove_spans(dec.body),
                })
                .collect(),
        ),
    }
}

fn remove_spans(exp: Spanned<Exp>) -> Spanned<Exp> {
    let (exp, _) = exp;
    (
        match exp {
            Exp::Var(v) => Exp::Var(Box::new(remove_spans_var(*v))),
            Exp::Nil => Exp::Nil,
            Exp::Int(i) => Exp::Int(i),
            Exp::String(s) => Exp::String(s),
            Exp::Call((symb, _), args) => Exp::Call(
                (symb, SimpleSpan::from(0..0)),
                args.into_iter().map(|e| remove_spans(e)).collect(),
            ),
            Exp::Op(op, lhs, rhs) => Exp::Op(
                op,
                Box::new(remove_spans(*lhs)),
                Box::new(remove_spans(*rhs)),
            ),
            Exp::Record(typ, fields) => Exp::Record(
                typ,
                fields
                    .into_iter()
                    .map(|field| EField {
                        name: field.name,
                        value: remove_spans(field.value),
                    })
                    .collect(),
            ),
            Exp::Seq(exps) => Exp::Seq(exps.into_iter().map(|e| remove_spans(e)).collect()),
            Exp::Assign(var, exp) => Exp::Assign(
                Box::new(remove_spans_var(*var)),
                Box::new(remove_spans(*exp)),
            ),
            Exp::If { cond, then, elsee } => Exp::If {
                cond: Box::new(remove_spans(*cond)),
                then: Box::new(remove_spans(*then)),
                elsee: elsee.map(|e| Box::new(remove_spans(*e))),
            },
            Exp::While { cond, body } => Exp::While {
                cond: Box::new(remove_spans(*cond)),
                body: Box::new(remove_spans(*body)),
            },
            Exp::Break => Exp::Break,
            Exp::For { var, lo, hi, body } => Exp::For {
                var,
                lo: Box::new(remove_spans(*lo)),
                hi: Box::new(remove_spans(*hi)),
                body: Box::new(remove_spans(*body)),
            },
            Exp::Let(decs, exps) => Exp::Let(
                decs.into_iter().map(remove_spans_dec).collect(),
                exps.into_iter().map(remove_spans).collect(),
            ),
            Exp::Array { typ: (typ, _), size, init } => Exp::Array {
                typ: (typ, SimpleSpan::from(0..0)),
                size: Box::new(remove_spans(*size)),
                init: Box::new(remove_spans(*init)),
            },
        },
        SimpleSpan::from(0..0),
    )
}

fn var(var: Var) -> Spanned<Exp> {
    (Exp::var(var), SimpleSpan::from(0..0))
}

fn nil() -> Spanned<Exp> {
    (Exp::Nil, SimpleSpan::from(0..0))
}

fn int(i: i32) -> Spanned<Exp> {
    (Exp::int(i), SimpleSpan::from(0..0))
}

fn string(s: String) -> Spanned<Exp> {
    (Exp::String(s), SimpleSpan::from(0..0))
}

fn call(name: Symbol, args: Vec<Spanned<Exp>>) -> Spanned<Exp> {
    (
        Exp::Call((name, SimpleSpan::from(0..0)), args),
        SimpleSpan::from(0..0),
    )
}

fn op(op: Oper, e1: Spanned<Exp>, e2: Spanned<Exp>) -> Spanned<Exp> {
    (
        Exp::Op(op, Box::new(e1), Box::new(e2)),
        SimpleSpan::from(0..0),
    )
}

fn record(typ: TypSymbol, fields: Vec<EField>) -> Spanned<Exp> {
    (Exp::Record(typ, fields), SimpleSpan::from(0..0))
}

fn seq(exps: Vec<Spanned<Exp>>) -> Spanned<Exp> {
    (Exp::Seq(exps), SimpleSpan::from(0..0))
}

fn assign(var: Var, exp: Spanned<Exp>) -> Spanned<Exp> {
    (
        Exp::Assign(Box::new(var), Box::new(exp)),
        SimpleSpan::from(0..0),
    )
}

fn iff(cond: Spanned<Exp>, then: Spanned<Exp>, elsee: Option<Spanned<Exp>>) -> Spanned<Exp> {
    (
        Exp::If {
            cond: Box::new(cond),
            then: Box::new(then),
            elsee: elsee.map(Box::new),
        },
        SimpleSpan::from(0..0),
    )
}

fn whilee(cond: Spanned<Exp>, body: Spanned<Exp>) -> Spanned<Exp> {
    (
        Exp::While {
            cond: Box::new(cond),
            body: Box::new(body),
        },
        SimpleSpan::from(0..0),
    )
}

fn breakk() -> Spanned<Exp> {
    (Exp::Break, SimpleSpan::from(0..0))
}

fn forr(var: Symbol, lo: Spanned<Exp>, hi: Spanned<Exp>, body: Spanned<Exp>) -> Spanned<Exp> {
    (
        Exp::For {
            var,
            lo: Box::new(lo),
            hi: Box::new(hi),
            body: Box::new(body),
        },
        SimpleSpan::from(0..0),
    )
}

fn lett(decs: Vec<Dec>, body: Vec<Spanned<Exp>>) -> Spanned<Exp> {
    (Exp::Let(decs, body), SimpleSpan::from(0..0))
}

fn array(typ: TypSymbol, size: Spanned<Exp>, init: Spanned<Exp>) -> Spanned<Exp> {
    (
        Exp::Array {
            typ: (typ, SimpleSpan::from(0..0)),
            size: Box::new(size),
            init: Box::new(init),
        },
        SimpleSpan::from(0..0),
    )
}

fn field(name: String, typ: String) -> Field {
    Field {
        name,
        typ: (typ, SimpleSpan::from(0..0)),
    }
}

fn var_dec(id: String, typ: Option<String>, exp: Spanned<Exp>) -> Dec {
    Dec::Var(
        (id, SimpleSpan::from(0..0)),
        typ.map(|s| (s, SimpleSpan::from(0..0))),
        Box::from(exp),
    )
}

fn symbol(symb: String) -> Spanned<Symbol> {
    (symb, SimpleSpan::from(0..0))
}

fn simple_var(id: String) -> Var {
    Var::simple((id, SimpleSpan::from(0..0)))
}

fn name_type_decl(name: String) -> TypeDecl {
    TypeDecl::Name((name, SimpleSpan::from(0..0)))
}

fn array_type_decl(name: String) -> TypeDecl {
    TypeDecl::Array((name, SimpleSpan::from(0..0)))
}

#[test]
pub fn test_parser() {
    let parse = move |input: &str| -> ParseResult<Spanned<Exp>, Rich<Token>> {
        let lexer = lexer();
        let tokens = lexer
            .parse(input)
            .unwrap()
            .into_iter()
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>();
        parser::exp_parser().parse(Stream::from_iter(tokens.into_iter()))
    };
    let parse_unwrap =
        move |input: &str| -> Spanned<Program> { remove_spans(parse(input).unwrap()) };
    assert_eq!(var(simple_var("foo".to_string())), parse_unwrap("foo"));
    assert_eq!(var(simple_var("foo".to_string())), parse_unwrap("(foo)"));
    assert_eq!(
        var(Var::field(simple_var("foo".to_string()), "bar".to_string())),
        parse_unwrap("foo.bar")
    );
    assert_eq!(
        var(Var::subscript(simple_var("foo".to_string()), int(42))),
        parse_unwrap("foo[42]")
    );
    assert_eq!(
        var(Var::subscript(
            Var::field(simple_var("foo".to_string()), "bar".to_string()),
            int(42)
        )),
        parse_unwrap("foo.bar[42]")
    );
    assert_eq!(nil(), parse_unwrap("nil"));
    assert_eq!(int(42), parse_unwrap("42"));
    assert_eq!(string("foo".to_string()), parse_unwrap("\"foo\""));
    assert_eq!(
        call("foo".to_string(), vec![int(1), int(2)]),
        parse_unwrap("foo(1,2)")
    );
    assert_eq!(seq(vec![int(1), int(2)]), parse_unwrap("(1;2)"));
    assert_eq!(op(Oper::Minus, int(0), int(1)), parse_unwrap("-1"));
    assert_eq!(op(Oper::Times, int(1), int(1)), parse_unwrap("1*1"));
    assert_eq!(
        op(Oper::Divide, op(Oper::Divide, int(1), int(2)), int(3)),
        parse_unwrap("1/2/3")
    );
    assert_eq!(
        op(
            Oper::Plus,
            op(Oper::Times, int(1), int(2)),
            op(Oper::Times, int(3), int(4))
        ),
        parse_unwrap("1*2+3*4")
    );
    assert_eq!(
        op(Oper::Plus, int(1), op(Oper::Times, int(2), int(3))),
        parse_unwrap("1+2*3")
    );
    assert_eq!(
        op(Oper::Times, op(Oper::Plus, int(1), int(2)), int(3)),
        parse_unwrap("(1+2)*3")
    );
    assert_eq!(
        op(
            Oper::Eq,
            op(Oper::Plus, int(1), int(2)),
            op(Oper::Plus, int(3), int(4))
        ),
        parse_unwrap("1+2=3+4")
    );
    assert!(parse("a=b=c").has_errors());
    assert_eq!(
        iff(
            var(simple_var("a".to_string())),
            var(simple_var("b".to_string())),
            Some(int(0))
        ),
        parse_unwrap("a&b")
    );
    assert_eq!(
        iff(
            var(simple_var("a".to_string())),
            int(1),
            Some(var(simple_var("b".to_string())))
        ),
        parse_unwrap("a|b")
    );
    assert_eq!(
        iff(
            var(simple_var("a".to_string())),
            int(1),
            Some(iff(
                var(simple_var("b".to_string())),
                var(simple_var("c".to_string())),
                Some(int(0))
            ))
        ),
        parse_unwrap("a|b&c")
    );
    assert_eq!(
        record(
            "foo".to_string(),
            vec![
                EField {
                    name: "a".to_string(),
                    value: int(1)
                },
                EField {
                    name: "b".to_string(),
                    value: int(2)
                }
            ]
        ),
        parse_unwrap("foo{a=1,b=2}")
    );
    assert_eq!(
        assign(
            simple_var("foo".to_string()),
            record(
                "bar".to_string(),
                vec![
                    EField {
                        name: "a".to_string(),
                        value: int(1)
                    },
                    EField {
                        name: "b".to_string(),
                        value: int(2)
                    }
                ]
            )
        ),
        parse_unwrap("foo:=bar{a=1,b=2}")
    );
    assert_eq!(iff(int(1), int(2), None), parse_unwrap("if 1 then 2"));
    assert_eq!(
        iff(int(1), int(2), Some(int(3))),
        parse_unwrap("if 1 then 2 else 3")
    );
    assert_eq!(
        iff(int(1), iff(int(2), int(3), Some(int(4))), None),
        parse_unwrap("if 1 then if 2 then 3 else 4")
    );
    assert_eq!(whilee(int(1), int(2)), parse_unwrap("while 1 do 2"));
    assert_eq!(
        forr("i".to_string(), int(1), int(10), int(11)),
        parse_unwrap("for i:=1 to 10 do 11")
    );
    assert_eq!(
        array("foo".to_string(), int(10), int(11)),
        parse_unwrap("foo[10] of 11")
    );
    assert_eq!(
        lett(
            vec![
                var_dec("a".to_string(), None, int(1)),
                var_dec("b".to_string(), Some("int".to_string()), int(2))
            ],
            vec![
                op(
                    Oper::Plus,
                    var(simple_var("a".to_string())),
                    var(simple_var("b".to_string()))
                ),
                nil()
            ]
        ),
        parse_unwrap("let var a := 1 var b: int := 2 in a + b;nil end")
    );
    assert_eq!(
        lett(
            vec![Dec::Type(vec![
                ("foo".to_string(), name_type_decl("int".to_string())),
                ("bar".to_string(), array_type_decl("int".to_string())),
                (
                    "baz".to_string(),
                    TypeDecl::Record(vec![
                        field("a".to_string(), "int".to_string()),
                        field("b".to_string(), "int".to_string())
                    ])
                ),
            ])],
            vec![nil()]
        ),
        parse_unwrap(
            "let type foo = int type bar = array of int type baz = {a: int, b: int} in nil end"
        )
    );
    assert_eq!(
        lett(
            vec![Dec::function(vec![
                FunDec {
                    name: "foo".to_string(),
                    params: vec![
                        field("a".to_string(), "int".to_string()),
                        field("b".to_string(), "int".to_string()),
                    ],
                    result: None,
                    body: nil()
                },
                FunDec {
                    name: "bar".to_string(),
                    params: vec![],
                    result: Some(symbol("int".to_string())),
                    body: int(2)
                },
            ])],
            vec![call(
                "foo".to_string(),
                vec![
                    call("bar".to_string(), vec![]),
                    call("bar".to_string(), vec![])
                ]
            )]
        ),
        parse_unwrap(
            "let function foo(a: int, b:int) = nil function bar(): int = 2 in foo(bar(), bar()) end"
        )
    );
}
