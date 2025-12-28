use crate::parse::lexer::lexer;
use crate::parse::parser;
use crate::semant::types::Type;
use crate::semant::{TypeError, TypeErrorKind, TypedExp, trans_exp};
use chumsky::Parser;
use chumsky::input::Stream;

#[test]
pub fn test_typecheck() {
    let typecheck = move |input: &str| -> Result<TypedExp, TypeError> {
        let lexer = lexer();
        let tokens = lexer
            .parse(input)
            .unwrap()
            .into_iter()
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>();
        trans_exp(
            parser::exp_parser()
                .parse(Stream::from_iter(tokens.into_iter()))
                .unwrap(),
        )
    };
    let typecheck_unwrap = move |input: &str| -> TypedExp { typecheck(input).unwrap() };

    assert_eq!(typecheck_unwrap("\"hi\"").1, Type::String);
    assert_eq!(typecheck_unwrap("42").1, Type::Int);
    assert_eq!(typecheck_unwrap("1+1").1, Type::Int);
    assert_eq!(typecheck_unwrap("1-1").1, Type::Int);
    assert_eq!(typecheck_unwrap("let var a := 5 in a end").1, Type::Int);
    assert_eq!(
        typecheck_unwrap("let var a: int := 5 in a end").1,
        Type::Int
    );

    let exp = typecheck("let var a: string:= 5 in a end");
    assert!(matches!(
        exp,
        Err(ref e)
            if matches!(
                e.kind(),
                TypeErrorKind::TypeMismatch {
                    found: Type::Int,
                    expected: Type::String,
                }
            )
    ));

    assert_eq!(
        typecheck_unwrap("let var foo := 0 in for x := 5 to 27 do foo := foo + x end").1,
        Type::Void
    );
    assert_eq!(
        typecheck_unwrap(
            "let type list = { first: int, rest: list } var foo : list := list {} in foo.first end"
        )
        .1,
        Type::Int
    );
}
