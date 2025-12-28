use crate::parse::lexer::lexer;
use crate::parse::parser;
use crate::semant::types::Type;
use crate::semant::{TypeError, TypeErrorKind, TypedExp, trans_exp};
use chumsky::Parser;
use chumsky::input::Stream;

// Generated using Gemini

// Helper to reduce boilerplate in tests
fn typecheck_driver(input: &str) -> Result<TypedExp, TypeError> {
    let lexer = lexer();
    let tokens = lexer
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|(tok, _)| tok)
        .collect::<Vec<_>>();

    // Note: Assuming your parser exposes an exp_parser function
    trans_exp(
        parser::exp_parser()
            .parse(Stream::from_iter(tokens.into_iter()))
            .unwrap(),
    )
}

fn check_ok(input: &str) -> Type {
    typecheck_driver(input).unwrap().1
}

fn check_err(input: &str) -> TypeErrorKind {
    typecheck_driver(input).unwrap_err().kind()
}

#[test]
fn test_basics_and_literals() {
    assert_eq!(check_ok("nil"), Type::Nil);
    assert_eq!(check_ok("10"), Type::Int);
    assert_eq!(check_ok("\"string\""), Type::String);
    assert_eq!(check_ok("-15"), Type::Int); // Assuming parser handles unary minus
}

#[test]
fn test_arrays() {
    // - Logic for Dec::Type (Array) and Exp::Array
    let code = "
        let
            type intArray = array of int
            var row := intArray [10] of 0
        in
            row[2]
        end";
    assert_eq!(check_ok(code), Type::Int);

    // Test Array Type Mismatch on Init
    let err_code = "
        let
            type intArray = array of int
        in
            intArray [10] of \"not an int\"
        end";

    if let TypeErrorKind::TypeMismatch { expected, found } = check_err(err_code) {
        assert_eq!(expected, Type::Int);
        assert_eq!(found, Type::String);
    } else {
        panic!("Expected TypeMismatch");
    }
}

#[test]
fn test_records_validation() {
    // - Logic for Exp::Record validation

    // 1. Missing Fields
    let code_missing = "
        let
            type point = {x: int, y: int}
            var p := point {x=1}
        in p end";

    match check_err(code_missing) {
        TypeErrorKind::MissingRecordFields { missing } => {
            assert!(missing.contains(&("y".to_string()))); // Assuming Symbol wraps String
        },
        _ => panic!("Expected MissingRecordFields"),
    }

    // 2. Extra/Unexpected Fields
    let code_extra = "
        let
            type point = {x: int}
            var p := point {x=1, z=5}
        in p end";

    match check_err(code_extra) {
        TypeErrorKind::UnexpectedRecordField(sym) => {
            assert_eq!(sym, "z");
        },
        _ => panic!("Expected UnexpectedRecordField"),
    }

    // 3. Field Type Mismatch
    let code_mismatch = "
        let
            type point = {x: int}
            var p := point {x=\"wrong\"}
        in p end";

    if let TypeErrorKind::TypeMismatch { expected, found } = check_err(code_mismatch) {
        assert_eq!(expected, Type::Int);
        assert_eq!(found, Type::String);
    } else {
        panic!("Expected TypeMismatch");
    }
}

#[test]
fn test_control_flow() {
    // - Logic for Exp::If, Exp::While, Exp::Assign

    // 1. If-Then (must be void)
    // Since '()' crashes your Seq handler, we use an assignment to produce Void.
    // We wrap it in a 'let' to declare the variable first.
    let if_then_void = "
        let
            var a := 0
        in
            if 1 then a := 5
        end";
    assert_eq!(check_ok(if_then_void), Type::Void);

    // 2. If-Then mismatch (Body is int, no else)
    // In Tiger, 'if 1 then 5' is invalid because if-then (without else) must produce Void.
    let if_void_err = "if 1 then 5";
    if let TypeErrorKind::TypeMismatch { expected, found } = check_err(if_void_err) {
        assert_eq!(expected, Type::Void);
        assert_eq!(found, Type::Int);
    } else {
        panic!("Expected TypeMismatch (Void vs Int)");
    }

    // 3. If-Then-Else match
    assert_eq!(check_ok("if 1 then 10 else 20"), Type::Int);

    // 4. If-Then-Else mismatch
    let if_mismatch = "if 1 then 10 else \"fail\"";
    if let TypeErrorKind::TypeMismatch { expected, found } = check_err(if_mismatch) {
        assert_eq!(expected, Type::Int);
        assert_eq!(found, Type::String);
    } else {
        panic!("Expected TypeMismatch");
    }

    // 5. While loop (body must be void)
    // We use 'break', which your trans_exp handles as returning Type::Void
    assert_eq!(check_ok("while 1 do break"), Type::Void);

    // 6. While loop body mismatch
    // 'while' body must return Void. '10' returns Int.
    let while_err = "while 1 do 10";
    if let TypeErrorKind::TypeMismatch { expected, found } = check_err(while_err) {
        assert_eq!(expected, Type::Void);
        assert_eq!(found, Type::Int);
    } else {
        panic!("Expected TypeMismatch in while body");
    }
}

#[test]
fn test_sequences() {
    // - Logic for Exp::Seq
    // According to your implementation, Seq returns the type of the *last* expression.

    // (int; int) -> int
    assert_eq!(check_ok("(10; 20)"), Type::Int);

    // (int; string) -> string
    assert_eq!(check_ok("(10; \"end\")"), Type::String);

    // Nested sequences
    assert_eq!(check_ok("(10; (20; 30))"), Type::Int);
}

#[test]
fn test_functions_and_recursion() {
    // - Logic for Dec::Function (Mutual recursion)

    // Mutual Recursion (even/odd)
    let mutual_rec = "
        let
            function even(n: int): int = if n = 0 then 1 else odd(n-1)
            function odd(n: int): int = if n = 0 then 0 else even(n-1)
        in
            even(10)
        end";
    assert_eq!(check_ok(mutual_rec), Type::Int);

    // Argument Count Mismatch
    let arg_count = "
        let
            function add(a: int, b: int): int = a + b
        in
            add(1)
        end";

    if let TypeErrorKind::ArgCountMismatch { expected, found } = check_err(arg_count) {
        assert_eq!(expected, 2);
        assert_eq!(found, 1);
    } else {
        panic!("Expected ArgCountMismatch");
    }
}

#[test]
fn test_recursive_types_and_cycles() {
    // - Logic for Dec::Type and `is_cyclic`

    // 1. Valid Recursive Type (List)
    // This works because the Record creates a pointer indirection (next_type_id), breaking the infinite size.
    let valid_list = "
        let
            type intlist = {head: int, tail: intlist}
            var list := intlist {head=0, tail=nil}
        in
            list.tail
        end";
    match check_ok(valid_list) {
        Type::Record(_, _) => (), // Pass
        _ => panic!("Expected Record type"),
    }

    // 2. Valid Mutually Recursive Types
    // type tree = { key: int, children: treelist }
    // type treelist = { head: tree, tail: treelist }
    let valid_mutual = "
        let
            type tree = {key: int, children: treelist}
            type treelist = {head: tree, tail: treelist}
            var t := tree {key=1, children=nil}
        in
            t.key
        end";
    assert_eq!(check_ok(valid_mutual), Type::Int);

    // 3. Illegal Cycle Detection
    // This triggers the `is_cyclic` check in `trans_dec`.
    // type a = b
    // type b = a
    // This is illegal because there is no Record/Array to terminate the size calculation.
    let illegal_cycle = "
        let
            type a = b
            type b = a
        in
            0
        end";

    match check_err(illegal_cycle) {
        TypeErrorKind::IllegalCycle => (), // Pass
        e => panic!("Expected IllegalCycle, got {:?}", e),
    }

    // 4. Illegal Long Cycle
    // type a = b
    // type b = c
    // type c = a
    let long_cycle = "
        let
            type a = b
            type b = c
            type c = a
        in 0 end";
    match check_err(long_cycle) {
        TypeErrorKind::IllegalCycle => (), // Pass
        e => panic!("Expected IllegalCycle, got {:?}", e),
    }
}