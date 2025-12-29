use crate::parse::ast::{Dec, Exp};
use crate::parse::lexer::lexer;
use crate::parse::parser;
use crate::semant::escape::find_escape;
use chumsky::Parser;
use chumsky::input::Stream;

fn escape_driver(input: &str) -> Exp {
    let lexer = lexer();
    let tokens = lexer
        .parse(input)
        .unwrap()
        .into_iter()
        .map(|(tok, _)| tok)
        .collect::<Vec<_>>();

    let exp = parser::exp_parser()
        .parse(Stream::from_iter(tokens.into_iter()))
        .unwrap()
        .0;
    find_escape(&exp);
    exp
}

fn is_escaping(flag: &std::rc::Rc<std::cell::RefCell<bool>>) -> bool {
    *flag.borrow()
}

#[test]
fn test_escape() {
    let code = "let var a := 5 in a + 1 end";
    let ast = escape_driver(code);
    if let Exp::Let(decs, _) = ast {
        if let Dec::Var(name, _, _, escape) = &decs[0] {
            assert_eq!(name.0.as_str(), "a");
            assert_eq!(is_escaping(escape), false);
        } else {
            panic!("Expected Var Dec");
        }
    } else {
        panic!("Expected Let Exp");
    }

    let code = "let 
                      var a := 5 
                      function f() = a + 1 
                    in f() end";
    let ast = escape_driver(code);
    if let Exp::Let(decs, _) = ast {
        if let Dec::Var(name, _, _, escape) = &decs[0] {
            assert_eq!(name.0.as_str(), "a");
            assert!(is_escaping(escape));
        } else {
            panic!("Expected Var declaration");
        }
    } else {
        panic!("Expected Let expression");
    }

    let code = "let
                      function f(x: int) = x + 1
                    in f(5) end";
    let ast = escape_driver(code);
    if let Exp::Let(decs, _) = ast {
        if let Dec::Function(funs) = &decs[0] {
            let f = &funs[0];
            let x_param = &f.params[0];
            assert_eq!(x_param.name.as_str(), "x");
            assert!(!is_escaping(&x_param.escaping));
        } else {
            panic!("Expected Function declaration");
        }
    } else {
        panic!("Expected Let expression");
    }

    let code = "let
                      function outer(x: int) =
                        let function inner() = x + 1
                        in inner() end
                    in outer(5) end";
    let ast = escape_driver(code);
    if let Exp::Let(decs, _) = ast {
        if let Dec::Function(funs) = &decs[0] {
            let outer = &funs[0];
            let x_param = &outer.params[0];

            assert_eq!(x_param.name.as_str(), "x");
            assert!(is_escaping(&x_param.escaping));
        } else {
            panic!("Expected Function declaration");
        }
    }

    let code = "for i := 0 to 10 do i + 1";
    let ast = escape_driver(code);
    if let Exp::For { var, escaping, .. } = ast {
        assert_eq!(var.as_str(), "i");
        assert!(!is_escaping(&escaping));
    } else {
        panic!("Expected For expression");
    }

    let code = "for i := 0 to 10 do
                      let function print_i() = i
                      in print_i() end";
    let ast = escape_driver(code);
    if let Exp::For { var, escaping, .. } = ast {
        assert_eq!(var.as_str(), "i");
        assert!(is_escaping(&escaping));
    } else {
        panic!("Expected For expression");
    }
}
