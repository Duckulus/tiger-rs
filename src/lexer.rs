#![allow(unused)]

use chumsky::prelude::*;
use chumsky::Parser;

#[derive(Clone, Debug)]
enum Token {
    ID(String),
    STRING(String), INT(i32),
    COMMA, COLON, SEMICOLON,
    LPAREN, RPAREN, LBRACK, RBRACK, LBRACE, RBRACE,
    DOT, PLUS, MINUS, TIMES, DIVIDE, EQ, NEQ, LT, LE, GT, GE,
    AND, OR,
    ASSIGN,
    ARRAY, IF, THEN, ELSE, WHILE, FOR, TO, DO, LET, IN, END, OF, BREAK, NIL, FUNCTION, VAR, TYPE
}

type Span = SimpleSpan;
type Spanned<T> = (T, Span);

fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char, Span>>> {
    let num = text::int(10).map(|s: &str| Token::INT(s.parse().unwrap()));

    let string = just('"').
        ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(|s: &str| {
            Token::STRING(s
                .replace("\\t", "\t")
                .to_string())
        });

    let ident = regex("[a-zA-Z][a-zA-Z0-9_]*").map(|id: &str| match id {
        "array" => Token::ARRAY,
        "if" => Token::IF,
        "then" => Token::THEN,
        "else" => Token::ELSE,
        "while" => Token::WHILE,
        "for" => Token::FOR,
        "to" => Token::TO,
        "do" => Token::DO,
        "let" => Token::LET,
        "in" => Token::IN,
        "end" => Token::END,
        "of" => Token::OF,
        "break" => Token::BREAK,
        "nil" => Token::NIL,
        "function" => Token::FUNCTION,
        "var" => Token::VAR,
        "type" => Token::TYPE,
        _ => Token::ID(id.to_string())
    });

    let sign = choice((
        just(",").to(Token::COMMA),
        just(":").to(Token::COLON),
        just(";").to(Token::SEMICOLON),
        just("(").to(Token::LPAREN),
        just(")").to(Token::RPAREN),
        just("[").to(Token::LBRACK),
        just("]").to(Token::RBRACK),
        just("{").to(Token::LBRACE),
        just("}").to(Token::RBRACE),
        just(".").to(Token::DOT),
        just("+").to(Token::PLUS),
        just("-").to(Token::MINUS),
        just("*").to(Token::TIMES),
        just("/").to(Token::DIVIDE),
        just("==").to(Token::EQ),
        just("!=").to(Token::NEQ),
        just("<=").to(Token::LE),
        just("<").to(Token::LT),
        just(">=").to(Token::GE),
        just(">").to(Token::GT),
        just("&&").to(Token::AND),
        just("||").to(Token::OR),
        just("=").to(Token::ASSIGN),
    ));

    let comment = just("/*").then(any().and_is(just("*/").not()).repeated()).then(just("*/")).padded();
    // let comment = recursive(|com: Recursive<dyn Parser<'_, &'a str, (), chumsky::extra::Full<chumsky::error::Rich<'a, char>, (), ()>>>| {
    //     com.padded().delimited_by(just("/*"), just("*/")).padded()
    // });

    let token = num.or(string).or(ident).or(sign);
    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .collect()
}

fn lex(input: &str) -> Vec<Spanned<Token>> {
    let lexer = lexer();
    lexer.parse(input).unwrap()
}

#[test]
pub fn test_lex() {
    dbg!(lex("3 4 5 /* 6 /* 7 */ 8 */ 9"));
}
