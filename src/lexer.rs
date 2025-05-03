use std::fmt;
use chumsky::prelude::*;
use chumsky::text::whitespace;
use chumsky::Parser;

#[rustfmt::skip]
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    ID(String),
    STRING(String), INT(i32),
    COMMA, COLON, SEMICOLON,
    LPAREN, RPAREN, LBRACK, RBRACK, LBRACE, RBRACE,
    DOT, PLUS, MINUS, TIMES, DIVIDE,
    EQ, NEQ, LT, LE, GT, GE, AND, OR,
    ASSIGN,
    ARRAY, IF, THEN, ELSE, WHILE, FOR, TO, DO, LET, IN, END, OF, BREAK, NIL, FUNCTION, VAR, TYPE,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Values
            Token::ID(s) => write!(f, "{}", s),
            Token::STRING(s) => write!(f, "\"{}\"", s),
            Token::INT(n) => write!(f, "{}", n),

            // Punctuation
            Token::COMMA => write!(f, ","),
            Token::COLON => write!(f, ":"),
            Token::SEMICOLON => write!(f, ";"),
            Token::LPAREN => write!(f, "("),
            Token::RPAREN => write!(f, ")"),
            Token::LBRACK => write!(f, "["),
            Token::RBRACK => write!(f, "]"),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),
            Token::DOT => write!(f, "."),

            // Operators
            Token::PLUS => write!(f, "+"),
            Token::MINUS => write!(f, "-"),
            Token::TIMES => write!(f, "*"),
            Token::DIVIDE => write!(f, "/"),
            Token::EQ => write!(f, "="),
            Token::NEQ => write!(f, "<>"),
            Token::LT => write!(f, "<"),
            Token::LE => write!(f, "<="),
            Token::GT => write!(f, ">"),
            Token::GE => write!(f, ">="),
            Token::AND => write!(f, "&"),
            Token::OR => write!(f, "|"),
            Token::ASSIGN => write!(f, ":="),

            // Keywords
            Token::ARRAY => write!(f, "array"),
            Token::IF => write!(f, "if"),
            Token::THEN => write!(f, "then"),
            Token::ELSE => write!(f, "else"),
            Token::WHILE => write!(f, "while"),
            Token::FOR => write!(f, "for"),
            Token::TO => write!(f, "to"),
            Token::DO => write!(f, "do"),
            Token::LET => write!(f, "let"),
            Token::IN => write!(f, "in"),
            Token::END => write!(f, "end"),
            Token::OF => write!(f, "of"),
            Token::BREAK => write!(f, "break"),
            Token::NIL => write!(f, "nil"),
            Token::FUNCTION => write!(f, "function"),
            Token::VAR => write!(f, "var"),
            Token::TYPE => write!(f, "type"),
        }
    }
}


fn string<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, char>>> {
    let escape = just("\\").ignore_then(choice((
        regex("[0-9]{3}")
            .try_map_with(|s: &str, extra| {
                s.parse::<u8>()
                    .map_err(|_| Rich::custom(extra.span(), "Ascii code out of range"))
            })
            .map(|i| (i as char).to_string()), // ascii code
        whitespace()
            .at_least(1)
            .ignore_then(just("\\"))
            .to("".to_string()), // whitespace continuation
        just("^").ignore_then(any().map(|c: char| {
            let mut c = c;
            if c.is_ascii_lowercase() {
                c = c.to_ascii_uppercase();
            }
            ((c as u8 - 64) as char).to_string()
        })), // control character
        just("\"").to("\"".to_string()), // \" -> "
        just("n").to("\n".to_string()),  // \n -> newline
        just("t").to("\t".to_string()),  // \t -> tab
        just("\\").to("\\".to_string()),
    )));
    escape
        .or(none_of("\"\\").map(|c: char| c.to_string()))
        .repeated()
        .collect()
        .map(|v: Vec<String>| Token::STRING(v.join("")))
        .delimited_by(just('\"'), just('\"'))
}

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> {
    let num = regex("[0-9]+")
        .try_map_with(|s: &str, extra| s.parse().map_err(|e| Rich::custom(extra.span(), e)))
        .map(Token::INT);

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
        _ => Token::ID(id.to_string()),
    });

    let sign = choice((
        just(":=").to(Token::ASSIGN),
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
        just("=").to(Token::EQ),
        just("<>").to(Token::NEQ),
        just("<=").to(Token::LE),
        just("<").to(Token::LT),
        just(">=").to(Token::GE),
        just(">").to(Token::GT),
        just("&").to(Token::AND),
        just("|").to(Token::OR),
    ));

    let comment = recursive(|com| {
        com.repeated()
            .padded_by(
                any()
                    .and_is(just("/*").not())
                    .and_is(just("*/").not())
                    .repeated(),
            )
            .delimited_by(just("/*"), just("*/"))
    })
    .padded();

    let token = choice((num, string(), ident, sign));

    // skip initial whitespace and comments
    let skip = choice((comment.clone(), one_of(" \t\n\r").map(|_| ()))).repeated();

    let tokens = token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect();

    skip.ignore_then(tokens)
}
