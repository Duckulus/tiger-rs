use chumsky::prelude::*;
use chumsky::text::{int, whitespace};
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

pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Token>, extra::Err<Rich<'a, char>>> {
    let num = int(10).map(|s: &str| Token::INT(s.parse().unwrap()));

    let string = choice((
        just("\\").ignore_then(regex("[0-9]{3}").map(|s: &str| {
            (s.parse::<u8>().unwrap() as char).to_string()
        })), // ascii code
        just("\\")
            .ignore_then(whitespace().at_least(1))
            .ignore_then(just("\\"))
            .to("".to_string()), // whitespace continuation
        just("\\^").ignore_then(any().map(|c: char| {
            let mut c = c;
            if c.is_ascii_lowercase() {
                c = c.to_ascii_uppercase();
            }
            ((c as u8 - 64) as char).to_string()
        })), // control character
        just("\\\"").to("\"".to_string()), // \" -> "
        just("\\n").to("\n".to_string()),  // \n -> newline
        just("\\t").to("\t".to_string()),  // \t -> tab
        just("\\\\").to("\\".to_string()), // \\ -> \

        none_of('"').and_is(just("\\").not()).map(|c: char| c.to_string()),
    ))
    .repeated()
    .collect()
    .map(|v: Vec<String>| v.join(""))
    .padded_by(just('"'))
    .map(|s: String| Token::STRING(s));

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
        just("!=").to(Token::NEQ),
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

    let token = choice((num, string, ident, sign));

    // skip initial whitespace and comments
    let skip = choice((comment.clone().map(|_| ()), one_of(" \t\n\r").map(|_| ()))).repeated();

    let tokens = token
        .padded_by(comment.clone().repeated())
        .padded()
        .repeated()
        .collect();

    skip.ignore_then(tokens)
}

#[test]
pub fn test_lexer() {
    let lex = move |input: &str| -> Vec<Token> {
        let lexer = lexer();
        lexer.parse(input).unwrap()
    };

    assert_eq!(0, lex("").len());
    assert_eq!(0, lex(" ").len());
    assert_eq!(0, lex("/* Hello World */").len());
    assert_eq!(0, lex("/* Hello /* World */ */").len());

    assert_eq!(vec![Token::INT(4)], lex("4"));
    assert_eq!(vec![Token::INT(4)], lex(" 4"));
    assert_eq!(vec![Token::INT(4)], lex("4 "));
    assert_eq!(vec![Token::INT(4)], lex(" 4 "));
    assert_eq!(vec![Token::INT(4)], lex("/* Hi */ 4 /* Hello World */"));
    assert_eq!(vec![Token::MINUS, Token::INT(4)], lex("-4"));

    assert_eq!(
        vec![
            Token::IF,
            Token::LPAREN,
            Token::INT(3),
            Token::GE,
            Token::ID("x".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE
        ],
        lex("if(3>=x){}")
    );

    assert_eq!(
        vec![Token::STRING("Hello World".to_string())],
        lex("\"Hello World\"")
    );
    assert_eq!(
        vec![Token::STRING("Hello\nWorld".to_string())],
        lex("\"Hello\\nWorld\"")
    );
    assert_eq!(vec![Token::STRING("\"".to_string())], lex("\"\\\"\""));
    assert_eq!(
        vec![Token::STRING("HelloWorld".to_string())],
        lex("\"Hello\\  \\World\"")
    );
    assert_eq!(vec![Token::STRING("ABC".to_string())], lex("\"\\065\\066\\067\""));
    assert_eq!(vec![Token::STRING("\u{1}".to_string())], lex("\"\\^a\""));

    let lexer = lexer();
    let queens = "/* A program to solve the 8-queens problem */
    let var N := 8

    type intArray = array of int

    var row := intArray [ N ] of 0
    var col := intArray [ N ] of 0
    var diag1 := intArray [N+N-1] of 0
    var diag2 := intArray [N+N-1] of 0

    function printboard() =
       (for i := 0 to N-1
	 do (for j := 0 to N-1
	      do print(if col[i]=j then \" O\" else \" .\");
	     print(\"\n\"));
         print(\"\n\"))

    function try(c:int) =
( /*  for i:= 0 to c do print(\".\"); print(\"\n\"); flush();*/
     if c=N
     then printboard()
     else for r := 0 to N-1
	   do if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
	           then (row[r]:=1; diag1[r+c]:=1; diag2[r+7-c]:=1;
		         col[c]:=r;
	                 try(c+1);
			 row[r]:=0; diag1[r+c]:=0; diag2[r+7-c]:=0)

)
 in try(0)
end
	";


    assert!(!lexer.parse(queens).has_errors());
    assert!(lexer.parse("\"\\\"").has_errors());
    assert!(lexer.parse("\"").has_errors());
}
