use chumsky::Parser;
use crate::lexer;
use crate::lexer::Token;

#[test]
pub fn test_lexer() {
    let lex = move |input: &str| -> Vec<Token> {
        let lexer = lexer::lexer();
        lexer
            .parse(input)
            .unwrap()
            .into_iter()
            .map(|(tok, _)| tok)
            .collect()
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

    assert_eq!(vec![Token::ID("q_q".to_string())], lex("q_q"));
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
    assert_eq!(
        vec![Token::STRING("ABC".to_string())],
        lex("\"\\065\\066\\067\"")
    );
    assert_eq!(vec![Token::STRING("\u{1}".to_string())], lex("\"\\^a\""));

    let lexer = lexer::lexer();
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