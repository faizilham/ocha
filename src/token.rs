use value::Value;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Delimiters
    SEMICOLON, COMMA, DOT,

    // Parentheses
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    LEFT_SQUARE, RIGHT_SQUARE,

    // Arithmetics and Logics
    PLUS, MINUS, STAR, SLASH,
    AND, OR, QUESTION, COLON,
    BANG, BANG_EQUAL, 
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // Literals
    IDENTIFIER, STRING, NUMBER,
    NIL, TRUE, FALSE,

    // Keywords
    IF, ELSE, WHILE, BREAK,
    FN, RETURN, LET,
    PRINT, // temporarily, later will changed to standard lib function

    EOF
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Value,
    pub line: i32    
}

impl Token {
    pub fn new (token_type: TokenType, lexeme: String, literal: Value, line: i32) -> Token {
        Token {token_type, lexeme, literal, line}
    }
}