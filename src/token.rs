#[allow(non_camel_case_types)]
#[derive(Debug)]
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

use std::f64::EPSILON;

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Nil
}

impl Value {
    pub fn to_string(&self) -> String {
        match *self {
            Value::Int(ref i) => format!("{}", i),
            Value::Float(ref f) => format!("{}", f),
            Value::Bool(ref b) => String::from( if *b {"true"} else {"false"} ),
            Value::Str(ref s) => s.clone(),
            Value::Nil => String::from("nil")
        }
    }

    pub fn to_bool(&self) -> bool {
        match *self {
            Value::Int(ref i) => *i != 0,
            Value::Float(ref f) => (*f).abs() < EPSILON,
            Value::Bool(ref b) => *b,
            Value::Str(ref s) => s.len() > 0,
            Value::Nil => false
        }
    }
}