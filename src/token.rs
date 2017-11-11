#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum TokenType {
    // Delimiters
    SEMICOLON, COMMA, 

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
pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
    literal: Value,
    line: i32    
}

impl<'a> Token<'a> {
    pub fn new (token_type: TokenType, lexeme: &'a str, literal: Value, line: i32) -> Token<'a> {
        Token {token_type, lexeme, literal, line}
    }
}

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Nil
}

const EPSILON : f64 = 1E-9;

impl Value {
    pub fn to_string(value : &Value) -> String {
        match *value {
            Value::Int(ref i) => format!("{}", i),
            Value::Float(ref f) => format!("{}", f),
            Value::Bool(ref b) => String::from( if *b {"true"} else {"false"} ),
            Value::Str(ref s) => s.clone(),
            Value::Nil => String::from("nil")
        }
    }

    pub fn is_truthy(value : &Value) -> bool {
        match *value {
            Value::Int(ref i) => *i != 0,
            Value::Float(ref f) => (*f).abs() < EPSILON,
            Value::Bool(ref b) => *b,
            Value::Str(ref s) => s.len() > 0,
            Value::Nil => false
        }
    }
}