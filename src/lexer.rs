use token::TokenType::*;

pub struct Lexer {
    start: i32,
    current: i32,
    source: String
}