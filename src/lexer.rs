use token::{Token, Value, TokenType};
use token::TokenType::*;

pub fn scan(source_string : String) -> Vec<Token> {
    let source = source_string.chars().collect();
    let mut tokens = Vec::new();
    let mut lexer = LexerState::new(&source);

    while !lexer.at_end() {
        lexer.start = lexer.current;
        scan_token(&mut lexer, &mut tokens);
    }

    tokens
}

fn scan_token(lexer : &mut LexerState, tokens : &mut Vec<Token>) -> Result<(), String>{
    let c = lexer.advance();

    let token = match c {
        // delimiters
        ';' =>  lexer.create_token(SEMICOLON),
        ',' =>  lexer.create_token(COMMA),
        '.' =>  lexer.create_token(DOT),

        // parentheses
        '(' =>  lexer.create_token(LEFT_PAREN),
        ')' =>  lexer.create_token(RIGHT_PAREN),
        '{' =>  lexer.create_token(LEFT_BRACE),
        '}' =>  lexer.create_token(RIGHT_BRACE),
        '[' =>  lexer.create_token(LEFT_SQUARE),
        ']' =>  lexer.create_token(RIGHT_SQUARE),

        // logics & arithmatics 
        '!' =>  if lexer.matches('=') { lexer.create_token(BANG_EQUAL) }
                else { lexer.create_token(BANG) },
        '=' =>  if lexer.matches('=') { lexer.create_token(EQUAL_EQUAL) }
                else { lexer.create_token(EQUAL) },
        '>' =>  if lexer.matches('=') { lexer.create_token(GREATER_EQUAL) }
                else { lexer.create_token(GREATER) },
        '<' =>  if lexer.matches('=') { lexer.create_token(LESS_EQUAL) }
                else { lexer.create_token(LESS) },

        '?' =>  lexer.create_token(QUESTION),
        ':' =>  lexer.create_token(COLON),

        '+' =>  lexer.create_token(PLUS),
        '-' =>  lexer.create_token(MINUS),
        '*' =>  lexer.create_token(STAR),

        '/' =>  if lexer.matches('/') {
                    while !lexer.matches('\n') { // comments
                        lexer.advance();
                    };
                    return Ok(());
                } else {
                    lexer.create_token(SLASH)
                },

        '"' =>  string(lexer),
        _ => return Err(format!("Unknown token {}", c)),
    }?;

    tokens.push(token);
    
    Ok(())
}

fn string(lexer : &mut LexerState) -> Result<Token, String>{
    while !lexer.at_end() && lexer.peek() != '"' {
        lexer.advance();
    }

    lexer.expect('"')?;

    let value = lexer.get_lexeme(lexer.start + 1, lexer.current - 1);
    lexer.create_literal(STRING, Value::Str(value))
}

struct LexerState<'a> {
    start: usize,
    current: usize,
    line: i32,
    source: &'a Vec<char>
}

impl<'a> LexerState<'a> {
    fn new (source : &'a Vec<char>) -> LexerState<'a>{
        return LexerState { start: 0, current: 0, line: 1, source};
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.previous()
    }

    fn matches(&mut self, expected : char) -> bool {
        if (self.peek_eq(expected)){
            self.current += 1;
            true
        } else {
            false
        }
    }

    fn expect(&mut self, expected : char) -> Result<(), String>{
        if (self.matches(expected)){
            Ok(())
        } else {
            Err(format!("Expected token {}", expected))
        }
    }

    fn at_end(&self) -> bool{
        self.current >= self.source.len()
    }

    fn peek(&self) -> char {
        if let Some(c) = self.source.get(self.current) {
            *c
        } else {
            '\0'
        }
    }

    fn peek_eq(&self, expected : char) -> bool {
        self.peek() == expected
    }

    fn previous(&self) -> char {
        if let Some(c) = self.source.get(self.current - 1) {
            *c
        } else {
            '\0'
        }
    }

    fn create_token(&self, token_type: TokenType) -> Result<Token, String> {
        let lexeme = self.get_lexeme(self.start, self.current);
        
        Ok(Token::new(token_type, lexeme, Value::Nil, self.line))
    }

    fn create_literal(&self, token_type: TokenType, literal: Value) -> Result<Token, String> {
        let lexeme = self.get_lexeme(self.start, self.current);

        Ok(Token::new(token_type, lexeme, literal, self.line))
    }

    fn get_lexeme(&self, start: usize, end: usize) -> String {
        self.source.iter()
            .skip(start)
            .take(end - start)
            .clone()            
            .collect()
    }
}