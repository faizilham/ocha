use std::rc::Rc;

use exception::Exception;
use exception::Exception::ParseErr;
use token::{Token, TokenType};
use token::TokenType::*;
use value::Value;

pub fn scan(source_string : String) -> Result<Vec<Token>, ()> {
    let mut tokens = Vec::new();
    let mut lexer = LexerState::new(&source_string);
    let mut has_error = false;

    while !lexer.at_end() {
        lexer.start = lexer.current;
        if let Err(exception) = scan_token(&mut lexer, &mut tokens) {
            exception.print();
            has_error = true;
        }
    }

    if !has_error {
        tokens.push(Token::new(EOF, String::new(), Value::Nil, lexer.line));
        Ok(tokens)
    } else {
        Err(())
    }
}

fn scan_token(lexer : &mut LexerState, tokens : &mut Vec<Token>) -> Result<(), Exception>{
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
                    return consume_line_comment(lexer);
                } else if lexer.matches('*') {
                    return consume_multiline_comment(lexer);
                } else {
                    lexer.create_token(SLASH)
                },

        '"' =>  string(lexer),
        _   =>  if is_numeric(c) {
                    number(lexer)
                } else if is_id_start(c) {
                    identifier(lexer)
                } else if is_whitespace(c) {
                    return consume_whitespace(lexer);
                } else {
                    return Err(lexer.error(&format!("Unexpected token {}", c)));
                }
    }?;

    tokens.push(token);
    Ok(())
}

fn string(lexer : &mut LexerState) -> Result<Token, Exception>{
    let mut escape_backslash = false;
    let mut value = String::new();

    while !lexer.at_end() {
        let c = lexer.advance();

        match (escape_backslash, c) {
            (false, '"') => break,
            (false, '\\') => escape_backslash = true,
            (false, c) => value.push(c),
            (true, c) => {
                value.push(decode(c));
                escape_backslash = false;
            }
        };
    }

    if lexer.previous() != '"' {
        return Err(lexer.error("Expect '\"' after string"));
    }

    lexer.create_literal(STRING, Value::Str(Rc::new(value)))
}

fn number(lexer : &mut LexerState) -> Result<Token, Exception>{
    let mut is_integer = true;

    while is_numeric(lexer.peek()) {
        lexer.advance();
    }

    if lexer.peek_eq('.') && is_numeric(lexer.peek_next()) {
        lexer.advance(); // consume '.'

        while is_numeric(lexer.peek()) {
            lexer.advance();
        }

        is_integer = false;
    }

    let value = lexer.get_current_lexeme();

    if is_integer {
        let value : i64 = value.parse().unwrap();
        lexer.create_literal(NUMBER, Value::Int(value))
    } else {
        let value : f64 = value.parse().unwrap();
        lexer.create_literal(NUMBER, Value::Float(value))
    }
}

fn identifier(lexer : &mut LexerState) -> Result<Token, Exception>{
    while is_id_part(lexer.peek()){
        lexer.advance();
    }

    let lexeme = lexer.get_current_lexeme();

    let token_type = find_keywords(&lexeme).unwrap_or(IDENTIFIER);

    match token_type {
        TRUE => lexer.create_literal(TRUE, Value::Bool(true)),
        FALSE => lexer.create_literal(FALSE, Value::Bool(false)),
        _ => lexer.create_token(token_type)
    }
}

fn consume_line_comment(lexer : &mut LexerState) -> Result<(), Exception> {
    while !lexer.matches('\n') { // comments
        lexer.advance();
    };
    lexer.line += 1;

    Ok(())
}

fn consume_multiline_comment(lexer : &mut LexerState) -> Result<(), Exception> {
    let mut prev_star = false;
    while !lexer.at_end(){
        let c = lexer.advance();

        match c {
            '*' =>  prev_star = true,
            '/' =>  if !prev_star { prev_star = false; }
                    else { return Ok(()) },
            _   =>  {
                        if c == '\n' { lexer.line += 1; }
                        prev_star = false;
                    }
        }
    }

    Err(lexer.error("Expect '*/' at the end of multiline comment"))
}

fn consume_whitespace(lexer : &mut LexerState) -> Result<(), Exception> {
    if lexer.previous() == '\n' {
        lexer.line += 1;
    }

    while is_whitespace(lexer.peek()){
        if lexer.advance() == '\n' {
            lexer.line += 1;
        }
    }

    Ok(())
}

struct LexerState {
    start: usize,
    current: usize,
    line: i32,
    source: Vec<char>
}

impl LexerState {
    fn new (source_string : &String) -> LexerState{
        let source = source_string.chars().collect();
        return LexerState { start: 0, current: 0, line: 1, source};
    }

    fn advance(&mut self) -> char {
        if !self.at_end() { self.current += 1; }
        self.previous()
    }

    fn matches(&mut self, expected : char) -> bool {
        if self.peek_eq(expected) {
            self.current += 1;
            true
        } else {
            false
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

    fn peek_next(&self) -> char {
        if let Some(c) = self.source.get(self.current + 1) {
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

    fn create_token(&self, token_type: TokenType) -> Result<Token, Exception> {
        let lexeme = self.get_current_lexeme();

        Ok(Token::new(token_type, lexeme, Value::Nil, self.line))
    }

    fn create_literal(&self, token_type: TokenType, literal: Value) -> Result<Token, Exception> {
        let lexeme = self.get_current_lexeme();

        Ok(Token::new(token_type, lexeme, literal, self.line))
    }

    fn error(&self, message: &str) -> Exception {
        ParseErr ( self.line, String::from(message) )
    }

    fn get_current_lexeme(&self) -> String {
        self.get_lexeme(self.start, self.current)
    }

    fn get_lexeme(&self, start: usize, end: usize) -> String {
        self.source.iter()
            .skip(start)
            .take(end - start)
            .clone()
            .collect()
    }
}

fn decode(code: char) -> char {
    match code {
        '\"' => '\"',
        '\\' => '\\',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        _ => code
    }
}

fn find_keywords(lexeme : &str) -> Option<TokenType>{
    let token_type = match lexeme {
        "nil" => NIL,
        "true" => TRUE,
        "false" => FALSE,
        "if" => IF,
        "else" => ELSE,
        "while" => WHILE,
        "break" => BREAK,
        "fn" => FN,
        "return" => RETURN,
        "let" => LET,
        "and" => AND,
        "or" => OR,
        "print" => PRINT, // temporarily
        _ => return None
    };

    Some(token_type)
}

fn is_whitespace(c : char) -> bool {
    c == ' ' || c == '\n' || c == '\r' || c == '\t'
}

fn is_numeric(c : char) -> bool {
    c >= '0' && c <= '9'
}

fn is_id_start(c : char) -> bool {
    c == '_' || ( c >= 'A' && c <= 'Z' ) || ( c >= 'a' && c <= 'z' )
}

fn is_id_part(c : char) -> bool {
    is_id_start(c) || is_numeric(c)
}
