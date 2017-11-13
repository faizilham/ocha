use token::{Token, Value, TokenType};
use token::TokenType::*;

use error::report_error;

pub fn scan(source_string : String) -> Result<Vec<Token>, String> {
    let source = source_string.chars().collect();
    let mut tokens = Vec::new();
    let mut lexer = LexerState::new(&source);
    let mut has_error = false;

    while !lexer.at_end() {
        lexer.start = lexer.current;
        if let Err(message) = scan_token(&mut lexer, &mut tokens) {
            report_error(lexer.line, &message);
            has_error = true;
        }
    }

    if !has_error {
        tokens.push(Token::new(EOF, String::new(), Value::Nil, lexer.line));
        Ok(tokens)
    } else {
        Err(format!("stopped because of lexer error"))
    }
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
                    return Err(format!("Unknown token {}", c));
                }
    }?;

    tokens.push(token);
    Ok(())
}

fn string(lexer : &mut LexerState) -> Result<Token, String>{
    let mut escape_backslash = false;
    let mut value = String::new();

    while !lexer.at_end() {
        let c = lexer.advance();

        match (escape_backslash, c) {
            (false, '"') => break,
            (false, '\\') => escape_backslash = true,
            (false, c) => value.push(c),
            (true, c) => if let Some(decoded) = decode(c) {
                            value.push(decoded);
                            escape_backslash = false;
                        } else {
                            value.push(c);
                            escape_backslash = false;
                        }
        };        
    }

    if lexer.previous() != '"' {
        return Err(format!("Expect '\"' after string"));
    }

    lexer.create_literal(STRING, Value::Str(value))
}

fn number(lexer : &mut LexerState) -> Result<Token, String>{
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

fn identifier(lexer : &mut LexerState) -> Result<Token, String>{
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

fn consume_line_comment(lexer : &mut LexerState) -> Result<(), String> {
    while !lexer.matches('\n') { // comments
        lexer.advance();
    };
    lexer.line += 1;

    Ok(())
}

fn consume_multiline_comment(lexer : &mut LexerState) -> Result<(), String> {
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

    Err(format!("Expect '*/' at the end of multiline comment"))
}

fn consume_whitespace(lexer : &mut LexerState) -> Result<(), String> {
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

    fn create_token(&self, token_type: TokenType) -> Result<Token, String> {
        let lexeme = self.get_current_lexeme();
        
        Ok(Token::new(token_type, lexeme, Value::Nil, self.line))
    }

    fn create_literal(&self, token_type: TokenType, literal: Value) -> Result<Token, String> {
        let lexeme = self.get_current_lexeme();

        Ok(Token::new(token_type, lexeme, literal, self.line))
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

fn decode(code: char) -> Option<char> {
    let decoded = match code {
        '\"' => '\"',
        '\\' => '\\',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        _ => return None
    };

    Some(decoded)
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