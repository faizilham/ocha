use token::{Token, Value, TokenType};
use token::TokenType::*;


pub struct Lexer {
    source: Vec<char>
}

struct LexerState {
    start: usize,
    current: usize,
    line: i32
}

impl Lexer {
    pub fn new (source_string : String) -> Lexer {
        let source = source_string.chars().collect();

        Lexer{source}
    }

    pub fn scan(&mut self) -> Vec<Token>{
        let mut tokens = Vec::new();
        let mut state = LexerState {start: 0, current: 0, line: 1};

        while !self.at_end(&state){
            let (token, next_state) = self.scan_token(state);
            state = next_state;
            if let Some(token) = token {
                tokens.push(token);
            }
        }

        tokens
    }

    fn scan_token(&self, mut state : LexerState) -> (Option<Token>, LexerState){
        state.start = state.current;        
        let c = self.advance(&mut state);

        let token = match *c {
            // delimiters
            ';' => self.create_token(&state, SEMICOLON),
            ',' => self.create_token(&state, COMMA),

            // parentheses
            '(' => self.create_token(&state, LEFT_PAREN),
            ')' => self.create_token(&state, RIGHT_PAREN),
            '{' => self.create_token(&state, LEFT_BRACE),
            '}' => self.create_token(&state, RIGHT_BRACE),
            '[' => self.create_token(&state, LEFT_SQUARE),
            ']' => self.create_token(&state, RIGHT_SQUARE),

            // logics & arithmatics 
            '!' => self.create_token(&state, if self.peek_eq(&state, '=') { BANG_EQUAL } else { BANG }),
            '=' => self.create_token(&state, if self.peek_eq(&state, '=') { EQUAL_EQUAL } else { EQUAL }),
            '>' => self.create_token(&state, if self.peek_eq(&state, '=') { GREATER_EQUAL } else { GREATER }),
            '<' => self.create_token(&state, if self.peek_eq(&state, '=') { LESS_EQUAL } else { LESS }),
            '+' => self.create_token(&state, PLUS),
            '-' => self.create_token(&state, MINUS),
            '*' => self.create_token(&state, STAR),
            '/' =>  if self.peek_eq(&state, '/') {
                        while !self.peek_eq(&state, '=') { // comments
                            self.advance(&mut state);
                        };

                        return (None, state);
                    } else {
                        self.create_token(&state, SLASH)
                    },

            _ => self.create_token(&state, NIL), // to be deleted
        };

        (Some(token), state)
    }

    fn advance(&self, state: &mut LexerState) -> &char{
        state.current += 1;
        self.source.get(state.current - 1).unwrap()
    }

    fn peek(&self, state: &LexerState) -> Option<&char> {
        if self.at_end(state) {
            None
        } else {
            Some(self.source.get(state.current).unwrap())
        }
    }

    fn peek_eq(&self, state: &LexerState, expected: char) -> bool {
        match self.peek(state) {
            Some(x) => *x == expected,
            None => false
        }
    }

    fn at_end(&self, state: &LexerState) -> bool {
        state.current >= self.source.len()
    }

    fn matches(&self, state: &mut LexerState, expected: char) -> bool {
        if self.peek_eq(state, expected) {
            state.current += 1;
            true
        } else {
            false
        }
    }

    fn create_token(&self, state: &LexerState, token_type: TokenType) -> Token {
        let lexeme = self.get_lexeme(state.start, state.current);
        
        Token::new(token_type, lexeme, Value::Nil, state.line)
    }

    fn create_literal(&self, state: &LexerState, token_type: TokenType, literal: Value) -> Token {
        let lexeme = self.get_lexeme(state.start, state.current);

        Token::new(token_type, lexeme, literal, state.line)
    }

    fn get_lexeme(&self, start: usize, end: usize) -> String {
        self.source.iter()
            .skip(start)
            .take(end - start)
            .clone()            
            .collect()
    }
}