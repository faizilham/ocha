use token::{TokenType, Token};
use token::TokenType::*;
use expr::Expr;
use error::error_message;

// use EOF as padding
static BINARY_PRECEDENCE: [[TokenType; 4]; 4] = [
    [BANG_EQUAL, EQUAL_EQUAL, EOF, EOF],
    [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL],
    [MINUS, PLUS, EOF, EOF],
    [SLASH, STAR, EOF, EOF]
];

pub fn parse(tokens: Vec<Token>) -> Result<Box<Expr>, String> {
    let mut parser = ParserState::new(tokens);

    expression(&mut parser)
}

fn expression(parser: &mut ParserState) -> Result<Box<Expr>, String>{
    ternary(parser)
}

fn ternary(parser: &mut ParserState) -> Result<Box<Expr>, String> {
    let mut expr = binary(parser, 0)?;

    if let Some(_) = parser.matches(QUESTION) {
        let true_branch = binary(parser, 0)?;
        parser.expect(COLON, "Expect ':' after true branch expression")?;
        let false_branch = binary(parser, 0)?;

        expr = Box::new(Expr::Ternary{condition: expr, true_branch, false_branch});
    }

    Ok(expr)
}

fn binary(parser: &mut ParserState, precedence: usize) -> Result<Box<Expr>, String>{
    if precedence >= BINARY_PRECEDENCE.len() {
        return unary(parser);
    }

    let mut expr = binary(parser, precedence + 1)?;

    while let Some(operator) = parser.match_all(&BINARY_PRECEDENCE[precedence]) {
        let right = binary(parser, precedence + 1)?;
        expr = Box::new(Expr::Binary{left: expr, right, operator: operator});
    }

    Ok(expr)
}

fn unary(parser: &mut ParserState) -> Result<Box<Expr>, String>{
    if let Some(operator) = parser.match_all(&[BANG, MINUS]) {
        let expr = primary(parser)?;
        Ok(Box::new(Expr::Unary{operator, expr}))
    } else {
        primary(parser)
    }
}

fn primary(parser: &mut ParserState) -> Result<Box<Expr>, String>{
    let token = parser.advance().unwrap();
    let expr = match token.token_type {
        NUMBER | NIL | TRUE | FALSE | STRING => Expr::Literal { value: token.literal },
        IDENTIFIER => Expr::Variable{name: token},
        LEFT_PAREN => return grouping(parser),
        _ => return Err(error_message(parser.last_line, "Expect expression"))
    };

    Ok(Box::new(expr))
}

fn grouping(parser: &mut ParserState) -> Result<Box<Expr>, String> {
    let expr = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')'")?;

    Ok(Box::new(Expr::Grouping{expr}))
}

struct ParserState {
    tokens: Vec<Token>,
    last_line: i32
}

impl ParserState {
    fn new(tokens : Vec<Token>) -> ParserState {
        ParserState{tokens: tokens, last_line: 1}
    }

    fn at_end(&self) -> bool{
        self.tokens.len() == 0
    }

    fn peek(&self) -> Option<&Token>{
        self.tokens.get(0)
    }

    fn peek_eq(&self, token_type: TokenType) -> bool {
        let token = self.peek();
        match token {
            Some(token) => (*token).token_type == token_type,
            None => false,
        }
    }

    fn advance(&mut self) -> Option<Token> {
        if !self.at_end() {
            let token = self.tokens.remove(0);
            self.last_line = token.line;

            Some(token)
        } else {
            None
        }
    }

    fn matches(&mut self, token_type: TokenType) -> Option<Token>{
        if self.peek_eq(token_type) {
            self.advance()
        } else {
            None
        }
    }

    fn expect(&mut self, token_type: TokenType, message: &str) -> Result<Token, String>{
        match self.matches(token_type) {
            Some(token) => Ok(token),
            None => Err(error_message(self.last_line, message))
        }
    }

    fn match_all(&mut self, token_types: &[TokenType]) -> Option<Token> {
        for token_type in token_types {
            if *token_type == EOF {
                break;
            } else if self.peek_eq(*token_type){
                return self.advance();
            }
        }

        None
    }
}