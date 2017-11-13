use token::{TokenType, Token};
use expr::Expr;
use error::error_message;

pub fn parse(tokens: Vec<Token>) -> Result<Box<Expr>, String> {
    let mut parser = ParserState::new(tokens);

    expression(&mut parser)
}

fn expression(parser: &mut ParserState) -> Result<Box<Expr>, String>{
    ternary(parser)
}

fn ternary(parser: &mut ParserState) -> Result<Box<Expr>, String> {
    let mut expr = binary(parser)?;

    if let Some(_) = parser.matches(TokenType::QUESTION) {
        let true_branch = binary(parser)?;
        parser.expect(TokenType::COLON, "Expect ':' after true branch expression")?;
        let false_branch = binary(parser)?;

        expr = Box::new(Expr::Ternary{condition: expr, true_branch, false_branch});
    }

    Ok(expr)
}

fn binary(parser: &mut ParserState) -> Result<Box<Expr>, String>{
    let mut expr = primary(parser)?;
    while let Some(operator) = parser.matches(TokenType::PLUS) {
        let right = primary(parser)?;

        expr = Box::new(Expr::Binary{left: expr, right, operator: operator});
    }

    Ok(expr)
}

fn primary(parser: &mut ParserState) -> Result<Box<Expr>, String>{
    let token = parser.advance().unwrap();
    let expr = match token.token_type {
        TokenType::NUMBER | TokenType::NIL | TokenType::TRUE | TokenType::FALSE | TokenType::STRING =>
            Box::new(Expr::Literal { value: token.literal } ),
        
        _ => return Err(error_message(parser.last_line, "Expect expression"))
    };

    Ok(expr)
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

    fn match_all(&mut self, token_types: Vec<TokenType>) -> Option<Token> {
        for token_type in token_types {
            if (self.peek_eq(token_type)){
                return self.advance();
            }
        }

        None
    }
}