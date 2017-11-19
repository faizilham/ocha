use exception::Exception;
use exception::Exception::ParseErr;
use std::rc::Rc;
use ast::expr::Expr;
use ast::stmt::Stmt;

use token::{TokenType, Token};
use token::TokenType::*;

// use EOF as padding
static BINARY_PRECEDENCE: [[TokenType; 4]; 4] = [
    [BANG_EQUAL, EQUAL_EQUAL, EOF, EOF],
    [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL],
    [MINUS, PLUS, EOF, EOF],
    [SLASH, STAR, EOF, EOF]
];

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Box<Stmt>>, ()> {
    let mut parser = ParserState::new(tokens);

    let mut statements : Vec<Box<Stmt>> = Vec::new();

    let mut has_error = false;

    while !parser.matches(EOF) {
        let stmt = statement(&mut parser);
        match stmt {
            Ok(stmt) => statements.push(stmt),
            Err(exception) => {
                exception.print();
                has_error = true;
                // include error synchronization here
            }
        };
    }

    if !has_error {
        Ok(statements)
    } else {
        Err(())
    }
}

// statement parsing

fn statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    if parser.matches(LEFT_BRACE) {
        block_statement(parser)
    } else if parser.matches(PRINT) {
        print_statement(parser)
    } else if parser.matches(IF) {
        if_statement(parser)
    } else if parser.matches(WHILE) {
        while_statement(parser)
    } else {
        expr_statement(parser)
    }
}

fn block_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let mut body : Vec<Box<Stmt>> = Vec::new();

    while !parser.at_end() && !parser.peek_eq(RIGHT_BRACE) {
        body.push(statement(parser)?);
    }

    parser.expect(RIGHT_BRACE, "Expect '}' at the end of block")?;

    Ok(Box::new(Stmt::Block { body }))
}

fn expr_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let expr = expression(parser)?;

    parser.expect(SEMICOLON, "Expect ';' after expression")?;

    Ok(Box::new(Stmt::Expression { expr }))
}

fn if_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    parser.expect(LEFT_PAREN, "Expect '(' after 'if'")?;
    let condition = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')' after condition")?;

    let true_branch = statement(parser)?;
    
    let false_branch = if parser.matches(ELSE) {
        Some(statement(parser)?)
    } else {
        None
    };

    Ok(Box::new(Stmt::If { condition, true_branch, false_branch }))
}

fn while_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    parser.expect(LEFT_PAREN, "Expect '(' after 'while'")?;
    let condition = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')' after condition")?;

    let body = statement(parser)?;
    Ok(Box::new(Stmt::While { condition, body }))    
}

fn print_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    parser.expect(LEFT_PAREN, "Expect '(' after 'print'")?;

    let mut exprs : Vec<Box<Expr>> = Vec::new();

    while !parser.at_end() && !parser.peek_eq(RIGHT_PAREN) {
        let expr = expression(parser)?;

        exprs.push(expr);

        if !parser.matches(COMMA) {
            break;
        }
    }

    parser.expect(RIGHT_PAREN, "Expect ')' after expression")?;
    parser.expect(SEMICOLON, "Expect ';' after print")?;

    Ok(Box::new(Stmt::Print { exprs }))
}

// expression parsing

fn expression(parser: &mut ParserState) -> Result<Box<Expr>, Exception>{
    ternary(parser)
}

fn ternary(parser: &mut ParserState) -> Result<Box<Expr>, Exception> {
    let mut expr = binary(parser, 0)?;

    if parser.matches(QUESTION) {
        let true_branch = binary(parser, 0)?;
        parser.expect(COLON, "Expect ':' after true branch expression")?;
        let false_branch = binary(parser, 0)?;

        expr = Box::new(Expr::Ternary{condition: expr, true_branch, false_branch});
    }

    Ok(expr)
}

fn binary(parser: &mut ParserState, precedence: usize) -> Result<Box<Expr>, Exception>{
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

fn unary(parser: &mut ParserState) -> Result<Box<Expr>, Exception>{
    if let Some(operator) = parser.match_all(&[BANG, MINUS]) {
        let expr = unary(parser)?;
        Ok(Box::new(Expr::Unary{operator, expr}))
    } else {
        primary(parser)
    }
}

fn primary(parser: &mut ParserState) -> Result<Box<Expr>, Exception>{
    let token = parser.advance().unwrap();
    let expr = match token.token_type {
        NUMBER | NIL | TRUE | FALSE | STRING => Expr::Literal { value: Rc::new(token.literal) },
        IDENTIFIER => Expr::Variable{name: token},
        LEFT_PAREN => return grouping(parser),
        _ => return Err(parser.error("Expect expression"))
    };

    Ok(Box::new(expr))
}

fn grouping(parser: &mut ParserState) -> Result<Box<Expr>, Exception> {
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

    fn get_matches(&mut self, token_type: TokenType) -> Option<Token> {
        if self.peek_eq(token_type) {
            self.advance()
        } else {
            None
        }
    }

    fn matches(&mut self, token_type: TokenType) -> bool {
        if let Some(_) = self.get_matches(token_type) {
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token_type: TokenType, message: &str) -> Result<Token, Exception>{
        match self.get_matches(token_type) {
            Some(token) => Ok(token),
            None => Err(self.error(message))
        }
    }

    fn error(&self, message: &str) -> Exception{
        ParseErr(self.last_line, String::from(message))
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