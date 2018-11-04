use ast::expr::{Expr, ExprNode};
use ast::stmt::{Stmt, StmtNode};
use exception::Exception;
use exception::Exception::ParseErr;
use token::{TokenType, Token};
use token::TokenType::*;
use token::Literal;

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
        let stmt = declaration(&mut parser);
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
fn declaration(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    if parser.matches(LET) {
        var_declaration(parser)
    } else {
        statement(parser)
    }
}

fn var_declaration(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let line = parser.last_line;

    let name = parser.expect(IDENTIFIER, "Expect identifier name")?;

    let expr = if parser.matches(EQUAL) {
        expression(parser)?
    } else {
        create_expr(line, ExprNode::Literal { value: Literal::Nil })
    };

    parser.expect(SEMICOLON, "Expect ';' after variable declaration")?;

    Ok(create_stmt(line, StmtNode::VarDecl { name, expr }))
}

fn statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    if parser.matches(LEFT_BRACE) {
        block(parser)
    } else if parser.matches(PRINT) {
        print_statement(parser)
    } else if parser.matches(IF) {
        if_statement(parser)
    } else if parser.matches(WHILE) {
        while_statement(parser)
    } else if parser.peek_eq(BREAK){
        break_statement(parser)
    } else {
        expr_statement(parser)
    }
}

fn block(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let line = parser.last_line;
    let mut body : Vec<Box<Stmt>> = Vec::new();

    while !parser.at_end() && !parser.peek_eq(RIGHT_BRACE) {
        body.push(declaration(parser)?);
    }

    parser.expect(RIGHT_BRACE, "Expect '}' at the end of block")?;

    Ok(create_stmt(line, StmtNode::Block { body }))
}

fn break_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let token = parser.advance().unwrap();
    parser.expect(SEMICOLON, "Expect ';' after break")?;

    if parser.loop_level == 0 {
        return Err(parser.exception("Invalid break outside of loop"));
    }

    Ok(create_stmt(token.line, StmtNode::Break { token }))
}

fn expr_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let expr = expression(parser)?;

    if parser.matches(EQUAL) {
        return assignment(parser, expr);
    }

    let semicolon = parser.expect(SEMICOLON, "Expect ';' after expression")?;

    Ok(create_stmt(semicolon.line, StmtNode::Expression { expr }))
}

fn assignment(parser: &mut ParserState, variable: Box<Expr>) -> Result<Box<Stmt>, Exception> {
    let line = parser.last_line;

    let expr = expression(parser)?;
    parser.expect(SEMICOLON, "Expect ';' after expression")?;

    match variable.node {
        ExprNode::Variable{ name } => Ok(create_stmt(line, StmtNode::Assignment{name, expr})),
        ExprNode::Get {..} => Ok(create_stmt(line, StmtNode::Set{get_expr: variable, expr})),
        _ => Err(parser.exception("Invalid assignment target"))
    }
}

fn if_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let line = parser.last_line;

    parser.expect(LEFT_PAREN, "Expect '(' after 'if'")?;
    let condition = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')' after condition")?;

    let true_branch = statement(parser)?;

    let false_branch = if parser.matches(ELSE) {
        Some(statement(parser)?)
    } else {
        None
    };

    Ok(create_stmt(line, StmtNode::If { condition, true_branch, false_branch }))
}

fn while_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let line = parser.last_line;

    parser.expect(LEFT_PAREN, "Expect '(' after 'while'")?;
    let condition = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')' after condition")?;

    parser.loop_level += 1;
    let body = statement(parser)?;
    parser.loop_level -= 1;
    Ok(create_stmt(line, StmtNode::While { condition, body }))
}

fn print_statement(parser: &mut ParserState) -> Result<Box<Stmt>, Exception> {
    let line = parser.last_line;

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

    Ok(create_stmt(line, StmtNode::Print { exprs }))
}

// expression parsing

fn expression(parser: &mut ParserState) -> Result<Box<Expr>, Exception>{
    ternary(parser)
}

fn ternary(parser: &mut ParserState) -> Result<Box<Expr>, Exception> {
    // TODO: fix this bug
    // 1 ? 2 : 3 ? 4 : 5 wont compile, but
    // 1 ? 2 : (3 ? 4 : 5) compiles

    let mut expr = binary(parser, 0)?;

    if parser.matches(QUESTION) {
        let line = parser.last_line;

        let true_branch = binary(parser, 0)?;
        parser.expect(COLON, "Expect ':' after true branch expression")?;
        let false_branch = binary(parser, 0)?;

        expr = create_expr(line, ExprNode::Ternary{condition: expr, true_branch, false_branch});
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
        expr = create_expr(operator.line, ExprNode::Binary{left: expr, right, operator: operator});
    }

    Ok(expr)
}

fn unary(parser: &mut ParserState) -> Result<Box<Expr>, Exception>{
    if let Some(operator) = parser.match_all(&[BANG, MINUS]) {
        let expr = unary(parser)?;
        Ok(create_expr(operator.line, ExprNode::Unary{operator, expr}))
    } else {
        call(parser)
    }
}

fn call(parser: &mut ParserState) -> Result<Box<Expr>, Exception>{
    let mut expr = primary(parser)?;

    while let Some(operator) = parser.get_matches(LEFT_SQUARE) {
        let member = expression(parser)?;
        parser.expect(RIGHT_SQUARE, "Expect ']")?;
        expr = create_expr(operator.line, ExprNode::Get {variable: expr, operator, member});
    }

    Ok(expr)
}

fn primary(parser: &mut ParserState) -> Result<Box<Expr>, Exception>{
    let token = parser.advance().unwrap();
    let expr = match token.token_type {
        NUMBER | NIL | TRUE | FALSE | STRING => create_expr(token.line, ExprNode::Literal { value: token.literal }),
        IDENTIFIER => create_expr(token.line, ExprNode::Variable{name: token}),
        LEFT_SQUARE => return list_init(parser),
        LEFT_PAREN => return grouping(parser),
        _ => return Err(parser.exception("Expect expression"))
    };

    Ok(expr)
}

fn grouping(parser: &mut ParserState) -> Result<Box<Expr>, Exception> {
    let line = parser.last_line;

    let expr = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')'")?;

    Ok(create_expr(line, ExprNode::Grouping{expr}))
}

fn list_init(parser: &mut ParserState) -> Result<Box<Expr>, Exception> {
    let line = parser.last_line;

    let mut exprs : Vec<Box<Expr>> = Vec::new();

    while !parser.at_end() && !parser.peek_eq(RIGHT_SQUARE) {
        let expr = expression(parser)?;

        exprs.push(expr);

        if !parser.matches(COMMA) {
            break;
        }
    }

    parser.expect(RIGHT_SQUARE, "Expect ']' after list expression")?;

    Ok(create_expr(line, ExprNode::ListInit {exprs}))
}

fn create_stmt(line: i32, node: StmtNode) -> Box<Stmt> {
    Box::new(Stmt::new(line, node))
}

fn create_expr(line: i32, node: ExprNode) -> Box<Expr> {
    Box::new(Expr::new(line, node))
}

struct ParserState {
    tokens: Vec<Token>,
    last_line: i32,
    loop_level: i32
}

impl ParserState {
    fn new(tokens : Vec<Token>) -> ParserState {
        ParserState{tokens: tokens, last_line: 1, loop_level: 0}
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
            None => Err(self.exception(message))
        }
    }

    fn exception(&self, message: &str) -> Exception{
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
