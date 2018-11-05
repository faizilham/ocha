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

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Box<Stmt>>, Vec<Exception>> {
    let mut parser = ParserState::new(tokens);

    let mut statements : Vec<Box<Stmt>> = Vec::new();

    let mut errors = Vec::new();

    while !parser.at_end() {
        let stmt = declaration(&mut parser);
        match stmt {
            Ok(stmt) => statements.push(stmt),
            Err(exception) => {
                errors.push(exception);
                synchronize(&mut parser);
            }
        };
    }

    if errors.len() == 0 {
        Ok(statements)
    } else {
        Err(errors)
    }
}

fn synchronize(parser: &mut ParserState) {
    match sync(parser) {
        _ => () // do nothing
    }
}

fn sync(parser: &mut ParserState) -> Result<(), ()>{
    let mut previous = parser.advance().ok_or(())?;

    while !parser.at_end() {
        if previous.token_type == SEMICOLON {
            return Ok(());
        }

        {
            let current = parser.peek().ok_or(())?;

            match current.token_type {
                FN | LET | IF | WHILE | PRINT | RETURN => {
                    return Ok(());
                }

                _ => (),
            }
        }

        previous = parser.advance().ok_or(())?;
    }

    Ok(())
}

type StmtResult = Result<Box<Stmt>, Exception>;

// statement parsing
fn declaration(parser: &mut ParserState) -> StmtResult {
    if parser.matches(LET) {
        var_declaration(parser)
    } else if parser.matches(FN) {
        func_declaration(parser)
    } else {
        statement(parser)
    }
}

fn var_declaration(parser: &mut ParserState) -> StmtResult {
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

fn func_declaration(parser: &mut ParserState) -> StmtResult {
    let line = parser.last_line;

    let name = parser.expect(IDENTIFIER, "Expect function name")?;

    parser.expect(LEFT_PAREN, "Expect '(' after function name")?;


    // read args
    let mut args = Vec::new();

    while !parser.at_end() && !parser.peek_eq(RIGHT_PAREN) {
        let token = parser.expect(IDENTIFIER, "Expect parameter name")?;
        args.push(token);

        if !parser.matches(COMMA) {
            break;
        }
    }

    parser.expect(RIGHT_PAREN, "Expect ')' after params")?;

    // read body
    parser.expect(LEFT_BRACE, "Expect '{' after function signature")?;

    let body = read_block(parser)?;

    Ok(create_stmt(line, StmtNode::FuncDecl { name, args, body }))
}

fn statement(parser: &mut ParserState) -> StmtResult {
    if parser.matches(LEFT_BRACE) {
        block(parser)
    } else if parser.matches(PRINT) {
        print_statement(parser)
    } else if parser.matches(IF) {
        if_statement(parser)
    } else if parser.matches(WHILE) {
        while_statement(parser)
    } else if parser.matches(BREAK){
        break_statement(parser)
    } else if parser.matches(RETURN){
        return_statement(parser)
    } else {
        expr_statement(parser)
    }
}

fn read_block(parser: &mut ParserState) -> Result<Vec<Box<Stmt>>, Exception> {
    let mut body : Vec<Box<Stmt>> = Vec::new();

    while !parser.at_end() && !parser.peek_eq(RIGHT_BRACE) {
        body.push(declaration(parser)?);
    }

    parser.expect(RIGHT_BRACE, "Expect '}' at the end of block")?;

    Ok(body)
}

fn block(parser: &mut ParserState) -> StmtResult {
    let line = parser.last_line;
    let body = read_block(parser)?;

    Ok(create_stmt(line, StmtNode::Block { body }))
}

fn break_statement(parser: &mut ParserState) -> StmtResult {
    let line = parser.last_line;
    parser.expect(SEMICOLON, "Expect ';' after break")?;

    Ok(create_stmt(line, StmtNode::Break))
}

fn return_statement(parser: &mut ParserState) -> StmtResult {
    let line = parser.last_line;

    let expr = if !parser.peek_eq(SEMICOLON) {
        Some(expression(parser)?)
    } else {
        None
    };

    parser.expect(SEMICOLON, "Expect ';' after return")?;

    Ok(create_stmt(line, StmtNode::Return { expr }))
}

fn expr_statement(parser: &mut ParserState) -> StmtResult {
    let expr = expression(parser)?;

    if parser.matches(EQUAL) {
        return assignment(parser, expr);
    }

    let semicolon = parser.expect(SEMICOLON, "Expect ';' after expression")?;

    Ok(create_stmt(semicolon.line, StmtNode::Expression { expr }))
}

fn assignment(parser: &mut ParserState, variable: Box<Expr>) -> StmtResult {
    let line = parser.last_line;

    let expr = expression(parser)?;
    parser.expect(SEMICOLON, "Expect ';' after expression")?;

    match variable.node {
        ExprNode::Variable{ name } => Ok(create_stmt(line, StmtNode::Assignment{name, expr})),
        ExprNode::Get {..} => Ok(create_stmt(line, StmtNode::Set{get_expr: variable, expr})),
        _ => Err(parser.exception("Invalid assignment target"))
    }
}

fn if_statement(parser: &mut ParserState) -> StmtResult {
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

fn while_statement(parser: &mut ParserState) -> StmtResult {
    let line = parser.last_line;

    parser.expect(LEFT_PAREN, "Expect '(' after 'while'")?;
    let condition = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')' after condition")?;

    let body = statement(parser)?;
    Ok(create_stmt(line, StmtNode::While { condition, body }))
}

fn print_statement(parser: &mut ParserState) -> StmtResult {
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

type ExprResult = Result<Box<Expr>, Exception>;

fn expression(parser: &mut ParserState) -> ExprResult {
    ternary(parser)
}

fn ternary(parser: &mut ParserState) -> ExprResult {
    let mut expr = binary(parser, 0)?;

    if parser.matches(QUESTION) {
        let line = parser.last_line;

        let true_branch = expression(parser)?;
        parser.expect(COLON, "Expect ':' after true branch expression")?;
        let false_branch = expression(parser)?;

        expr = create_expr(line, ExprNode::Ternary{condition: expr, true_branch, false_branch});
    }

    Ok(expr)
}

fn binary(parser: &mut ParserState, precedence: usize) -> ExprResult {
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

fn unary(parser: &mut ParserState) -> ExprResult {
    if let Some(operator) = parser.match_all(&[BANG, MINUS]) {
        let expr = unary(parser)?;
        Ok(create_expr(operator.line, ExprNode::Unary{operator, expr}))
    } else {
        call(parser)
    }
}

fn call(parser: &mut ParserState) -> ExprResult {
    let mut expr = primary(parser)?;

    while let Some(operator) = parser.match_all(&[LEFT_SQUARE, LEFT_PAREN]) {
        expr = match operator.token_type {
            LEFT_SQUARE => finish_get(parser, expr, operator)?,
            LEFT_PAREN => finish_fncall(parser, expr)?,
            _ => unreachable!(),
        };
    }

    Ok(expr)
}

fn finish_fncall(parser: &mut ParserState, callee: Box<Expr>) -> ExprResult {
    let line = parser.last_line;
    let mut args = Vec::new();

    while !parser.at_end() && !parser.peek_eq(RIGHT_PAREN) {
        let expr = expression(parser)?;
        args.push(expr);

        if !parser.matches(COMMA) {
            break;
        }
    }

    parser.expect(RIGHT_PAREN, "Expect ')' after function call")?;


    Ok(create_expr(line, ExprNode::FuncCall{ callee, args }))
}

fn finish_get(parser: &mut ParserState, callee: Box<Expr>, operator: Token) -> ExprResult {
    let member = expression(parser)?;
    parser.expect(RIGHT_SQUARE, "Expect ']")?;
    Ok(create_expr(operator.line, ExprNode::Get {callee, operator, member}))
}

fn primary(parser: &mut ParserState) -> ExprResult {
    let token = parser.expect_advance("Expect expression")?;

    let expr = match token.token_type {
        NUMBER | NIL | TRUE | FALSE | STRING => create_expr(token.line, ExprNode::Literal { value: token.literal }),
        IDENTIFIER => create_expr(token.line, ExprNode::Variable{name: token}),
        LEFT_SQUARE => return list_init(parser),
        LEFT_PAREN => return grouping(parser),
        _ => return Err(parser.exception("Expect expression"))
    };

    Ok(expr)
}

fn grouping(parser: &mut ParserState) -> ExprResult {
    let line = parser.last_line;

    let expr = expression(parser)?;
    parser.expect(RIGHT_PAREN, "Expect ')'")?;

    Ok(create_expr(line, ExprNode::Grouping{expr}))
}

fn list_init(parser: &mut ParserState) -> ExprResult {
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
}

impl ParserState {
    fn new(tokens : Vec<Token>) -> ParserState {
        ParserState{ tokens: tokens, last_line: 1 }
    }

    fn at_end(&self) -> bool{
        self.tokens.len() == 0 || self.peek_eq(EOF)
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

    fn expect_advance(&mut self, message: &str) -> Result<Token, Exception> {
        if let Some(token) = self.advance() {
            Ok(token)
        } else {
            Err(self.exception(message))
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
