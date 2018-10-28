use ast::expr::Expr;
use token::Token;

#[derive(Debug)]
pub enum Stmt {
    Assignment { name: Token, expr: Box<Expr> },
    Block { body: Vec<Box<Stmt>> },
    Break { token: Token },
    Expression { expr: Box<Expr> },
    If { condition: Box<Expr>, true_branch: Box<Stmt>, false_branch: Option<Box<Stmt>> },
    Print { exprs: Vec<Box<Expr>> },
    Set { get_expr: Box<Expr>, expr: Box<Expr> },
    VarDecl { name: Token, expr: Box<Expr> },
    While { condition: Box<Expr>, body: Box<Stmt> },
}

pub trait StmtVisitor<T> {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> T;
    fn visit_block(&mut self, body: &Vec<Box<Stmt>>) -> T;
    fn visit_break(&mut self, token: &Token) -> T;
    fn visit_expression(&mut self, expr: &Box<Expr>) -> T;
    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> T;
    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> T;
    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> T;
    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>) -> T;
    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> T;
}

impl Stmt {
    pub fn accept<T, Visitor: StmtVisitor<T>>(stmt: &Box<Stmt>, visitor: &mut Visitor) -> T {
        match stmt.as_ref() {
            Stmt::Assignment{name, expr} => visitor.visit_assignment(name, expr),
            Stmt::Block{body} => visitor.visit_block(body),
            Stmt::Break{token} => visitor.visit_break(token),
            Stmt::Expression{expr} => visitor.visit_expression(expr),
            Stmt::If{condition, true_branch, false_branch} => visitor.visit_if(condition, true_branch, false_branch),
            Stmt::Print{exprs} => visitor.visit_print(exprs),
            Stmt::Set{get_expr, expr} => visitor.visit_set(get_expr, expr),
            Stmt::VarDecl{name, expr} => visitor.visit_vardecl(name, expr),
            Stmt::While{condition, body} => visitor.visit_while(condition, body),
        }
    }
}
