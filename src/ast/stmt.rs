use ast::expr::Expr;
use token::Token;

#[derive(Debug)]
pub struct Stmt {
    pub line: i32,
    pub node: StmtNode,
}
#[derive(Debug)]
pub enum StmtNode {
    Assignment { name: Token, expr: Box<Expr> },
    Block { body: Vec<Box<Stmt>> },
    Break,
    Expression { expr: Box<Expr> },
    FuncDecl { name: Token, args: Vec<Token>, body: Vec<Box<Stmt>> },
    If { condition: Box<Expr>, true_branch: Box<Stmt>, false_branch: Option<Box<Stmt>> },
    Print { exprs: Vec<Box<Expr>> },
    Return { expr: Option<Box<Expr>> },
    Set { get_expr: Box<Expr>, expr: Box<Expr> },
    VarDecl { name: Token, expr: Box<Expr> },
    While { condition: Box<Expr>, body: Box<Stmt> },
}

pub trait StmtVisitor<T> {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> T;
    fn visit_block(&mut self, body: &Vec<Box<Stmt>>) -> T;
    fn visit_break(&mut self) -> T;
    fn visit_expression(&mut self, expr: &Box<Expr>) -> T;
    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>) -> T;
    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> T;
    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> T;
    fn visit_return(&mut self, expr: &Option<Box<Expr>>) -> T;
    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> T;
    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>) -> T;
    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> T;
}

impl Stmt {
    pub fn new(line: i32, node: StmtNode) -> Stmt {
        Stmt{ line, node }
    }
    pub fn accept<T, Visitor: StmtVisitor<T>>(stmt: &Box<Stmt>, visitor: &mut Visitor) -> T {
        match &stmt.node {
            StmtNode::Assignment{name, expr} => visitor.visit_assignment(name, expr),
            StmtNode::Block{body} => visitor.visit_block(body),
            StmtNode::Break => visitor.visit_break(),
            StmtNode::Expression{expr} => visitor.visit_expression(expr),
            StmtNode::FuncDecl{name, args, body} => visitor.visit_funcdecl(name, args, body),
            StmtNode::If{condition, true_branch, false_branch} => visitor.visit_if(condition, true_branch, false_branch),
            StmtNode::Print{exprs} => visitor.visit_print(exprs),
            StmtNode::Return{expr} => visitor.visit_return(expr),
            StmtNode::Set{get_expr, expr} => visitor.visit_set(get_expr, expr),
            StmtNode::VarDecl{name, expr} => visitor.visit_vardecl(name, expr),
            StmtNode::While{condition, body} => visitor.visit_while(condition, body),
        }
    }
}
