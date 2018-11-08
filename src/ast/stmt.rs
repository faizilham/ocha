use ast::expr::Expr;
use token::Token;
use helper::PCell;
use resolver::ResolverData;

#[derive(Debug)]
pub struct Stmt {
    pub line: i32,
    pub node: StmtNode,
}
#[derive(Debug)]
pub enum StmtNode {
    Assignment { name: Token, expr: Box<Expr>, resolve: ResolverData },
    Block { body: Vec<Box<Stmt>>, has_captured: PCell<bool>, num_vars: PCell<usize> },
    Break,
    Expression { expr: Box<Expr> },
    FuncDecl { name: Token, args: Vec<Token>, body: Vec<Box<Stmt>>, id: PCell<usize>, has_captured: PCell<bool> },
    If { condition: Box<Expr>, true_branch: Box<Stmt>, false_branch: Option<Box<Stmt>> },
    Print { exprs: Vec<Box<Expr>> },
    Return { expr: Option<Box<Expr>> },
    Set { get_expr: Box<Expr>, expr: Box<Expr> },
    VarDecl { name: Token, expr: Box<Expr>, is_captured: PCell<bool> },
    While { condition: Box<Expr>, body: Box<Stmt> },
}

pub trait StmtVisitor<T> {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>, resolve: &ResolverData) -> T;
    fn visit_block(&mut self, body: &Vec<Box<Stmt>>, has_captured: &PCell<bool>, num_vars: &PCell<usize>) -> T;
    fn visit_break(&mut self) -> T;
    fn visit_expression(&mut self, expr: &Box<Expr>) -> T;
    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>, id: &PCell<usize>, has_captured: &PCell<bool>) -> T;
    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> T;
    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> T;
    fn visit_return(&mut self, expr: &Option<Box<Expr>>) -> T;
    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> T;
    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>, is_captured: &PCell<bool>) -> T;
    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> T;
}

impl Stmt {
    pub fn new(line: i32, node: StmtNode) -> Stmt {
        Stmt{ line, node }
    }
    pub fn accept<T, Visitor: StmtVisitor<T>>(stmt: &Box<Stmt>, visitor: &mut Visitor) -> T {
        match &stmt.node {
            StmtNode::Assignment{name, expr, resolve} => visitor.visit_assignment(name, expr, resolve),
            StmtNode::Block{body, has_captured, num_vars} => visitor.visit_block(body, has_captured, num_vars),
            StmtNode::Break => visitor.visit_break(),
            StmtNode::Expression{expr} => visitor.visit_expression(expr),
            StmtNode::FuncDecl{name, args, body, id, has_captured} => visitor.visit_funcdecl(name, args, body, id, has_captured),
            StmtNode::If{condition, true_branch, false_branch} => visitor.visit_if(condition, true_branch, false_branch),
            StmtNode::Print{exprs} => visitor.visit_print(exprs),
            StmtNode::Return{expr} => visitor.visit_return(expr),
            StmtNode::Set{get_expr, expr} => visitor.visit_set(get_expr, expr),
            StmtNode::VarDecl{name, expr, is_captured} => visitor.visit_vardecl(name, expr, is_captured),
            StmtNode::While{condition, body} => visitor.visit_while(condition, body),
        }
    }
}
