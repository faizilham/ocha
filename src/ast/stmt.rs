use ast::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
    Expression { expr: Box<Expr> },
    If { condition: Box<Expr>, true_branch: Box<Stmt>, false_branch: Option<Box<Stmt>> },
    Print { exprs: Vec<Box<Expr>> },
    While { condition: Box<Expr>, body: Box<Stmt> },
}

pub trait StmtVisitor<T> {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> T;
    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> T;
    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> T;
    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> T;
}

impl Stmt {
    pub fn accept<T, Visitor: StmtVisitor<T>>(stmt: &Box<Stmt>, visitor: &mut Visitor) -> T {
        match **stmt {
            Stmt::Expression{ref expr} => visitor.visit_expression(expr),
            Stmt::If{ref condition, ref true_branch, ref false_branch} => visitor.visit_if(condition, true_branch, false_branch),
            Stmt::Print{ref exprs} => visitor.visit_print(exprs),
            Stmt::While{ref condition, ref body} => visitor.visit_while(condition, body),
        }
    }
}
