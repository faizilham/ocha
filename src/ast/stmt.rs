use ast::expr::Expr;

#[derive(Debug)]
pub enum Stmt {
    Expression { expr: Box<Expr> },
    Print { exprs: Vec<Box<Expr>> },
}

pub trait StmtVisitor<T> {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> T;
    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> T;
}

impl Stmt {
    pub fn accept<T, Visitor: StmtVisitor<T>>(stmt: &Box<Stmt>, visitor: &mut Visitor) -> T {
        match **stmt {
            Stmt::Expression{ref expr} => visitor.visit_expression(expr),
            Stmt::Print{ref exprs} => visitor.visit_print(exprs),
        }
    }
}
