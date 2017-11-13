use token::{Token, Value};

#[derive(Debug)]
pub enum Expr {
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Grouping { expr: Box<Expr> },
    Literal { value: Value },
    Unary { operator: Token, expr: Box<Expr> },
    Ternary { condition: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr> },
    Variable { name: Token },
}

pub trait ExprVisitor<T> {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> T;
    fn visit_grouping(&mut self, expr: &Box<Expr>) -> T;
    fn visit_literal(&mut self, value: &Value) -> T;
    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> T;
    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> T;
    fn visit_variable(&mut self, name: &Token) -> T;
}

impl Expr {
    pub fn accept<T, Visitor: ExprVisitor<T>>(expr: &Box<Expr>, visitor: &mut Visitor) -> T {
        match **expr {
            Expr::Binary{ref left, ref operator, ref right} => visitor.visit_binary(left, operator, right),
            Expr::Grouping{ref expr} => visitor.visit_grouping(expr),
            Expr::Literal{ref value} => visitor.visit_literal(value),
            Expr::Unary{ref operator, ref expr} => visitor.visit_unary(operator, expr),
            Expr::Ternary{ref condition, ref true_branch, ref false_branch} => visitor.visit_ternary(condition, true_branch, false_branch),
            Expr::Variable{ref name} => visitor.visit_variable(name),
        }
    }
}
