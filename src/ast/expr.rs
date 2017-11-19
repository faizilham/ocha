use std::rc::Rc;
use token::Token;
use value::Value;

#[derive(Debug)]
pub enum Expr {
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    Get { variable: Box<Expr>, operator: Token, member: Box<Expr> },
    Grouping { expr: Box<Expr> },
    Literal { value: Rc<Value> },
    ListInit { exprs: Vec<Box<Expr>> },
    Unary { operator: Token, expr: Box<Expr> },
    Ternary { condition: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr> },
    Variable { name: Token },
}

pub trait ExprVisitor<T> {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> T;
    fn visit_get(&mut self, variable: &Box<Expr>, operator: &Token, member: &Box<Expr>) -> T;
    fn visit_grouping(&mut self, expr: &Box<Expr>) -> T;
    fn visit_literal(&mut self, value: &Rc<Value>) -> T;
    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> T;
    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> T;
    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> T;
    fn visit_variable(&mut self, name: &Token) -> T;
}

impl Expr {
    pub fn accept<T, Visitor: ExprVisitor<T>>(expr: &Box<Expr>, visitor: &mut Visitor) -> T {
        match **expr {
            Expr::Binary{ref left, ref operator, ref right} => visitor.visit_binary(left, operator, right),
            Expr::Get{ref variable, ref operator, ref member} => visitor.visit_get(variable, operator, member),
            Expr::Grouping{ref expr} => visitor.visit_grouping(expr),
            Expr::Literal{ref value} => visitor.visit_literal(value),
            Expr::ListInit{ref exprs} => visitor.visit_listinit(exprs),
            Expr::Unary{ref operator, ref expr} => visitor.visit_unary(operator, expr),
            Expr::Ternary{ref condition, ref true_branch, ref false_branch} => visitor.visit_ternary(condition, true_branch, false_branch),
            Expr::Variable{ref name} => visitor.visit_variable(name),
        }
    }
}
