use token::{Token, Value};

#[derive(Debug)]
pub enum Expr {
    Binary { left: Box<Expr>, right: Box<Expr>, operator: Token },
    Grouping { expr: Box<Expr> },
    Literal { value: Value },
    Unary { operator: Token, expr: Box<Expr> },
    Ternary { condition: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr> },
    Variable { name: Token }
}