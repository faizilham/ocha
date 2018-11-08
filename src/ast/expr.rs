use token::Token;
use program_data::Literal;
use resolver::ResolverData;

#[derive(Debug)]
pub struct Expr {
    pub line: i32,
    pub node: ExprNode,
}
#[derive(Debug)]
pub enum ExprNode {
    Binary { left: Box<Expr>, operator: Token, right: Box<Expr> },
    FuncCall { callee: Box<Expr>, args: Vec<Box<Expr>> },
    Get { callee: Box<Expr>, operator: Token, member: Box<Expr> },
    Grouping { expr: Box<Expr> },
    Literal { value: Literal },
    ListInit { exprs: Vec<Box<Expr>> },
    Unary { operator: Token, expr: Box<Expr> },
    Ternary { condition: Box<Expr>, true_branch: Box<Expr>, false_branch: Box<Expr> },
    Variable { name: Token, resolve: ResolverData },
}

pub trait ExprVisitor<T> {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> T;
    fn visit_funccall(&mut self, callee: &Box<Expr>, args: &Vec<Box<Expr>>) -> T;
    fn visit_get(&mut self, callee: &Box<Expr>, operator: &Token, member: &Box<Expr>) -> T;
    fn visit_grouping(&mut self, expr: &Box<Expr>) -> T;
    fn visit_literal(&mut self, value: &Literal) -> T;
    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> T;
    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> T;
    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> T;
    fn visit_variable(&mut self, name: &Token, resolve: &ResolverData) -> T;
}

impl Expr {
    pub fn new(line: i32, node: ExprNode) -> Expr {
        Expr{ line, node }
    }
    pub fn accept<T, Visitor: ExprVisitor<T>>(expr: &Box<Expr>, visitor: &mut Visitor) -> T {
        match &expr.node {
            ExprNode::Binary{left, operator, right} => visitor.visit_binary(left, operator, right),
            ExprNode::FuncCall{callee, args} => visitor.visit_funccall(callee, args),
            ExprNode::Get{callee, operator, member} => visitor.visit_get(callee, operator, member),
            ExprNode::Grouping{expr} => visitor.visit_grouping(expr),
            ExprNode::Literal{value} => visitor.visit_literal(value),
            ExprNode::ListInit{exprs} => visitor.visit_listinit(exprs),
            ExprNode::Unary{operator, expr} => visitor.visit_unary(operator, expr),
            ExprNode::Ternary{condition, true_branch, false_branch} => visitor.visit_ternary(condition, true_branch, false_branch),
            ExprNode::Variable{name, resolve} => visitor.visit_variable(name, resolve),
        }
    }
}
