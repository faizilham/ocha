use ast::expr::{ExprVisitor, Expr};
use token::Token;
use value::Value;

pub struct AstPrinter {}

impl AstPrinter {
    pub fn evaluate(&mut self, expr: &Box<Expr>) -> String{
        Expr::accept(expr, self)
    }
}

impl ExprVisitor<String> for AstPrinter{
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> String {
        let leftval = self.evaluate(left);
        let rightval = self.evaluate(right);
        format!("({} {} {})", operator.lexeme.clone(), leftval, rightval)
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> String {
        let value = self.evaluate(expr);
        format!("(group {})", value)
    }

    fn visit_literal(&mut self, value: &Value) -> String {
        value.to_string()
    }

    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> String {
        let value = self.evaluate(expr);
        format!("({} {})", operator.lexeme.clone(), value)
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> String {
        let condval = self.evaluate(condition);
        let trueval = self.evaluate(true_branch);
        let falseval = self.evaluate(false_branch);
        format!("(:? {} {} {})", condval, trueval, falseval)
    }

    fn visit_variable(&mut self, name: &Token) -> String {
        name.lexeme.clone()
    }
}
