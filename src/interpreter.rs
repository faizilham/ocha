use ast::expr::{ExprVisitor, Expr};
use token::Token;
use token::TokenType::*;
use value::Value;
use value::Value::*;

use error::{error_message, print_error};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter{}
    }

    pub fn interpret(&mut self, expr: &Box<Expr>){
        let result = self.evaluate(expr);

        match result {
            Ok(value) => println!("{}", value.to_string()),
            Err(message) => print_error(&message)
        };
    }

    fn evaluate(&mut self, expr: &Box<Expr>) -> Result<Value, String> {
        Expr::accept(expr, self)
    }
}

impl ExprVisitor<Result<Value, String>> for Interpreter {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> Result<Value, String> {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;

        let value = match operator.token_type {
            // equality
            BANG_EQUAL      => Bool( !left_val.equals(&right_val) ),
            EQUAL_EQUAL     => Bool( left_val.equals(&right_val) ),
            GREATER         => Bool( left_val.ordering(&right_val)? > 0 ),
            GREATER_EQUAL   => Bool( left_val.ordering(&right_val)? >= 0 ),
            LESS            => Bool( left_val.ordering(&right_val)? < 0 ),
            LESS_EQUAL      => Bool( left_val.ordering(&right_val)? <= 0 ),
            
            STAR            => match (left_val, right_val) {
                                (Int(ref a), Int(ref b))        => Int(a * b),
                                (Float(ref a), Float(ref b))    => Float(a * b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) * b),
                                (Float(ref a), Int(ref b))      => Float(a * (*b as f64)),

                                (_, _) => return err(operator.line, "Invalid type for operator *")
                            },

            MINUS           => match (left_val, right_val) {
                                (Int(ref a), Int(ref b))        => Int(a - b),
                                (Float(ref a), Float(ref b))    => Float(a - b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) - b),
                                (Float(ref a), Int(ref b))      => Float(a - (*b as f64)),

                                (_, _) => return err(operator.line, "Invalid type for operator -")
                            },

            SLASH           => match (left_val, right_val) {
                                (Float(ref a), Float(ref b))    => Float(a / b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) / b),
                                (Float(ref a), Int(ref b))      => Float(a / (*b as f64)),
                                (Int(_), Int(0))                => return err(operator.line, "Division by zero"),
                                (Int(ref a), Int(ref b))        => Int(a / b),
                                
                                (_, _) => return err(operator.line, "Invalid type for operator /")
                            },

            PLUS            => match (left_val, right_val) {
                                (Int(ref a), Int(ref b))        => Int(a + b),
                                (Float(ref a), Float(ref b))    => Float(a - b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) + b),
                                (Float(ref a), Int(ref b))      => Float(a + (*b as f64)),
                                (Str(ref a), ref b)             => Str( format!("{}{}", a, b.to_string()) ),
                                (ref a, Str(ref b))             => Str( format!("{}{}", a.to_string(), b) ),

                                (_, _) => return err(operator.line, "Invalid type for operator +")
                            },

            _ => return err(operator.line, "Operator error")
        };

        Ok(value)
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> Result<Value, String> {
        self.evaluate(expr)
    }

    fn visit_literal(&mut self, value: &Value) -> Result<Value, String> {
        // optimize this so it doesn't need copying
        Ok(value.copy())
    }

    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> Result<Value, String> {
        let value = self.evaluate(expr)?;

        let value = match operator.token_type {
            BANG    => Bool(!value.to_bool()),
            MINUS   => match value {
                        Int(value) => Value::Int(-value),
                        Float(value) => Value::Float(-value),
                        _ => return err(operator.line, "Invalid value type for operator -")
                    },
            _       => return err(operator.line, "Operator error")
        };

        Ok(value)
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> Result<Value, String> {
        let cond_value = self.evaluate(condition)?;

        if cond_value.to_bool() {
            self.evaluate(true_branch)
        } else {
            self.evaluate(false_branch)    
        }
    }

    fn visit_variable(&mut self, name: &Token) -> Result<Value, String> {
        err(name.line, "not yet implemented")
    }
}

fn err(line : i32, message : &str) -> Result<Value, String> {
    Err(error_message(line, message))
}