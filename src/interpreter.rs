use std::rc::Rc;

use ast::expr::{Expr, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
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

    // pub fn interpret_expr(&mut self, expr: &Box<Expr>){
    //     let result = self.evaluate(expr);

    //     match result {
    //         Ok(value) => println!("{}", value.to_string()),
    //         Err(message) => print_error(&message)
    //     };
    // }

    pub fn interpret(&mut self, statements: &Vec<Box<Stmt>>){
        for statement in statements {
            if let Err(message) = self.execute(&statement) {
                print_error(&message);
                return;
            }
        }
    }

    fn execute(&mut self, stmt: &Box<Stmt>) -> Result<(), String>{
        Stmt::accept(stmt, self)
    }

    fn evaluate(&mut self, expr: &Box<Expr>) -> Result<Rc<Value>, String> {
        Expr::accept(expr, self)
    }
}

impl StmtVisitor<Result<(), String>> for Interpreter {
    fn visit_expression(&mut self, expr: &Box<Expr>) -> Result<(), String>{
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> Result<(), String>{
        let cond_value = self.evaluate(condition)?;

        if cond_value.to_bool() {
            self.execute(true_branch)?;
        } else if let &Some(ref else_branch) = false_branch {
            self.execute(else_branch)?;
        }

        Ok(())
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> Result<(), String>{
        let mut values : Vec<Rc<Value>> = Vec::new();

        for expr in exprs {
            let value = self.evaluate(&expr)?;
            values.push(value);
        }

        for value in values {
            print!("{} ", (*value).to_string());
        }

        println!();

        Ok(())
    }

}

impl ExprVisitor<Result<Rc<Value>, String>> for Interpreter {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> Result<Rc<Value>, String> {
        let left_val = &*self.evaluate(left)?;
        let right_val = &*self.evaluate(right)?;

        let value = match operator.token_type {
            // equality
            BANG_EQUAL      => Bool( left_val != right_val ),
            EQUAL_EQUAL     => Bool( left_val == right_val ),
            GREATER         => Bool( left_val.ordering(&right_val)? > 0 ),
            GREATER_EQUAL   => Bool( left_val.ordering(&right_val)? >= 0 ),
            LESS            => Bool( left_val.ordering(&right_val)? < 0 ),
            LESS_EQUAL      => Bool( left_val.ordering(&right_val)? <= 0 ),
            
            STAR            => match (left_val, right_val) {
                                (&Int(ref a), &Int(ref b))        => Int(a * b),
                                (&Float(ref a), &Float(ref b))    => Float(a * b),
                                (&Int(ref a), &Float(ref b))      => Float((*a as f64) * b),
                                (&Float(ref a), &Int(ref b))      => Float(a * (*b as f64)),

                                (_, _) => return err(operator.line, "Invalid type for operator *")
                            },

            MINUS           => match (left_val, right_val) {
                                (&Int(ref a), &Int(ref b))        => Int(a - b),
                                (&Float(ref a), &Float(ref b))    => Float(a - b),
                                (&Int(ref a), &Float(ref b))      => Float((*a as f64) - b),
                                (&Float(ref a), &Int(ref b))      => Float(a - (*b as f64)),

                                (_, _) => return err(operator.line, "Invalid type for operator -")
                            },

            SLASH           => match (left_val, right_val) {
                                (&Float(ref a), &Float(ref b))    => Float(a / b),
                                (&Int(ref a), &Float(ref b))      => Float((*a as f64) / b),
                                (&Float(ref a), &Int(ref b))      => Float(a / (*b as f64)),
                                (&Int(_), &Int(0))                => return err(operator.line, "Division by zero"),
                                (&Int(ref a), &Int(ref b))        => Int(a / b),
                                
                                (_, _) => return err(operator.line, "Invalid type for operator /")
                            },

            PLUS            => match (left_val, right_val) {
                                (&Int(ref a), &Int(ref b))        => Int(a + b),
                                (&Float(ref a), &Float(ref b))    => Float(a - b),
                                (&Int(ref a), &Float(ref b))      => Float((*a as f64) + b),
                                (&Float(ref a), &Int(ref b))      => Float(a + (*b as f64)),
                                (&Str(ref a), ref b)              => Str( format!("{}{}", a, b.to_string()) ),
                                (ref a, &Str(ref b))              => Str( format!("{}{}", a.to_string(), b) ),

                                (_, _) => return err(operator.line, "Invalid type for operator +")
                            },

            _ => return err(operator.line, "Operator error")
        };

        Ok(Rc::new(value))
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> Result<Rc<Value>, String> {
        self.evaluate(expr)
    }

    fn visit_literal(&mut self, value: &Rc<Value>) -> Result<Rc<Value>, String> {
        // optimize this so it doesn't need copying
        Ok(value.clone())
    }

    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> Result<Rc<Value>, String> {
        let value = &*self.evaluate(expr)?;

        let value = match operator.token_type {
            BANG    => Bool(!value.to_bool()),
            MINUS   => match value {
                        &Int(value) => Value::Int(-value),
                        &Float(value) => Value::Float(-value),
                        _ => return err(operator.line, "Invalid value type for operator -")
                    },
            _       => return err(operator.line, "Operator error")
        };

        Ok(Rc::new(value))
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> Result<Rc<Value>, String> {
        let cond_value = &*self.evaluate(condition)?;

        if cond_value.to_bool() {
            self.evaluate(true_branch)
        } else {
            self.evaluate(false_branch)    
        }
    }

    fn visit_variable(&mut self, name: &Token) -> Result<Rc<Value>, String> {
        err(name.line, "not yet implemented")
    }
}

fn err(line : i32, message : &str) -> Result<Rc<Value>, String> {
    Err(error_message(line, message))
}