use ast::expr::{Expr, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use environment::Environment;
use exception::Exception;
use exception::Exception::RuntimeErr;
use exception::Exception::BreakException;
use heap::Heap;
use token::Token;
use token::TokenType::*;
use token::Literal;
use value::Value;
use value::Value::*;
use value::list::VecList;

pub struct Interpreter {
    env: Environment,
    heap: Heap
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter{ env: Environment::new(), heap: Heap::new() }
    }

    // pub fn interpret_expr(&mut self, expr: &Box<Expr>){
    //     let result = self.evaluate(expr);

    //     match result {
    //         Ok(value) => println!("{}", value.to_string()),
    //         Err(exception) => exception.print()
    //     };
    // }

    pub fn interpret(&mut self, statements: &Vec<Box<Stmt>>) -> Result<(), ()>{
        for statement in statements {
            if let Err(exception) = self.execute(&statement) {
                exception.print();

                self.heap.sweep();
                return Err(());
            }
        }

        self.heap.sweep();
        Ok(())
    }

    fn execute(&mut self, stmt: &Box<Stmt>) -> Result<(), Exception>{
        Stmt::accept(stmt, self)
    }

    fn evaluate(&mut self, expr: &Box<Expr>) -> Result<Value, Exception> {
        Expr::accept(expr, self)
    }
}

impl StmtVisitor<Result<(), Exception>> for Interpreter {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> Result<(), Exception>{
        self.env.check_declared(name)?;

        let value = self.evaluate(expr)?;
        self.env.put(name, value);

        Ok(())
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>) -> Result<(), Exception>{
        for statement in body {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn visit_break(&mut self, _: &Token) -> Result<(), Exception>{
        Err(BreakException)
    }


    fn visit_expression(&mut self, expr: &Box<Expr>) -> Result<(), Exception>{
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> Result<(), Exception>{
        let cond_value = self.evaluate(condition)?;

        if cond_value.is_truthy() {
            self.execute(true_branch)?;
        } else if let &Some(ref else_branch) = false_branch {
            self.execute(else_branch)?;
        }

        Ok(())
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> Result<(), Exception>{
        let mut values : Vec<Value> = Vec::new();

        for expr in exprs {
            let value = self.evaluate(&expr)?;
            values.push(value);
        }

        for value in values {
            print!("{} ", value.to_string());
        }

        println!();

        Ok(())
    }

    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> Result<(), Exception> {
        if let Expr::Get {ref variable, ref operator, ref member } = **get_expr {
            if let List(ref lref) = self.evaluate(variable)? {
                if let Int(index) = self.evaluate(member)? {
                    let value = self.evaluate(expr)?;
                    let list = lref.get_ref();

                    match list.put(index, value) {
                        Ok(_) => return Ok(()),
                        Err(message) => return err_stmt(operator.line, message)
                    }
                } else {
                    return err_stmt(operator.line, "Invalid member type for get operator")
                }
            }

            err_stmt(operator.line, "Invalid container type for get operator")
        } else {
            // it should not get here
            err_stmt(0, "Set statement error")
        }
    }

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>) -> Result<(), Exception>{
        let value = self.evaluate(expr)?;
        self.env.put(name, value);

        Ok(())
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> Result<(), Exception>{
        loop {
            let cond_value = self.evaluate(condition)?;
            if !cond_value.is_truthy() { break; }

            match self.execute(body) {
                Err(BreakException) => break,
                Err(e) => return Err(e),
                _ => ()
            }
        }

        Ok(())
    }
}

impl ExprVisitor<Result<Value, Exception>> for Interpreter {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> Result<Value, Exception> {
        let left_val = self.evaluate(left)?;
        let right_val = self.evaluate(right)?;
        let line = operator.line;

        let value = match operator.token_type {
            // equality
            BANG_EQUAL      => Bool( left_val != right_val ),
            EQUAL_EQUAL     => Bool( left_val == right_val ),
            GREATER         => Bool( order_value(line, &left_val, &right_val)? > 0 ),
            GREATER_EQUAL   => Bool( order_value(line, &left_val, &right_val)? >= 0 ),
            LESS            => Bool( order_value(line, &left_val, &right_val)? < 0 ),
            LESS_EQUAL      => Bool( order_value(line, &left_val, &right_val)? <= 0 ),

            STAR            => match (left_val, right_val) {
                                (Int(ref a), Int(ref b))        => Int(a * b),
                                (Float(ref a), Float(ref b))    => Float(a * b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) * b),
                                (Float(ref a), Int(ref b))      => Float(a * (*b as f64)),

                                (_, _) => return err(line, "Invalid type for operator *")
                            },

            MINUS           => match (left_val, right_val) {
                                (Int(ref a), Int(ref b))        => Int(a - b),
                                (Float(ref a), Float(ref b))    => Float(a - b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) - b),
                                (Float(ref a), Int(ref b))      => Float(a - (*b as f64)),

                                (_, _) => return err(line, "Invalid type for operator -")
                            },

            SLASH           => match (left_val, right_val) {
                                (Float(ref a), Float(ref b))    => Float(a / b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) / b),
                                (Float(ref a), Int(ref b))      => Float(a / (*b as f64)),
                                (Int(_), Int(0))                => return err(line, "Division by zero"),
                                (Int(ref a), Int(ref b))        => Int(a / b),

                                (_, _) => return err(line, "Invalid type for operator /")
                            },

            PLUS            => match (left_val, right_val) {
                                // math addition
                                (Int(ref a), Int(ref b))        => Int(a + b),
                                (Float(ref a), Float(ref b))    => Float(a - b),
                                (Int(ref a), Float(ref b))      => Float((*a as f64) + b),
                                (Float(ref a), Int(ref b))      => Float(a + (*b as f64)),

                                // string addition
                                (ref a, ref b) => {
                                    if is_string(a) || is_string(b) {
                                        let s = format!("{}{}", a.to_string(), b.to_string());
                                        self.heap.allocate_str(s)
                                    } else {
                                        return err(line, "Invalid type for operator +")
                                    }
                                }
                            },

            _ => return err(line, "Operator error")
        };

        Ok(value)
    }

    fn visit_get(&mut self, variable: &Box<Expr>, operator: &Token, member: &Box<Expr>) -> Result<Value, Exception> {

        if let List(ref lref) = self.evaluate(variable)? {
            if let Int(index) = self.evaluate(member)? {
                let list = lref.get_ref();
                match list.get(index) {
                    Ok(value) => return Ok(value),
                    Err(message) => return err(operator.line, message)
                }
            } else {
                return err(operator.line, "Invalid member type for get operator")
            }
        }

        err(operator.line, "Invalid container type for get operator")
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> Result<Value, Exception> {
        self.evaluate(expr)
    }

    fn visit_literal(&mut self, value: &Literal) -> Result<Value, Exception> {
        let val = match value {
            Literal::Nil => Nil,
            Literal::Int(i) => Int(*i),
            Literal::Float(f) => Float(*f),
            Literal::Bool(b) => Bool(*b),
            Literal::Str(s) => self.heap.allocate_str(s.clone()),
        };

        Ok(val)
    }

    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> Result<Value, Exception> {
        let list = VecList::new();

        for expr in exprs {
            let value = self.evaluate(expr)?;
            list.push(value);
        }

        Ok(self.heap.allocate_list(list))
    }

    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> Result<Value, Exception> {
        let value = self.evaluate(expr)?;

        let value = match operator.token_type {
            BANG    => Bool(!value.is_truthy()),
            MINUS   => match value {
                        Int(value) => Value::Int(-value),
                        Float(value) => Value::Float(-value),
                        _ => return err(operator.line, "Invalid value type for operator -")
                    },
            _       => return err(operator.line, "Operator error")
        };

        Ok(value)
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> Result<Value, Exception> {
        let cond_value = self.evaluate(condition)?;

        if cond_value.is_truthy() {
            self.evaluate(true_branch)
        } else {
            self.evaluate(false_branch)
        }
    }

    fn visit_variable(&mut self, name: &Token) -> Result<Value, Exception> {
        self.env.get(name)
    }
}

fn order_value (line: i32, left_val: &Value, right_val: &Value) -> Result<i32, Exception> {
    left_val.ordering(right_val).map_err(|message| {
        RuntimeErr(line, String::from(message))
    })
}

fn is_string(value: &Value) -> bool {
    if let Str(_) = value {
        true
    } else {
        false
    }
}

fn err(line : i32, message : &str) -> Result<Value, Exception> {
    Err(RuntimeErr(line, String::from(message)))
}

fn err_stmt(line: i32, message: &str) -> Result<(), Exception> {
    Err(RuntimeErr(line, String::from(message)))
}
