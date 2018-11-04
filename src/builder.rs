use ast::expr::{Expr, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use exception::Exception;
use token::Token;
use token::TokenType::*;
use token::Literal;
use vm::Chunk;
use vm::Bytecode;

struct Builder {
    pub codes: Vec<Bytecode>,
    pub literals: Vec<Literal>,
}

pub fn build(statements: Vec<Box<Stmt>>) -> Result<Chunk, ()> {
    let mut builder = Builder::new();

    for statement in statements {
        if let Err(e) = builder.generate(&statement) {
            println!("Error: {}", e); // TODO: line error
        }
    }

    builder.emit(Bytecode::HALT);

    let Builder { codes, literals } = builder;
    Ok(Chunk { codes, literals })
}

type BuilderResult = Result<(), &'static str>;


impl Builder {
    fn new () -> Builder {
        Builder { codes: Vec::new(), literals: Vec::new() }
    }

    fn generate(&mut self, stmt: &Box<Stmt>) -> BuilderResult {
        Stmt::accept(stmt, self)
    }

    fn generate_expr(&mut self, expr: &Box<Expr>) -> BuilderResult {
        Expr::accept(expr, self)
    }

    fn emit(&mut self, bytecode : Bytecode) {
        self.codes.push(bytecode);
    }
}

impl StmtVisitor<BuilderResult> for Builder {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_break(&mut self, token: &Token) -> BuilderResult {
        unimplemented!();
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> BuilderResult {
        self.generate_expr(expr)?;
        self.emit(Bytecode::POP);

        Ok(())
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> BuilderResult {
        for expr in exprs {
            self.generate_expr(expr)?;
        }

        self.emit(Bytecode::PRINT(exprs.len()));

        Ok(())
    }

    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> BuilderResult {
        unimplemented!();
    }

}

impl ExprVisitor<BuilderResult> for Builder {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> BuilderResult {
        self.generate_expr(left)?;
        self.generate_expr(right)?;

        match operator.token_type {
            BANG_EQUAL      => self.emit(Bytecode::NEQ),
            EQUAL_EQUAL     => self.emit(Bytecode::EQ),
            GREATER         => self.emit(Bytecode::GT),
            GREATER_EQUAL   => self.emit(Bytecode::GTE),
            LESS            => self.emit(Bytecode::LT),
            LESS_EQUAL      => self.emit(Bytecode::LTE),
            STAR            => self.emit(Bytecode::MUL),
            MINUS           => self.emit(Bytecode::SUB),
            SLASH           => self.emit(Bytecode::DIV),
            PLUS            => self.emit(Bytecode::ADD),

            _ => unreachable!()
        };

        Ok(())
    }

    fn visit_get(&mut self, variable: &Box<Expr>, _: &Token, member: &Box<Expr>) -> BuilderResult {
        self.generate_expr(variable)?;
        self.generate_expr(member)?;

        self.emit(Bytecode::GET_LIST);

        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> BuilderResult {
        self.generate_expr(expr)
    }

    fn visit_literal(&mut self, value: &Literal) -> BuilderResult {
        let idx = self.literals.len();
        self.literals.push(value.clone()); //TODO: optimize this with no clone
        self.emit(Bytecode::CONST(idx));

        Ok(())
    }

    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> BuilderResult {
        for expr in exprs {
            self.generate_expr(expr)?;
        }

        self.emit(Bytecode::BUILD_LIST(exprs.len()));

        Ok(())
    }

    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> BuilderResult {
        self.generate_expr(expr)?;

        match operator.token_type {
            BANG      => self.emit(Bytecode::NOT),
            MINUS     => self.emit(Bytecode::NEG),

            _ => unreachable!()
        };

        Ok(())
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_variable(&mut self, name: &Token) -> BuilderResult {
        unimplemented!();
    }

}
