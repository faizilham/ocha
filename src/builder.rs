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
    last_line: i32,
}

pub fn build(statements: Vec<Box<Stmt>>) -> Result<Chunk, ()> {
    let mut builder = Builder::new();

    for statement in statements {
        if let Err(e) = builder.generate(&statement) {
            e.print();
        }
    }

    let line = builder.last_line;
    builder.emit(line, Bytecode::HALT);

    let Builder { codes, literals, .. } = builder;
    Ok(Chunk { codes, literals })
}

type BuilderResult = Result<(), Exception>;


impl Builder {
    fn new () -> Builder {
        Builder { codes: Vec::new(), literals: Vec::new(), last_line: 0 }
    }

    fn generate(&mut self, stmt: &Box<Stmt>) -> BuilderResult {
        self.last_line = stmt.line;
        Stmt::accept(stmt, self)
    }

    fn generate_expr(&mut self, expr: &Box<Expr>) -> BuilderResult {
        self.last_line = expr.line;
        Expr::accept(expr, self)
    }

    fn emit(&mut self, line: i32, bytecode : Bytecode) {
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
        let line = self.last_line;
        self.generate_expr(expr)?;
        self.emit(line, Bytecode::POP);

        Ok(())
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> BuilderResult {
        let line = self.last_line;

        for expr in exprs {
            self.generate_expr(expr)?;
        }

        self.emit(line, Bytecode::PRINT(exprs.len()));

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
        let line = self.last_line;

        self.generate_expr(left)?;
        self.generate_expr(right)?;

        match operator.token_type {
            BANG_EQUAL      => self.emit(line, Bytecode::NEQ),
            EQUAL_EQUAL     => self.emit(line, Bytecode::EQ),
            GREATER         => self.emit(line, Bytecode::GT),
            GREATER_EQUAL   => self.emit(line, Bytecode::GTE),
            LESS            => self.emit(line, Bytecode::LT),
            LESS_EQUAL      => self.emit(line, Bytecode::LTE),
            STAR            => self.emit(line, Bytecode::MUL),
            MINUS           => self.emit(line, Bytecode::SUB),
            SLASH           => self.emit(line, Bytecode::DIV),
            PLUS            => self.emit(line, Bytecode::ADD),

            _ => unreachable!()
        };

        Ok(())
    }

    fn visit_get(&mut self, variable: &Box<Expr>, _: &Token, member: &Box<Expr>) -> BuilderResult {
        let line = self.last_line;

        self.generate_expr(variable)?;
        self.generate_expr(member)?;

        self.emit(line, Bytecode::GET_LIST);

        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> BuilderResult {
        self.generate_expr(expr)
    }

    fn visit_literal(&mut self, value: &Literal) -> BuilderResult {
        let line = self.last_line;

        let idx = self.literals.len();
        self.literals.push(value.clone()); //TODO: optimize this with no clone
        self.emit(line, Bytecode::CONST(idx));

        Ok(())
    }

    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> BuilderResult {
        let line = self.last_line;

        for expr in exprs {
            self.generate_expr(expr)?;
        }

        self.emit(line, Bytecode::BUILD_LIST(exprs.len()));

        Ok(())
    }

    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> BuilderResult {
        let line = self.last_line;

        self.generate_expr(expr)?;

        match operator.token_type {
            BANG      => self.emit(line, Bytecode::NOT),
            MINUS     => self.emit(line, Bytecode::NEG),

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
