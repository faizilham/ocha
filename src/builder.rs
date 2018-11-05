use std::collections::HashMap;

use ast::expr::{Expr, ExprNode, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use exception::Exception;
use token::Token;
use token::TokenType::*;
use token::Literal;
use vm::Chunk;
use vm::Bytecode;
use line_data::LineData;
use symbol_table::SymbolTable;

enum BranchType {
    BR,
    BRF
}

type Label = i32;

enum Context {
    WhileCtx { end_label: Label },
}

struct Blocks {
    main: Vec<Bytecode>,
    functions: Vec<Vec<Bytecode>>,
    literals: Vec<Literal>,
    line_data: LineData,
}

impl Blocks {
    pub fn new () -> Blocks {
        Blocks{
            main: Vec::new(),
            functions: Vec::new(),
            literals: Vec::new(),
            line_data: LineData::new(),
        }
    }

    pub fn merge(&mut self) {
        for func in self.functions.drain(..) {
            self.main.extend(func);
        }
    }

    pub fn into_chunk(mut self) -> Chunk {
        self.merge();
        let Blocks { main: codes, literals, line_data, .. } = self;
        Chunk { codes, literals, line_data }
    }
}

struct Builder<'a> {
    codes: &'a mut Vec<Bytecode>,
    literals: &'a mut Vec<Literal>,
    line_data: &'a mut LineData,
    labels: HashMap<Label, Vec<(usize, BranchType)>>,
    next_label: Label,
    last_line: i32,
    contexts: Vec<Context>,
    symbols: SymbolTable,
}

pub fn build(statements: Vec<Box<Stmt>>) -> Result<Chunk, Vec<Exception>> {
    let mut blocks = Blocks::new();
    {
        let mut builder = Builder::new(&mut blocks);

        let mut errors = Vec::new();

        for statement in statements {
            if let Err(e) = builder.generate(&statement) {
                errors.push(e);
            }
        }

        if errors.len() > 0 {
            return Err(errors);
        }

        let line = builder.last_line;
        builder.emit(line, Bytecode::HALT);
    }

    let chunk = blocks.into_chunk();
    Ok(chunk)
}

type BuilderResult = Result<(), Exception>;


impl<'a> Builder<'a> {
    fn new (blocks: &mut Blocks) -> Builder {
        Builder {
            codes: &mut blocks.main,
            literals: &mut blocks.literals,
            line_data: &mut blocks.line_data,
            labels: HashMap::new(),
            last_line: 0,
            next_label: 0,
            contexts: Vec::new(),
            symbols: SymbolTable::new(),
        }
    }

    fn generate(&mut self, stmt: &Box<Stmt>) -> BuilderResult {
        self.last_line = stmt.line;
        Stmt::accept(stmt, self)
    }

    fn generate_expr(&mut self, expr: &Box<Expr>) -> BuilderResult {
        self.last_line = expr.line;
        Expr::accept(expr, self)
    }

    fn next_pos(&self) -> usize {
        self.codes.len()
    }

    fn emit(&mut self, line: i32, bytecode : Bytecode) -> usize {
        let index = self.next_pos();
        self.codes.push(bytecode);

        self.line_data.add(index, line);

        index
    }

    fn replace(&mut self, index: usize, bytecode: Bytecode) {
        if let Some(code) = self.codes.get_mut(index) {
            *code = bytecode;
        } else {
            unreachable!();
        }
    }

    fn create_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label += 1;

        self.labels.insert(label, Vec::new());

        label
    }

    fn branch_placeholder(&mut self, line: i32, branch_type: BranchType, label: Label) {
        let position = self.emit(line, Bytecode::NOP);
        let branches = self.labels.get_mut(&label).unwrap();

        branches.push((position, branch_type));
    }

    fn set_label_position(&mut self, label: Label, label_pos: usize) {
        let branches = self.labels.remove(&label).unwrap();

        for (br_pos, br_type) in branches {
            let bytecode = match br_type {
                BranchType::BR => Bytecode::BR(label_pos),
                BranchType::BRF => Bytecode::BRF(label_pos)
            };

            self.replace(br_pos, bytecode);
        }
    }

    fn get_context(&self) -> Option<&Context> {
        self.contexts.get(self.contexts.len() - 1)
    }
}

impl<'a> StmtVisitor<BuilderResult> for Builder<'a> {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> BuilderResult {
        let offset = self.symbols.get(name)?;
        self.generate_expr(expr)?;

        self.emit(name.line, Bytecode::STORE(offset));

        Ok(())
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>) -> BuilderResult {
        for statement in body {
            self.generate(statement)?;
        }
        Ok(())
    }

    fn visit_break(&mut self) -> BuilderResult {
        let line = self.last_line;
        let label : Label;

        if let Some(Context::WhileCtx { end_label }) = self.get_context() {
            // branch to end of loop
            label = *end_label;
        } else {
            return error(line, "Invalid break outside of loop")
        }

        self.branch_placeholder(line, BranchType::BR, label);
        Ok(())
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> BuilderResult {
        let line = self.last_line;
        self.generate_expr(expr)?;
        self.emit(line, Bytecode::POP);

        Ok(())
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> BuilderResult {
        let line = self.last_line;

        // generate condition
        self.generate_expr(condition)?;

        let else_start_label = self.create_label();

        // placeholder brf jump to after true branch (i.e. else case)
        self.branch_placeholder(line, BranchType::BRF, else_start_label);

        // generate true branch
        self.generate(true_branch)?;
        let mut else_start_position = self.next_pos(); // instruction after the true branch


        if let Some(false_branch) = false_branch {
            let block_end_label = self.create_label();

            // add br between true & false branch
            self.branch_placeholder(line, BranchType::BR, block_end_label);

            else_start_position += 1;

            self.generate(false_branch)?;

            let block_end_position = self.next_pos(); // instruction after false branch
            self.set_label_position(block_end_label, block_end_position)
        }

        self.set_label_position(else_start_label, else_start_position);

        Ok(())
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
        if let ExprNode::Get {variable, operator, member } = &get_expr.node {
            self.generate_expr(expr)?;
            self.generate_expr(variable)?;
            self.generate_expr(member)?;

            self.emit(operator.line, Bytecode::SET_LIST);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>) -> BuilderResult {
        self.symbols.add_local(name);
        self.generate_expr(expr)?;

        Ok(())
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> BuilderResult {
        let line = self.last_line;

        let while_start_pos = self.next_pos();
        let while_end = self.create_label();

        self.generate_expr(condition)?;

        // brf to loop end
        self.branch_placeholder(line, BranchType::BRF, while_end);

        // generate body
        self.contexts.push(Context::WhileCtx { end_label: while_end });

        self.generate(body)?;

        self.contexts.pop();

        self.emit(line, Bytecode::BR(while_start_pos)); // br to top

        let while_end_pos = self.next_pos();
        self.set_label_position(while_end, while_end_pos);

        Ok(())
    }

}

impl<'a> ExprVisitor<BuilderResult> for Builder<'a> {
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
        let line = self.last_line;

        // generate condition
        self.generate_expr(condition)?;


        let else_start_label = self.create_label();
        let block_end_label = self.create_label();

        // placeholder brf jumping to after true branch / else
        self.branch_placeholder(line, BranchType::BRF, else_start_label);

        // generate true branch
        self.generate_expr(true_branch)?;

        // add br between true & false branch, jumping to end of ternary
        self.branch_placeholder(line, BranchType::BR, block_end_label);

        let else_start_position = self.next_pos(); // instruction after the true branch

        self.generate_expr(false_branch)?;

        let block_end_position = self.next_pos(); // instruction after false branch

        self.set_label_position(else_start_label, else_start_position);
        self.set_label_position(block_end_label, block_end_position);

        Ok(())
    }

    fn visit_variable(&mut self, name: &Token) -> BuilderResult {
        let offset = self.symbols.get(name)?;

        self.emit(name.line, Bytecode::LOAD(offset));

        Ok(())
    }

}

fn error(line: i32, message: &'static str) -> BuilderResult {
    Err(Exception::ParseErr(line, String::from(message)))
}
