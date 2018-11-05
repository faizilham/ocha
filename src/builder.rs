use std::rc::Rc;
use std::cell::RefCell;

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

type BranchPlaceholder = (usize, BranchType); // (position, type)

type Label = u32;

struct LabelData {
    position: usize,
    placeholders: Vec<BranchPlaceholder>
}

enum Context {
    WhileCtx { end_label: Label },
}

struct SubProg {
    codes: Vec<Bytecode>,
    labels: Vec<LabelData>,
    next_label: Label,
}

type SubProgRef = Rc<RefCell<SubProg>>;

impl SubProg {
    pub fn new_ref() -> SubProgRef {
        Rc::new(RefCell::new(SubProg::new()))
    }

    pub fn new() -> SubProg {
        SubProg { codes: Vec::new(), labels: Vec::new(), next_label: 0 }
    }

    pub fn next_pos(&self) -> usize {
        self.codes.len()
    }

    pub fn emit(&mut self, bytecode : Bytecode) -> usize {
        let index = self.next_pos();
        self.codes.push(bytecode);

        index
    }

    pub fn create_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label += 1;

        self.labels.push( LabelData{ position: 0, placeholders: Vec::new() } );

        label
    }

    pub fn branch_placeholder(&mut self, branch_type: BranchType, label: Label) -> usize {
        let position = self.emit(Bytecode::NOP);
        let label_data = self.labels.get_mut(label as usize).unwrap();

        label_data.placeholders.push((position, branch_type));

        position
    }

    pub fn set_label_position(&mut self, label: Label, position: usize) {
        let label_data = self.labels.get_mut(label as usize).unwrap();

        label_data.position = position;
    }

    pub fn enclose_labels(&mut self) {
        for LabelData { position, placeholders } in self.labels.drain(..) {
            for (br_pos, br_type) in placeholders {
                let bytecode = match br_type {
                    BranchType::BR => Bytecode::BR(position),
                    BranchType::BRF => Bytecode::BRF(position)
                };

                let code = self.codes.get_mut(br_pos).unwrap();
                *code = bytecode;
            }
        }
    }
}

struct Blocks {
    subprograms: Vec<SubProgRef>
}

impl Blocks {
    pub fn new () -> Blocks {
        Blocks{
            subprograms: vec![ SubProg::new_ref() ]
        }
    }

    fn enclose_and_merge(self) -> Vec<Bytecode> {
        let Blocks { mut subprograms } = self;
        let mut codes = Vec::new();

        for rf in subprograms.drain(..) {
            let mut sub = rf.borrow_mut();
            sub.enclose_labels();

            codes.extend(&sub.codes);
        }

        codes
    }

    pub fn main(&self) -> SubProgRef {
        self.subprograms.get(0).unwrap().clone()
    }

    pub fn create_sub(&mut self) -> SubProgRef {
        let last = self.subprograms.len() - 1;

        self.subprograms.push(SubProg::new_ref());

        self.subprograms.get(last).unwrap().clone()
    }
}

struct Builder<'a> {
    blocks: &'a mut Blocks,
    current_subprog: SubProgRef,
    literals: Vec<Literal>,

    line_data: LineData,
    last_line: i32,

    contexts: Vec<Context>,
    symbols: SymbolTable,
}

pub fn build(statements: Vec<Box<Stmt>>) -> Result<Chunk, Vec<Exception>> {
    let mut blocks = Blocks::new();

    let Builder{literals, line_data, ..} = {
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

        builder
    };

    let codes = blocks.enclose_and_merge();

    let chunk = Chunk { codes, literals, line_data };
    Ok(chunk)
}

type BuilderResult = Result<(), Exception>;


impl<'a> Builder<'a> {
    fn new (blocks: &mut Blocks) -> Builder {
        let current_subprog = blocks.main();

        Builder {
            blocks: blocks,
            current_subprog,
            literals: Vec::new(),
            line_data: LineData::new(),
            last_line: 0,
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
        self.current_subprog.borrow().next_pos()
    }

    fn emit(&mut self, line: i32, bytecode : Bytecode) -> usize {
        let index = self.current_subprog.borrow_mut().emit(bytecode);
        self.line_data.add(index, line);

        index
    }

    fn create_label(&mut self) -> Label {
        self.current_subprog.borrow_mut().create_label()
    }

    fn branch_placeholder(&mut self, line: i32, branch_type: BranchType, label: Label) {
        let index = self.current_subprog.borrow_mut().branch_placeholder(branch_type, label);
        self.line_data.add(index, line);
    }

    fn set_label_position(&mut self, label: Label, position: usize) {
        self.current_subprog.borrow_mut().set_label_position(label, position);
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

    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>) -> BuilderResult {
        let current = self.current_subprog.clone();

        unimplemented!();
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
        if let ExprNode::Get {callee, operator, member } = &get_expr.node {
            self.generate_expr(expr)?;
            self.generate_expr(callee)?;
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

    fn visit_funccall(&mut self, callee: &Box<Expr>, args: &Vec<Box<Expr>>) -> BuilderResult {
        unimplemented!();
    }

    fn visit_get(&mut self, callee: &Box<Expr>, _: &Token, member: &Box<Expr>) -> BuilderResult {
        let line = self.last_line;

        self.generate_expr(callee)?;
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
