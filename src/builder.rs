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

type Label = i32;

struct LabelData {
    position: usize,
    placeholders: Vec<BranchPlaceholder>
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ContextType {
    MainCtx,
    FuncCtx,
}

use self::ContextType::*;

#[derive(Debug, PartialEq, Clone, Copy)]
struct Context {
    ctx_type: ContextType,
    while_end_label: Label,
}

impl Context {
    pub fn new(ctx_type: ContextType, while_end_label: Label) -> Context {
        Context{ ctx_type, while_end_label }
    }
}

struct Block {
    codes: Vec<Bytecode>,
    labels: Vec<LabelData>,
    next_label: Label,
}

type BlockRef = Rc<RefCell<Block>>;

impl Block {
    pub fn new_ref() -> BlockRef {
        Rc::new(RefCell::new(Block::new()))
    }

    pub fn new() -> Block {
        Block { codes: Vec::new(), labels: Vec::new(), next_label: 0 }
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

    pub fn enclose_labels(&mut self, offset: usize) {
        for LabelData { position, placeholders } in self.labels.drain(..) {
            let absolute_position = position + offset;

            for (br_pos, br_type) in placeholders {
                let bytecode = match br_type {
                    BranchType::BR => Bytecode::BR(absolute_position),
                    BranchType::BRF => Bytecode::BRF(absolute_position)
                };

                let code = self.codes.get_mut(br_pos).unwrap();
                *code = bytecode;
            }
        }
    }
}

struct Builder {
    blocks: Vec<BlockRef>,
    current_subprog: BlockRef,
    literals: Vec<Literal>,

    line_data: LineData,
    last_line: i32,

    context: Context,
    symbols: SymbolTable,
}

fn enclose_and_merge(mut blocks : Vec<BlockRef>) -> Vec<Bytecode> {
    let mut codes = Vec::new();

    for rf in blocks.drain(..) {
        let mut sub = rf.borrow_mut();
        sub.enclose_labels(codes.len());

        codes.extend(&sub.codes);
    }

    codes
}

pub fn build(statements: Vec<Box<Stmt>>) -> Result<Chunk, Vec<Exception>> {
    let mut builder = Builder::new();

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

    let Builder{blocks, literals, line_data, ..} = builder;

    let codes = enclose_and_merge(blocks);

    println!("{:?}", &codes);

    let chunk = Chunk { codes, literals, line_data };
    Ok(chunk)
}

type StmtResult = Result<bool, Exception>; // result: is statement returned
type ExprResult = Result<(), Exception>;

impl Builder {
    fn new () -> Builder {
        let current_subprog = Block::new_ref();
        let blocks = vec![ current_subprog.clone() ];
        let context = Context::new(MainCtx, -1);

        Builder {
            blocks,
            current_subprog,
            literals: Vec::new(),
            line_data: LineData::new(),
            last_line: 0,
            context,
            symbols: SymbolTable::new(),
        }
    }

    // visit function
    fn generate(&mut self, stmt: &Box<Stmt>) -> StmtResult {
        self.last_line = stmt.line;
        Stmt::accept(stmt, self)
    }

    fn generate_expr(&mut self, expr: &Box<Expr>) -> ExprResult {
        self.last_line = expr.line;
        Expr::accept(expr, self)
    }

    // block management
    fn create_block(&mut self) -> BlockRef {
        let block = Block::new_ref();
        self.blocks.push(block.clone());

        block
    }

    // code emitter functions
    fn next_pos(&self) -> usize {
        self.current_subprog.borrow().next_pos()
    }

    fn emit(&mut self, line: i32, bytecode : Bytecode) -> usize {
        let index = self.current_subprog.borrow_mut().emit(bytecode);
        self.line_data.add(index, line);

        index
    }

    // label functions
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
}

impl StmtVisitor<StmtResult> for Builder {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> StmtResult {
        let offset = self.symbols.get(name)?;
        self.generate_expr(expr)?;

        self.emit(name.line, Bytecode::STORE(offset));

        Ok(false)
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>) -> StmtResult {
        let mut block_returned = false;
        for statement in body {
            block_returned = self.generate(statement)?;

            if block_returned {
                break; // do not generate unreachable codes
            }
        }

        Ok(block_returned)
    }

    fn visit_break(&mut self) -> StmtResult {
        let line = self.last_line;
        let label = self.context.while_end_label;

        if label < 0 {
            return error(line, "Invalid break outside of loop")
        }

        self.branch_placeholder(line, BranchType::BR, label);
        Ok(false)
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> StmtResult {
        let line = self.last_line;
        self.generate_expr(expr)?;
        self.emit(line, Bytecode::POP);

        Ok(false)
    }

    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>) -> StmtResult {
        let subprog = self.current_subprog.clone();
        let ctx = self.context;

        // start function block & context
        self.current_subprog = self.create_block();
        self.context = Context::new(FuncCtx, -1);

        let mut block_returned = false;

        for stmt in body {
            block_returned = self.generate(stmt)?;

            if block_returned {
                break; // do not generate unreachable codes
            }
        }

        if !block_returned {
            let line = self.last_line;
            self.emit(line, Bytecode::NIL);
            self.emit(line, Bytecode::RET);
        }

        // end function block & context
        self.current_subprog = subprog;
        self.context = ctx;

        // TODO: add function to symbol table

        Ok(false)
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> StmtResult {
        let line = self.last_line;

        // generate condition
        self.generate_expr(condition)?;

        let else_start_label = self.create_label();

        // placeholder brf jump to after true branch (i.e. else case)
        self.branch_placeholder(line, BranchType::BRF, else_start_label);

        // generate true branch
        let true_returned = self.generate(true_branch)?;
        let mut else_start_position = self.next_pos(); // instruction after the true branch

        let mut false_returned = false;

        if let Some(false_branch) = false_branch {
            let block_end_label = self.create_label();

            // add br between true & false branch
            self.branch_placeholder(line, BranchType::BR, block_end_label);

            else_start_position += 1;

            false_returned = self.generate(false_branch)?;

            let block_end_position = self.next_pos(); // instruction after false branch
            self.set_label_position(block_end_label, block_end_position)
        }

        self.set_label_position(else_start_label, else_start_position);

        Ok(true_returned && false_returned)
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> StmtResult {
        let line = self.last_line;

        for expr in exprs {
            self.generate_expr(expr)?;
        }

        self.emit(line, Bytecode::PRINT(exprs.len()));

        Ok(false)
    }

    fn visit_return(&mut self, expr: &Option<Box<Expr>>) -> StmtResult {
        let line = self.last_line;

        if self.context.ctx_type != FuncCtx {
            return error(line, "Invalid return outside of loop");
        }

        if let Some(expr) = expr {
            self.generate_expr(expr)?;
        } else {
            self.emit(line, Bytecode::NIL);
        }

        self.emit(line, Bytecode::RET);

        Ok(true)
    }

    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> StmtResult {
        if let ExprNode::Get {callee, operator, member } = &get_expr.node {
            self.generate_expr(expr)?;
            self.generate_expr(callee)?;
            self.generate_expr(member)?;

            self.emit(operator.line, Bytecode::SET_LIST);

            Ok(false)
        } else {
            unreachable!();
        }
    }

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>) -> StmtResult {
        self.symbols.add_local(name);
        self.generate_expr(expr)?;

        Ok(false)
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> StmtResult {
        let line = self.last_line;

        let while_start_pos = self.next_pos();
        let while_end = self.create_label();

        self.generate_expr(condition)?;

        // brf to loop end
        self.branch_placeholder(line, BranchType::BRF, while_end);

        // save previous while label
        let last_label = self.context.while_end_label;
        self.context.while_end_label = while_end;

        // generate body
        let while_returned = self.generate(body)?;

        // restore previous while label
        self.context.while_end_label = last_label;

        self.emit(line, Bytecode::BR(while_start_pos)); // br to top

        let while_end_pos = self.next_pos();
        self.set_label_position(while_end, while_end_pos);

        Ok(while_returned)
    }

}

impl ExprVisitor<ExprResult> for Builder {
    fn visit_binary(&mut self, left: &Box<Expr>, operator: &Token, right: &Box<Expr>) -> ExprResult {
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

    fn visit_funccall(&mut self, callee: &Box<Expr>, args: &Vec<Box<Expr>>) -> ExprResult {
        unimplemented!();
    }

    fn visit_get(&mut self, callee: &Box<Expr>, _: &Token, member: &Box<Expr>) -> ExprResult {
        let line = self.last_line;

        self.generate_expr(callee)?;
        self.generate_expr(member)?;

        self.emit(line, Bytecode::GET_LIST);

        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> ExprResult {
        self.generate_expr(expr)
    }

    fn visit_literal(&mut self, value: &Literal) -> ExprResult {
        let line = self.last_line;

        match value {
            Literal::Nil => {
                self.emit(line, Bytecode::NIL);
            },

            Literal::Bool(b) => {
                self.emit(line, Bytecode::BOOL(*b));
            }

            _ => {
                let idx = self.literals.len();
                self.literals.push(value.clone()); //TODO: optimize this with no clone
                self.emit(line, Bytecode::CONST(idx));
            }
        }

        Ok(())
    }

    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> ExprResult {
        let line = self.last_line;

        for expr in exprs {
            self.generate_expr(expr)?;
        }

        self.emit(line, Bytecode::BUILD_LIST(exprs.len()));

        Ok(())
    }

    fn visit_unary(&mut self, operator: &Token, expr: &Box<Expr>) -> ExprResult {
        let line = self.last_line;

        self.generate_expr(expr)?;

        match operator.token_type {
            BANG      => self.emit(line, Bytecode::NOT),
            MINUS     => self.emit(line, Bytecode::NEG),

            _ => unreachable!()
        };

        Ok(())
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> ExprResult {
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

    fn visit_variable(&mut self, name: &Token) -> ExprResult {
        let offset = self.symbols.get(name)?;

        self.emit(name.line, Bytecode::LOAD(offset));

        Ok(())
    }

}

fn error(line: i32, message: &'static str) -> StmtResult {
    Err(Exception::ParseErr(line, String::from(message)))
}
