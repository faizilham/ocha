use std::rc::Rc;
use std::cell::RefCell;

use ast::expr::{Expr, ExprNode, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use exception::Exception;
use token::Token;
use token::TokenType::*;
use program_data::{Literal, FunctionSignature, LineData};

use vm::Module;
use vm::Bytecode;
use symbol_table::{SymbolType, SymbolTable, SymbolTableRef};

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
    line_data: LineData,
}

type BlockRef = Rc<RefCell<Block>>;

impl Block {
    pub fn new_ref() -> BlockRef {
        Rc::new(RefCell::new(Block::new()))
    }

    pub fn new() -> Block {
        Block { codes: Vec::new(), labels: Vec::new(), next_label: 0, line_data: LineData::new() }
    }

    pub fn next_pos(&self) -> usize {
        self.codes.len()
    }

    pub fn emit(&mut self, line: i32, bytecode : Bytecode) -> usize {
        let index = self.next_pos();
        self.codes.push(bytecode);
        self.line_data.add(line);

        index
    }

    pub fn create_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label += 1;

        self.labels.push( LabelData{ position: 0, placeholders: Vec::new() } );

        label
    }

    pub fn branch_placeholder(&mut self, line: i32, branch_type: BranchType, label: Label) -> usize {
        let position = self.emit(line, Bytecode::NOP);
        let label_data = self.labels.get_mut(label as usize).expect("Label not found");

        label_data.placeholders.push((position, branch_type));

        position
    }

    pub fn set_label_position(&mut self, label: Label, position: usize) {
        let label_data = self.labels.get_mut(label as usize).expect("Label not found");

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

                let code = self.codes.get_mut(br_pos).expect("Invalid bytecode index");
                *code = bytecode;
            }
        }
    }
}

struct Builder {
    blocks: Vec<BlockRef>,
    current_subprog: BlockRef,
    literals: Vec<Literal>,
    functions: Vec<FunctionSignature>,

    last_line: i32,

    context: Context,
    symbol_tables: Vec<SymbolTableRef>,
}

fn enclose_and_merge(mut blocks : Vec<BlockRef>, functions: &mut Vec<FunctionSignature>) -> (Vec<Bytecode>, LineData) {
    let mut merged_codes = Vec::new();
    let mut merged_line = LineData::new();
    let mut i = 0;

    for rf in blocks.drain(..) {
        let mut sub = rf.borrow_mut();
        sub.enclose_labels(merged_codes.len());

        let entry_point = merged_codes.len();

        let func = functions.get_mut(i).expect("Function not found while building");
        func.entry_point = entry_point;

        merged_codes.extend(&sub.codes);
        merged_line.extend(&sub.line_data);
        i += 1;
    }

    (merged_codes, merged_line)
}

pub fn build(statements: Vec<Box<Stmt>>) -> Result<Module, Vec<Exception>> {
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

    let Builder{blocks, literals, mut functions, ..} = builder;

    let (codes, line_data) = enclose_and_merge(blocks, &mut functions);

    println!("{:?}", &codes); // TODO: remove this

    Ok(Module { codes, literals, functions, line_data })
}

type StmtResult = Result<bool, Exception>; // result: is statement returned
type ExprResult = Result<(), Exception>;

impl Builder {
    fn new () -> Builder {
        let current_subprog = Block::new_ref();
        let blocks = vec![ current_subprog.clone() ];
        let context = Context::new(MainCtx, -1);
        let functions = vec![ FunctionSignature::new(0) ]; // first function = main function
        let symbol_tables = vec![ SymbolTable::new_ref() ];

        Builder {
            blocks,
            current_subprog,
            literals: Vec::new(),
            functions,
            last_line: 0,
            context,
            symbol_tables,
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
    fn create_function_block(&mut self, num_args: usize) -> (BlockRef, usize) {
        let block = Block::new_ref();
        self.blocks.push(block.clone());

        let func_id = self.functions.len();
        self.functions.push(FunctionSignature::new(num_args));

        (block, func_id)
    }

    // code emitter functions
    fn next_pos(&self) -> usize {
        self.current_subprog.borrow().next_pos()
    }

    fn emit(&mut self, line: i32, bytecode : Bytecode) -> usize {
        let index = self.current_subprog.borrow_mut().emit(line, bytecode);

        index
    }

    // label functions
    fn create_label(&mut self) -> Label {
        self.current_subprog.borrow_mut().create_label()
    }

    fn branch_placeholder(&mut self, line: i32, branch_type: BranchType, label: Label) {
        self.current_subprog.borrow_mut().branch_placeholder(line, branch_type, label);
    }

    fn set_label_position(&mut self, label: Label, position: usize) {
        self.current_subprog.borrow_mut().set_label_position(label, position);
    }

    // symbols
    fn symtable(&self) -> SymbolTableRef {
        let last = self.symbol_tables.len() - 1;
        self.symbol_tables.get(last).unwrap().clone()
    }

    fn add_var(&mut self, name: &Token) -> isize {
        let symtable = self.symtable();
        let result = symtable.borrow_mut().add_var(name);

        result
    }

    fn add_func(&mut self, name: &Token, func_id: usize) {
        let symtable = self.symtable();
        let result = symtable.borrow_mut().add_func(name, func_id);

        result
    }

    fn get_symbol(&self, name: &Token) -> Result<SymbolType, Exception> {
        // TODO: handle global variable & closure

        let mut i = 0;

        for symtable in self.symbol_tables.iter().rev() {
            if let Some(symbol) = symtable.borrow().get(name) {
                if i > 0  {
                    if let SymbolType::Var(_) = symbol {
                        return Err(Exception::ParseErr(name.line,
                            String::from("Can't reference global variable")));
                    }
                }

                return Ok(*symbol);
            }

            i += 1;
        }

        Err(SymbolTable::declare_err(name))
    }
}

impl StmtVisitor<StmtResult> for Builder {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> StmtResult {

        // TODO: handle global variable & closure
        if let SymbolType::Var(offset) = self.get_symbol(name)? {
            self.generate_expr(expr)?;
            self.emit(name.line, Bytecode::STORE(offset));
            return Ok(false);
        }

        error(name.line, "Can't assign value to function")
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
        let (func_block, func_id) = self.create_function_block(args.len());
        self.current_subprog = func_block;
        self.context = Context::new(FuncCtx, -1);

        // add function to symbol table
        self.add_func(name, func_id);

        // push new symbol table and put args inside it
        let symtable = SymbolTable::new_ref();
        {
            let mut rf = symtable.borrow_mut();
            let offset_start = -(args.len() as isize + 3);

            let mut i = 0;
            for arg in args {
                rf.add_var_offset(arg, i + offset_start);
                i += 1;
            }

        }
        self.symbol_tables.push(symtable);


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
        self.symbol_tables.pop();
        self.current_subprog = subprog;
        self.context = ctx;

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
        self.add_var(name);
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
        let line = self.last_line;

        for arg in args {
            self.generate_expr(arg)?;
        }

        self.generate_expr(callee)?;

        self.emit(line, Bytecode::CALL(args.len()));

        Ok(())
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

        // TODO: handle global variable & closure
        let bytecode = match self.get_symbol(name)? {
            SymbolType::Var(offset) => Bytecode::LOAD(offset),
            SymbolType::Func(id) => Bytecode::LOAD_FUNC(id),
        };

        self.emit(name.line, bytecode);

        Ok(())
    }

}

fn error(line: i32, message: &'static str) -> StmtResult {
    Err(Exception::ParseErr(line, String::from(message)))
}
