use std::cell::Cell;

use ast::expr::{Expr, ExprNode, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use exception::Exception;
use token::Token;
use token::TokenType::*;
use program_data::{Literal, FunctionSignature, LineData};
use resolver::{ResolverInfo, ResolverData, ResolveType, SymbolType, ScopeData, VariableData};
use vm::{Bytecode, Module};

mod block;

use self::block::{BranchType, Block, BlockRef, Label};

pub fn build(statements: Vec<Box<Stmt>>, resolver_info: ResolverInfo) -> Result<Module, Vec<Exception>> {
    let mut builder = Builder::new(resolver_info);

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

struct Builder {
    blocks: Vec<BlockRef>,
    current_subprog: BlockRef,
    literals: Vec<Literal>,

    functions: Vec<FunctionSignature>,
    scopes: Vec<ScopeData>,
    variables: Vec<VariableData>,
    resolves: Vec<ResolverData>,

    last_line: i32,
    loop_end_label: Label,
}

type StmtResult = Result<bool, Exception>; // result: is statement returned
type ExprResult = Result<(), Exception>;

impl Builder {
    fn new (resolver_info: ResolverInfo) -> Builder {
        let ResolverInfo {functions, scopes, variables, resolves } = resolver_info;
        let mut blocks = Vec::with_capacity(functions.len());

        for _ in &functions {
            blocks.push(Block::new_ref());
        }

        let current_subprog = blocks.get(0).expect("Invalid block in builder").clone();

        Builder {
            blocks,
            current_subprog,
            literals: Vec::new(),

            functions,
            scopes,
            variables,
            resolves,

            last_line: 0,
            loop_end_label: -1,
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
    fn get_function_block(&mut self, func_id : usize) -> BlockRef {
        self.blocks.get(func_id).expect("Invalid block in builder").clone()
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
}

impl StmtVisitor<StmtResult> for Builder {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>, id: &Cell<usize>) -> StmtResult {
        let ResolverData{symbol_type, resolve_type} = *self.resolves.get(id.get()).unwrap();

        if let SymbolType::Var(offset) = symbol_type {
            self.generate_expr(expr)?;

            // TODO: handle closure
            let bytecode = match resolve_type {
                ResolveType::Global     => Bytecode::STORE_GLOBAL(offset),
                ResolveType::Local      => Bytecode::STORE(offset),
                ResolveType::Closure(_) => unimplemented!(),
            };

            self.emit(name.line, bytecode);
            return Ok(false);
        }

        error(name.line, "Can't assign value to function")
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>, id: &Cell<usize>) -> StmtResult {
        let mut block_returned = false;
        for statement in body {
            block_returned = self.generate(statement)?;

            if block_returned {
                break; // do not generate unreachable codes
            }
        }

        // pop local scope variables
        let count;
        {
            let scope = self.scopes.get(id.get()).unwrap();
            count = scope.num_vardecl;
        }


        let line = self.last_line;
        self.emit(line, Bytecode::POP(count));

        // TODO: handle has_captured

        // TODO: handle restore on exception

        Ok(block_returned)
    }

    fn visit_break(&mut self) -> StmtResult {
        let line = self.last_line;
        let label = self.loop_end_label;

        if label < 0 {
            return error(line, "Invalid break outside of loop")
        }

        self.branch_placeholder(line, BranchType::BR, label);
        Ok(false)
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> StmtResult {
        let line = self.last_line;
        self.generate_expr(expr)?;
        self.emit(line, Bytecode::POP(1));

        Ok(false)
    }

    fn visit_funcdecl(&mut self, _name: &Token, _args: &Vec<Token>, body: &Vec<Box<Stmt>>, id: &Cell<usize>) -> StmtResult {
        let subprog = self.current_subprog.clone();

        // start function block
        let func_block = self.get_function_block(id.get());
        self.current_subprog = func_block;

        // push new symbol table and put args inside it

        let mut block_returned = false;

        for stmt in body {
            block_returned = self.generate(stmt)?;

            if block_returned {
                break; // do not generate unreachable codes
            }
        }

        if !block_returned {
            // add empty return if not yet exist
            // TODO: handle has_captured

            let line = self.last_line;
            self.emit(line, Bytecode::NIL);
            self.emit(line, Bytecode::RET);
        }

        // NOTE: no need to pop local var here, as it already handle by RET

        // TODO: handle restore on exception

        // end function block
        self.current_subprog = subprog;

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

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>, id: &Cell<usize>) -> StmtResult {
        // TODO: handle is_captured
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
        let last_label = self.loop_end_label;
        self.loop_end_label = while_end;

        // generate body
        let while_returned = self.generate(body)?;

        // restore previous while label
        self.loop_end_label = last_label;

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

    fn visit_variable(&mut self, name: &Token, id: &Cell<usize>) -> ExprResult {

        let ResolverData{symbol_type, resolve_type} = *self.resolves.get(id.get()).unwrap();

        // TODO: handle closure

        let bytecode = match symbol_type {
            SymbolType::Var(offset) =>
                match resolve_type {
                    ResolveType::Global      => Bytecode::LOAD_GLOBAL(offset),
                    ResolveType::Local       => Bytecode::LOAD(offset),
                    ResolveType::Closure(_)  => unimplemented!(),
                },
            SymbolType::Func(id)    => Bytecode::LOAD_FUNC(id),
        };

        self.emit(name.line, bytecode);

        Ok(())
    }

}

fn error(line: i32, message: &'static str) -> StmtResult {
    Err(Exception::ParseErr(line, String::from(message)))
}
