
use ast::expr::{Expr, ExprNode, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use helper::{PCell, new_pcell};
use program_data::{Literal, FunctionSignature};
use token::Token;
use exception::Exception;

mod context;
const STACK_FRAME_SIZE : isize = 3;

pub use self::context::SymbolType;
use self::context::{ContextType, SymbolTable, SymbolTableRef};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ResolveType {
    Global,
    Local,
    Closure(usize)
}

#[derive(Debug)]
pub struct ResolverData {
    pub resolve_type: PCell<ResolveType>,
    pub symbol_type: PCell<SymbolType>,
}

use self::SymbolType::*;
use self::ResolveType::*;
use self::ContextType::*;

impl ResolverData {
    pub fn new() -> ResolverData {
        ResolverData {resolve_type: new_pcell(Global), symbol_type: new_pcell(Var(0))}
    }
}

struct Resolver {
    symbol_table: SymbolTableRef,
    functions: Vec<FunctionSignature>,
    last_line: i32,
}


type StmtResult = Result<(bool), Exception>; // returned
type ExprResult = Result<(bool), Exception>; // has_closure

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            symbol_table: SymbolTable::new_ref(GlobalCtx, 0, 0, None),
            functions: Vec::new(),
            last_line: 0,
        }
    }

    // visit function
    fn resolve(&mut self, stmt: &Box<Stmt>) -> StmtResult {
        self.last_line = stmt.line;
        Stmt::accept(stmt, self)
    }

    fn resolve_expr(&mut self, expr: &Box<Expr>) -> ExprResult {
        self.last_line = expr.line;
        Expr::accept(expr, self)
    }

    // symbols
    fn add_var(&self, name: &Token) -> Result<isize, Exception> {
        let mut symtable = self.symbol_table.borrow_mut();
        symtable.add_var(name)
    }

    fn add_func(&mut self, name: &Token, num_args: usize) -> Result<usize, Exception> {
        let func_id = self.functions.len();
        self.functions.push(FunctionSignature::new(num_args));

        let mut symtable = self.symbol_table.borrow_mut();
        symtable.add_func(name, func_id)?;

        Ok(func_id)
    }

    fn add_args(&mut self, args: &Vec<Token>) -> Result<(), Exception> {
        let mut symtable = self.symbol_table.borrow_mut();

        let offset_start = -(args.len() as isize + STACK_FRAME_SIZE);

        let mut i = 0;
        for arg in args {
            symtable.add_var_offset(arg, i + offset_start)?;
            i += 1;
        }

        Ok(())
    }

    fn get_symbol(&self, name: &Token) -> Result<(SymbolType, ResolveType), Exception> {
        let mut rf = self.symbol_table.clone();

        let current_context_level;
        {
            let table = rf.borrow();
            current_context_level = table.context_level;
        }

        let mut env_level = 0;

        loop {
            rf = {
                let table = rf.borrow();
                let context_type = table.context_type;
                let context_level = table.context_level;
                let scope_level = table.scope_level;

                if let Some(symbol) = table.get(name) {
                    let resolve_type;

                    if context_level == current_context_level {
                        // local if in the same context
                        resolve_type = Local;
                    } else if !symbol.is_var() {
                        // functions are always globally resolved
                        resolve_type = Global;
                    } else if context_type == GlobalCtx && scope_level == 0 {
                        // always global if in outermost scope of global context
                        resolve_type = Global;
                    } else {
                        resolve_type = Closure(env_level);
                    }

                    return Ok((symbol, resolve_type));
                }

                if let Some(parent) = &table.parent {
                    parent.clone()
                } else {
                    return Err(SymbolTable::declare_err(name));
                }
            };

            env_level += 1;
        }
    }
}

impl StmtVisitor<StmtResult> for Resolver {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> StmtResult {
        let line = self.last_line;
        if let (SymbolType::Var(offset), resolve) = self.get_symbol(name)? {
            self.resolve_expr(expr)?;

            // TODO: change is_captured if resolve type is closure

            return Ok(false);
        }

        error(name.line, "Can't assign value to function")
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>, has_captured: &PCell<bool>) -> StmtResult {
        // create new symbol table
        let current_symtable = self.symbol_table.clone();
        let new_symtable = SymbolTable::create_local_scope(&self.symbol_table);
        self.symbol_table = new_symtable;

        let mut block_returned = false;

        for stmt in body {
            block_returned = self.resolve(stmt)?;

            if block_returned {
                break;
            }
        }

        // end function
        self.symbol_table = current_symtable;
        // TODO: update has_captured

        Ok(block_returned)
    }

    fn visit_break(&mut self) -> StmtResult {
        Ok(false)
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> StmtResult {
        self.resolve_expr(expr)?;
        Ok(false)
    }

    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>, has_captured: &PCell<bool>) -> StmtResult {

        // create new function and put to current symbol table
        self.add_func(name, args.len())?;

        // create new symbol table
        let current_symtable = self.symbol_table.clone();
        let new_symtable = SymbolTable::create_function_scope(&self.symbol_table);
        self.symbol_table = new_symtable;

        // push new symbol table and put args inside it
        self.add_args(args)?;

        let mut block_returned = false;

        for stmt in body {
            block_returned = self.resolve(stmt)?;

            if block_returned {
                break;
            }
        }

        // end function
        self.symbol_table = current_symtable;
        // TODO: update has_captured


        Ok(block_returned)
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> StmtResult {
        self.resolve_expr(condition)?;
        let true_returned = self.resolve(true_branch)?;

        let mut false_returned = false;

        if let Some(false_branch) = false_branch {
            false_returned = self.resolve(false_branch)?;
        }

        Ok(true_returned && false_returned)
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> StmtResult {
        for expr in exprs {
            self.resolve_expr(expr)?;
        }

        Ok(false)
    }

    fn visit_return(&mut self, expr: &Option<Box<Expr>>) -> StmtResult {
        let line = self.last_line;
        let ctx_type = self.symbol_table.borrow().context_type;

        if ctx_type != FuncCtx {
            return error(line, "Invalid return outside of function");
        }

        if let Some(expr) = expr {
            self.resolve_expr(expr)?;
        }

        Ok(true)
    }

    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> StmtResult {
        self.resolve_expr(get_expr)?;
        self.resolve_expr(expr)?;

        Ok(false)
    }

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>, _is_captured: &PCell<bool>) -> StmtResult {
        self.add_var(name)?;
        self.resolve_expr(expr)?;

        Ok(false)
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> StmtResult {
        self.resolve_expr(condition)?;
        self.resolve(body)
    }
}

impl ExprVisitor<ExprResult> for Resolver {
    fn visit_binary(&mut self, left: &Box<Expr>, _: &Token, right: &Box<Expr>) -> ExprResult {
        let left_has_closure = self.resolve_expr(left)?;
        let right_has_closure = self.resolve_expr(right)?;

        Ok(left_has_closure && right_has_closure)
    }

    fn visit_funccall(&mut self, callee: &Box<Expr>, args: &Vec<Box<Expr>>) -> ExprResult {
        let mut has_closure = false;
        for arg in args {
            has_closure = self.resolve_expr(arg)? || has_closure;
        }
        has_closure = self.resolve_expr(callee)? || has_closure;

        Ok(has_closure)
    }

    fn visit_get(&mut self, callee: &Box<Expr>, _: &Token, member: &Box<Expr>) -> ExprResult {
        let mut has_closure;

        has_closure = self.resolve_expr(callee)?;
        has_closure = self.resolve_expr(member)? || has_closure;

        Ok(has_closure)
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> ExprResult {
        self.resolve_expr(expr)
    }

    fn visit_literal(&mut self, _: &Literal) -> ExprResult {
        Ok(false)
    }

    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> ExprResult {
        let mut has_closure = false;

        for expr in exprs {
            has_closure = self.resolve_expr(expr)? || has_closure;
        }

        Ok(has_closure)
    }

    fn visit_unary(&mut self, _: &Token, expr: &Box<Expr>) -> ExprResult {
        self.resolve_expr(expr)
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> ExprResult {
        let mut has_closure;

        has_closure = self.resolve_expr(condition)?;
        has_closure = self.resolve_expr(true_branch)? || has_closure;
        has_closure = self.resolve_expr(false_branch)? || has_closure;

        Ok(has_closure)
    }

    fn visit_variable(&mut self, name: &Token, resolve: &ResolverData) -> ExprResult {
        let (symbol_type, resolve_type) = self.get_symbol(name)?;

        resolve.symbol_type.set(symbol_type);
        resolve.resolve_type.set(resolve_type);

        let mut has_closure = false;
        if let Closure(_) = resolve_type {
            // TODO: change is_captured if resolve type is closure
            has_closure = true;
        }

        Ok(has_closure)
    }
}

fn error(line: i32, message: &'static str) -> StmtResult {
    Err(Exception::ParseErr(line, String::from(message)))
}
