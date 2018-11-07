use std::rc::Rc;
use std::cell::{Cell, RefCell};

use ast::expr::{Expr, ExprNode, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use program_data::Literal;
use token::Token;
use exception::Exception;

mod context;

pub use self::context::SymbolType;
use self::context::{Context, ContextType, ContextRef, SymbolTable, SymbolTableRef};

pub type Enclosed = Rc<Cell<bool>>;

pub fn create_enclosed() -> Enclosed {
    Rc::new(Cell::new(false))
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ResolveType {
    Global,
    Local,
    Closure(usize)
}

#[derive(Debug, Clone, Copy)]
pub struct ResolverData {
    pub resolve_type: ResolveType,
    pub symbol_type: SymbolType,
}

pub type ResolverDataRef = Rc<RefCell<ResolverData>>;

use self::SymbolType::*;
use self::ResolveType::*;
use self::ContextType::*;

impl ResolverData {
    pub fn new() -> ResolverData {
        ResolverData {resolve_type: Global, symbol_type: Var(0)}
    }

    pub fn new_ref() -> ResolverDataRef {
        Rc::new(RefCell::new(ResolverData::new()))
    }
}

struct Resolver {
    context: ContextRef,
    last_line: i32,
}


type StmtResult = Result<(), Exception>;
type ExprResult = Result<(), Exception>;

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            context: Context::new_ref(GlobalCtx, None),
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
    fn add_var(&mut self, name: &Token) -> Result<isize, Exception> {
        let context = self.context.borrow_mut();
        let mut symtable = context.local_symbols.borrow_mut();
        symtable.add_var(name)
    }

    fn add_func(&mut self, name: &Token, func_id: usize) -> Result<(), Exception> {
        let context = self.context.borrow_mut();
        let mut symtable = context.local_symbols.borrow_mut();
        symtable.add_func(name, func_id)
    }

    fn add_args(&mut self, args: &Vec<Token>) -> Result<(), Exception> {
        let context = self.context.borrow_mut();
        let mut symtable = context.local_symbols.borrow_mut();

        let offset_start = -(args.len() as isize + 3);

        let mut i = 0;
        for arg in args {
            symtable.add_var_offset(arg, i + offset_start)?;
            i += 1;
        }

        Ok(())
    }

    fn get_symbol(&self, name: &Token) -> Result<(SymbolType, ResolveType), Exception> {

        let mut rf = self.context.clone();
        let mut level = 0;

        loop {
            rf = {
                let context = rf.borrow();
                let symtable = context.local_symbols.borrow();

                if let Some(symbol) = symtable.get(name) {
                    let is_local = level == 0;
                    let is_var = symbol.is_var();
                    let ctx_type = context.ctx_type;

                    let resolve =
                        if !is_var || ctx_type == GlobalCtx {
                            ResolveType::Global
                        } else if is_local {
                            ResolveType::Local
                        } else {
                            ResolveType::Closure(level)
                        };

                    return Ok((symbol, resolve));
                }

                if let Some(parent) = &context.parent {
                    parent.clone()
                } else {
                    return Err(SymbolTable::declare_err(name));
                }
            };

            level += 1;
        }
    }

    fn count_symbols(&self) -> usize {
        let context = self.context.borrow();
        let symtable = context.local_symbols.borrow();

        symtable.len()
    }

    fn enter_scope(&mut self) -> SymbolTableRef {
        let mut context = self.context.borrow_mut();
        let current_symtable = context.local_symbols.clone();

        context.local_symbols = SymbolTable::create_child(&current_symtable);

        current_symtable
    }

    fn restore_scope(&mut self, symtable: SymbolTableRef) {
        let mut context = self.context.borrow_mut();
        context.local_symbols = symtable;
    }
}

impl StmtVisitor<StmtResult> for Resolver {
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>) -> StmtResult {
        self.resolve_expr(expr)
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>) -> StmtResult {
        unimplemented!();
    }

    fn visit_break(&mut self) -> StmtResult {
        Ok(())
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> StmtResult {
        self.resolve_expr(expr)?;
        Ok(())
    }

    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>) -> StmtResult {
        unimplemented!();
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> StmtResult {
        self.resolve_expr(condition)?;
        self.resolve(true_branch)?;

        if let Some(false_branch) = false_branch {
            self.resolve(false_branch)?;
        }

        Ok(())
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> StmtResult {
        for expr in exprs {
            self.resolve_expr(expr)?;
        }

        Ok(())
    }

    fn visit_return(&mut self, expr: &Option<Box<Expr>>) -> StmtResult {
        let line = self.last_line;
        let ctx_type = self.context.borrow().ctx_type;

        if ctx_type != FuncCtx {
            return error(line, "Invalid return outside of loop");
        }

        if let Some(expr) = expr {
            self.resolve_expr(expr)?;
        }

        Ok(())
    }

    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> StmtResult {
        self.resolve_expr(get_expr)?;
        self.resolve_expr(expr)?;

        Ok(())
    }

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>, enclosed: &Enclosed) -> StmtResult {
        unimplemented!();
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> StmtResult {
        self.resolve_expr(condition)?;
        self.resolve(body)?;

        Ok(())
    }
}

impl ExprVisitor<ExprResult> for Resolver {
    fn visit_binary(&mut self, left: &Box<Expr>, _: &Token, right: &Box<Expr>) -> ExprResult {
        self.resolve_expr(left)?;
        self.resolve_expr(right)?;

        Ok(())
    }

    fn visit_funccall(&mut self, callee: &Box<Expr>, args: &Vec<Box<Expr>>) -> ExprResult {
        for arg in args {
            self.resolve_expr(arg)?;
        }
        self.resolve_expr(callee)?;

        Ok(())
    }

    fn visit_get(&mut self, callee: &Box<Expr>, _: &Token, member: &Box<Expr>) -> ExprResult {
        self.resolve_expr(callee)?;
        self.resolve_expr(member)?;

        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> ExprResult {
        self.resolve_expr(expr)
    }

    fn visit_literal(&mut self, _: &Literal) -> ExprResult {
        Ok(())
    }

    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> ExprResult {
        for expr in exprs {
            self.resolve_expr(expr)?;
        }

        Ok(())
    }

    fn visit_unary(&mut self, _: &Token, expr: &Box<Expr>) -> ExprResult {
        self.resolve_expr(expr)
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> ExprResult {
        self.resolve_expr(condition)?;
        self.resolve_expr(true_branch)?;
        self.resolve_expr(false_branch)?;

        Ok(())
    }

    fn visit_variable(&mut self, name: &Token, resolve: &ResolverDataRef) -> ExprResult {
        let (symbol_type, resolve_type) = self.get_symbol(name)?;

        let mut res_data = resolve.borrow_mut();
        (*res_data).symbol_type = symbol_type;
        (*res_data).resolve_type = resolve_type;

        Ok(())
    }
}

fn error(line: i32, message: &'static str) -> StmtResult {
    Err(Exception::ParseErr(line, String::from(message)))
}
