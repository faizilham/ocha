use std::cell::Cell;

use ast::expr::{Expr, ExprVisitor};
use ast::stmt::{Stmt, StmtVisitor};
use helper::PCell;
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

use self::ResolveType::*;
use self::ContextType::*;

#[derive(Debug)]
pub struct ResolverData {
    pub resolve_type: ResolveType,
    pub symbol_type: SymbolType,
}

impl ResolverData {
    pub fn new(resolve_type: ResolveType, symbol_type: SymbolType) -> ResolverData {
        ResolverData { resolve_type, symbol_type }
    }
}

pub struct ScopeData {
    pub has_captured: bool,
    pub num_vardecl: usize
}

impl ScopeData {
    pub fn new() -> Self {
        Self { has_captured: false, num_vardecl: 0}
    }
}

pub struct VariableData {
    pub is_captured: bool
}

impl VariableData {
    pub fn new() -> Self {
        Self { is_captured: false }
    }
}

pub struct ResolverInfo {
    pub functions: Vec<FunctionSignature>,
    pub scopes: Vec<ScopeData>,
    pub variables: Vec<VariableData>,
    pub resolves: Vec<ResolverData>
}

pub fn resolve(statements: &Vec<Box<Stmt>>) -> Result<ResolverInfo, Vec<Exception>> {
    let mut resolver = Resolver::new();
    let mut errors = Vec::new();

    for statement in statements {
        if let Err(e) = resolver.resolve(&statement) {
            errors.push(e);
        }
    }

    if errors.len() > 0 {
        return Err(errors);
    }

    Ok(resolver.to_resolver_info())
}

struct Resolver {
    symbol_table: SymbolTableRef,
    functions: Vec<FunctionSignature>,
    scopes: Vec<ScopeData>,
    variables: Vec<VariableData>,
    resolves: Vec<ResolverData>,

    last_line: i32,
}


type StmtResult = Result<(bool), Exception>; // returned
type ExprResult = Result<(bool), Exception>; // has_closure

impl Resolver {
    pub fn new() -> Resolver {
        let functions = vec![ FunctionSignature::new(0) ]; // first function = global code

        Resolver {
            symbol_table: SymbolTable::new_ref(GlobalCtx, 0, 0, None),
            functions,
            scopes: Vec::new(),
            variables: Vec::new(),
            resolves: Vec::new(),
            last_line: 0,
        }
    }

    pub fn to_resolver_info (self) -> ResolverInfo {
        let Self { functions, scopes, variables, resolves, ..} = self;

        ResolverInfo { functions, scopes, variables, resolves }
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
    fn add_var(&mut self, name: &Token) -> Result<usize, Exception> {
        let var_id = self.variables.len();
        self.variables.push(VariableData::new());

        let mut symtable = self.symbol_table.borrow_mut();
        symtable.add_var(name, var_id)?;

        Ok(var_id)
    }

    fn add_func(&mut self, name: &Token, num_args: usize) -> Result<usize, Exception> {
        let func_id = self.functions.len();
        self.functions.push(FunctionSignature::new(num_args));

        let mut symtable = self.symbol_table.borrow_mut();
        symtable.add_func(name, func_id)?;

        Ok(func_id)
    }

    fn add_resolve(&mut self, resolve_type: ResolveType, symbol_type: SymbolType) -> usize {
        let id = self.resolves.len();
        self.resolves.push(ResolverData::new(resolve_type, symbol_type));

        id
    }

    fn add_scope(&mut self) -> usize {
        let id = self.scopes.len();
        self.scopes.push(ScopeData::new());

        id
    }

    fn add_args(&mut self, args: &Vec<Token>) -> Result<(), Exception> {
        let mut symtable = self.symbol_table.borrow_mut();

        let offset_start = -(args.len() as isize + STACK_FRAME_SIZE);


        let mut i = 0;
        for arg in args {
            // TODO: handle id & closure offset for args
            symtable.add_var_offset(arg, 0, i + offset_start)?;
            i += 1;
        }

        Ok(())
    }

    fn get_symbol(&mut self, name: &Token) -> Result<(PCell<SymbolType>, ResolveType), Exception> {
        let mut rf = self.symbol_table.clone();

        let current_context_level;
        {
            let table = rf.borrow();
            current_context_level = table.context_level;
        }

        let mut env_level = 0;

        loop {
            rf = {
                let mut table = rf.borrow_mut();
                let context_type = table.context_type;
                let context_level = table.context_level;
                let scope_level = table.scope_level;

                if let Some(symbol) = table.get(name) {
                    let resolve_type;

                    if context_level == current_context_level {
                        // local if in the same context
                        resolve_type = Local;
                    } else if context_type == GlobalCtx && scope_level == 0 {
                        // always global if in outermost scope of global context
                        resolve_type = Global;
                    } else if let SymbolType::Var{id, offset, capture_offset} = symbol.get() {
                        // if a variable not local nor global, it is a closure

                        // update closure index and is_captured if not yet referenced as closure
                        if capture_offset < 0 {
                            let capture_offset = table.increase_capture_offset();
                            self.variables.get_mut(id).unwrap().is_captured = true;

                            symbol.set(SymbolType::Var{id, offset, capture_offset});
                        }

                        resolve_type = Closure(env_level);
                    } else {
                        // functions are always globally resolved
                        resolve_type = Global;
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
    fn visit_assignment(&mut self, name: &Token, expr: &Box<Expr>, id: &Cell<usize>) -> StmtResult {
        let (symbol, resolve_type) = self.get_symbol(name)?;
        let symbol_type = symbol.get();

        if let SymbolType::Var{..} = symbol_type {
            self.resolve_expr(expr)?;

            let var_id = self.add_resolve(resolve_type, symbol_type);
            id.set(var_id);

            return Ok(false);
        }

        error(name.line, "Can't assign value to function")
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>, id: &Cell<usize>) -> StmtResult {

        let scope_id = self.add_scope();
        id.set(scope_id);

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
        let count = self.symbol_table.borrow().len();

        if count > 0 {
            let scope = self.scopes.get_mut(scope_id).unwrap();
            scope.num_vardecl = count;
        }

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

    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>, id: &Cell<usize>) -> StmtResult {

        // create new function and put to current symbol table
        let func_id = self.add_func(name, args.len())?;
        id.set(func_id);

        // create new symbol table
        let current_symtable = self.symbol_table.clone();
        let new_symtable = SymbolTable::create_function_scope(&self.symbol_table);
        self.symbol_table = new_symtable;

        // push new symbol table and put args inside it
        self.add_args(args)?;

        for stmt in body {
            let block_returned = self.resolve(stmt)?;

            if block_returned {
                break;
            }
        }

        // end function
        self.symbol_table = current_symtable;
        // TODO: update has_captured


        Ok(false)
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
        // TODO: handle func context has_captured

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

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>, id: &Cell<usize>) -> StmtResult {
        let var_id = self.add_var(name)?;
        id.set(var_id);

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
        let left_is_capturing = self.resolve_expr(left)?;
        let right_is_capturing = self.resolve_expr(right)?;

        Ok(left_is_capturing && right_is_capturing)
    }

    fn visit_funccall(&mut self, callee: &Box<Expr>, args: &Vec<Box<Expr>>) -> ExprResult {
        let mut is_capturing = false;
        for arg in args {
            is_capturing = self.resolve_expr(arg)? || is_capturing;
        }
        is_capturing = self.resolve_expr(callee)? || is_capturing;

        Ok(is_capturing)
    }

    fn visit_get(&mut self, callee: &Box<Expr>, _: &Token, member: &Box<Expr>) -> ExprResult {
        let mut is_capturing;

        is_capturing = self.resolve_expr(callee)?;
        is_capturing = self.resolve_expr(member)? || is_capturing;

        Ok(is_capturing)
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>) -> ExprResult {
        self.resolve_expr(expr)
    }

    fn visit_literal(&mut self, _: &Literal) -> ExprResult {
        Ok(false)
    }

    fn visit_listinit(&mut self, exprs: &Vec<Box<Expr>>) -> ExprResult {
        let mut is_capturing = false;

        for expr in exprs {
            is_capturing = self.resolve_expr(expr)? || is_capturing;
        }

        Ok(is_capturing)
    }

    fn visit_unary(&mut self, _: &Token, expr: &Box<Expr>) -> ExprResult {
        self.resolve_expr(expr)
    }

    fn visit_ternary(&mut self, condition: &Box<Expr>, true_branch: &Box<Expr>, false_branch: &Box<Expr>) -> ExprResult {
        let mut is_capturing;

        is_capturing = self.resolve_expr(condition)?;
        is_capturing = self.resolve_expr(true_branch)? || is_capturing;
        is_capturing = self.resolve_expr(false_branch)? || is_capturing;

        Ok(is_capturing)
    }

    fn visit_variable(&mut self, name: &Token, id: &Cell<usize>) -> ExprResult {
        let (symbol_type, resolve_type) = self.get_symbol(name)?;

        let var_id = self.add_resolve(resolve_type, symbol_type.get());
        id.set(var_id);

        let mut is_capturing = false;
        if let Closure(_) = resolve_type {
            is_capturing = true;
        }

        Ok(is_capturing)
    }
}

fn error(line: i32, message: &'static str) -> StmtResult {
    Err(Exception::ParseErr(line, String::from(message)))
}
