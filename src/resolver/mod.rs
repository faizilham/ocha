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

pub struct ResolverData {
    pub resolve_type: ResolveType,
    pub symbol_type: SymbolType,
}

impl ResolverData {
    pub fn new(resolve_type: ResolveType, symbol_type: SymbolType) -> ResolverData {
        ResolverData { resolve_type, symbol_type }
    }
}

pub struct FunctionData {
    pub signature: FunctionSignature,
    pub captured_args: Vec<(isize, isize)>, // offset, captured offset
    pub num_captured: usize,
}

impl FunctionData {
    pub fn new(num_args: usize) -> Self {
        Self {
            signature: FunctionSignature::new(num_args),
            captured_args: Vec::new(),
            num_captured: 0
        }
    }
}

pub struct ScopeData {
    pub num_vardecl: usize,
    pub captured_args: Vec<(isize, isize)>, // offset, captured index
    pub num_captured: usize,
}

impl ScopeData {
    pub fn new() -> Self {
        Self {
            captured_args: Vec::new(),
            num_vardecl: 0,
            num_captured: 0
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct VariableData {
    pub is_captured: bool,
    pub offset: isize,
    pub captured_offset: isize
}

impl VariableData {
    pub fn new(offset: isize) -> Self {
        Self { is_captured: false, offset, captured_offset: -1 }
    }
}

pub struct ResolverInfo {
    pub functions: Vec<FunctionData>,
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
    functions: Vec<FunctionData>,
    scopes: Vec<ScopeData>,
    variables: Vec<VariableData>,
    resolves: Vec<ResolverData>,

    last_line: i32,
}


type StmtResult = Result<(bool, bool), Exception>; // returned, is_capturing
type ExprResult = Result<(bool), Exception>; // is_capturing

impl Resolver {
    pub fn new() -> Resolver {
        let functions = vec![
            FunctionData::new(0) // first function = global code
        ];

        Resolver {
            symbol_table: SymbolTable::new_ref(0, GlobalCtx, 0, 0, None),
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

        let mut symtable = self.symbol_table.borrow_mut();
        let offset = symtable.add_var(name, var_id)?;
        self.variables.push(VariableData::new(offset));

        Ok(var_id)
    }

    fn add_func(&mut self, name: &Token, num_args: usize) -> Result<usize, Exception> {
        let func_id = self.functions.len();
        self.functions.push(FunctionData::new(num_args));

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

        let var_id_start = self.variables.len();
        let mut i = 0;

        for arg in args {
            let id = var_id_start + i;
            let offset = (i as isize) + offset_start;

            symtable.add_var_offset(arg, id, offset)?;
            self.variables.push(VariableData::new(offset));
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
                let is_top_scope = table.scope_level == 0;

                if let Some(symbol) = table.get(name) {
                    let resolve_type;

                    if context_level == current_context_level {
                        // local if in the same context
                        resolve_type = Local;
                    } else if context_type == GlobalCtx && is_top_scope {
                        // always global if in outermost scope of global context
                        resolve_type = Global;
                    } else if let SymbolType::Var{id, offset, capture_offset} = symbol.get() {
                        // if a variable not local nor global, it is a closure

                        // update closure index and is_captured if not yet referenced as closure
                        if capture_offset < 0 {
                            let capture_offset = table.increase_capture_offset();

                            let vardata = self.variables.get_mut(id).expect("Invalid resolved variable id");
                            vardata.is_captured = true;
                            vardata.captured_offset = capture_offset;

                            // update count and captured args in function / scope
                            if is_top_scope {
                                let function_data = self.functions.get_mut(table.context_id).expect("Invalid resolved function id");
                                function_data.num_captured += 1;

                                if offset < 0 {
                                    function_data.captured_args.push((offset, capture_offset));
                                }
                            } else {
                                let scope_data = self.scopes.get_mut(table.context_id).expect("Invalid resolved scope id");
                                scope_data.num_captured += 1;

                                if offset < 0 {
                                    scope_data.captured_args.push((offset, capture_offset));
                                }
                            }

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
            let mut is_capturing = self.resolve_expr(expr)?;

            let var_id = self.add_resolve(resolve_type, symbol_type);
            id.set(var_id);

            if let Closure(..) = resolve_type {
                is_capturing = true;
            }

            return Ok((false, is_capturing));
        }

        error(name.line, "Can't assign value to function")
    }

    fn visit_block(&mut self, body: &Vec<Box<Stmt>>, id: &Cell<usize>) -> StmtResult {

        let scope_id = self.add_scope();
        id.set(scope_id);

        // create new symbol table
        let current_symtable = self.symbol_table.clone();
        let new_symtable = SymbolTable::create_local_scope(&self.symbol_table, scope_id);
        self.symbol_table = new_symtable;

        let mut is_returned = false;
        let mut is_capturing = false;

        for stmt in body {
            let (block_returned, block_capturing) = self.resolve(stmt)?;

            is_returned = block_returned || is_returned;
            is_capturing = block_capturing || is_capturing;

            if block_returned {
                break;
            }
        }

        // end function
        let count = self.symbol_table.borrow().count_vars();

        if count > 0 {
            let scope = self.scopes.get_mut(scope_id).unwrap();
            scope.num_vardecl = count;
        }

        self.symbol_table = current_symtable;

        Ok((is_returned, is_capturing))
    }

    fn visit_break(&mut self) -> StmtResult {
        Ok((false, false))
    }

    fn visit_expression(&mut self, expr: &Box<Expr>) -> StmtResult {
        let is_capturing = self.resolve_expr(expr)?;
        Ok((false, is_capturing))
    }

    fn visit_funcdecl(&mut self, name: &Token, args: &Vec<Token>, body: &Vec<Box<Stmt>>, id: &Cell<usize>) -> StmtResult {

        // create new function and put to current symbol table
        let func_id = self.add_func(name, args.len())?;
        id.set(func_id);

        // create new symbol table
        let current_symtable = self.symbol_table.clone();
        let new_symtable = SymbolTable::create_function_scope(&self.symbol_table, func_id);
        self.symbol_table = new_symtable;

        // push new symbol table and put args inside it
        self.add_args(args)?;

        let mut is_capturing = false;

        for stmt in body {
            let (block_returned, block_capturing) = self.resolve(stmt)?;

            is_capturing = block_capturing || is_capturing;

            if block_returned {
                break;
            }
        }

        if is_capturing {
            let function_data = self.functions.get_mut(func_id).unwrap();
            function_data.signature.is_closure = true;
        }

        // end function
        self.symbol_table = current_symtable;

        Ok((false, false))
    }

    fn visit_if(&mut self, condition: &Box<Expr>, true_branch: &Box<Stmt>, false_branch: &Option<Box<Stmt>>) -> StmtResult {
        let mut is_capturing = self.resolve_expr(condition)?;
        let mut is_returned;

        let (true_returned, true_capturing) = self.resolve(true_branch)?;
        is_returned = true_returned;
        is_capturing = true_capturing || is_capturing;

        if let Some(false_branch) = false_branch {
            let (false_returned, false_capturing) = self.resolve(false_branch)?;
            is_returned = false_returned || is_capturing;
            is_capturing = false_capturing || is_capturing;
        }

        Ok((is_returned, is_capturing))
    }

    fn visit_print(&mut self, exprs: &Vec<Box<Expr>>) -> StmtResult {
        let mut is_capturing = false;

        for expr in exprs {
            is_capturing = self.resolve_expr(expr)? || is_capturing;
        }

        Ok((false, is_capturing))
    }

    fn visit_return(&mut self, expr: &Option<Box<Expr>>) -> StmtResult {
        let mut is_capturing = false;
        if let Some(expr) = expr {
            is_capturing = self.resolve_expr(expr)? || is_capturing;
        }

        Ok((true, is_capturing))
    }

    fn visit_set(&mut self, get_expr: &Box<Expr>, expr: &Box<Expr>) -> StmtResult {
        let mut is_capturing = self.resolve_expr(get_expr)?;
        is_capturing = self.resolve_expr(expr)? || is_capturing;

        Ok((false, is_capturing))
    }

    fn visit_vardecl(&mut self, name: &Token, expr: &Box<Expr>, id: &Cell<usize>) -> StmtResult {
        let var_id = self.add_var(name)?;
        id.set(var_id);

        let is_capturing = self.resolve_expr(expr)?;

        Ok((false, is_capturing))
    }

    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> StmtResult {
        let cond_capturing = self.resolve_expr(condition)?;
        let (returned, body_capturing) = self.resolve(body)?;

        Ok((returned, cond_capturing || body_capturing))
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
