use helper::{PRefCell, new_prefcell};
use std::collections::HashMap;

use exception::Exception;
use exception::Exception::ParseErr;
use token::Token;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ContextType {
    GlobalCtx,
    FuncCtx,
}

// Symbol tables

#[derive(Debug, Clone, Copy)]
pub enum SymbolType {
    Var(isize),
    Func(usize),
}

impl SymbolType {
    pub fn is_var(&self) -> bool {
        if let SymbolType::Var(_) = self {
            return true
        }

        false
    }
}

pub struct SymbolTable {
    symbols: HashMap<String, SymbolType>,
    var_offset: isize,
    pub context_type: ContextType,
    pub context_level: usize,
    pub scope_level: usize,

    pub parent: Option<SymbolTableRef>, // parent scope in the same context
}

pub type SymbolTableRef = PRefCell<SymbolTable>;

impl SymbolTable {
    pub fn new (context_type: ContextType, context_level: usize, scope_level: usize, parent: Option<SymbolTableRef>) -> SymbolTable {
        let mut var_offset = 0;

        if let Some(symtable) = &parent {
            let rf = symtable.borrow();
            var_offset = rf.var_offset;
        }

        SymbolTable { symbols: HashMap::new(), var_offset, context_type, context_level, scope_level, parent }
    }

    pub fn new_ref(context_type: ContextType, context_level: usize, scope_level: usize, parent: Option<SymbolTableRef>) -> SymbolTableRef {
        new_prefcell(SymbolTable::new(context_type, context_level, scope_level, parent))
    }

    pub fn create_local_scope(parent: &SymbolTableRef) -> SymbolTableRef {
        let (context_type, context_level, scope_level) = {
            let symtable = parent.borrow();
            (symtable.context_type, symtable.context_level, symtable.scope_level)
        };

        SymbolTable::new_ref(context_type, context_level, scope_level + 1, Some(parent.clone()))
    }

    pub fn create_function_scope(parent: &SymbolTableRef) -> SymbolTableRef {
        let (context_type, context_level) = {
            let symtable = parent.borrow();
            (ContextType::FuncCtx, symtable.context_level + 1)
        };

        SymbolTable::new_ref(context_type, context_level, 0, Some(parent.clone()))
    }

    fn add(&mut self, name: &Token, symbol: SymbolType) -> Result<(), Exception> {
        if self.symbols.contains_key(&name.lexeme) {
            return Err (
                ParseErr(name.line, format!("Identifier '{}' is already declared", &name.lexeme))
            );
        }

        self.symbols.insert(name.lexeme.clone(), symbol);

        Ok(())
    }

    pub fn add_var(&mut self, name: &Token) -> Result<isize, Exception> {
        let offset = self.var_offset;
        self.add_var_offset(name, offset)?;
        self.var_offset += 1;

        Ok(offset)
    }

    pub fn add_var_offset(&mut self, name: &Token, offset: isize) -> Result<(), Exception> {
        self.add(name, SymbolType::Var(offset))
    }

    pub fn add_func(&mut self, name: &Token, func_id: usize) -> Result<(), Exception> {
        self.add(name, SymbolType::Func(func_id))
    }

    pub fn get(&self, name: &Token) -> Option<SymbolType> {
        if let Some(symbol) = self.symbols.get(&name.lexeme) {
            Some(*symbol)
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn declare_err(name: &Token) -> Exception {
        ParseErr(name.line, format!("Identifier '{}' is not declared", &name.lexeme))
    }
}
