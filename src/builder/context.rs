use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;

use exception::Exception;
use exception::Exception::ParseErr;
use token::Token;

use super::block::Label;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ContextType {
    GlobalCtx,
    FuncCtx,
}

pub struct Context {
    pub ctx_type: ContextType,
    pub while_end_label: Label, // should be negative when outside while
    pub parent: Option<ContextRef>,
    pub local_symbols: SymbolTableRef,
}

pub type ContextRef = Rc<RefCell<Context>>;

impl Context {
    pub fn new(ctx_type: ContextType, parent: Option<ContextRef>) -> Context {
        let local_symbols = SymbolTable::new_ref(None);
        Context{ ctx_type, while_end_label: -1, parent, local_symbols }
    }

    pub fn new_ref(ctx_type: ContextType, parent: Option<ContextRef>) -> ContextRef {
        let ctx = Context::new(ctx_type, parent);

        Rc::new(RefCell::new(ctx))
    }

    pub fn create_child(parent: &ContextRef, ctx_type: ContextType) -> ContextRef {
        Context::new_ref(ctx_type, Some(parent.clone()))
    }
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

    pub parent: Option<SymbolTableRef>, // parent scope in the same context
}

pub type SymbolTableRef = Rc<RefCell<SymbolTable>>;

impl SymbolTable {
    pub fn new (parent: Option<SymbolTableRef>) -> SymbolTable {

        let var_offset = match &parent {
            None => 0,
            Some(symtable) => {
                let rf = symtable.borrow();
                rf.var_offset
            }
        };

        SymbolTable { symbols: HashMap::new(), var_offset, parent }
    }

    pub fn new_ref(parent: Option<SymbolTableRef>) -> SymbolTableRef {
        Rc::new(RefCell::new(SymbolTable::new(parent)))
    }

    pub fn create_child(parent: &SymbolTableRef) -> SymbolTableRef {
        SymbolTable::new_ref(Some(parent.clone()))
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
            return Some(*symbol);
        }

        if let Some(parent) = &self.parent {
            parent.borrow().get(name)
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
