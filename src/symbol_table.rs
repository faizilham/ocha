use std::collections::HashMap;

use exception::Exception;
use exception::Exception::ParseErr;
use token::Token;

#[derive(Debug, Clone, Copy)]
pub enum SymbolType {
    Var(isize),
    Func(usize),
}

pub struct SymbolTable {
    symbols: HashMap<String, SymbolType>,
    var_offset: isize,
}

impl SymbolTable {
    pub fn new () -> SymbolTable {
        SymbolTable { symbols: HashMap::new(), var_offset: 0 }
    }

    pub fn add(&mut self, name: &Token, symbol: SymbolType) {
        self.symbols.insert(name.lexeme.clone(), symbol);
    }

    pub fn add_var(&mut self, name: &Token) -> isize {
        let offset = self.var_offset;
        self.add(name, SymbolType::Var(offset));
        self.var_offset += 1;

        offset
    }

    pub fn add_func(&mut self, name: &Token, func_id: usize) {
        self.add(name, SymbolType::Func(func_id));
    }

    pub fn get(&mut self, name: &Token) -> Result<SymbolType, Exception> {
        if let Some(symbol) = self.symbols.get(&name.lexeme) {
            Ok(*symbol)
        } else {
            Err(declare_err(name))
        }
    }
}

fn declare_err(name: &Token) -> Exception {
    ParseErr(name.line, format!("Identifier '{}' is not declared", &name.lexeme))
}
