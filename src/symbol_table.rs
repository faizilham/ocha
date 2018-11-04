use std::collections::HashMap;

use exception::Exception;
use exception::Exception::ParseErr;
use token::Token;

pub struct SymbolTable {
    symbols: HashMap<String, isize>,
    local_offset: isize,
}

impl SymbolTable {
    pub fn new () -> SymbolTable {
        SymbolTable { symbols: HashMap::new(), local_offset: 0 }
    }

    pub fn add(&mut self, name: &Token, offset: isize) {
        self.symbols.insert(name.lexeme.clone(), offset);
    }

    pub fn add_local(&mut self, name: &Token) -> isize {
        let offset = self.local_offset;
        self.add(name, offset);
        self.local_offset += 1;

        offset
    }

    pub fn get(&mut self, name: &Token) -> Result<isize, Exception> {
        if let Some(offset) = self.symbols.get(&name.lexeme) {
            Ok(*offset)
        } else {
            Err(declare_err(name))
        }
    }
}

fn declare_err(name: &Token) -> Exception {
    ParseErr(name.line, format!("Identifier '{}' is not declared", &name.lexeme))
}
