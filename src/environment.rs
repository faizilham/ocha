use std::collections::HashMap;

use exception::Exception;
use exception::Exception::RuntimeErr;
use token::Token;
use value::Value;

pub struct Environment {
    symbols: HashMap<String, Value>
}

impl Environment {
    pub fn new () -> Environment {
        Environment { symbols: HashMap::new() }
    }

    pub fn has(&mut self, name: &Token) -> bool {
        self.symbols.contains_key(&name.lexeme)
    }

    pub fn check_declared(&mut self, name: &Token) -> Result<(), Exception> {
        if !self.has(name) {
            Err(declare_err(name))
        } else {
            Ok(())
        }
    }

    pub fn put(&mut self, name: &Token, value: Value) {
        self.symbols.insert(name.lexeme.clone(), value);
    }

    pub fn get(&mut self, name: &Token) -> Result<Value, Exception> {
        if let Some(ref value) = self.symbols.get(&name.lexeme) {
            Ok((*value).clone())
        } else {
            Err(declare_err(name))
        }
    }
}

fn declare_err(name: &Token) -> Exception {
    RuntimeErr(name.line, format!("Identifier '{}' is not declared", &name.lexeme))
}