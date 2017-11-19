use std::rc::Rc;
use std::collections::HashMap;
use exception::Exception;
use exception::Exception::RuntimeErr;
use token::Token;
use value::Value;

pub struct Environment {
    symbols: HashMap<String, Rc<Value>>
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

    pub fn put(&mut self, name: &Token, value: Rc<Value>) {
        self.symbols.insert(name.lexeme.clone(), value);
    }

    pub fn get(&mut self, name: &Token) -> Result<Rc<Value>, Exception> {
        if let Some(ref val) = self.symbols.get(&name.lexeme) {
            Ok((*val).clone())
        } else {
            Err(declare_err(name))
        }
    }
}

fn declare_err(name: &Token) -> Exception {
    RuntimeErr(name.line, format!("Identifier '{}' is not declared", &name.lexeme))
}