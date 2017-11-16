mod ast;
// mod astprinter;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod token;
mod value;

use error::throw_error;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

pub fn run_file(filename : String) -> Result<(), String> {
    let contents = match read_file(&filename) {
        Ok(contents) => contents,
        _ => return throw_error(format!("File '{}' not found", filename))
    };

    let tokens = lexer::scan(contents)?;
    let statements = parser::parse(tokens)?;

    let mut inter = interpreter::Interpreter::new();

    inter.interpret(&statements);

    Ok(())
}

fn read_file (filename : &String) -> Result<String, Box<Error>> {
    // open file
    let mut file = File::open(filename)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}