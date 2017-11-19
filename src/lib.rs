mod ast;
// mod astprinter;
mod environment;
mod exception;
mod interpreter;
mod lexer;
mod parser;
mod token;
mod value;

use exception::print_error;
use std::fs::File;
use std::io::prelude::*;

pub fn run_file(filename : String) -> Result<(), ()> {
    let source = read_file(&filename)?;
    let tokens = lexer::scan(source)?;
    let statements = parser::parse(tokens)?;

    let mut inter = interpreter::Interpreter::new();
    inter.interpret(&statements)?;

    Ok(())
}

fn read_file (filename : &String) -> Result<String, ()> {
    // open file
    let file = File::open(filename);

    if let Ok(mut file) = file {
        let mut buffer = String::new();

        if let Ok(_) = file.read_to_string(&mut buffer) {
            return Ok(buffer)
        } 
    }

    print_error(&format!("File '{}' not found", filename));
    Err(())
}