mod ast;
mod builder;
mod exception;
mod heap;
mod lexer;
mod line_data;
mod parser;
mod symbol_table;
mod token;
mod value;
mod vm;

use std::fs::File;
use std::io::prelude::*;

use exception::Exception;
use vm::VM;

pub fn print_error(message : &str){
    eprintln!("Error: {}", message);
}

fn print_exception(exception: Exception) -> () {
    print_error(&exception.to_string());
    ()
}

fn print_exceptions(exceptions: Vec<Exception>) -> () {
    for e in exceptions {
        print_exception(e);
    }

    ()
}

pub fn run_file(filename : String) -> Result<(), ()> {
    let source = read_file(&filename)?;

    let chunk = parse_and_build(source).map_err(print_exceptions)?;

    execute(chunk).map_err(print_exception)
}

fn parse_and_build(source : String) -> Result<vm::Chunk, Vec<Exception>> {
    let tokens = lexer::scan(source)?;
    let statements = parser::parse(tokens)?;

    builder::build(statements)
}

fn execute(chunk: vm::Chunk) -> Result<(), Exception> {
    let mut vm = VM::new(chunk);
    vm.run()
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
