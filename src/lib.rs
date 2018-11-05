mod ast;
mod builder;
mod exception;
mod heap;
mod io;
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
use vm::{VM, Chunk};
use io::OchaIO;

pub fn run_file(filename : String) -> Result<(), ()> {
    let io =  OchaIO::stdio();

    run_file_with_io(filename, &io)
}

fn run_file_with_io(filename : String, io: &OchaIO) -> Result<(), ()> {
    let source = read_file(&filename, io)?;
    let chunk = parse_and_build(source).map_err(|e| report_exceptions(io, e))?;

    execute(chunk, &io).map_err(|e| report_exception(io, e))
}

fn read_file (filename : &String, io: &OchaIO) -> Result<String, ()> {
    // open file
    let file = File::open(filename);

    if let Ok(mut file) = file {
        let mut buffer = String::new();

        if let Ok(_) = file.read_to_string(&mut buffer) {
            return Ok(buffer)
        }
    }

    io.error(&format!("Error: File '{}' not found", filename));
    Err(())
}

fn parse_and_build(source : String) -> Result<Chunk, Vec<Exception>> {
    let tokens = lexer::scan(source)?;
    let statements = parser::parse(tokens)?;

    builder::build(statements)
}

fn execute(chunk: Chunk, io: &OchaIO) -> Result<(), Exception> {
    let mut vm = VM::new(chunk, io);
    vm.run()
}

// Exception handlers
fn report_exception(io: &OchaIO, exception: Exception) -> () {
    io.error(&exception.to_string());
    ()
}

fn report_exceptions(io: &OchaIO, exceptions: Vec<Exception>) -> () {
    for e in exceptions {
        report_exception(io, e);
    }

    ()
}


// TODO: integration test
