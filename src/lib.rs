use std::fs::File;
use std::io::prelude::*;
use std::error::Error;

mod error;
mod token;
mod lexer;
mod expr;
mod parser;
mod astprinter;

use error::throw_error;

pub fn run_file(filename : String) -> Result<(), String> {
    let contents = match read_file(&filename) {
        Ok(contents) => contents,
        _ => return throw_error(format!("File '{}' not found", filename))
    };

    let tokens = lexer::scan(contents)?;

    let expr = parser::parse(tokens)?;

    let mut printer = astprinter::AstPrinter{};
    let result = printer.evaluate(&expr);

    println!("{}", result);

    Ok(())
}

fn read_file (filename : &String) -> Result<String, Box<Error>> {
    // open file
    let mut file = File::open(filename)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}