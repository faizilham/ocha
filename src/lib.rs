use std::fs::File;
use std::io::prelude::*;
use std::error::Error;

mod error;
mod token;

use error::throw_error;

pub fn run_file(filename : String) -> Result<(), String> {
    let contents = match read_file(&filename) {
        Ok(contents) => contents,
        _ => return throw_error(format!("File '{}' not found", filename))
    };

    println!("{}", contents);
    
    Ok(())
}

fn read_file (filename : &String) -> Result<String, Box<Error>> {
    // open file
    let mut file = File::open(filename)?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    Ok(contents)
}