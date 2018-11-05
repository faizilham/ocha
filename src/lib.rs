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
use io::{OchaIO, OchaStdIO};

pub fn run_file(filename : String) -> Result<(), ()> {
    let mut io =  OchaStdIO::new();

    run_file_with_io(filename, &mut io)
}

fn run_file_with_io(filename : String, io: &mut OchaIO) -> Result<(), ()> {
    let source = read_file_with_io(&filename, io)?;
    let chunk = parse_and_build(source).map_err(|e| report_exceptions(io, e))?;

    execute(chunk, io).map_err(|e| report_exception(io, e))
}

fn read_file_with_io (filename : &str, io: &mut OchaIO) -> Result<String, ()> {
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

fn execute(chunk: Chunk, io: &mut OchaIO) -> Result<(), Exception> {
    let mut vm = VM::new(chunk, io);
    vm.run()
}

// Exception handlers
fn report_exception(io: &mut OchaIO, exception: Exception) -> () {
    io.error(&exception.to_string());
    ()
}

fn report_exceptions(io: &mut OchaIO, exceptions: Vec<Exception>) -> () {
    for e in exceptions {
        report_exception(io, e);
    }

    ()
}


// integration test
#[cfg(test)]
mod test {
    use io::OchaIO;

    pub struct MockIO {
        at_start: bool,
        pub out: Vec<String>,
        pub err: Vec<String>
    }

    impl MockIO {
        pub fn new() -> MockIO {
            MockIO {
                at_start: true,
                out: Vec::new(),
                err: Vec::new()
            }
        }
    }

    impl OchaIO for MockIO {
        fn write(&mut self, s: &str) {
            if self.at_start {
                self.at_start = false;
                self.out.push(format!("{}", s));
            } else {
                let last = self.out.len() - 1;
                let current_str = self.out.get_mut(last).unwrap();

                current_str.push_str(&format!(" {}", s));
            }
        }

        fn writeln(&mut self) {
            if self.at_start {
                self.out.push(String::new());
            } else {
                self.at_start = true;
            }
        }

        fn error(&mut self, err: &str) {
            self.err.push(String::from(err));
        }
    }

    use super::run_file_with_io;

    use std::fs::File;
    use std::io::{BufRead, BufReader};

    fn read_output(filename: String) -> Vec<String> {
        let file = File::open(filename).unwrap();

        let mut result = Vec::new();

        for line in BufReader::new(file).lines() {
            result.push(String::from(line.unwrap()));
        }

        result
    }

    fn run_test(filename: String) -> (Result<(), ()>, MockIO) {
        let mut io = MockIO::new();

        let result = run_file_with_io(filename, &mut io);

        (result, io)
    }

    fn test(testname: &str) {
        let out_file = format!("test/{}.out", testname);
        let ocha_file = format!("test/{}.ocha", testname);

        let expected = read_output(out_file);

        let (result, io) = run_test(ocha_file);

        assert_eq!(result, Ok(()));

        let MockIO { out, .. } = io;

        assert_eq!(out, expected);
    }

    #[test]
    fn test_expression() {
        test("expression");
    }

    #[test]
    fn test_variable() {
        test("variable");
    }

    #[test]
    fn test_list() {
        test("list");
    }

    #[test]
    fn test_condition() {
        test("condition");
    }

    #[test]
    fn test_loop() {
        test("loop");
    }
}
