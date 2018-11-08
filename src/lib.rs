mod ast;
mod builder;
mod exception;
mod heap;
mod helper;
mod io;
mod lexer;
mod program_data;
mod parser;
mod resolver;
mod token;
mod value;
mod vm;

use std::fs::File;
use std::io::prelude::*;

use exception::Exception;
use vm::{VM, Module};
use io::{OchaIO, OchaStdIO};

pub fn run_file(filename : String) -> Result<(), ()> {
    let mut io =  OchaStdIO::new();

    run_file_with_io(filename, &mut io)
}

fn run_file_with_io(filename : String, io: &mut OchaIO) -> Result<(), ()> {
    let source = read_file_with_io(&filename, io)?;
    let module = parse_and_build(source).map_err(|e| report_exceptions(io, e))?;

    execute(module, io).map_err(|e| report_exception(io, e))
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

fn parse_and_build(source : String) -> Result<Module, Vec<Exception>> {
    let tokens = lexer::scan(source)?;
    let statements = parser::parse(tokens)?;

    let functions = resolver::resolve(&statements)?;

    builder::build(statements, functions)
}

fn execute(module: Module, io: &mut OchaIO) -> Result<(), Exception> {
    let mut vm = VM::new(module, io);
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
    // Success cases
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

    #[test]
    fn test_function() {
        test("function");
    }

    #[test]
    fn test_local_scope() {
        test("local_scope");
    }

    #[test]
    fn test_global() {
        test("global");
    }

    // Error cases

    #[test]
    fn test_err_parser() {
        test_err("err_parser");
    }

    #[test]
    fn test_err_resolver() {
        test_err("err_resolver");
    }

    // test helper funcitons
    use super::run_file_with_io;
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    fn read_output(filename: String) -> Vec<String> {
        let file = File::open(filename).expect("Output test file not found");

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

    fn test_err(testname: &str) {
        let out_file = format!("test/{}.out", testname);
        let ocha_file = format!("test/{}.ocha", testname);

        let expected = read_output(out_file);

        let (result, io) = run_test(ocha_file);

        assert_eq!(result, Err(()));

        let MockIO { err, .. } = io;

        assert_eq!(err.len(), expected.len());

        for (e, exp) in err.iter().zip(expected.iter()) {
            let e = e.to_lowercase();
            let exp = exp.to_lowercase();

            assert!(e.contains(&exp))
        }
    }

    // Mock IO object
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
}
