pub enum Exception {
    ParseErr(i32, String), // line, message
    RuntimeErr(i32, String), // line, message
}

use self::Exception::*;

impl Exception {
    pub fn to_string(&self) -> String {
        match self {
            ParseErr(line, message) => report_error(*line, message),
            RuntimeErr(line, message) => report_error(*line, message),
        }
    }
}

fn error_message(line: i32, message: &str) -> String{
    format!("{} [line {}]", message, line)
}

pub fn report_error(line: i32, message: &str) -> String {
    if line < 0 {
        String::from(message)
    } else {
        error_message(line, message)
    }
}

// pub fn print_error(message : &str){
//     eprintln!("Error: {}", message);
// }
