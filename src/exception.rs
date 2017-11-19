pub enum Exception {
    ParseErr(i32, String), // line, message
    RuntimeErr(i32, String) // line, message
}

impl Exception {
    pub fn print(&self) {
        match self {
            &Exception::ParseErr(ref line, ref message) => report_error(*line, message),
            &Exception::RuntimeErr(ref line, ref message) => report_error(*line, message),
        };
    }
}

fn error_message(line: i32, message: &str) -> String{
    format!("{} [line {}]", message, line)
}

fn report_error(line: i32, message: &str){
    print_error(error_message(line, message).as_str());
}

pub fn print_error(message : &str){
    eprintln!("Error: {}", message);
}