pub fn throw_error(message : String) -> Result<(), String> {
    print_error(&message);
    Err(message)
}

pub fn error_message(line: i32, message: &str) -> String{
    format!("{} [line {}]", message, line)
}

pub fn report_error(line: i32, message: &str){
    print_error(error_message(line, message).as_str());
}

pub fn print_error(message : &str){
    eprintln!("Error: {}", message);
}