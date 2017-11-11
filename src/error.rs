pub fn throw_error(message : String) -> Result<(), String> {
    print_error(&message);
    Err(message)
}

pub fn report_error(line: i32, message: &str){
    print_error(format!("{} [line {}]", message, line).as_str());
}

pub fn print_error(message : &str){
    eprintln!("Error: {}", message);
}