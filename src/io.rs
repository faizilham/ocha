pub struct OchaIO {
    write_fn: fn(&str),
    writeln_fn: fn(),
    error_fn: fn(&str),
}

impl OchaIO {
    pub fn new(write_fn: fn(&str), writeln_fn: fn(), error_fn: fn(&str)) -> OchaIO {
        OchaIO {write_fn, writeln_fn, error_fn}
    }

    pub fn stdio() -> OchaIO {
        OchaIO::new(print, println, print_error)
    }

    pub fn write(&self, s: &str) {
        (self.write_fn)(s);
    }

    pub fn writeln(&self) {
        (self.writeln_fn)();
    }

    pub fn error(&self, err: &str) {
        (self.error_fn)(err);
    }
}

// STDOUT
pub fn print(s : &str) {
    print!("{} ", s);
}

pub fn println() {
    println!();
}

// STDERR
pub fn print_error(message : &str){
    eprintln!("{}", message);
}
