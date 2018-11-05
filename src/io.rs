pub trait OchaIO {
    fn write(&mut self, s: &str);
    fn writeln(&mut self);
    fn error(&mut self, err: &str);
}

pub struct OchaStdIO {}

impl OchaStdIO {
    pub fn new() -> OchaStdIO {
        OchaStdIO {}
    }
}

impl OchaIO for OchaStdIO {
    fn write(&mut self, s: &str) {
        print!("{} ", s);
    }

    fn writeln(&mut self) {
        println!();
    }

    fn error(&mut self, err: &str) {
        eprintln!("{}", err);
    }
}
