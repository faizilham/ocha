use std::cell::Cell;

use heap::Traceable;

#[derive(Debug)]
pub struct OchaStr {
    marked: Cell<bool>,
    string: String
}

impl OchaStr {
    pub fn new (string: String) -> OchaStr {
        OchaStr { marked: Cell::new(false), string }
    }

    pub fn to_string(&self) -> String {
        self.string.clone()
    }

    pub fn raw<'a>(&'a self) -> &'a String {
        &self.string
    }
}

impl PartialEq for OchaStr {
    fn eq(&self, other: &OchaStr) -> bool {
        self.string == other.string
    }
}

impl Traceable for OchaStr {
    fn mark(&self, marked: bool) {
        self.marked.set(marked);
    }

    fn is_traced(&self) -> bool {
        self.marked.get()
    }
}
