use heap::HeapPtr;
use std::cell::Cell;
use std::cell::RefCell;
use heap::Traceable;

/*** Value Declaration ***/

/// Ocha runtime value object
#[derive(Debug)]
pub enum Value {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),

    // heap objects
    Str(HeapPtr<OchaStr>),
    List(HeapPtr<VecList>),
}

use self::Value::*;

impl Value {
    pub fn ordering (&self, other: &Value) -> Result<i32, &'static str> {
        match (self, other) {
            (&Int(a), &Int(b)) => {
                Ok(if a > b { 1 } else if a < b { -1 } else { 0 })
            },
            (&Float(a), &Float(b)) => {
                Ok(if a > b { 1 } else if a < b { -1 } else { 0 })
            },

            (Str(a), Str(b)) => {
                let a = a.get_ref();
                let b = b.get_ref();

                let sa = a.raw();
                let sb = b.raw();

                Ok(if sa > sb { 1 } else if sa < sb { -1 } else { 0 })
            },

            (_, _) => Err("Invalid type for partial ordering")
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            &Bool(b) => b,
            Nil => false,
            _ => true
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Nil => String::from("nil"),
            Int(i) => format!("{}", i),
            Float(f) => format!("{}", f),
            Bool(b) => String::from( if *b {"true"} else {"false"} ),
            Str(s) => s.get_ref().to_string(),
            List(_) => String::from("[list]")
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Int(a), &Int(b)) => a == b,
            (&Float(a), &Float(b)) => a == b,
            (&Bool(a), &Bool(b)) => a == b,
            (Str(a), Str(b)) => {
                a.get_ref().raw() == b.get_ref().raw()
            },

            (List(a), List(b)) => {
                a == b
            }

            (Nil, Nil) => true,
            (_, _) => false
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match self {
            &Int(i) => Int(i),
            &Float(f) => Float(f),
            &Bool(b) => Bool(b),
            Str(r) => Str(r.clone()),
            List(r) => List(r.clone()),
            Nil => Nil
        }
    }
}


/*** OchaStr Declaration ***/

/// Garbage-collected string object
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

/*** VecList Declaration ***/

/// Garbage-collected list object
#[derive(Debug)]
pub struct VecList {
    marked: Cell<bool>,
    values: RefCell<Vec<Value>>
}

impl VecList {
    pub fn new () -> VecList {
        VecList {marked: Cell::new(false), values: RefCell::new(Vec::new())}
    }

    pub fn push(&self, value: Value){
        self.values.borrow_mut().push(value);
    }

    pub fn get(&self, index: i64) -> Result<Value, &'static str> {
        if let Some(value) = self.values.borrow().get(index as usize) {
            Ok((value).clone())
        } else {
            Err("Index out of bound")
        }
    }

    pub fn put(&self, index: i64, value: Value) -> Result<(), &'static str>{
        let mut val_ref = self.values.borrow_mut();

        if index >= 0 && (index as usize) < val_ref.len() {
            let index = index as usize;
            val_ref.remove(index);
            val_ref.insert(index, value);
            Ok(())
        } else {
            Err("Index out of bound")
        }
    }
}

impl Traceable for VecList {
    fn mark(&self, marked: bool) {
        self.marked.set(marked);
    }

    fn is_traced(&self) -> bool {
        self.marked.get()
    }
}