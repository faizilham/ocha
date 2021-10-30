use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;
use std::slice::Iter;

use heap::{HeapPtr, Traceable};
use helper::{PRefCell, new_prefcell};
use program_data::FunctionSignature;

/*** Value Declaration ***/

/// Ocha runtime value object
#[derive(Debug)]
pub enum Value {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),
    Func(OchaFunc),

    // heap objects
    Str(HeapPtr<OchaStr>),
    List(HeapPtr<VecList>),
}

use self::Value::*;

pub fn get_traceable(value: &Value) -> Option<Rc<dyn Traceable>> {
    match value {
        Str(s) => Some(s.get_ref()),
        List(l) => Some(l.get_ref()),
        Func(f) => {
            if let Some(env) = &f.env {
                Some(env.get_ref())
            } else {
                None
            }
        }
        _ => None
    }
}

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
            List(_) => String::from("[list]"),
            Func(f) => format!("Func({})", f.signature.num_args),
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
            Func(f) => Func(f.clone()),
            Nil => Nil
        }
    }
}


/*** OchaStr Declaration ***/

/// Garbage-collected string object
#[derive(Debug)]
pub struct OchaStr {
    traced: Cell<bool>,
    string: String,
    literal : bool
}

impl OchaStr {
    pub fn new (string: String, literal: bool) -> OchaStr {
        OchaStr { traced: Cell::new(false), string, literal }
    }

    pub fn to_string(&self) -> String {
        self.string.clone()
    }

    pub fn raw (&self) -> &String {
        &self.string
    }
}

impl PartialEq for OchaStr {
    fn eq(&self, other: &OchaStr) -> bool {
        self.string == other.string
    }
}

impl Traceable for OchaStr {
    fn trace(&self) {
        self.traced.set(true);
    }

    fn reset_trace(&self) {
        self.traced.set(false);
    }

    fn is_traced(&self) -> bool {
        self.literal || self.traced.get()
    }
}

/*** VecList Declaration ***/

/// Garbage-collected list object
#[derive(Debug)]
pub struct VecList {
    traced: Cell<bool>,
    values: RefCell<Vec<Value>>
}

impl VecList {
    pub fn from (vector : Vec<Value>) -> VecList {
        VecList { traced: Cell::new(false), values: RefCell::new(vector) }
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
    fn trace(&self) {
        if self.is_traced() {
            return;
        }

        self.traced.set(true);

        for val in self.values.borrow().iter() {
            if let Some(traceable) = get_traceable(val) {
                traceable.trace()
            }
        }
    }

    fn reset_trace(&self) {
        self.traced.set(false);
    }

    fn is_traced(&self) -> bool {
        self.traced.get()
    }
}

/*** OchaFunc Declaration ***/

#[derive(Debug, Clone)]
pub enum CapturedVar {
    Empty,
    Unclosed(usize),
    Closed(Value)
}

#[derive(Debug, Clone)]
pub struct Environment {
    traced: Cell<bool>,
    parent: Option<HeapPtr<Environment>>,
    upvalues: Vec<PRefCell<CapturedVar>>,
}

impl Environment {
    pub fn new(parent: Option<HeapPtr<Environment>>, size: usize) -> Self {
        let mut upvalues = Vec::with_capacity(size);

        for _ in 0..size {
            upvalues.push(new_prefcell(CapturedVar::Empty));
        }

        Self { traced: Cell::new(false), parent, upvalues}
    }

    pub fn get(&self, level: usize, new_index: usize) -> PRefCell<CapturedVar> {
        if level == 1 {
            return self.upvalues.get(new_index).expect("Invalid upvalue index").clone();
        } else if level > 1 {
            if let Some(parent) = &self.parent {
                return parent.get_ref().get(level - 1, new_index);
            }

            panic!("Accessing empty parent environment");
        }

        panic!("Accessing invalid environment level");
    }

    pub fn values(&self) -> Iter<PRefCell<CapturedVar>> {
        self.upvalues.iter()
    }
}

impl Traceable for Environment {
    fn trace(&self) {
        if self.is_traced() {
            return;
        }

        self.traced.set(true);

        // trace upvalues and parent
        for upvalue in self.upvalues.iter() {
            if let CapturedVar::Closed(value) = &*upvalue.borrow() {
                if let Some(traceable) = get_traceable(value) {
                    traceable.trace();
                }
            }
        }

        if let Some(parent) = &self.parent {
            parent.get_ref().trace();
        }
    }

    fn reset_trace(&self) {
        self.traced.set(false);
    }

    fn is_traced(&self) -> bool {
        self.traced.get()
    }
}

#[derive(Debug, Clone)]
pub struct OchaFunc {
    pub signature: FunctionSignature,
    env: Option<HeapPtr<Environment>>,
}

impl OchaFunc {
    pub fn new(signature: FunctionSignature, env: Option<HeapPtr<Environment>>) -> OchaFunc {
        OchaFunc { signature, env }
    }

    pub fn get_env_var(&self, level: usize, new_index: usize) -> Option<PRefCell<CapturedVar>> {
        if let Some(env) = &self.env {
            Some(env.get_ref().get(level, new_index))
        } else {
            None
        }
    }

    pub fn get_env(&self) -> Option<HeapPtr<Environment>> {
        if let Some(env) = &self.env {
            Some(env.clone())
        } else {
            None
        }
    }
}
