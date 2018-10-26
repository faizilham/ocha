use std::rc::Rc;
use std::rc::Weak;

pub mod list;
use self::list::VecList;

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Weak<String>),
    List(Weak<VecList>),
    Nil
}

use self::Value::*;

pub fn unbox<T> (reference: &Weak<T>) -> Rc<T> {
    reference.upgrade().unwrap()
}

impl Value {
    pub fn ordering (&self, other: &Value) -> Result<i32, &'static str> {
        let order = match (self, other) {
            (&Int(ref a), &Int(ref b)) => if a > b { 1 } else if a < b { -1 } else { 0 },
            (&Float(ref a), &Float(ref b)) => if a > b { 1 } else if a < b { -1 } else { 0 },
            (&Str(ref a), &Str(ref b)) => {
                let ra = unbox(a);
                let rb = unbox(b);
                if ra > rb { 1 } else if ra < rb { -1 } else { 0 }
            },
            (_, _) => return Err("Invalid type for partial ordering")
        };

        Ok(order)
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            &Bool(ref b) => *b,
            &Nil => false,
            _ => true
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            &Int(ref i) => format!("{}", i),
            &Float(ref f) => format!("{}", f),
            &Bool(ref b) => String::from( if *b {"true"} else {"false"} ),
            &Str(ref s) => (*unbox(s)).clone(),
            &List(_) => String::from("[List]"),
            &Nil => String::from("nil")
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Int(ref a), &Int(ref b)) => a == b,
            (&Float(ref a), &Float(ref b)) => a == b,
            (&Bool(ref a), &Bool(ref b)) => a == b,
            (&Str(ref a), &Str(ref b)) => unbox(a) == unbox(b),
            (&List(ref a), &List(ref b)) => Rc::ptr_eq(&unbox(a), &unbox(b)),
            (&Nil, &Nil) => true,
            (_, _) => false
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match self {
            &Int(ref i) => Int(*i),
            &Float(ref f) => Float(*f),
            &Bool(ref b) => Bool(*b),
            &Str(ref s) => Str(s.clone()),
            &List(ref l) => List(l.clone()),
            &Nil => Nil
        }
    }
}
