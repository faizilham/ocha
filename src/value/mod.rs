use std::rc::Rc;
use std::rc::Weak;

use super::heap::HeapObj;
use super::heap::Object;

pub mod list;

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Obj(Weak<HeapObj>),
    Nil
}

use self::Value::*;

pub fn unbox<T> (reference: &Weak<T>) -> Rc<T> {
    reference.upgrade().unwrap()
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

            (&Obj(ref a), &Obj(ref b)) => {
                let oa = unbox(a);
                let ob = unbox(b);

                if let (&Object::Str(ref sa), &Object::Str(ref sb)) = (oa.borrow(), ob.borrow()) {
                    Ok(if sa > sb { 1 } else if sa < sb { -1 } else { 0 })
                } else {
                    Err("Invalid type for partial ordering")
                }
            },

            (_, _) => Err("Invalid type for partial ordering")
        }
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
            &Obj(ref o) => unbox(o).to_string(),
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
            (&Obj(ref a), &Obj(ref b)) => {
                let oa = unbox(a);
                let ob = unbox(b);

                if let (&Object::Str(ref sa), &Object::Str(ref sb)) = (oa.borrow(), ob.borrow()) {
                    sa == sb
                } else {
                    Rc::ptr_eq(&oa, &ob)
                }
            },

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
            &Obj(ref o) => Obj(o.clone()),
            &Nil => Nil
        }
    }
}
