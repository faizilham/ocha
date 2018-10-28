pub mod ocha_str;
pub mod list;

use self::ocha_str::OchaStr;
use self::list::VecList;

use heap::HeapPtr;

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

            (&Str(ref a), &Str(ref b)) => {
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
            &Bool(ref b) => *b,
            &Nil => false,
            _ => true
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            &Nil => String::from("nil"),
            &Int(ref i) => format!("{}", i),
            &Float(ref f) => format!("{}", f),
            &Bool(ref b) => String::from( if *b {"true"} else {"false"} ),
            &Str(ref s) => s.get_ref().to_string(),
            &List(_) => String::from("[list]")
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Int(ref a), &Int(ref b)) => a == b,
            (&Float(ref a), &Float(ref b)) => a == b,
            (&Bool(ref a), &Bool(ref b)) => a == b,
            (&Str(ref a), &Str(ref b)) => {
                a.get_ref().raw() == b.get_ref().raw()
            },

            (&List(ref a), &List(ref b)) => {
                a == b
            }

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
            &Str(ref r) => Str(r.clone()),
            &List(ref r) => List(r.clone()),
            &Nil => Nil
        }
    }
}
