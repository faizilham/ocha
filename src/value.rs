use std::f64::EPSILON;

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Nil
}

use self::Value::*;

impl Value {
    pub fn to_string(&self) -> String {
        match *self {
            Value::Int(ref i) => format!("{}", i),
            Value::Float(ref f) => format!("{}", f),
            Value::Bool(ref b) => String::from( if *b {"true"} else {"false"} ),
            Value::Str(ref s) => s.clone(),
            Value::Nil => String::from("nil")
        }
    }

    pub fn to_bool(&self) -> bool {
        match *self {
            Value::Int(ref i) => *i != 0,
            Value::Float(ref f) => (*f).abs() < EPSILON,
            Value::Bool(ref b) => *b,
            Value::Str(ref s) => s.len() > 0,
            Value::Nil => false
        }
    }

    pub fn copy(&self) -> Value {
        match *self {
            Value::Int(ref i) => Value::Int(*i),
            Value::Float(ref f) => Value::Float(*f),
            Value::Bool(ref b) => Value::Bool(*b),
            Value::Str(ref s) => Value::Str(s.clone()),
            Value::Nil => Value::Nil
        }
    }

    pub fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (&Int(ref a), &Int(ref b)) => a == b,
            (&Float(ref a), &Float(ref b)) => a == b,
            (&Bool(ref a), &Bool(ref b)) => a == b,
            (&Str(ref a), &Str(ref b)) => a == b,
            (&Nil, &Nil) => true,
            (_, _) => false
        }
    }

    pub fn ordering (&self, other: &Value) -> Result<i32, &'static str> {
        let order = match (self, other) {
            (&Int(ref a), &Int(ref b)) => if a > b { 1 } else if a < b { -1 } else { 0 },
            (&Float(ref a), &Float(ref b)) => if a > b { 1 } else if a < b { -1 } else { 0 },
            (&Str(ref a), &Str(ref b)) => if a > b { 1 } else if a < b { -1 } else { 0 },
            (&Bool(_), &Bool(_)) => return Err("Cannot compare two boolean values"),            
            (&Nil, &Nil) => return Err("Cannot compare two nil values"),
            (_, _) => return Err("Cannot compare two different type values")
        };

        Ok(order)
    }
}