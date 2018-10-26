use value::Value;
use std::cell::RefCell;

#[derive(Debug)]
pub struct VecList {
    values: RefCell<Vec<Value>>
}

impl VecList {
    pub fn new () -> VecList {
        VecList {values: RefCell::new(Vec::new())}
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
