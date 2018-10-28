use std::cell::Cell;
use std::cell::RefCell;

use value::Value;
use heap::Traceable;

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
