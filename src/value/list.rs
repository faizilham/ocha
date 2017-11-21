use value::Value;

#[derive(Debug)]
pub struct VecList {
    values: Vec<Value>
}

impl VecList {
    pub fn new () -> VecList {
        VecList {values: Vec::new()}
    }

    pub fn push(&mut self, value: Value){
        self.values.push(value);
    }

    pub fn get(&self, index: i64) -> Result<Value, &'static str> {
        if let Some(ref value) = self.values.get(index as usize) {
            Ok((*value).clone())
        } else {
            Err("Index out of bound")
        }
    }

    pub fn put(&mut self, index: i64, value: Value) -> Result<(), &'static str>{
        if index >= 0 && (index as usize) < self.values.len() {
            let index = index as usize;
            self.values.remove(index);
            self.values.insert(index, value);
            Ok(())
        } else {
            Err("Index out of bound")
        }
    }
}