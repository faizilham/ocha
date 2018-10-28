use std::rc::Rc;

use value::Value;
use value::list::VecList;
use value::ocha_str::OchaStr;

pub trait Traceable {
    fn mark(&self, marked: bool);
    fn is_traced(&self) -> bool;
}

pub struct Heap {
    objs: Vec<Rc<Traceable>>
}

impl Heap {
    pub fn new() -> Heap {
        Heap { objs: Vec::new() }
    }

    pub fn allocate_str(&mut self, s: String) -> Value {
        let obj = OchaStr::new(s);

        let rf = Rc::new(obj);
        self.objs.push(rf.clone());

        Value::Str(Rc::downgrade(&rf))
    }

    pub fn allocate_list(&mut self, list: VecList) -> Value {
        let rf = Rc::new(list);
        self.objs.push(rf.clone());

        Value::List(Rc::downgrade(&rf))
    }

    pub fn sweep(&mut self) {
        let mut i = 0;
        while i < self.objs.len() {
            if self.objs[i].is_traced() {
                self.objs[i].mark(false);
                i += 1;
            } else {
                self.objs.remove(i);
            }
        }
    }
}
