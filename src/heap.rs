use std::rc::Rc;
use std::rc::Weak;

use value::Value;
use value::list::VecList;
use value::ocha_str::OchaStr;

pub trait Traceable {
    fn mark(&self, marked: bool);
    fn is_traced(&self) -> bool;
}

#[derive(Debug)]
pub struct HeapPtr<T>(Weak<T>);

impl<T> HeapPtr<T> {
    pub fn new(rf: Rc<T>) -> HeapPtr<T> {
        HeapPtr(Rc::downgrade(&rf))
    }

    pub fn get_ref (&self) -> Rc<T> {
        if let Some(rf) = self.0.upgrade() {
            rf
        } else {
            panic!("Runtime Error: Inaccessible heap object");
        }
    }
}

impl<T> PartialEq for HeapPtr<T> {
    fn eq(&self, other: &HeapPtr<T>) -> bool {
        Rc::ptr_eq(&self.get_ref(), &other.get_ref())
    }
}

impl<T> Clone for HeapPtr<T> {
    fn clone(&self) -> HeapPtr<T> {
        HeapPtr(self.0.clone())
    }
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

        Value::Str(HeapPtr::new(rf))
    }

    pub fn allocate_list(&mut self, list: VecList) -> Value {
        let rf = Rc::new(list);
        self.objs.push(rf.clone());

        Value::List(HeapPtr::new(rf))
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
