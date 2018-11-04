use std::rc::Rc;
use std::rc::Weak;

pub trait Traceable {
    fn trace(&self);
    fn reset_trace(&self);
    fn is_traced(&self) -> bool;
}

#[derive(Debug)]
pub struct HeapPtr<T : Traceable>(Weak<T>);

impl<T : Traceable> HeapPtr<T> {
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

impl<T : Traceable> PartialEq for HeapPtr<T> {
    fn eq(&self, other: &HeapPtr<T>) -> bool {
        Rc::ptr_eq(&self.get_ref(), &other.get_ref())
    }
}

impl<T : Traceable> Clone for HeapPtr<T> {
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

    pub fn allocate<T : Traceable + 'static>(&mut self, obj: T) -> HeapPtr<T> {
        let rf = Rc::new(obj);
        self.objs.push(rf.clone());

        HeapPtr::new(rf)
    }

    pub fn sweep(&mut self) {
        let mut i = 0;
        while i < self.objs.len() {
            if self.objs[i].is_traced() {
                self.objs[i].reset_trace();
                i += 1;
            } else {
                self.objs.remove(i);
            }
        }
    }

    pub fn clear(&mut self) {
        self.objs.clear();
    }

    pub fn size(&self) -> usize {
        self.objs.len()
    }
}
