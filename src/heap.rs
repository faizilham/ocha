use std::rc::Rc;
use std::cell::Cell;

use value::Value;
use value::list::VecList;

#[derive(Debug)]
enum Object {
    Str(String),
    List(VecList),
}

#[derive(Debug)]
pub struct HeapObj {
    marked: Cell<bool>,
    obj: Object
}

impl HeapObj {
    // gc related
    pub fn mark(&self, marked: bool) {
        self.marked.set(marked);
    }

    pub fn untraceable(&self) -> bool {
        !self.marked.get()
    }

    // object related
    pub fn to_string(&self) -> String {
        match self.obj {
            Object::Str(ref s) => s.clone(),
            Object::List(_) => String::from("[list]")
        }
    }

    pub fn get_string<'a> (&'a self) -> Option<&'a String> {
        if let Object::Str(ref s) = self.obj {
            Some(s)
        } else {
            None
        }
    }

    pub fn get_list<'a> (&'a self) -> Option<&'a VecList> {
        if let Object::List(ref list) = self.obj {
            Some(list)
        } else {
            None
        }
    }
}

pub struct Heap {
    objs: Vec<Rc<HeapObj>>
}

impl Heap {
    pub fn new() -> Heap {
        Heap { objs: Vec::new() }
    }

    fn allocate(&mut self, obj : Object) -> Value {
        let heapobj = HeapObj { marked: Cell::new(false), obj };
        let rf = Rc::new(heapobj);

        self.objs.push(rf.clone());

        Value::Obj(Rc::downgrade(&rf))
    }

    pub fn allocate_str(&mut self, s: String) -> Value {
        self.allocate(Object::Str(s))
    }

    pub fn allocate_list(&mut self, list: VecList) -> Value {
        self.allocate(Object::List(list))
    }

    pub fn sweep(&mut self) {
        let mut i = 0;
        while i < self.objs.len() {

            if self.objs[i].untraceable() {
                self.objs.remove(i);
            } else {
                self.objs[i].mark(false);
                i += 1;
            }
        }

    }
}
