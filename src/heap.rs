use std::rc::Rc;

use value::Value;
use value::list::VecList;

#[derive(Debug)]
pub enum Object {
    Str(String),
    List(VecList),
}

impl Object {
    pub fn to_string(&self) -> String {
        match self {
            Object::Str(ref s) => s.clone(),
            Object::List(_) => String::from("[list]")
        }
    }
}

#[derive(Debug)]
pub struct HeapObj {
    mark: bool,
    obj: Object
}

impl HeapObj {
    pub fn borrow(&self) -> &Object {
        &self.obj
    }

    pub fn borrow_mut(&mut self) -> &mut Object {
        &mut self.obj
    }

    pub fn to_string(&self) -> String {
        self.obj.to_string()
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
        let heapobj = HeapObj { mark: false, obj };
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
}
