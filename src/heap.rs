use std::rc::Rc;

use value::Value;
use value::list::VecList;

enum Obj {
    Str(Rc<String>),
    List(Rc<VecList>),
}

struct HeapObj {
    mark: bool,
    obj: Obj
}

pub struct Heap {
    objs: Vec<HeapObj>
}

impl Heap {
    pub fn new() -> Heap {
        Heap { objs: Vec::new() }
    }

    pub fn allocate_str(&mut self, s: String) -> Value {
        let rf = Rc::new(s);
        let obj = Obj::Str(rf.clone());
        let heapobj = HeapObj { mark: false, obj };

        self.objs.push(heapobj);

        Value::Str(Rc::downgrade(&rf))
    }

    pub fn allocate_list(&mut self, list: VecList) -> Value {
        let rf = Rc::new(list);
        let obj = Obj::List(rf.clone());
        let heapobj = HeapObj { mark: false, obj };

        self.objs.push(heapobj);

        Value::List(Rc::downgrade(&rf))
    }
}
