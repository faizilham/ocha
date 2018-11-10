use std::cell::{Cell, RefCell};
use std::rc::Rc;

pub type PCell<T> = Rc<Cell<T>>;

pub fn new_pcell<T> (t : T) -> PCell<T> {
    Rc::new(Cell::new(t))
}

pub type PRefCell<T> = Rc<RefCell<T>>;

pub fn new_prefcell<T> (t : T) -> PRefCell<T> {
    Rc::new(RefCell::new(t))
}
