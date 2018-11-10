use std::cell::RefCell;
use std::rc::Rc;

pub type PRefCell<T> = Rc<RefCell<T>>;

pub fn new_prefcell<T> (t : T) -> PRefCell<T> {
    Rc::new(RefCell::new(t))
}
