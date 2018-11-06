use std::rc::Rc;
use std::cell::RefCell;

use super::block::Label;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ContextType {
    MainCtx,
    FuncCtx,
}

#[derive(Debug)]
pub struct Context {
    pub ctx_type: ContextType,
    pub while_end_label: Label, // should be negative when outside while
    pub parent: Option<ContextRef>,
}

pub type ContextRef = Rc<RefCell<Context>>;

impl Context {
    pub fn new(ctx_type: ContextType, parent: Option<ContextRef>) -> Context {
        Context{ ctx_type, while_end_label: -1, parent }
    }

    pub fn new_ref(ctx_type: ContextType, parent: Option<ContextRef>) -> ContextRef {
        let ctx = Context::new(ctx_type, parent);

        Rc::new(RefCell::new(ctx))
    }

    pub fn create_child(parent: &ContextRef, ctx_type: ContextType) -> ContextRef {
        Context::new_ref(ctx_type, Some(parent.clone()))
    }
}
