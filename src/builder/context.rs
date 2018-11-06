use super::block::Label;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ContextType {
    MainCtx,
    FuncCtx,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Context {
    pub ctx_type: ContextType,
    pub while_end_label: Label,
}

impl Context {
    pub fn new(ctx_type: ContextType, while_end_label: Label) -> Context {
        Context{ ctx_type, while_end_label }
    }
}
