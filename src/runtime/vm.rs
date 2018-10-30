use value::VecList;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Bytecode {
    NOP     = 0x00,

    // memory and constants
    PUSH    = 0x01,
    POP     = 0x02,
    CONST   = 0x03,
    STORE   = 0x04,
    LOAD    = 0x05,

    // math & logic
    ADD     = 0x06,
    SUB     = 0x07,
    MUL     = 0x08,
    DIV     = 0x09,
    EQ      = 0x0a,
    NEQ     = 0x0b,
    GT      = 0x0c,
    GTE     = 0x0d,
    LT      = 0x0e,
    LTE     = 0x0f,

}

pub struct VM {
    stack: VecList,
}
