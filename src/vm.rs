use value::Value;
use token::Literal;
use heap::Heap;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Bytecode {
    HALT,
    NOP,

    // memory and constants
    POP,
    CONST(usize),
    STORE(usize),
    LOAD(usize),

    // math & logic
    ADD,
    SUB,
    MUL,
    DIV,
    EQ,
    NEQ,
    GT,
    GTE,
    LT,
    LTE,
    NEG,
    NOT,

    // to be removed
    PRINT(usize),
}

use self::Bytecode::*;
use self::Value::*;

pub struct Chunk {
    pub codes: Vec<Bytecode>,
    pub literals: Vec<Literal>
}

pub struct VM {
    codes: Vec<Bytecode>,
    constants: Vec<Value>,
    stack: Vec<Value>,
    heap: Heap,
    ip: usize,
}

impl VM {
    pub fn new (chunk: Chunk) -> VM {
        let Chunk { codes, mut literals } = chunk;

        let stack = Vec::new();
        let mut heap = Heap::new();

        let mut constants = Vec::with_capacity(literals.len());

        for literal in literals.drain(..) {
            let value = match literal {
                Literal::Nil => Nil,
                Literal::Int(i) => Int(i),
                Literal::Float(f) => Float(f),
                Literal::Bool(b) => Bool(b),
                Literal::Str(s) => heap.allocate_str(s)
            };

            constants.push(value);
        }

        VM { codes, constants, stack, heap, ip: 0 }
    }

    pub fn run(&mut self) {
        if let Err(e) = self.run_loop() {
            println!("Error: {}", e);
        }

        self.heap.sweep();
    }

    fn run_loop(&mut self) -> Result<(), &'static str> {
        loop {
            let code = self.codes[self.ip];
            self.ip += 1;

            match code {
                HALT => break,
                NOP => (), // do nothing
                POP => {
                    self.pop();
                },

                CONST(idx) => {
                    let value = self.get_constant(idx);
                    self.push(value);
                },

                STORE(_) => unimplemented!(),
                LOAD(_) => unimplemented!(),

                ADD => {
                    let right = self.pop();
                    let left = self.pop();

                    let result = match (left, right) {
                        (Int(a), Int(b))        => Int(a + b),
                        (Float(a), Float(b))    => Float(a + b),
                        (Int(a), Float(b))      => Float((a as f64) + b),
                        (Float(a), Int(b))      => Float(a + (b as f64)),

                        (a, b) => {
                            if is_string(&a) || is_string(&b) {
                                let s = format!("{}{}", a.to_string(), b.to_string());
                                self.heap.allocate_str(s)
                            } else {
                                return Err("Invalid type for operator +")
                            }
                        }
                    };

                    self.push(result);
                },

                SUB => {
                    let right = self.pop();
                    let left = self.peek(0);

                    *left = match (&left, right) {
                        (Int(a), Int(b))        => Int(*a - b),
                        (Float(a), Float(b))    => Float(*a - b),
                        (Int(a), Float(b))      => Float((*a as f64) - b),
                        (Float(a), Int(b))      => Float(*a - (b as f64)),

                        (_, _) => return Err("Invalid type for operator -")
                    };
                },

                MUL => {
                    let right = self.pop();
                    let left = self.peek(0);

                    *left = match (&left, right) {
                        (Int(a), Int(b))        => Int(*a * b),
                        (Float(a), Float(b))    => Float(*a * b),
                        (Int(a), Float(b))      => Float((*a as f64) * b),
                        (Float(a), Int(b))      => Float(*a * (b as f64)),

                        (_, _) => return Err("Invalid type for operator *")
                    };
                },

                DIV =>  {
                    let right = self.pop();
                    let left = self.peek(0);

                    *left = match (&left, right) {
                        (Float(a), Float(b))    => Float(*a / b),
                        (Int(a), Float(b))      => Float((*a as f64) / b),
                        (Float(a), Int(b))      => Float(*a / (b as f64)),
                        (Int(a), Int(b))        => {
                            if b == 0 {
                                return Err("Division by zero")
                            }
                            Int(*a / b)
                        },

                        (_, _) => return Err("Invalid type for operator /")
                    };
                },

                NEG => {
                    let value = self.peek(0);

                    *value = match value {
                        Int(value) => Int(-*value),
                        Float(value) => Float(-*value),
                        _ => return Err("Invalid value type for operator -")
                    }
                }

                NOT => {
                    let value = self.peek(0);
                    *value = Bool(!value.is_truthy());
                }

                // equality
                EQ  => {
                    let b = self.pop();
                    let a = self.peek(0);

                    *a = Bool(*a == b);
                },
                NEQ => {
                    let b = self.pop();
                    let a = self.peek(0);

                    *a = Bool(*a != b);
                },

                // ordering
                GT  => {
                    let b = self.pop();
                    let a = self.peek(0);

                    *a = Bool(a.ordering(&b)? > 0);
                },
                GTE => {
                    let b = self.pop();
                    let a = self.peek(0);

                    *a = Bool(a.ordering(&b)? >= 0);
                },
                LT  => {
                    let b = self.pop();
                    let a = self.peek(0);

                    *a = Bool(a.ordering(&b)? < 0);
                },
                LTE => {
                    let b = self.pop();
                    let a = self.peek(0);

                    *a = Bool(a.ordering(&b)? <= 0);
                },

                PRINT(count) => {
                    let mut values : Vec<Value> = Vec::with_capacity(count);
                    let mut i = 0;

                    while i < count {
                        values.push(self.pop());
                        i += 1;
                    }

                    for value in values.iter().rev() {
                        print!("{} ", value.to_string());

                    }

                    println!();
                }
            };
        };

        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        if let Some(val) = self.stack.pop() {
            val
        } else {
            panic!("Stack underflow");
        }
    }

    fn peek(&mut self, offset: usize) -> &mut Value {
        let last = self.stack.len() - offset - 1;
        if let Some(val) = self.stack.get_mut(last) {
            val
        } else {
            panic!("Stack underflow");
        }
    }

    fn get_constant(&self, idx : usize) -> Value {
        if let Some(val) = self.constants.get(idx) {
            val.clone()
        } else {
            panic!("Unknown constant");
        }
    }
}

fn is_string(value: &Value) -> bool {
    if let Str(_) = value {
        true
    } else {
        false
    }
}
