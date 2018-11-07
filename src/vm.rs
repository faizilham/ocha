use std::cmp::max;

use exception::Exception;
use exception::Exception::RuntimeErr;
use heap::{Heap, Traceable, HeapPtr};
use io::OchaIO;

use program_data::{Literal, FunctionSignature, LineData};

use value::{Value, OchaStr, VecList, OchaFunc};
use value::get_traceable;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Bytecode {
    HALT,
    NOP,

    // constants
    CONST(usize),
    NIL,
    BOOL(bool),

    // memory
    POP(usize),
    STORE(isize),
    LOAD(isize),
    STORE_GLOBAL(isize),
    LOAD_GLOBAL(isize),

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

    // list operations
    BUILD_LIST(usize),
    GET_LIST, // get member
    SET_LIST, // set member

    // branches
    BR(usize),
    BRF(usize), // branch if false

    // functions
    LOAD_FUNC(usize),
    CALL(usize),
    RET,

    // to be removed
    PRINT(usize),
}

use self::Bytecode::*;
use self::Value::*;

pub struct Module {
    pub codes: Vec<Bytecode>,
    pub literals: Vec<Literal>,
    pub functions: Vec<FunctionSignature>,
    pub line_data: LineData,
}

pub struct VM<'io> {
    codes: Vec<Bytecode>,
    functions: Vec<FunctionSignature>,
    constants: Vec<Value>,
    stack: Vec<Value>,
    heap: Heap,
    ip: usize,
    fp: usize,
    line_data: LineData,
    max_objects: usize,

    io: &'io mut OchaIO
}

const INITIAL_GC_THRESHOLD : usize = 50;

impl<'io> VM<'io> {
    pub fn new (module: Module, io: &'io mut OchaIO) -> VM {
        let Module { codes, mut literals, line_data, functions } = module;

        let stack = Vec::with_capacity(256);
        let heap = Heap::new();
        let constants = Vec::with_capacity(literals.len());

        let mut vm = VM {
            codes,
            functions,
            constants,
            stack,
            heap,
            line_data,
            ip: 0,
            fp: 0,
            max_objects: INITIAL_GC_THRESHOLD,

            io
        };

        for literal in literals.drain(..) {
            let value = match literal {
                Literal::Nil => Nil,
                Literal::Int(i) => Int(i),
                Literal::Float(f) => Float(f),
                Literal::Bool(b) => Bool(b),
                Literal::Str(s) => vm.allocate_str_literal(s)
            };

            vm.constants.push(value);
        }

        vm
    }

    pub fn run(&mut self) -> Result<(), Exception> {
        let result = self.run_loop().map_err(|message| {
            let last = self.ip - 1;
            let line = self.line_data.get_line(last);

            RuntimeErr(line, String::from(message))
        });

        self.cleanup();

        result
    }

    fn run_loop(&mut self) -> Result<(), &'static str> {
        loop {
            let code = self.codes[self.ip];
            self.ip += 1;

            match code {
                HALT => break,
                NOP => (), // do nothing

                // constants
                CONST(idx) => {
                    let value = self.get_constant(idx);
                    self.push(value);
                },

                NIL => {
                    self.push(Value::Nil);
                },

                BOOL(val) => {
                    self.push(Value::Bool(val));
                },

                // memory
                POP(count) => {
                    let new_len = self.stack.len() - count;
                    self.stack.truncate(new_len);
                },

                STORE(offset) => {
                    let fp = self.fp;
                    self.store(fp, offset);
                },

                LOAD(offset) => {
                    let fp = self.fp;
                    self.load(fp, offset);
                },

                STORE_GLOBAL(offset) => {
                    self.store(0, offset);
                },

                LOAD_GLOBAL(offset) => {
                  self.load(0, offset);
                },

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
                                self.allocate_str(s)
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

                // list operator
                BUILD_LIST(count) => {
                    let start = self.stack.len() - count;
                    let values = self.stack.split_off(start);
                    let veclist = self.allocate_list(VecList::from(values));

                    self.push(veclist);
                },

                GET_LIST => {
                    let index = self.pop();
                    let listval = self.peek(0);

                    let result = if let List(lref) = listval {
                        if let Int(index) = index {
                            lref.get_ref().get(index)
                        } else {
                            Err("Invalid member type for get operator")
                        }
                    } else {
                        Err("Invalid container type for get operator")
                    };

                    *listval = result?;
                },

                SET_LIST => {
                    let index = self.pop();
                    let listval = self.pop();
                    let value = self.pop();

                    if let List(lref) = listval {
                        if let Int(index) = index {
                            lref.get_ref().put(index, value)?;
                        } else {
                            return Err("Invalid member type for get operator")
                        }
                    } else {
                        return Err("Invalid container type for get operator")
                    }
                },

                // branches
                BR(position) => {
                    self.ip = position;
                }

                BRF(position) => {
                    let value = self.pop();
                    if !value.is_truthy() {
                        self.ip = position;
                    }
                }

                // functions
                LOAD_FUNC(id) => {
                    let signature = *self.functions.get(id).expect("Unknown function loaded");
                    self.push(Func(OchaFunc::new(signature)));
                },

                CALL(num_args) => {
                    // check function value
                    let entry_point = if let Func(func) = self.peek(0) {
                        if func.signature.num_args != num_args {
                            return Err("Wrong number of arguments in function call");
                        }

                        func.signature.entry_point
                    } else {
                        return Err("Invalid type for call operator")
                    };

                    let ip = self.ip as i64;
                    let fp = self.fp as i64;

                    self.push_int(ip);
                    self.push_int(fp);

                    self.fp = self.stack.len();
                    self.ip = entry_point;
                },

                RET => {
                    let ret_val = self.pop();

                    self.stack.truncate(self.fp);
                    let fp = self.pop_int() as usize;
                    let ip = self.pop_int() as usize;

                    let num_args = if let Func(func) = self.pop() {
                        func.signature.num_args
                    } else {
                        unreachable!();
                    };

                    let len = self.stack.len();
                    self.stack.truncate(len - num_args);

                    self.push(ret_val);
                    self.fp = fp;
                    self.ip = ip;
                }

                PRINT(count) => {
                    let start = self.stack.len() - count;

                    let values = self.stack.split_off(start);

                    for value in values {
                        self.io.write(&value.to_string());
                    }

                    self.io.writeln();
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

    fn push_int(&mut self, i: i64) {
        self.push(Int(i));
    }

    fn pop_int(&mut self) -> i64 {
        if let Int(i) = self.pop() {
            return i;
        }

        panic!("Invalid int value");
    }

    fn peek(&mut self, offset: usize) -> &mut Value {
        let last = self.stack.len() - offset - 1;
        if let Some(val) = self.stack.get_mut(last) {
            val
        } else {
            panic!("Stack underflow");
        }
    }

    fn store(&mut self, start: usize, offset: isize) {
        let value = self.pop();
        let pos = (start as isize) + offset;

        let var = self.stack.get_mut(pos as usize).expect("Invalid stack access");
        *var = value;
    }

    fn load (&mut self, start : usize, offset: isize) {
        let pos = (start as isize) + offset;
        let value = self.stack.get(pos as usize)
            .expect("Invalid stack access").clone();
        self.stack.push(value);
    }

    fn get_constant(&self, idx : usize) -> Value {
        if let Some(val) = self.constants.get(idx) {
            val.clone()
        } else {
            panic!("Unknown constant");
        }
    }

    // heap allocation & gc
    fn allocate<T : Traceable + 'static>(&mut self, obj: T) -> HeapPtr<T> {
        // schedule gc
        let num_objects = self.heap.size();
        if num_objects >= self.max_objects {
            self.gc()
        }

        self.heap.allocate(obj)
    }

    fn allocate_str_literal(&mut self, string: String) -> Value {
        let ochastr = OchaStr::new(string, true);
        Value::Str(self.allocate(ochastr))
    }

    fn allocate_str(&mut self, string: String) -> Value {
        let ochastr = OchaStr::new(string, false);
        Value::Str(self.allocate(ochastr))
    }

    fn allocate_list(&mut self, list: VecList) -> Value {
        Value::List(self.allocate(list))
    }

    fn gc(&mut self) {
        self.trace();
        self.heap.sweep();

        let num_objects = self.heap.size();
        self.max_objects = max(num_objects * 2, INITIAL_GC_THRESHOLD);
    }

    fn trace(&mut self) {
        for value in self.stack.iter() {
            if let Some(obj) = get_traceable(value) {
                obj.trace();
            }
        }
    }

    fn cleanup(&mut self) {
        self.heap.clear();
        self.constants.clear();
    }
}

fn is_string(value: &Value) -> bool {
    if let Str(_) = value {
        true
    } else {
        false
    }
}
