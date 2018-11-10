#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Nil
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionSignature {
    pub entry_point: usize,
    pub num_args: usize,
    pub has_captured: bool,
    pub is_capturing: bool
}

impl FunctionSignature {
    pub fn new(num_args: usize) -> FunctionSignature {
        FunctionSignature {
            entry_point: 0,
            num_args,
            has_captured: false,
            is_capturing: false
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct LineEncoding {
    count: usize,
    line: i32,
}

impl LineEncoding {
    pub fn new(line: i32) -> LineEncoding {
        LineEncoding {count: 1, line}
    }
}

pub struct LineData{
    data: Vec<LineEncoding>
}

impl LineData {
    pub fn new() -> LineData {
        let data = Vec::new();
        LineData { data }
    }

    pub fn add(&mut self, line: i32) {
        let len = self.data.len();
        if len == 0 {
            self.data.push(LineEncoding::new(line));
            return;
        }

        {
            let last = self.data.get_mut(len - 1).unwrap();

            if last.line == line {
                (*last).count += 1;
                return;
            }
        }

        self.data.push(LineEncoding::new(line));
    }

    pub fn get_line(&self, index: usize) -> i32 {
        let mut total = 0;
        for item in self.data.iter() {
            total += item.count;

            if index < total {
                return item.line;
            }
        }

        -1
    }

    pub fn extend(&mut self, other : &LineData) {
        self.data.extend(&other.data);
    }
}
