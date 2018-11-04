#[derive(Debug, Clone)]
struct LineEncoding {
    start: usize,
    end: usize,
    line: i32,
}

impl LineEncoding {
    pub fn new(index: usize, line: i32) -> LineEncoding {
        LineEncoding {start: index, end: index, line}
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

    pub fn add(&mut self, index: usize, line: i32) {
        let len = self.data.len();
        if len == 0 {
            self.data.push(LineEncoding::new(index, line));
            return;
        }

        {
            let last = self.data.get_mut(len - 1).unwrap();

            if last.line == line {
                (*last).end = index;
                return;
            }
        }

        self.data.push(LineEncoding::new(index, line));
    }

    pub fn get_line(&self, index: usize) -> i32 {
        for item in self.data.iter() {
            if item.start <= index && item.end > index {
                return item.line;
            }
        }

        -1
    }
}
