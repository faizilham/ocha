use std::rc::Rc;
use std::cell::RefCell;

use program_data::LineData;
use vm::Bytecode;

// label related
pub type Label = i32;

pub enum BranchType {
    BR,
    BRF
}

struct LabelData {
    position: usize,
    placeholders: Vec<(usize, BranchType)> // (position, type)
}

// block related
pub struct Block {
    pub codes: Vec<Bytecode>,
    pub line_data: LineData,

    labels: Vec<LabelData>,
    next_label: Label,
}

pub type BlockRef = Rc<RefCell<Block>>;

impl Block {
    pub fn new_ref() -> BlockRef {
        Rc::new(RefCell::new(Block::new()))
    }

    pub fn new() -> Block {
        Block { codes: Vec::new(), labels: Vec::new(), next_label: 0, line_data: LineData::new() }
    }

    pub fn next_pos(&self) -> usize {
        self.codes.len()
    }

    pub fn emit(&mut self, line: i32, bytecode : Bytecode) -> usize {
        let index = self.next_pos();
        self.codes.push(bytecode);
        self.line_data.add(line);

        index
    }

    pub fn create_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label += 1;

        self.labels.push( LabelData{ position: 0, placeholders: Vec::new() } );

        label
    }

    pub fn branch_placeholder(&mut self, line: i32, branch_type: BranchType, label: Label) -> usize {
        let position = self.emit(line, Bytecode::NOP);
        let label_data = self.labels.get_mut(label as usize).expect("Label not found");

        label_data.placeholders.push((position, branch_type));

        position
    }

    pub fn set_label_position(&mut self, label: Label, position: usize) {
        let label_data = self.labels.get_mut(label as usize).expect("Label not found");

        label_data.position = position;
    }

    pub fn enclose_labels(&mut self, offset: usize) {
        for LabelData { position, placeholders } in self.labels.drain(..) {
            let absolute_position = position + offset;

            for (br_pos, br_type) in placeholders {
                let bytecode = match br_type {
                    BranchType::BR => Bytecode::BR(absolute_position),
                    BranchType::BRF => Bytecode::BRF(absolute_position)
                };

                let code = self.codes.get_mut(br_pos).expect("Invalid bytecode index");
                *code = bytecode;
            }
        }
    }
}
