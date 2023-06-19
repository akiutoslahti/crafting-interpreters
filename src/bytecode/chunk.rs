use std::fmt::Display;

use super::value::Value;

pub enum OpCode {
    Return,
    Constant,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "{:16}", "OP_RETURN"),
            OpCode::Constant => write!(f, "{:16}", "OP_CONSTANT"),
        }
    }
}

pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<usize>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn is_eof(&self, offset: usize) -> bool {
        offset >= self.code.len()
    }

    pub fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn read_chunk(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    pub fn get_line(&self, offset: usize) -> usize {
        self.lines[offset]
    }

    pub fn write_opcode(&mut self, opcode: OpCode, line: usize) {
        match opcode {
            OpCode::Return => self.write_chunk(0x00, line),
            OpCode::Constant => self.write_chunk(0x01, line),
        }
    }

    pub fn read_opcode(&self, offset: usize) -> OpCode {
        let opcode = self.read_chunk(offset);
        match opcode {
            0x00 => OpCode::Return,
            0x01 => OpCode::Constant,
            _ => panic!("Unkown OpCode ({:x})", opcode),
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> u8 {
        let idx = self.constants.len();
        self.constants.push(constant);
        idx as u8
    }

    pub fn get_constant(&self, offset: usize) -> (u8, Value) {
        let constant = self.read_chunk(offset);
        let value = self.constants[constant as usize];
        (constant, value)
    }
}
