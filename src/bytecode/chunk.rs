use std::fmt::Display;

use super::value::Value;

pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::Return => write!(f, "{:16}", "OP_RETURN"),
            OpCode::Constant => write!(f, "{:16}", "OP_CONSTANT"),
            OpCode::Negate => write!(f, "{:16}", "OP_NEGATE"),
            OpCode::Add => write!(f, "{:16}", "OP_ADD"),
            OpCode::Sub => write!(f, "{:16}", "OP_SUB"),
            OpCode::Mul => write!(f, "{:16}", "OP_MUL"),
            OpCode::Div => write!(f, "{:16}", "OP_DIV"),
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

    #[cfg(feature = "debug_print_code")]
    pub fn at_eof(&self, offset: usize) -> bool {
        offset >= self.code.len()
    }

    pub fn write_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn read_chunk(&self, offset: usize) -> u8 {
        self.code[offset]
    }

    #[cfg(feature = "debug")]
    pub fn get_line(&self, offset: usize) -> usize {
        self.lines[offset]
    }

    pub fn write_opcode(&mut self, opcode: OpCode, line: usize) {
        match opcode {
            OpCode::Return => self.write_chunk(0x00, line),
            OpCode::Constant => self.write_chunk(0x01, line),
            OpCode::Negate => self.write_chunk(0x02, line),
            OpCode::Add => self.write_chunk(0x03, line),
            OpCode::Sub => self.write_chunk(0x04, line),
            OpCode::Mul => self.write_chunk(0x05, line),
            OpCode::Div => self.write_chunk(0x06, line),
        }
    }

    pub fn read_opcode(&self, offset: usize) -> OpCode {
        let opcode = self.read_chunk(offset);
        match opcode {
            0x00 => OpCode::Return,
            0x01 => OpCode::Constant,
            0x02 => OpCode::Negate,
            0x03 => OpCode::Add,
            0x04 => OpCode::Sub,
            0x05 => OpCode::Mul,
            0x06 => OpCode::Div,
            _ => panic!("Unkown OpCode ({:x})", opcode),
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        let constant = self.constants.len();
        self.constants.push(value);
        constant
    }

    pub fn get_constant(&self, offset: usize) -> (u8, Value) {
        let constant = self.read_chunk(offset);
        let value = self.constants[constant as usize];
        (constant, value)
    }
}
