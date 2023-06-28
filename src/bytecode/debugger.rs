use crate::bytecode::chunk::OpCode;

use super::chunk::Chunk;

#[cfg(feature = "debug_print_code")]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset: usize = 0;
    while !chunk.at_eof(offset) {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04x} ", offset);

    let line = chunk.get_line(offset);
    if offset > 0 && line == chunk.get_line(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", line);
    }

    let opcode = chunk.read_opcode(offset);
    match opcode {
        OpCode::Return => simple_instruction(opcode, offset),
        OpCode::Constant => constant_instruction(opcode, chunk, offset),
        OpCode::Negate => simple_instruction(opcode, offset),
        OpCode::Add => simple_instruction(opcode, offset),
        OpCode::Sub => simple_instruction(opcode, offset),
        OpCode::Mul => simple_instruction(opcode, offset),
        OpCode::Div => simple_instruction(opcode, offset),
    }
}

fn simple_instruction(opcode: OpCode, offset: usize) -> usize {
    println!("{}", opcode);
    offset + 1
}

fn constant_instruction(opcode: OpCode, chunk: &Chunk, offset: usize) -> usize {
    let (constant, value) = chunk.get_constant(offset + 1);
    println!("{:16} {:4} '{}'", opcode, constant, value);
    offset + 2
}
