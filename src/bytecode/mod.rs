use crate::bytecode::{
    chunk::{Chunk, OpCode},
    debugger::disassemble_chunk,
};

use self::value::Value;

mod chunk;
mod debugger;
mod value;

#[allow(dead_code)]
pub struct Bytecode {
    print_tokens: bool,
    print_ast: bool,
}

impl Bytecode {
    pub fn new(print_tokens: bool, print_ast: bool) -> Self {
        Self {
            print_tokens,
            print_ast,
        }
    }

    pub fn run_pathname(&self, pathname: &str) {
        println!("Bytecode compiler for pathname: {}", pathname);
    }

    pub fn run_prompt(&self) {
        let mut chunk = Chunk::new();
        let constant = chunk.add_constant(Value::Number(1.2f64));
        chunk.write_opcode(OpCode::Constant, 123);
        chunk.write_chunk(constant, 123);
        chunk.write_opcode(OpCode::Return, 123);
        disassemble_chunk(&chunk, "test chunk")
    }
}
