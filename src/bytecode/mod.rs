use crate::bytecode::{
    chunk::{Chunk, OpCode},
    debugger::disassemble_chunk,
    vm::Vm,
};

mod chunk;
mod debugger;
mod value;
mod vm;

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
        let mut vm = Vm::new();
        let mut chunk = Chunk::new();

        let constant = chunk.add_constant(1.2f64);
        chunk.write_opcode(OpCode::Constant, 123);
        chunk.write_chunk(constant, 123);

        let constant = chunk.add_constant(3.4f64);
        chunk.write_opcode(OpCode::Constant, 123);
        chunk.write_chunk(constant, 123);

        chunk.write_opcode(OpCode::Add, 123);

        let constant = chunk.add_constant(5.6f64);
        chunk.write_opcode(OpCode::Constant, 123);
        chunk.write_chunk(constant, 123);

        chunk.write_opcode(OpCode::Div, 123);
        chunk.write_opcode(OpCode::Negate, 123);

        chunk.write_opcode(OpCode::Return, 123);

        // disassemble_chunk(&chunk, "test chunk");
        let _ = vm.interpret(&chunk);
    }
}
