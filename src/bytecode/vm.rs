use std::{cell::RefCell, rc::Rc};

#[cfg(feature = "debug_trace_execution")]
use crate::bytecode::debugger::disassemble_instruction;

use super::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    value::Value,
};

pub enum InterpretError {
    CompileError,
    RuntimeError,
}

pub struct Vm {
    chunk: Rc<RefCell<Chunk>>,
    ip: usize,
    stack: Vec<Value>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            chunk: Rc::new(RefCell::new(Chunk::new())),
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, src: &str) -> Result<(), InterpretError> {
        let chunk = Rc::new(RefCell::new(Chunk::new()));
        let mut compiler = Compiler::new(src, chunk.clone());
        self.chunk = chunk;

        match compiler.compile() {
            Ok(_) => {
                self.ip = 0;
                if self.run().is_err() {
                    return Err(InterpretError::RuntimeError);
                }
            }
            Err(_) => {
                return Err(InterpretError::CompileError);
            }
        }

        Ok(())
    }

    fn run(&mut self) -> Result<(), ()> {
        let chunk = self.chunk.borrow_mut();

        macro_rules! read_opcode {
            () => {{
                let opcode = chunk.read_opcode(self.ip);
                self.ip += 1;
                opcode
            }};
        }

        macro_rules! read_constant {
            () => {{
                let (_, value) = chunk.get_constant(self.ip);
                self.ip += 1;
                value
            }};
        }

        macro_rules! unary_op {
            ( $op:tt) => {{
                let val = self.stack.pop().unwrap();
                self.stack.push($op val);
            }};
        }

        macro_rules! binary_op {
            ( $op:tt) => {{
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(a $op b);
            }};
        }

        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                print!("          ");
                self.stack.iter().for_each(|val| print!("[ {} ]", val));
                println!();
                disassemble_instruction(&chunk, self.ip);
            }
            let instruction = read_opcode!();
            match instruction {
                OpCode::Return => {
                    println!("{}", self.stack.pop().unwrap());
                    return Ok(());
                }
                OpCode::Constant => self.stack.push(read_constant!()),
                OpCode::Negate => unary_op!(-),
                OpCode::Add => binary_op!(+),
                OpCode::Sub => binary_op!(-),
                OpCode::Mul => binary_op!(*),
                OpCode::Div => binary_op!(/),
            }
        }
    }
}
