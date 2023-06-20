use std::{
    fs,
    io::{self, Write},
    path::Path,
};

use crate::bytecode::vm::Vm;

mod chunk;
mod compiler;
#[cfg(feature = "debug_trace_execution")]
mod debugger;
mod scanner;
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

    fn run(&self, src: &str, vm: &mut Vm) {
        let _ = vm.interpret(src);
    }

    pub fn run_pathname(&self, pathname: &str) {
        let src = fs::read_to_string(Path::new(pathname))
            .unwrap_or_else(|_| panic!("Couldn't read \"{}\" pathname to UTF-8 string", pathname));
        self.run(&src, &mut Vm::new());
    }

    pub fn run_prompt(&self) {
        let mut vm = Vm::new();
        loop {
            print!("> ");
            #[allow(unused_must_use)]
            {
                io::stdout().flush();
            }
            let mut line = String::new();
            io::stdin()
                .read_line(&mut line)
                .expect("Couldn't read from stdin");
            if line == "\n" {
                break;
            }
            self.run(&line, &mut vm);
        }
    }
}
