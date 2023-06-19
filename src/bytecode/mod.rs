#[allow(dead_code)]
pub struct ByteCode {
    print_tokens: bool,
    print_ast: bool,
}

impl ByteCode {
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
        println!("REPL for Bytecode compiler");
    }
}
