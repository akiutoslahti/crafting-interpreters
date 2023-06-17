use crate::{
    interpreter::Interpreter, parser::parse_statements, resolver::resolve_variables,
    scanner::scan_tokens,
};
use std::{
    fs,
    io::{self, Write},
    path::Path,
    rc::Rc,
};

pub struct TreeWalk {
    print_tokens: bool,
    print_ast: bool,
}

impl TreeWalk {
    pub fn new(print_tokens: bool, print_ast: bool) -> Self {
        Self {
            print_tokens,
            print_ast,
        }
    }

    fn run(&self, source: Rc<String>, interpreter: &mut Interpreter) {
        match scan_tokens(&source) {
            Ok(tokens) => {
                if self.print_tokens {
                    println!("Tokens {:#?}", tokens);
                }
                match parse_statements(&source, tokens) {
                    Ok(statements) => {
                        if self.print_ast {
                            println!("Statements {:#?}", statements);
                        }
                        match resolve_variables(&source, interpreter, &statements) {
                            Ok(_) => {
                                if let Err(err) = interpreter.interpret(source.clone(), statements)
                                {
                                    eprintln!("{}", err);
                                }
                            }
                            Err(errors) => errors.iter().for_each(|e| eprintln!("{}", e)),
                        }
                    }
                    Err(errors) => errors.iter().for_each(|e| eprintln!("{}", e)),
                }
            }
            Err(errors) => errors.iter().for_each(|e| eprintln!("{}", e)),
        }
    }

    pub fn run_pathname(&self, pathname: &str) {
        let source = fs::read_to_string(Path::new(pathname))
            .unwrap_or_else(|_| panic!("Couldn't read \"{}\" pathname to UTF-8 string", pathname));
        self.run(Rc::new(source), &mut Interpreter::new());
    }

    pub fn run_prompt(&self) {
        let mut interpreter = Interpreter::new();
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
            self.run(Rc::new(line), &mut interpreter);
        }
    }
}
