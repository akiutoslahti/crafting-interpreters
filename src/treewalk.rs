use crate::{interpreter::Interpreter, parser::Parser, scanner::scan_tokens};
use std::{
    fs,
    io::{self, Write},
    path::Path,
};

// TODO Create scanner, parser and interpreter outside run() function to keep state and fix REPL
fn run(source: &str) {
    match scan_tokens(source) {
        Ok(tokens) => {
            println!("Tokens {:#?}", tokens);
            let mut parser = Parser::new(source, tokens);
            match parser.parse() {
                Ok(statements) => {
                    println!("Statements {:#?}", statements);
                    let mut interpreter = Interpreter::new(source);
                    if let Err(err) = interpreter.interpret(statements) {
                        eprintln!("{}", err);
                    }
                }
                Err(errors) => errors.iter().for_each(|e| eprintln!("{}", e)),
            }
        }
        Err(errors) => errors.iter().for_each(|e| eprintln!("{}", e)),
    }
}

pub fn run_pathname(pathname: &str) {
    let source = fs::read_to_string(Path::new(pathname))
        .unwrap_or_else(|_| panic!("Couldn't read \"{}\" pathname to UTF-8 string", pathname));
    run(&source);
}

pub fn run_prompt() {
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
        run(&line);
    }
}
