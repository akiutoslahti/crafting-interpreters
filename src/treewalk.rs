use crate::scanner::Scanner;
use std::{
    fs,
    io::{self, Write},
    path::Path,
};

fn run(source: &str) {
    let mut scanner = Scanner::new(source);
    match scanner.scan_tokens() {
        Ok(tokens) => println!("{}", tokens),
        Err(errors) => errors.iter().for_each(|e| println!("{}", e)),
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
