use crate::bytecode::scanner::{ScanningError, TokenType};

use super::scanner::Scanner;

pub fn compile(src: &str) {
    let mut scanner = Scanner::new(src);
    let mut line = 0;
    loop {
        match scanner.scan_token() {
            Ok(token) => {
                if token.tokentype == TokenType::Eof {
                    println!("{:4} {:?}", line + 1, TokenType::Eof);
                    break;
                }

                if token.line != line {
                    print!("{:4} ", token.line);
                    line = token.line;
                } else {
                    print!("   | ");
                }
                println!(
                    "{:?} \"{}\"",
                    token.tokentype,
                    &src[token.offset..token.offset + token.length]
                );
            }
            Err(ScanningError(err, line)) => {
                println!(
                    "Scanning error. {}\n\n{:4}{}",
                    err,
                    line,
                    src.lines().nth(line - 1).unwrap()
                );
            }
        }
    }
}
