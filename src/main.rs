use clap::{Parser, ValueEnum};

mod ast;
mod bytecode;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod treewalk;

#[derive(Clone, ValueEnum, Debug)]
enum Variant {
    /// Tree-Walk interpreter
    TreeWalk,
    /// Bytecode compiler
    Bytecode,
}

/// Lox Tree-Walk interpreter and Bytecode compiler
#[derive(Parser, Debug)]
#[command(long_about = None)]
struct Args {
    /// Lox variant
    #[arg(value_enum, short, long, default_value_t = Variant::TreeWalk)]
    variant: Variant,
    /// Source pathname
    pathname: Option<String>,
}

fn main() {
    let args: Args = Args::parse();

    match args.variant {
        Variant::TreeWalk => match args.pathname {
            Some(pathname) => treewalk::run_pathname(&pathname),
            None => treewalk::run_prompt(),
        },
        Variant::Bytecode => match args.pathname {
            Some(pathname) => bytecode::run_pathname(&pathname),
            None => bytecode::run_prompt(),
        },
    }
}
