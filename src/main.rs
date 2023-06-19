use clap::{Parser, ValueEnum};
use crafting_interpreters::{bytecode::ByteCode, treewalk::TreeWalk};

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
    #[arg(value_enum, long, default_value_t = Variant::TreeWalk)]
    variant: Variant,
    /// Print tokens
    #[arg(long, default_value = "false")]
    print_tokens: bool,
    /// Print AST
    #[arg(long, default_value = "false")]
    print_ast: bool,
    /// Source pathname
    pathname: Option<String>,
}

fn main() {
    let args: Args = Args::parse();
    let pathname = args.pathname;

    match args.variant {
        Variant::TreeWalk => {
            let treewalk = TreeWalk::new(args.print_tokens, args.print_ast);
            match pathname {
                Some(pathname) => treewalk.run_pathname(&pathname),
                None => treewalk.run_prompt(),
            }
        }
        Variant::Bytecode => {
            let bytecode = ByteCode::new(args.print_tokens, args.print_ast);
            match pathname {
                Some(pathname) => bytecode.run_pathname(&pathname),
                None => bytecode.run_prompt(),
            }
        }
    }
}
