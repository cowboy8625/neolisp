mod cli;
mod error;
mod eval;
mod parser;
mod repl;
#[cfg(test)]
mod tests;

use crate::error::print_error;
use crate::eval::{eval, Env};
use crate::parser::parser;
use chumsky::prelude::Parser as ChumskyParser;
use clap::Parser as ClapParser;

fn main() -> rustyline::Result<()> {
    let args = cli::Cli::parse();

    if args.repl {
        repl::run(args)?;
        return Ok(());
    }

    let Some(filename) = args.files.first() else {
        println!("no file specified");
        return Ok(());
    };

    let Ok(src) = std::fs::read_to_string(filename) else {
        panic!("failed to read file")
    };

    let ast = match parser().parse(src.clone()) {
        Ok(ast) => ast,
        Err(error) => {
            for e in error.iter() {
                print_error(&src, e);
            }
            return Ok(());
        }
    };
    let mut env = Env::new();
    for expr in ast {
        let r = eval(&expr, &mut env);
        match r {
            Ok(_) => {}
            Err(e) => {
                println!("{e}");
                break;
            }
        }
    }
    Ok(())
}
