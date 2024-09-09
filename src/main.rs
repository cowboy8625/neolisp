mod ast;
mod builtins;
mod cli;
mod compiler;
mod environment;
mod error;
mod eval;
mod parser;
mod repl;
#[cfg(test)]
mod tests;
mod vm;

// use crate::environment::Env;
// use crate::error::print_error;
// use crate::eval::eval;
// use crate::parser::parser;
// use chumsky::prelude::Parser as ChumskyParser;
use clap::Parser as ClapParser;
use compiler::compile;

fn main() -> anyhow::Result<()> {
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

    let program = match compile(&src) {
        Ok(program) => program,
        Err(e) => {
            for e in e {
                println!("{:?}", e);
            }
            return Ok(());
        }
    };

    let mut machine = vm::Machine::new(program);
    machine.run()?;
    Ok(())
}
