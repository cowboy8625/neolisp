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

use clap::Parser as ClapParser;
use compiler::{compile, decompile};

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

    let program = match compile(&src, args.test) {
        Ok(program) => program,
        Err(e) => {
            for e in e {
                println!("{:?}", e);
            }
            return Ok(());
        }
    };

    println!("{:?}", &program[64..]);
    let (header, instructions) = match decompile(&program) {
        Ok(result) => result,
        Err(e) => {
            println!("{:?}", e.0);
            for i in e.1 {
                println!("{:?}", i);
            }
            return Ok(());
        }
    };
    println!("{:?}", header);
    for i in instructions {
        println!("{:?}", i);
    }

    let mut machine = vm::Machine::new(program);
    machine.run()?;
    Ok(())
}
