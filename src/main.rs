// TODO: Maybe worth futher transformation of ir before bytecode is generated or
// TODO: See if using the instructions in the decompiler is better
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

    let binary_name = filename.split('.').collect::<Vec<&str>>()[0];
    std::fs::write(binary_name, program.clone())?;

    let mut machine = vm::Machine::new(program);
    machine.run()?;
    Ok(())
}

#[test]
fn test_c() {
    let src = "(fn add (a b) (+ a b))";
    // TODO: Check if the size count is correct for Instructions
}
