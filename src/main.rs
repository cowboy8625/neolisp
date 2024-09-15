mod ast;
mod builtins;
mod cli;
mod compiler;
mod environment;
mod error;
mod eval;
mod parser;
mod repl;
mod symbol_table;
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

    if args.decompile {
        let binary_name = filename.split('.').collect::<Vec<&str>>()[0];
        let (header, decompiled_program) = match compiler::decompile(&program) {
            Ok(result) => result,
            Err((msg, e)) => {
                eprintln!("{msg}");
                for e in e {
                    eprintln!("{:?}", e);
                }
                return Ok(());
            }
        };
        std::fs::write(
            format!("{binary_name}.xxd"),
            format!(
                "{:?}\n{}",
                header,
                decompiled_program
                    .into_iter()
                    .map(|i| format!("{i}\n"))
                    .collect::<String>()
            )
            .as_bytes(),
        )?;
    }

    let mut machine = vm::Machine::new(program, args.decompile);
    for i in args.breakpoints {
        machine.add_breakpoint(i);
    }
    machine.run()?;

    Ok(())
}
