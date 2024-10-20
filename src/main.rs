use neolisp::{
    cli::{Cli, Command},
    debugger::Debugger,
    emitter::{Emitter, EmitterOptions},
    error::Error,
    machine::Machine,
    parser::parser,
    repl,
    symbol_table::SymbolTableBuilder,
};

use chumsky::prelude::Parser;
use clap::Parser as ClapParser;

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    let Some(command) = args.command.clone() else {
        repl::run(args)?;
        return Ok(());
    };

    match command {
        Command::Build { .. } => todo!("Not build implemented yet"),
        Command::Run(r) if r.repl => {
            repl::run(args)?;
            Ok(())
        }
        Command::Run(r) => {
            let filename = r.file.unwrap_or("main.nl".to_string());
            let src = std::fs::read_to_string(&filename)?;
            let options = EmitterOptions { no_main: r.no_main };

            if src.is_empty() {
                Error::EmptyFile.report(&filename, &src)?;
                return Ok(());
            }
            let ast = match parser().parse(src.clone()) {
                Ok(ast) => ast,
                Err(errors) => {
                    for error in errors {
                        error.report(&filename, &src)?;
                    }
                    return Ok(());
                }
            };

            if args.ast_debug {
                eprintln!("{ast:#?}");
                return Ok(());
            }

            let mut symbol_table = match SymbolTableBuilder::default().build(&ast) {
                Ok(t) => t,
                Err(errors) => {
                    for error in errors {
                        error.report(&filename, &src)?;
                    }
                    return Ok(());
                }
            };
            let myabe_instructions = Emitter::new(&mut symbol_table, options).compile(&ast);

            let instructions = match myabe_instructions {
                Ok(program) => program,
                Err(errors) => {
                    for error in errors {
                        error.report(&filename, &src)?;
                    }
                    return Ok(());
                }
            };
            if r.decompile {
                let mut offset = 0;
                for int in instructions.iter() {
                    eprintln!("{offset:02X} {offset:>2}  {:?}", int);
                    offset += int.size();
                }
                return Ok(());
            }

            let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
            let mut machine = Machine::new(program);

            let mut debugger = Debugger::new(&mut machine)?.with_breakpoints(r.breakpoints.clone());
            debugger.run()?;

            Ok(())
        }
        Command::Test { file } => {
            let filename = file.unwrap_or("main.nl".to_string());
            let src = std::fs::read_to_string(&filename)?;
            neolisp::parser::parse_or_report(&filename, &src);
            Ok(())
        }
    }
}
