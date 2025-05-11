use neolisp::{
    cli::{Cli, Command},
    compiler::Compiler,
    debugger::Debugger,
    machine::Machine,
    repl,
    symbol_table::SymbolTable,
};

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
            let filename = r.file.unwrap_or("src/main.nl".to_string());
            let src = std::fs::read_to_string(&filename)?;
            let mut symbol_table = SymbolTable::default();
            let compiler = Compiler::default()
                .no_main(r.no_main)
                .debug_ast(args.ast_debug)
                .debug_symbol_table(args.symbol_table_debug)
                .decompile(r.decompile)
                .compile(&src, &mut symbol_table);
            let instructions = match compiler {
                Ok(Some(instructions)) => instructions,
                Ok(None) => return Ok(()),
                Err(errors) => {
                    for error in errors {
                        error.report(&filename, &src);
                    }
                    return Ok(());
                }
            };

            let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
            let mut machine = Machine::new(program, symbol_table);

            if !r.breakpoints.is_empty() {
                let mut debugger =
                    Debugger::new(&mut machine)?.with_breakpoints(r.breakpoints.clone());
                debugger.run()?;
                return Ok(());
            }

            if let Err(error) = machine.run() {
                error.report(&filename, &src);
            }

            Ok(())
        }
        Command::Test(t) => {
            let filename = t.file.unwrap_or("main.nl".to_string());
            let src = std::fs::read_to_string(&filename)?;
            let mut symbol_table = SymbolTable::default();
            let compiler = Compiler::default()
                .no_main(true)
                .debug_ast(args.ast_debug)
                .decompile(t.decompile)
                .with_test(true)
                .compile(&src, &mut symbol_table);

            let instructions = match compiler {
                Ok(Some(instructions)) => instructions,
                Ok(None) => return Ok(()),
                Err(errors) => {
                    for error in errors {
                        error.report(&filename, &src);
                    }
                    return Ok(());
                }
            };

            let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
            let mut machine = Machine::new(program, symbol_table);

            if !t.breakpoints.is_empty() {
                let mut debugger =
                    Debugger::new(&mut machine)?.with_breakpoints(t.breakpoints.clone());
                debugger.run()?;
            } else {
                match machine.run() {
                    Ok(()) => {}
                    Err(error) => {
                        error.report(&filename, &src);
                    }
                }
            }
            Ok(())
        }
    }
}
