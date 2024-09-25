mod ast;
mod builtins;
mod cli;
mod compiler2;
mod environment;
mod error;
mod eval;
mod parser;
mod repl;
mod symbol_table;
#[cfg(test)]
mod tests;
mod vm;

const OPERATORS: &[&str] = &["+", "-", "="];
const BUILTINS: &[&str] = &["print", "nth", "length", "assert-eq", "list", "cons", "car"];
const KEYWORDS: &[&str] = &["var", "let", "fn", "if", "lambda"];

use clap::Parser as ClapParser;

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

    let program = compile(&src)?;

    if args.decompile {
        for (i, int) in program.iter().enumerate() {
            eprintln!("{i:02X} {i:>2}  {:?}", int);
        }
        return Ok(());
    }
    eprintln!("compiled to {} instructions", program.len());
    eprintln!("running...");
    let mut machine = vm::Machine::new(program);
    for i in args.breakpoints {
        machine.add_breakpoint(i);
    }
    machine.run();

    Ok(())
}

fn compile(src: &str) -> anyhow::Result<Vec<vm::Instruction>> {
    use crate::parser::parser;
    use crate::symbol_table::SymbolWalker;
    use chumsky::prelude::Parser;

    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let stage1_data = compiler2::Stage1Compiler::new(symbol_table.clone()).compiler(&ast);
    let instructions = compiler2::compile_to_instructions(&mut symbol_table, &stage1_data);
    Ok(instructions)
}
