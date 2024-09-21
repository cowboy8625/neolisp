mod ast;
mod builtins;
mod cli;
mod environment;
mod error;
mod eval;
mod hir_generator;
mod instruction_generator;
mod parser;
mod repl;
mod symbol_table;
#[cfg(test)]
mod tests;
mod vm;

const OPERATORS: &[&str] = &["+", "="];
const BUILTINS: &[&str] = &["print", "nth", "length", "assert-eq", "list", "cons", "car"];
const KEYWORDS: &[&str] = &["var", "let", "fn", "if", "lambda"];

use clap::Parser as ClapParser;
// use compiler::compile;

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
            eprintln!("{i:02X}  {:?}", int);
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
    use crate::hir_generator::HirCompiler;
    use crate::instruction_generator::InstructionCompiler;
    use crate::parser::parser;
    use crate::symbol_table::SymbolWalker;
    use chumsky::prelude::Parser;

    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let hir = HirCompiler::new(symbol_table.clone())
        .compile(&ast)
        .unwrap();
    let instructions = InstructionCompiler::new(symbol_table)
        .compile(&hir)
        .unwrap();
    Ok(instructions)
}
