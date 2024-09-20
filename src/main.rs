mod ast;
mod builtins;
mod cli;
// mod compiler;
mod environment;
mod error;
mod eval;
mod hir_generator;
mod instruction_generator;
// mod lir_generator;
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

    // let program = match compile(&src) {
    //     Ok(program) => program,
    //     Err(e) => {
    //         for e in e {
    //             println!("{:?}", e);
    //         }
    //         return Ok(());
    //     }
    // };

    // let binary_name = filename.split('.').collect::<Vec<&str>>()[0];
    // std::fs::write(binary_name, program.clone())?;

    // if args.decompile {
    //     let binary_name = filename.split('.').collect::<Vec<&str>>()[0];
    //     let (header, decompiled_program) = match compiler::decompile(&program) {
    //         Ok(result) => result,
    //         Err((msg, e)) => {
    //             eprintln!("{msg}");
    //             for e in e {
    //                 eprintln!("{:?}", e);
    //             }
    //             return Ok(());
    //         }
    //     };
    //     std::fs::write(
    //         format!("{binary_name}.xxd"),
    //         format!(
    //             "{:?}\n{}",
    //             header,
    //             decompiled_program
    //                 .into_iter()
    //                 .map(|i| format!("{i}\n"))
    //                 .collect::<String>()
    //         )
    //         .as_bytes(),
    //     )?;
    // }

    let program = compile(&src)?;
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
