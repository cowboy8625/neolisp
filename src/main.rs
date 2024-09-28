mod ast;
mod cli;
mod compiler;
mod docs;
mod error;
mod parser;
mod repl;
mod symbol_table;
#[cfg(test)]
mod tests;
mod vm;

const OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "=", ">", "<", ">=", "<=", "and", "or", "mod",
];
const BUILTINS: &[&str] = &["print", "nth", "length", "assert-eq", "list", "cons", "car"];
const KEYWORDS: &[&str] = &["var", "let", "fn", "if", "lambda"];

use clap::Parser as ClapParser;
fn main() -> anyhow::Result<()> {
    let args = cli::Cli::parse();

    match args.command {
        cli::Commands::Build { decompile, file } => build(file, decompile),
        cli::Commands::Run { repl, .. } if repl => {
            repl::run(args)?;
            return Ok(());
        }
        cli::Commands::Run {
            breakpoints,
            decompile,
            file,
            ..
        } => run(file, breakpoints, decompile),
        cli::Commands::Test { file: _ } => todo!(),
    }
}

fn build(file: Option<String>, decompile: bool) -> anyhow::Result<()> {
    let filename = file.clone().unwrap_or("main.nl".to_string());
    eprintln!("Compiling [{filename}]");
    let Ok(src) = std::fs::read_to_string(filename) else {
        panic!("failed to read file")
    };
    let now = std::time::Instant::now();
    let program = compiler::compile(&src)?;
    eprintln!("Compiled in {}ms", now.elapsed().as_millis());
    eprintln!("Compiled to {} bytes", program.len());

    if decompile {
        display_instructions(&program);
        return Ok(());
    }
    // Save to file

    let compiled_filename = get_compiled_filename(file)?;
    std::fs::write(compiled_filename, program)?;

    Ok(())
}

fn get_compiled_filename(file: Option<String>) -> anyhow::Result<String> {
    let filename = file.unwrap_or("main.nl".to_string());
    Ok(filename.split('.').next().unwrap().to_string())
}

fn run(file: Option<String>, breakpoints: Vec<usize>, decompile: bool) -> anyhow::Result<()> {
    build(file.clone(), decompile)?;
    if decompile {
        return Ok(());
    }

    let compiled_filename = get_compiled_filename(file)?;

    let program = std::fs::read(compiled_filename)?;
    let mut machine = vm::Machine::new(program);
    for i in breakpoints {
        machine.add_breakpoint(i);
    }
    eprintln!("Running...");
    machine.run();
    Ok(())
}

fn display_instructions(program: &[u8]) {
    let instructions = vm::decompile(&program);
    let mut offset = 0;
    for int in instructions.iter() {
        eprintln!("{offset:02X} {offset:>2}  {:?}", int);
        offset += int.size();
    }
}
