use neolisp::{
    cli::{Cli, Command},
    compiler::{compile, simple_display_instructions, CompilerOptions},
    repl,
    vm::{self, Machine},
};

use clap::Parser as ClapParser;
fn main() -> anyhow::Result<()> {
    let args = Cli::parse();

    let Some(command) = args.command.clone() else {
        repl::run(args)?;
        return Ok(());
    };

    match command {
        Command::Build { decompile, file } => build(file, decompile, &CompilerOptions::default()),
        Command::Run { repl, .. } if repl => {
            repl::run(args)?;
            Ok(())
        }
        Command::Run {
            breakpoints,
            decompile,
            file,
            no_main,
            ..
        } => {
            let options = CompilerOptions { no_main };
            run(file, breakpoints, decompile, &options)
        }
        Command::Test { file: _ } => todo!(),
    }
}

fn build(file: Option<String>, decompile: bool, options: &CompilerOptions) -> anyhow::Result<()> {
    let filename = file.clone().unwrap_or("main.nl".to_string());
    eprintln!("Compiling [{filename}]");
    let Ok(src) = std::fs::read_to_string(filename) else {
        panic!("failed to read file")
    };
    let now = std::time::Instant::now();
    let program = compile(&src, options)?;
    eprintln!("Compiled in {}ms", now.elapsed().as_millis());
    eprintln!("Compiled to {} bytes", program.len());

    if decompile {
        let instructions = vm::decompile(&program);
        simple_display_instructions(&instructions);
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

fn run(
    file: Option<String>,
    breakpoints: Vec<usize>,
    decompile: bool,
    options: &CompilerOptions,
) -> anyhow::Result<()> {
    build(file.clone(), decompile, options)?;
    if decompile {
        return Ok(());
    }

    let compiled_filename = get_compiled_filename(file)?;

    let program = std::fs::read(compiled_filename)?;
    let mut machine = Machine::new(program);
    for i in breakpoints {
        machine.add_breakpoint(i);
    }
    eprintln!("Running...");
    machine.run();
    Ok(())
}
