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
            decompile,
            file,
            #[cfg(debug_assertions)]
            breakpoints,
            new_vm,
            no_main,
            ..
        } if new_vm => run_new_vm(
            file,
            #[cfg(debug_assertions)]
            {
                breakpoints
            },
            decompile,
            no_main,
        ),
        Command::Run {
            #[cfg(debug_assertions)]
            breakpoints,
            decompile,
            file,
            no_main,
            ..
        } => {
            let options = CompilerOptions { no_main };
            run(
                file,
                #[cfg(debug_assertions)]
                {
                    breakpoints
                },
                decompile,
                &options,
            )
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
    #[cfg(debug_assertions)] breakpoints: Vec<usize>,
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
    #[cfg(debug_assertions)]
    {
        for i in breakpoints {
            machine.add_breakpoint(i);
        }
    }
    eprintln!("Running...");
    machine.run()?;
    Ok(())
}

fn run_new_vm(
    file: Option<String>,
    #[cfg(any(debug_assertions, test))] breakpoints: Vec<usize>,
    decompile: bool,
    no_main: bool,
) -> anyhow::Result<()> {
    let filename = file.unwrap_or("main.nl".to_string());

    let src = std::fs::read_to_string(filename)?;

    let options = neolisp::machine::CompilerOptions { no_main };
    let instructions = neolisp::machine::compile(&src, options)?;

    if decompile {
        let mut offset = 0;
        for int in instructions.iter() {
            eprintln!("{offset:02X} {offset:>2}  {:?}", int);
            offset += int.size();
        }
        return Ok(());
    }
    let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
    let mut vm = neolisp::machine::Machine::new(program);

    #[cfg(debug_assertions)]
    for breakpoint in breakpoints {
        vm.add_breakpoint(breakpoint);
    }

    vm.run()?;

    Ok(())
}
