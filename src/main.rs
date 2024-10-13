// use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
// use chumsky::prelude::*;
// use neolisp::parser::parser;
// fn main() {
//     let src = include_str!("../main.nl");
//     let (ast, errors) = match parser().parse(src) {
//         Ok(ast) => (ast, vec![]),
//         Err(errors) => (vec![], errors),
//     };
//     println!("{ast:#?}");
//     println!("{errors:#?}");
//
//     let mut colors = ColorGenerator::new();
//
//     // Generate & choose some colours for each of our elements
//
//     for error in errors {
//         Report::build(ReportKind::Error, "main.nl", error.span().start)
//             .with_code(1)
//             .with_message(error.message())
//             .with_label(
//                 Label::new(("main.nl", error.span().clone()))
//                     .with_message(format!("This is of type {}", "Nat".fg(Color::Cyan)))
//                     .with_color(Color::Red),
//             )
//             // .with_label(
//             //     Label::new(("main.nl", error.span().clone()))
//             //         .with_message(format!("This is of type {}", "Str".fg(b)))
//             //         .with_color(b),
//             // )
//             // .with_label(
//             //     Label::new(("main.nl", error.span().clone()))
//             //         .with_message(format!(
//             //             "The values are outputs of this {} expression",
//             //             "match".fg(out),
//             //         ))
//             //         .with_color(out),
//             // )
//             .with_note(format!(
//                 "Outputs of {} expressions must coerce to the same type",
//                 "match".fg(Color::Green)
//             ))
//             .finish()
//             .print(("main.nl", Source::from(src)))
//             .unwrap();
//     }
// }
use neolisp::{
    cli::{Cli, Command},
    compiler::{compile, CompilerOptions},
    machine::Machine,
    repl,
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
        Command::Run { repl, .. } if repl => {
            repl::run(args)?;
            Ok(())
        }
        Command::Run {
            decompile,
            file,
            no_main,
            ..
        } => run(file, decompile, no_main),
        Command::Test { file } => {
            let filename = file.unwrap_or("main.nl".to_string());
            let src = std::fs::read_to_string(&filename)?;
            neolisp::parser::parse_or_report(&filename, &src);
            Ok(())
        }
    }
}

// fn build(file: Option<String>, decompile: bool, options: &CompilerOptions) -> anyhow::Result<()> {
//     let filename = file.clone().unwrap_or("main.nl".to_string());
//     eprintln!("Compiling [{filename}]");
//     let Ok(src) = std::fs::read_to_string(filename) else {
//         panic!("failed to read file")
//     };
//     let now = std::time::Instant::now();
//     let program = compile(&src, options)?;
//     eprintln!("Compiled in {}ms", now.elapsed().as_millis());
//     eprintln!("Compiled to {} bytes", program.len());
//
//     if decompile {
//         let instructions = vm::decompile(&program);
//         simple_display_instructions(&instructions);
//         return Ok(());
//     }
//     // Save to file
//
//     let compiled_filename = get_compiled_filename(file)?;
//     std::fs::write(compiled_filename, program)?;
//
//     Ok(())
// }

// fn get_compiled_filename(file: Option<String>) -> anyhow::Result<String> {
//     let filename = file.unwrap_or("main.nl".to_string());
//     Ok(filename.split('.').next().unwrap().to_string())
// }

fn run(file: Option<String>, decompile: bool, no_main: bool) -> anyhow::Result<()> {
    let filename = file.unwrap_or("main.nl".to_string());

    let src = std::fs::read_to_string(filename)?;

    let options = CompilerOptions { no_main };
    let instructions = compile(&src, options)?;

    if decompile {
        let mut offset = 0;
        for int in instructions.iter() {
            eprintln!("{offset:02X} {offset:>2}  {:?}", int);
            offset += int.size();
        }
        return Ok(());
    }
    let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
    let mut vm = Machine::new(program);

    vm.run()?;

    Ok(())
}
