use crate::cli::Cli;
use crate::compiler::CompilerOptions;
use crate::compiler::{compile_to_instructions, simple_display_instructions};
use crate::docs;
use crate::parser::parser;
use crate::vm::Machine;
use chumsky::prelude::Parser;
use rustyline::config::Configurer;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers, Result};
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
const HELP: &str = r#"
commands
:q|e|exit|quit            - quit
:help                     - show this help message
:clear                    - clear the terminal screen
:help-doc <function name> - with a give function name will return the help documentation
"#;
struct ShouldRunIfThereIsANewLine;

impl rustyline::ConditionalEventHandler for ShouldRunIfThereIsANewLine {
    fn handle(
        &self,
        _: &rustyline::Event,
        _: rustyline::RepeatCount,
        _: bool,
        ctx: &rustyline::EventContext,
    ) -> Option<Cmd> {
        let mut stack: i32 = 0;
        for c in ctx.line().chars() {
            if c == '(' {
                stack += 1;
            } else if c == ')' {
                stack -= 1;
            }
        }

        if stack != 0 {
            Some(Cmd::Newline)
        } else {
            Some(Cmd::AcceptLine)
        }
    }
}

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputValidator {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
}

#[derive(Debug, Default)]
enum ReplOutputMode {
    Ast,
    Op,
    #[default]
    Eval,
}

pub fn run(args: Cli) -> Result<()> {
    let h = InputValidator {
        brackets: MatchingBracketValidator::new(),
        highlighter: MatchingBracketHighlighter::new(),
    };
    let mut rl = Editor::new()?;
    rl.set_edit_mode(args.editor_mode.into());
    rl.set_helper(Some(h));
    rl.bind_sequence(
        KeyEvent(KeyCode::Enter, Modifiers::NONE),
        EventHandler::Conditional(Box::new(ShouldRunIfThereIsANewLine)),
    );
    if rl.load_history(&args.history_path).is_err() {
        println!("No previous history.");
    }

    let help_docs = docs::load_doc();
    let compiler_options = CompilerOptions { no_main: true };
    let mut repl_output_mode = ReplOutputMode::default();

    loop {
        let input = rl.readline("> ")?;

        if input.is_empty() {
            continue;
        }
        rl.add_history_entry(input.as_str())?;

        // Run command
        let command = input.split(' ').collect::<Vec<&str>>();
        if command.is_empty() {
            continue;
        }
        match command[0] {
            ":q" | ":e" | ":exit" | ":quit" => break,
            ":output-mode" if command.len() == 2 => match command[1] {
                "ast" => repl_output_mode = ReplOutputMode::Ast,
                "op" => repl_output_mode = ReplOutputMode::Op,
                "eval" => repl_output_mode = ReplOutputMode::Eval,
                _ => {
                    println!("unknown option {}", command[0]);
                    continue;
                }
            },
            ":help" => println!("{HELP}"),
            ":clear" => rl.clear_screen()?,
            ":help-doc" if command.len() == 2 => {
                let name = command[1];
                let Some(docs) = help_docs.get(name) else {
                    println!("No docs for {name}");
                    continue;
                };

                println!("{docs}");
            }
            _ => (),
        }

        // Ignore commands
        match command[0] {
            ":help-doc" | ":help" | ":clear" | ":env" | ":output-mode" => continue,
            _ => (),
        };

        match repl_output_mode {
            ReplOutputMode::Ast => {
                let ast = parser().parse(input).map_err(|e| {
                    anyhow::anyhow!(e.iter().map(|e| format!("{e}\n")).collect::<String>())
                });
                match ast {
                    Ok(v) => println!("{v:#?}"),
                    Err(e) => println!("{e}"),
                }
            }
            ReplOutputMode::Op => {
                let instructions = match compile_to_instructions(&input, &compiler_options) {
                    Ok(v) => v,
                    Err(e) => {
                        println!("{e}");
                        continue;
                    }
                };
                simple_display_instructions(&instructions);
            }
            ReplOutputMode::Eval => {
                let program = match crate::compiler::compile(&input, &compiler_options) {
                    Ok(v) => v,
                    Err(e) => {
                        println!("{e}");
                        continue;
                    }
                };
                let mut machine = Machine::new(program);
                machine.run();
                if let Some(last) = machine.stack.last() {
                    eprintln!("{last}");
                }
            }
        }
    }
    if rl.append_history(&args.history_path).is_err() {
        std::fs::write(&args.history_path, "")?;
        rl.save_history(&args.history_path)?;
    }
    Ok(())
}
