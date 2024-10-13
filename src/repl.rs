use crate::cli::Cli;
use crate::debugger::Debugger;
use crate::docs;
use crate::machine::{Machine, MachineOptions};
use anyhow::Result;
use crossterm::style::Stylize;
use rustyline::config::Configurer;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers};
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
const HELP: &str = r#"
commands
:q|e|exit|quit              - quit
:help                       - show this help message
:clear                      - clear the terminal screen
:help-doc <function name>   - with a give function name will return the help documentation
:debugger                   - start the debugger
:builtin-id <function name> - with a give function name will return the builtin id
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

enum ReplCommand {
    Exit,
    Help,
    Clear,
    HelpDoc(String),
    Debugger,
    BuiltinId(usize),
    Error(String),
}

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
struct InputValidator {
    #[rustyline(Validator)]
    brackets: MatchingBracketValidator,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
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
        KeyEvent(KeyCode::Char('c'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Interrupt),
    );
    rl.bind_sequence(
        KeyEvent(KeyCode::Enter, Modifiers::NONE),
        EventHandler::Conditional(Box::new(ShouldRunIfThereIsANewLine)),
    );
    if rl.load_history(&args.history_path).is_err() {
        println!("No previous history.");
    }

    let help_docs = docs::load_doc();
    let mut machine = Machine::default();

    loop {
        let input = match rl.readline("> ") {
            Ok(input) => input,
            Err(rustyline::error::ReadlineError::Interrupted) => {
                break;
            }
            Err(rustyline::error::ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        };

        if input.is_empty() {
            continue;
        }

        rl.add_history_entry(input.as_str())?;
        if let Some(command) = parse_input(&input) {
            match command {
                ReplCommand::Exit => break,
                ReplCommand::Help => {
                    println!("{HELP}");
                    continue;
                }
                ReplCommand::Clear => {
                    rl.clear_screen()?;
                    continue;
                }
                ReplCommand::HelpDoc(name) => {
                    let Some(docs) = help_docs.get(&name) else {
                        println!("No docs for {name}");
                        continue;
                    };
                    println!("{docs}");
                }
                ReplCommand::Debugger => {
                    machine.set_options(MachineOptions { quiet: true });
                    Debugger::new(&mut machine)?.run()?;
                    machine.set_options(MachineOptions::default());
                    continue;
                }
                ReplCommand::BuiltinId(idx) => {
                    println!("{idx}: {}", &super::BUILTINS[idx]);
                    continue;
                }
                ReplCommand::Error(message) => {
                    println!("{message}");
                    continue;
                }
            }
        }

        match machine.run_from_string(&input) {
            Ok(()) => (),
            Err(e) => println!("{e}"),
        }
        if let Some(last) = machine.pop()? {
            eprintln!(":{last}");
        }
    }
    if rl.append_history(&args.history_path).is_err() {
        std::fs::write(&args.history_path, "")?;
        rl.save_history(&args.history_path)?;
    }
    Ok(())
}

fn parse_input(input: &str) -> Option<ReplCommand> {
    if !input.starts_with(':') {
        return None;
    }

    let command = input.split(' ').collect::<Vec<_>>();
    match command[0] {
        ":q" | ":e" | ":exit" | ":quit" => Some(ReplCommand::Exit),
        ":help" => Some(ReplCommand::Help),
        ":clear" => Some(ReplCommand::Clear),
        ":help-doc" if command.len() == 2 => Some(ReplCommand::HelpDoc(command[1].to_string())),
        ":help-doc" => Some(ReplCommand::Error(":help-doc <name>".to_string())),
        ":debugger" => Some(ReplCommand::Debugger),
        ":builtin-id" if command.len() == 2 => {
            Some(ReplCommand::BuiltinId(command[1].parse().ok()?))
        }
        _ => Some(ReplCommand::Error(format!(
            "Invalid command: {}",
            input.red()
        ))),
    }
}
