use crate::cli::Cli;
use crate::docs;
use crate::vm::Machine;
use rustyline::config::Configurer;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers, Result};
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
const HELP: &str = r#"
commands
:q|e|exit|quit   - quit
:help  - show this help message
:clear -  clear the terminal screen
:env   -  show environment variables
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

        if stack > 0 || stack < 0 {
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
            ":help-doc" | ":help" | ":clear" | ":env" => continue,
            _ => (),
        }

        let program = match crate::compiler::compile(&input) {
            Ok(v) => v,
            Err(e) => {
                println!("{e}");
                continue;
            }
        };
        let mut machine = Machine::new(program);
        machine.run();
    }
    if rl.append_history(&args.history_path).is_err() {
        std::fs::write(&args.history_path, "")?;
        rl.save_history(&args.history_path)?;
    }
    Ok(())
}
