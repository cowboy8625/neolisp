use crate::cli::Cli;
use crate::error::print_error;
use crate::eval::{eval, Env};
use crate::parser::parse_expr;
use chumsky::prelude::*;
use rustyline::config::Configurer;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Cmd, Editor, EventHandler, KeyCode, KeyEvent, Modifiers, Result};
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};

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
        KeyEvent(KeyCode::Char('s'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Complete),
    );

    let mut env = Env::new();
    if let Err(_) = rl.load_history(&args.history_path) {
        println!("No previous history.");
    }

    loop {
        let input = rl.readline("> ")?;

        if input.is_empty() {
            continue;
        }
        rl.add_history_entry(input.as_str())?;

        match input.as_str() {
            ":q" => break,
            _ => (),
        }

        let ast = match parse_expr().parse(input.clone()) {
            Ok(expr) => expr,
            Err(error) => {
                for e in error {
                    print_error(&input, &e);
                }
                continue;
            }
        };
        match eval(&ast, &mut env) {
            Ok(r) => println!("{r}"),
            Err(e) => println!("{e}"),
        }
    }
    if rl.append_history(&args.history_path).is_err() {
        std::fs::write(&args.history_path, "")?;
        rl.save_history(&args.history_path)?;
    }
    Ok(())
}
