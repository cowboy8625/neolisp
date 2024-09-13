use crate::cli::Cli;
use crate::environment::Env;
use crate::error::print_error;
use crate::eval::eval;
use crate::parser::parse_expr;
use chumsky::prelude::*;
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
    if rl.load_history(&args.history_path).is_err() {
        println!("No previous history.");
    }

    loop {
        let input = rl.readline("> ")?;

        if input.is_empty() {
            continue;
        }
        rl.add_history_entry(input.as_str())?;

        // Run command
        match input.as_str() {
            ":q" | ":e" | ":exit" | ":quit" => break,
            ":help" => println!("{HELP}"),
            ":clear" => rl.clear_screen()?,
            ":env" => {
                println!("Environment:");
                let mut e = Vec::new();
                for (k, v) in &env.data {
                    e.push(format!("{k} : {}", v.expr.type_of()));
                }
                e.sort();
                let max = e
                    .iter()
                    .map(|a| a.split_once(':').unwrap().0.len())
                    .max()
                    .unwrap_or_default();
                println!(
                    "{}",
                    e.iter()
                        .filter_map(|x| x.split_once(':'))
                        .map(|(k, v)| format!("{k:>max$} : {v}"))
                        .collect::<Vec<String>>()
                        .join("\n")
                );
            }
            _ => (),
        }

        // Ignore commands
        match input.as_str() {
            ":help" | ":clear" | ":env" => continue,
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
