#[cfg(debug_assertions)]
use clap::ValueHint;
use clap::{Parser, Subcommand, ValueEnum};
#[cfg(windows)]
const HISTORY_PATH: &str = "%TEMP%.history";
#[cfg(unix)]
const HISTORY_PATH: &str = "/tmp/.history";

#[derive(Debug, Parser)]
#[command(about = "A fictional versioning CLI", long_about = None, color = clap::ColorChoice::Always)]
pub struct Cli {
    #[arg(short='H', long, default_value_t = String::from(HISTORY_PATH))]
    pub history_path: String,
    #[arg(short, long, default_value_t = EditMode::Vi)]
    pub editor_mode: EditMode,
    #[command(subcommand)]
    pub command: Option<Command>,
}
#[derive(Debug, Subcommand, Clone)]
pub enum Command {
    Build {
        #[arg(short = 'd', long, default_value_t = false)]
        decompile: bool,
        file: Option<String>,
    },
    Run {
        #[arg(short, long, default_value_t = false)]
        repl: bool,
        #[cfg(debug_assertions)]
        #[arg(long, help = "IP address to break on", value_delimiter = ',', value_hint = ValueHint::Other)]
        breakpoints: Vec<usize>,
        #[arg(short = 'd', long, default_value_t = false)]
        decompile: bool,
        #[arg(
            long,
            help = "compile with no main function as entry point",
            default_value_t = false
        )]
        no_main: bool,
        file: Option<String>,
    },
    Test {
        file: Option<String>,
    },
}

#[derive(Debug, ValueEnum, Clone, Copy)]
pub enum EditMode {
    Vi,
    Emacs,
}

impl std::fmt::Display for EditMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Vi => write!(f, "vi"),
            Self::Emacs => write!(f, "emacs"),
        }
    }
}

impl From<EditMode> for rustyline::config::EditMode {
    fn from(mode: EditMode) -> Self {
        match mode {
            EditMode::Vi => rustyline::config::EditMode::Vi,
            EditMode::Emacs => rustyline::config::EditMode::Emacs,
        }
    }
}
