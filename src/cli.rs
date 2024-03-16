use clap::{Parser, ValueEnum};
#[cfg(windows)]
const HISTORY_PATH: &str = "%TEMP%.history";
#[cfg(unix)]
const HISTORY_PATH: &str = "/tmp/.history";

#[derive(Debug, Parser)]
#[command(about = "A fictional versioning CLI", long_about = None, color = clap::ColorChoice::Always)]
pub struct Cli {
    #[arg(short, long, default_value_t = EditMode::Vi)]
    pub editor_mode: EditMode,
    #[arg(short, long, default_value_t = false)]
    pub repl: bool,
    #[arg(short='H', long, default_value_t = String::from(HISTORY_PATH))]
    pub history_path: String,
    pub files: Vec<String>,
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
