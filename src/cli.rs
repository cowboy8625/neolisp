use clap::{ArgAction, Parser, Subcommand, ValueEnum, ValueHint};
#[cfg(windows)]
const HISTORY_PATH: &str = "%TEMP%.history";
#[cfg(unix)]
const HISTORY_PATH: &str = "/tmp/.history";

#[derive(Debug, Parser)]
#[command(about = "neolisp - a simple Lisp interpreter", long_about = None, color = clap::ColorChoice::Always)]
pub struct Cli {
    #[arg(short, long, default_value_t = false)]
    pub ast_debug: bool,
    #[arg(short, long, default_value_t = false)]
    pub no_color: bool,
    #[arg(long, default_value_t = false)]
    pub symbol_table_debug: bool,
    #[arg(short='H', long, default_value_t = String::from(HISTORY_PATH))]
    pub history_path: String,
    #[arg(short, long, default_value_t = EditMode::Vi)]
    pub editor_mode: EditMode,
    #[command(subcommand)]
    pub command: Option<Command>,
}

#[derive(Debug, Default, Clone)]
pub enum Decompile {
    #[default]
    All,
    Function(String),
}

impl std::str::FromStr for Decompile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() || s == "all" {
            Ok(Decompile::All)
        } else {
            Ok(Decompile::Function(s.to_string()))
        }
    }
}

impl std::fmt::Display for Decompile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::All => write!(f, "all"),
            Self::Function(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Debug, Clone, Parser)]
pub struct Run {
    #[arg(short, long, default_value_t = false)]
    pub repl: bool,
    #[arg(long, help = "IP address to break on", value_delimiter = ',', value_hint = ValueHint::Other)]
    pub breakpoints: Vec<usize>,
    #[arg(
        short,
        long,
        help = "empty to decompile all or name to decompile a specific function",
        value_name = "NAME",
        value_hint = ValueHint::Other,
        default_missing_value = "all",
        num_args(0..=1),
        action = ArgAction::Set)]
    pub decompile: Option<Decompile>,
    #[arg(
        long,
        help = "compile with no main function as entry point",
        default_value_t = false
    )]
    pub no_main: bool,
    pub file: Option<String>,
}

#[derive(Debug, Clone, Parser)]
pub struct Test {
    #[arg(long, help = "IP address to break on", value_delimiter = ',', value_hint = ValueHint::Other)]
    pub breakpoints: Vec<usize>,
    #[arg(
        short,
        long,
        help = "empty to decompile all or name to decompile a specific function",
        value_name = "NAME",
        value_hint = ValueHint::Other,
        default_missing_value = "all",
        num_args(0..=1),
        action = ArgAction::Set)]
    pub decompile: Option<Decompile>,
    pub file: Option<String>,
}

#[derive(Debug, Subcommand, Clone)]
pub enum Command {
    Build {
        #[arg(
            short,
            long,
            help = "empty to decompile all or name to decompile a specific function",
            value_name = "NAME",
            value_hint = ValueHint::Other,
            default_missing_value = "all",
            num_args(0..=1),
            action = ArgAction::Set)]
        decompile: Option<Decompile>,
        file: Option<String>,
    },
    Run(Run),
    Test(Test),
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
