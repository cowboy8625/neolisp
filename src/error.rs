use super::ast::Span;
use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Source};

fn empty_file_example() -> String {
    let open_pran = "(".fg(Color::Yellow);
    let close_pran = ")".fg(Color::Yellow);
    let keyword = "fn".fg(Color::Cyan);
    let id_main = "main".fg(Color::Blue);
    let id_print = "print".fg(Color::Blue);
    let string = r#"Hello, World!""#.fg(Color::Green);
    let newline = r#""\n""#.fg(Color::Green);
    format!(
        "{}{} {} {}{} {}{} {} {}{}{}",
        open_pran,
        keyword,
        id_main,
        open_pran,
        close_pran,
        open_pran,
        id_print,
        string,
        newline,
        close_pran,
        close_pran
    )
}
pub enum ErrorType {
    MissingClosingParenthesis,
    MissingOpeningParenthesis,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    StackUnderflow,
    Internal(String),
    ExpectedFound {
        span: Span,
        expected: String,
        found: String,
        note: Option<String>,
        help: Option<String>,
    },
    MissingClosingParenthesis(Span),
    MissingOpeningParenthesis(Span),
    NotCallable(Span, String),
    SymbolNotDefined(Span, String),
    EmptyFile,
    MainNotDefined,
    RunTimeError {
        name: String,
        span: Span,
        stack_trace: Vec<(String, Span)>,
        message: String,
        code: String,
        note: Option<String>,
        help: Option<String>,
    },
    Redefined {
        original_span: Span,
        original_name: String,
        new_span: Span,
        new_name: String,
        note: Option<String>,
        help: Option<String>,
    },
    TestNotDefined {
        span: Span,
        name: String,
    },
    Temporary {
        span: Span,
        message: String,
    },
}

impl Error {
    fn code(&self) -> String {
        match self {
            Self::Internal(..) => String::from("E999"),
            Self::ExpectedFound { .. } => String::from("E000"),
            Self::MissingClosingParenthesis(..) => String::from("E001"),
            Self::MissingOpeningParenthesis(..) => String::from("E001"),
            Self::NotCallable(..) => String::from("E002"),
            Self::SymbolNotDefined(..) => String::from("E003"),
            Self::EmptyFile => todo!(),
            Self::MainNotDefined => String::from("E004"),
            Self::RunTimeError { code, .. } => code.clone(),
            Self::Redefined { .. } => String::from("E005"),
            Self::TestNotDefined { .. } => String::from("E006"),
            Self::StackUnderflow => String::from("EFFF"),
            Self::Temporary { .. } => String::from("EFFF"),
        }
    }

    fn error_kind(&self) -> ReportKind {
        match self {
            Self::Internal(..) => ReportKind::Custom("Internal", Color::Red),
            Self::RunTimeError { .. } => ReportKind::Custom("RunTimeError", Color::Red),
            Self::StackUnderflow => ReportKind::Custom("RunTimeError", Color::Red),
            _ => ReportKind::Error,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Internal(..) => &(0..0),
            Self::ExpectedFound { span, .. } => span,
            Self::MissingClosingParenthesis(span) => span,
            Self::MissingOpeningParenthesis(span) => span,
            Self::NotCallable(span, _) => span,
            Self::SymbolNotDefined(span, _) => span,
            Self::EmptyFile => &(0..0),
            Self::MainNotDefined => &(0..0),
            Self::RunTimeError { span, .. } => span,
            Self::Redefined { new_span, .. } => new_span,
            Self::TestNotDefined { span, .. } => span,
            Self::Temporary { span, .. } => span,
            Self::StackUnderflow => &(0..0),
        }
    }

    pub fn second_label<'a>(&'a self, filename: &'a str) -> Option<Label<(&'a str, Span)>> {
        match self {
            Self::Internal(..) => None,
            Self::ExpectedFound { .. } => None,
            Self::MissingClosingParenthesis(_) => None,
            Self::MissingOpeningParenthesis(_) => None,
            Self::NotCallable(..) => None,
            Self::SymbolNotDefined(..) => None,
            Self::EmptyFile => None,
            Self::MainNotDefined => None,
            Self::RunTimeError { .. } => None,
            Self::Redefined { original_span, .. } => Some(
                Label::new((filename, original_span.clone()))
                    .with_message("this definition was shadowed by the previous one")
                    .with_color(Color::Blue),
            ),
            Self::TestNotDefined { span, .. } => Some(
                Label::new((filename, span.clone()))
                    .with_message("this test is not defined")
                    .with_color(Color::Blue),
            ),
            Self::Temporary { .. } => None,
            Self::StackUnderflow => None,
        }
    }

    pub fn message(&self) -> String {
        match self {
            Self::Internal(message) => message.to_string(),
            Self::ExpectedFound {
                expected, found, ..
            } => {
                format!("expected {expected} found {found}")
            }
            Self::MissingClosingParenthesis(_) => "missing closing parenthesis".to_string(),
            Self::MissingOpeningParenthesis(_) => "missing opening parenthesis".to_string(),
            Self::NotCallable(_, expr) => format!("{expr}'s are not callable"),
            Self::SymbolNotDefined(_, symbol) => format!("'{symbol}' is not defined"),
            Self::EmptyFile => "empty file".to_string(),
            Self::MainNotDefined => "main is not defined".to_string(),
            Self::RunTimeError { message, .. } => message.to_string(),
            Self::Redefined { .. } => "redefined".to_string(),
            Self::TestNotDefined { name, .. } => format!("test '{name}' is not defined"),
            Self::Temporary { message, .. } => message.to_string(),
            Self::StackUnderflow => "stack underflow".to_string(),
        }
    }

    fn note(&self) -> Option<String> {
        match self {
            Self::Internal(..) => None,
            Self::ExpectedFound { note, .. } => note.clone(),
            Self::MissingClosingParenthesis(..) => None,
            Self::MissingOpeningParenthesis(..) => None,
            Self::NotCallable(..) => None,
            Self::SymbolNotDefined(..) => None,
            Self::EmptyFile => None,
            Self::MainNotDefined => None,
            Self::RunTimeError { note, .. } => note.clone(),
            Self::Redefined { note, .. } => note.clone(),
            Self::TestNotDefined { .. } => None,
            Self::Temporary { .. } => None,
            Self::StackUnderflow => None,
        }
    }
    fn help(&self) -> Option<String> {
        match self {
            Self::Internal(..) => None,
            Self::ExpectedFound{help, ..} => help.clone(),
            Self::MissingClosingParenthesis(..) => None,
            Self::MissingOpeningParenthesis(..) => None,
            Self::NotCallable(..) => None,
            Self::SymbolNotDefined(..) => None,
            Self::EmptyFile => Some(
                format!("Hey, you have an empty file!
 If your unsure how to get started with the language, take a look at the docs but here is a quick start tip.
 Put this 👇 in your file and try again.
 {}", empty_file_example())
           ),
            Self::MainNotDefined => Some("If this is intentinal you may want to add the `--no-main` flag.".to_string()),
            Self::RunTimeError { help, .. } => help.clone(),
            Self::Redefined { help, .. } => help.clone(),
            Self::TestNotDefined { .. } => None,
            Self::Temporary{..} => None,
            Self::StackUnderflow => None,
        }
    }

    pub fn report(&self, filename: &str, src: &str, has_color: bool) {
        let src = if src.is_empty() {
            " ".to_string()
        } else {
            src.to_string()
        };
        let mut report = Report::build(self.error_kind(), (filename, self.span().clone()))
            .with_config(Config::default().with_color(has_color))
            .with_code(self.code())
            .with_message(self.message())
            .with_label(
                Label::new((filename, self.span().clone()))
                    .with_message(self.message())
                    .with_color(Color::Red),
            );
        if let Some(label) = self.second_label(filename) {
            report = report.with_label(label);
        }
        if let Some(note) = self.note() {
            report = report.with_note(note);
        }

        if let Some(help) = self.help() {
            report = report.with_help(help);
        }
        report
            .finish()
            .print((filename, Source::from(src)))
            .expect("failed to print error");
    }
}

impl chumsky::Error<char> for Error {
    type Span = std::ops::Range<usize>;
    type Label = ErrorType;

    fn expected_input_found<It: IntoIterator<Item = Option<char>>>(
        span: Self::Span,
        expected: It,
        found: Option<char>,
    ) -> Self {
        Self::ExpectedFound {
            span,
            expected: expected
                .into_iter()
                .filter_map(|c| c.map(|c| c.to_string()))
                .collect(),
            found: found.map(|c| c.to_string()).unwrap_or_default(),
            note: None,
            help: None,
        }
    }

    fn with_label(self, label: Self::Label) -> Self {
        let Self::ExpectedFound { span, .. } = self else {
            return self;
        };
        match label {
            ErrorType::MissingClosingParenthesis => Self::MissingClosingParenthesis(span),
            ErrorType::MissingOpeningParenthesis => Self::MissingOpeningParenthesis(span),
        }
    }

    fn merge(mut self, mut other: Self) -> Self {
        if let (
            Self::ExpectedFound { expected, .. },
            Self::ExpectedFound {
                expected: expected_other,
                ..
            },
        ) = (&mut self, &mut other)
        {
            expected.push_str(expected_other);
        }
        self
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}
