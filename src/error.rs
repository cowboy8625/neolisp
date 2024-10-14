use super::ast::Span;
use anyhow::Result;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

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

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
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
        span: Span,
        name: String,
        message: String,
        code: String,
        note: Option<String>,
        help: Option<String>,
    },
}

impl Error {
    fn code(&self) -> String {
        match self {
            Error::ExpectedFound { .. } => String::from("E000"),
            Error::MissingClosingParenthesis(..) => String::from("E001"),
            Error::MissingOpeningParenthesis(..) => String::from("E001"),
            Error::NotCallable(..) => String::from("E002"),
            Error::SymbolNotDefined(..) => String::from("E003"),
            Error::EmptyFile => todo!(),
            Error::MainNotDefined => String::from("E004"),
            Error::RunTimeError { code, .. } => code.clone(),
        }
    }

    fn error_kind(&self) -> ReportKind {
        match self {
            Self::RunTimeError { .. } => ReportKind::Custom("RunTimeError", Color::Red),
            _ => ReportKind::Error,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::ExpectedFound { span, .. } => span,
            Self::MissingClosingParenthesis(span) => span,
            Self::MissingOpeningParenthesis(span) => span,
            Self::NotCallable(span, _) => span,
            Self::SymbolNotDefined(span, _) => span,
            Self::EmptyFile => &(0..0),
            Self::MainNotDefined => &(0..0),
            Self::RunTimeError { span, .. } => span,
        }
    }

    pub fn message(&self) -> String {
        match self {
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
        }
    }

    fn note(&self) -> Option<String> {
        match self {
            Error::ExpectedFound { note, .. } => note.clone(),
            Error::MissingClosingParenthesis(..) => todo!(),
            Error::MissingOpeningParenthesis(..) => None,
            Error::NotCallable(..) => None,
            Error::SymbolNotDefined(..) => None,
            Error::EmptyFile => None,
            Error::MainNotDefined => None,
            Error::RunTimeError { note, .. } => note.clone(),
        }
    }
    fn help(&self) -> Option<String> {
        match self {
            Error::ExpectedFound{help, ..} => help.clone(),
            Error::MissingClosingParenthesis(..) => None,
            Error::MissingOpeningParenthesis(..) => None,
            Error::NotCallable(..) => None,
            Error::SymbolNotDefined(..) => None,
            Error::EmptyFile => Some(
                format!("Hey, you have an empty file!
 If your unsure how to get started with the language, take a look at the docs but here is a quick start tip.
 Put this 👇 in your file and try again.
 {}", empty_file_example())
           ),
            Error::MainNotDefined => Some("If this is intentinal you may want to add the `--no-main` flag.".to_string()),
            Error::RunTimeError { help, .. } => help.clone(),
        }
    }

    pub fn report(&self, filename: &str, src: &str) -> Result<()> {
        let src = if src.is_empty() {
            " ".to_string()
        } else {
            src.to_string()
        };
        let mut report = Report::build(self.error_kind(), filename, self.span().start)
            .with_code(self.code())
            .with_message(self.message())
            .with_label(
                Label::new((filename, self.span().clone()))
                    .with_message(self.message())
                    .with_color(Color::Red),
            );
        if let Some(note) = self.note() {
            report = report.with_note(note);
        }

        if let Some(help) = self.help() {
            report = report.with_help(help);
        }
        report.finish().print((filename, Source::from(src)))?;
        Ok(())
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
