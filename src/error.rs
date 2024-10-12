#![allow(dead_code)]
pub enum ErrorType {
    MissingClosingParenthesis,
    MissingOpeningParenthesis,
}

type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ExpectedFound(Span, Vec<Option<char>>, Option<char>),
    MissingClosingParenthesis(Span),
    MissingOpeningParenthesis(Span),
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::ExpectedFound(span, ..) => span,
            Self::MissingClosingParenthesis(span) => span,
            Self::MissingOpeningParenthesis(span) => span,
        }
    }

    pub fn message(&self) -> String {
        match self {
            Self::ExpectedFound(_, expected, found) => {
                format!("expected {expected:?} found {found:?}")
            }
            Self::MissingClosingParenthesis(_) => "missing closing parenthesis".to_string(),
            Self::MissingOpeningParenthesis(_) => "missing opening parenthesis".to_string(),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedFound(span, expected, found) => {
                write!(f, "expected ")?;
                for (i, e) in expected.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", {e:?}")?;
                    } else {
                        write!(f, "{e:?} ")?;
                    }
                }
                write!(f, "found {found:?} at {span:?}")
            }
            Self::MissingClosingParenthesis(span) => {
                write!(f, "{}:{} missing closing parenthesis", span.start, span.end)
            }
            Self::MissingOpeningParenthesis(span) => {
                write!(f, "{}:{} missing opening parenthesis", span.start, span.end)
            }
        }
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
        Self::ExpectedFound(span, expected.into_iter().collect(), found)
    }

    fn with_label(self, label: Self::Label) -> Self {
        let Self::ExpectedFound(span, _, _) = self else {
            return self;
        };
        match label {
            ErrorType::MissingClosingParenthesis => Self::MissingClosingParenthesis(span),
            ErrorType::MissingOpeningParenthesis => Self::MissingOpeningParenthesis(span),
        }
    }

    fn merge(mut self, mut other: Self) -> Self {
        if let (Self::ExpectedFound(_, expected, _), Self::ExpectedFound(_, expected_other, _)) =
            (&mut self, &mut other)
        {
            expected.append(expected_other);
        }
        self
    }
}

pub fn print_error(src: &str, error: &Error) {
    let span = match error {
        Error::ExpectedFound(span, _, _) => span,
        Error::MissingClosingParenthesis(span) => span,
        Error::MissingOpeningParenthesis(span) => span,
    };
    let mut index = 0;
    let mut row = 0;
    for line in src.lines() {
        let line_span = index..(index + line.len());
        if line_span.contains(&span.start) {
            break;
        }
        index += line.len();
        row += 1;
    }
    let col = span.end - index;

    let message = match error {
        Error::ExpectedFound(_, expected, found) => {
            format!("expected {expected:?} but found {found:?} at {span:?}")
        }
        Error::MissingClosingParenthesis(_) => format!("missing closing parenthesis: {span:?}"),
        Error::MissingOpeningParenthesis(_) => format!("missing opening parenthesis: {span:?}"),
    };
    println!("\x1b[31m{row}:{col}:{span:?}Error: {message}\x1b[0m");
    for (line_num, line) in src.lines().enumerate() {
        let ln = format!("{line_num: >4} | ");
        if row == line_num {
            println!("{ln}{line}");
            println!(
                "{}\x1b[31m{:>col$}\x1b[0m {row}:{col} {message}",
                (0..ln.len()).map(|_| ' ').collect::<String>(),
                '^'
            );
        } else if (row.saturating_sub(3)..=row.saturating_add(3)).contains(&line_num) {
            println!("{ln}{line}");
        }
    }
}
