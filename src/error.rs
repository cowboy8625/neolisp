use anyhow::Result;
use ariadne::{Color, Label, Report, ReportKind, Source};
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
    NotCallable(Span, String),
    SymbolNotDefined(Span, String),
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::ExpectedFound(span, ..) => span,
            Self::MissingClosingParenthesis(span) => span,
            Self::MissingOpeningParenthesis(span) => span,
            Self::NotCallable(span, _) => span,
            Self::SymbolNotDefined(span, _) => span,
        }
    }

    pub fn message(&self) -> String {
        match self {
            Self::ExpectedFound(_, expected, found) => {
                format!("expected {expected:?} found {found:?}")
            }
            Self::MissingClosingParenthesis(_) => "missing closing parenthesis".to_string(),
            Self::MissingOpeningParenthesis(_) => "missing opening parenthesis".to_string(),
            Self::NotCallable(_, expr) => format!("{expr}'s are not callable"),
            Self::SymbolNotDefined(_, symbol) => format!("'{symbol}' is not defined"),
        }
    }

    pub fn report(&self, filename: &str, src: &str) -> Result<()> {
        Report::build(ReportKind::Error, filename, self.span().start)
            .with_code(1)
            .with_message(self.message())
            .with_label(
                Label::new((filename, self.span().clone()))
                    .with_message(self.message())
                    .with_color(Color::Red),
            )
            // .with_label(
            //     Label::new(("main.nl", error.span().clone()))
            //         .with_message(format!("This is of type {}", "Str".fg(b)))
            //         .with_color(b),
            // )
            // .with_label(
            //     Label::new(("main.nl", error.span().clone()))
            //         .with_message(format!(
            //             "The values are outputs of this {} expression",
            //             "match".fg(out),
            //         ))
            //         .with_color(out),
            // )
            // .with_note(format!(
            //     "Outputs of {} expressions must coerce to the same type",
            //     "match".fg(Color::Green)
            // ))
            .finish()
            .print((filename, Source::from(src)))?;
        Ok(())
    }
}
// use chumsky::prelude::*;
// use neolisp::parser::parser;
// fn main() {
//     let src = include_str!("../main.nl");
//     let (ast, errors) = match parser().parse(src) {
//         Ok(ast) => (ast, vec![]),
//         Err(errors) => (vec![], errors),
//     };
//     println!("{ast:#?}");
//     println!("{errors:#?}");
//
//     let mut colors = ColorGenerator::new();
//
//     // Generate & choose some colours for each of our elements
//
//     for error in errors {
//         Report::build(ReportKind::Error, "main.nl", error.span().start)
//             .with_code(1)
//             .with_message(error.message())
//             .with_label(
//                 Label::new(("main.nl", error.span().clone()))
//                     .with_message(format!("This is of type {}", "Nat".fg(Color::Cyan)))
//                     .with_color(Color::Red),
//             )
//             // .with_label(
//             //     Label::new(("main.nl", error.span().clone()))
//             //         .with_message(format!("This is of type {}", "Str".fg(b)))
//             //         .with_color(b),
//             // )
//             // .with_label(
//             //     Label::new(("main.nl", error.span().clone()))
//             //         .with_message(format!(
//             //             "The values are outputs of this {} expression",
//             //             "match".fg(out),
//             //         ))
//             //         .with_color(out),
//             // )
//             .with_note(format!(
//                 "Outputs of {} expressions must coerce to the same type",
//                 "match".fg(Color::Green)
//             ))
//             .finish()
//             .print(("main.nl", Source::from(src)))
//             .unwrap();
//     }
// }

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
