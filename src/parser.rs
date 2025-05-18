use crate::ast::{Expr, Span, Spanned};
use crate::error::{Error, ErrorType};
use chumsky::prelude::*;

pub fn parse_expr() -> impl Parser<char, Spanned, Error = Error> {
    recursive(|expr| {
        let comment = just(';').then(take_until(just('\n'))).padded();

        let int = text::digits(10)
            .then(just('_').ignore_then(text::digits(10)).repeated())
            .map(|(first, rest)| {
                let mut number = first;
                for part in rest {
                    number.push_str(&part);
                }
                number
            });

        let float = int
            .then_ignore(just('.'))
            .then(text::digits(10))
            .then(just('_').ignore_then(text::digits(10)).repeated())
            .map(|((integer_part, fractional_part), fractional_rest)| {
                let mut number = format!("{}.{}", integer_part, fractional_part);
                for part in fractional_rest {
                    number.push_str(&part);
                }
                number
            });

        let number = float
            .or(int)
            .or(just('-').repeated().then(float.or(int)).map(|(sign, num)| {
                let sign = if sign.len() % 2 == 0 { "" } else { "-" };
                format!("{sign}{num}")
            }))
            .map_with_span(|s: String, span: Span| {
                Spanned::from((Expr::Number(s.replace("_", "").parse().unwrap()), span))
            })
            .padded();

        let boolean = just("true")
            .map_with_span(|_, span| Spanned::from((Expr::Bool(true), span)))
            .or(just("false").map_with_span(|_, span| (Expr::Bool(false), span).into()))
            .padded();

        let string = one_of('"')
            .ignore_then(
                choice((
                    none_of("\"\\").map(|c: char| c.to_string()),
                    just("\\\"").map(|_| "\"".to_string()),
                    just("\\'").map(|_| "'".to_string()),
                    just("\\n").map(|_| "\n".to_string()),
                    just("\\t").map(|_| "\t".to_string()),
                    just("\\r").map(|_| "\r".to_string()),
                    just("\\0").map(|_| "\0".to_string()),
                    just("\\x1b").map(|_| "\x1b".to_string()),
                    just("\\\\").map(|_| "\\".to_string()),
                ))
                .repeated(),
            )
            .then_ignore(one_of('"'))
            .map_with_span(|chars, span| (Expr::String(chars.join("")), span).into())
            .padded();

        let punctuation = one_of(r#"!$,_-./:;?+<=>#%&*@[\]{|}`^~"#);
        let letters = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
        let digit = one_of("0123456789");

        let symbol = choice((letters.clone(), punctuation.clone()))
            .then(choice((letters, punctuation, digit)).repeated())
            .padded()
            .map_with_span(|(start, end), span| {
                (
                    Expr::Symbol(format!("{start}{}", end.iter().collect::<String>())),
                    span,
                )
                    .into()
            });

        let atom = number
            .or(string)
            .or(boolean)
            .or(symbol)
            .padded_by(comment.repeated());

        let quoted = just('\'')
            .ignore_then(expr.clone())
            .padded_by(comment.repeated())
            .map_with_span(|e: Spanned, span: Span| {
                (
                    Expr::List(vec![
                        (Expr::Symbol("quote".to_string()), span.clone()).into(),
                        e,
                    ]),
                    span,
                )
                    .into()
            });

        let unquote = just(',')
            .ignore_then(expr.clone())
            .padded_by(comment.repeated())
            .map_with_span(|e: Spanned, span: Span| {
                (
                    Expr::List(vec![
                        (Expr::Symbol("unquote".to_string()), span.clone()).into(),
                        e,
                    ]),
                    span,
                )
                    .into()
            });

        let unquote_splicing = just(",@")
            .ignore_then(expr.clone())
            .padded_by(comment.repeated())
            .map_with_span(|e: Spanned, span: Span| {
                (
                    Expr::List(vec![
                        (Expr::Symbol("unquote-splicing".to_string()), span.clone()).into(),
                        e,
                    ]),
                    span,
                )
                    .into()
            });

        let quasiquote = just('`')
            .ignore_then(expr.clone())
            .padded_by(comment.repeated())
            .map_with_span(|e: Spanned, span: Span| {
                (
                    Expr::List(vec![
                        (Expr::Symbol("quasiquote".to_string()), span.clone()).into(),
                        e,
                    ]),
                    span,
                )
                    .into()
            });

        let list = expr
            .repeated()
            .delimited_by(
                just('(')
                    .padded()
                    .map_err(|e| Error::with_label(e, ErrorType::MissingOpeningParenthesis)),
                just(')')
                    .padded()
                    .map_err(|e| Error::with_label(e, ErrorType::MissingClosingParenthesis)),
            )
            .padded_by(comment.repeated())
            .padded()
            .map_with_span(|list, span| Spanned::from((Expr::List(list), span)));

        list.or(quoted)
            .or(quasiquote)
            .or(unquote_splicing)
            .or(unquote)
            .or(atom)
    })
}

pub fn parser() -> impl Parser<char, Vec<Spanned>, Error = Error> {
    parse_expr().repeated().padded().then_ignore(end())
}

pub fn parse_or_report(filename: &str, src: &str) -> Vec<Spanned> {
    use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
    match parser().parse(src) {
        Ok(ast) => ast,
        Err(errors) => {
            errors.into_iter().for_each(|error| {
                Report::build(ReportKind::Error, (filename, error.span().clone()))
                    .with_code(1)
                    .with_message(error.message())
                    .with_label(
                        Label::new((filename, error.span().clone()))
                            .with_message(format!("This is of type {}", "Nat".fg(Color::Cyan)))
                            .with_color(Color::Red),
                    )
                    .with_note(format!(
                        "Outputs of {} expressions must coerce to the same type",
                        "match".fg(Color::Green)
                    ))
                    .finish()
                    .eprint((filename, Source::from(src)))
                    .unwrap();
            });
            Vec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn quasiquote() {
        use pretty_assertions::assert_eq;
        let src = r#"
        `(1 ,(list n))
        "#;
        let ast = parser().parse(src).unwrap();
        assert_eq!(ast, vec![]);
    }
}
