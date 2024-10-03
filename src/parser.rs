use crate::ast::{Expr, Span, Spanned};
use crate::error::{Error, ErrorType};
use chumsky::prelude::*;

pub fn parse_expr() -> impl Parser<char, Spanned<Expr>, Error = Error> {
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
            .clone()
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
            .or(expr.clone())
            .padded_by(comment.repeated());

        let quoted = just('\'')
            .ignore_then(atom.clone())
            .padded_by(comment.repeated())
            .map_with_span(|e: Spanned<Expr>, span: Span| {
                (
                    Expr::List(vec![
                        (Expr::Symbol("quote".to_string()), span.clone()).into(),
                        e,
                    ]),
                    span,
                )
                    .into()
            });

        let list = atom
            .repeated()
            .delimited_by(
                just('(').map_err(|e| Error::with_label(e, ErrorType::MissingOpeningParenthesis)),
                just(')').map_err(|e| Error::with_label(e, ErrorType::MissingClosingParenthesis)),
            )
            .padded_by(comment.repeated())
            .padded()
            .map_with_span(|list, span| Spanned::from((Expr::List(list), span)));

        list.or(quoted)
    })
}

pub fn parser() -> impl Parser<char, Vec<Spanned<Expr>>, Error = Error> {
    parse_expr().repeated().padded().then_ignore(end())
}
