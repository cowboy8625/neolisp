use crate::ast::{Expr, Span, Spanned};
use crate::error::{Error, ErrorType};
use chumsky::prelude::*;

pub fn parse_expr() -> impl Parser<char, Spanned<Expr>, Error = Error> {
    recursive(|expr| {
        let comment = just(';').then(take_until(just('\n'))).padded();

        let int = text::int(10);

        let number = int
            .or(just('-').repeated().then(int).map(|(sign, num)| {
                let sign = if sign.len() % 2 == 0 { "" } else { "-" };
                format!("{sign}{num}")
            }))
            .map_with_span(|s: String, span: Span| {
                Spanned::from((Expr::Number(s.parse().unwrap()), span))
            })
            .padded();

        let boolean = just("true")
            .map_with_span(|_, span| Spanned::from((Expr::Bool(true), span)))
            .or(just("false").map_with_span(|_, span| (Expr::Bool(false), span).into()))
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

        let string = just('"')
            .ignore_then(choice((
                none_of('"')
                    .repeated()
                    .at_least(1)
                    .collect::<String>()
                    .map(|s| s.replace("\\t", "\t"))
                    .map(|s| s.replace("\\n", "\n"))
                    .map_with_span(|s, span| (Expr::String(s), span).into())
                    .then_ignore(just('"')),
                just('"').map_with_span(|_, span| (Expr::String("".to_string()), span).into()),
            )))
            .padded();

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
