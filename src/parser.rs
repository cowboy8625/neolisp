use crate::environment::Env;
use crate::error::{Error, ErrorType};
use chumsky::prelude::*;

pub type Span = std::ops::Range<usize>;
#[derive(Debug, Clone)]
pub struct Spanned<T>
where
    T: std::fmt::Display + std::fmt::Debug + Clone,
{
    pub expr: T,
    pub span: Span,
}

impl<T> PartialEq for Spanned<T>
where
    T: std::fmt::Display + std::fmt::Debug + Clone + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl<T> From<(T, Span)> for Spanned<T>
where
    T: std::fmt::Display + std::fmt::Debug + Clone,
{
    fn from((expr, span): (T, Span)) -> Self {
        Self { expr, span }
    }
}

impl<T> From<(T, &Span)> for Spanned<T>
where
    T: std::fmt::Display + std::fmt::Debug + Clone,
{
    fn from((expr, span): (T, &Span)) -> Self {
        Self {
            expr,
            span: span.clone(),
        }
    }
}

impl<T> std::fmt::Display for Spanned<T>
where
    T: std::fmt::Display + std::fmt::Debug + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}

pub type Builtin = fn(Span, &[Spanned<Expr>], &mut Env) -> Result<Spanned<Expr>, String>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    String(String),
    Symbol(String),
    Number(f64),
    List(Vec<Spanned<Expr>>),
    Builtin(Builtin, String),
    Func(Func),
    Lambda(Lambda),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::List(list) => {
                write!(f, "(")?;
                for (i, item) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
            Self::Builtin(b, _) => write!(f, "{b:?}"),
            Self::Func(func) => write!(f, "{func}"),
            Self::Lambda(func) => write!(f, "{func}"),
        }
    }
}

impl Expr {
    pub fn type_of(&self) -> String {
        match self {
            Self::Bool(..) => "Bool".to_string(),
            Self::String(..) => "String".to_string(),
            Self::Symbol(..) => "Symbol".to_string(),
            Self::Number(..) => "Number".to_string(),
            Self::List(..) => "List".to_string(),
            Self::Builtin(..) => "Builtin".to_string(),
            Self::Func(..) => "Function".to_string(),
            Self::Lambda(..) => "Lambda".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: Box<Spanned<Expr>>,
    pub params: Box<Spanned<Expr>>,
    pub body: Box<Spanned<Expr>>,
    pub help_doc: Option<Box<Spanned<Expr>>>,
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(fn {} {} {})", self.name, self.params, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub params: Box<Spanned<Expr>>,
    pub body: Box<Spanned<Expr>>,
}

impl std::fmt::Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(lambda {} {})", self.params, self.body)
    }
}

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
            .ignore_then(
                none_of('"')
                    .repeated()
                    .at_least(1)
                    .collect::<String>()
                    .map(|s| s.replace("\\t", "\t"))
                    .map(|s| s.replace("\\n", "\n"))
                    .map_with_span(|s, span| (Expr::String(s), span).into()),
            )
            .then_ignore(just('"'))
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
