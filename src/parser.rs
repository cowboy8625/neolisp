use chumsky::prelude::*;

use crate::environment::Env;
use crate::error::{Error, ErrorType};

// TODO: make Expr remember index's for error handling
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    String(String),
    Symbol(String),
    Number(f64),
    List(Vec<Expr>),
    Builtin(fn(&[Expr], &mut Env) -> Result<Expr, String>, String),
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
            Self::Func(func) => write!(f, "{func:?}"),
            Self::Lambda(func) => write!(f, "{func:?}"),
        }
    }
}

impl Expr {
    pub fn type_of(&self) -> String {
        match self {
            Self::Bool(_) => "Bool".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Symbol(_) => "Symbol".to_string(),
            Self::Number(_) => "Number".to_string(),
            Self::List(_) => "List".to_string(),
            Self::Builtin(_, _) => "Builtin".to_string(),
            Self::Func(_) => "Function".to_string(),
            Self::Lambda(_) => "Lambda".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: Box<Expr>,
    pub params: Box<Expr>,
    pub body: Box<Expr>,
    pub help_doc: Option<Box<Expr>>,
}

impl From<Func> for Expr {
    fn from(func: Func) -> Self {
        Self::Func(func)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub params: Box<Expr>,
    pub body: Box<Expr>,
}

impl From<Lambda> for Expr {
    fn from(func: Lambda) -> Self {
        Self::Lambda(func)
    }
}

pub fn parse_expr() -> impl Parser<char, Expr, Error = Error> {
    recursive(|expr| {
        let comment = just(';').then(take_until(just('\n'))).padded();

        let int = text::int(10);

        let number = int.or(just('-').repeated().then(int).map(|(sign, num)|{
            let sign = if sign.iter().count() % 2 == 0 { "" } else { "-" };
            format!("{sign}{num}")
        }))
            .map(|s: String| Expr::Number(s.parse().unwrap()))
            .padded();

        let boolean = just("true")
            .map(|_| Expr::Bool(true))
            .or(just("false").map(|_| Expr::Bool(false)))
            .padded();

        let punctuation = one_of(r#"!$,_-./:;?+<=>#%&*@[\]{|}`^~"#);
        let letters = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
        let digit = one_of("0123456789");

        let symbol = choice((letters.clone(), punctuation.clone()))
            .then(choice((letters, punctuation, digit)).repeated())
            .padded()
            .map(|(start, end)| Expr::Symbol(format!("{start}{}", end.iter().collect::<String>())));

        let string = just('"')
            .ignore_then(
                none_of('"')
                    .repeated()
                    .at_least(1)
                    .collect::<String>()
                    .map(|s| s.replace("\\t", "\t"))
                    .map(|s| s.replace("\\n", "\n"))
                    .map(|s| Expr::String(s)),
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
            .map(|e| Expr::List(vec![Expr::Symbol("quote".to_string()), e]));

        let list = atom
            .repeated()
            .delimited_by(
                just('(').map_err(|e| Error::with_label(e, ErrorType::MissingOpeningParenthesis)),
                just(')').map_err(|e| Error::with_label(e, ErrorType::MissingClosingParenthesis)),
            )
            .padded_by(comment.repeated())
            .padded()
            .map(Expr::List);

        list.or(quoted)
    })
}

pub fn parser() -> impl Parser<char, Vec<Expr>, Error = Error> {
    parse_expr().repeated().padded().then_ignore(end())
}
