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

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Bool(bool),
    String(String),
    Symbol(String),
    Number(f64),
    List(Vec<Spanned<Expr>>),
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
        }
    }
}
