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

impl Expr {
    // pub fn map<F>(&self, f: F) -> Self
    // where
    //     F: Fn(&Expr) -> Expr,
    // {
    //     match self {
    //         Self::Bool(b) => Self::Bool(b.clone()),
    //         Self::String(s) => Self::String(s.clone()),
    //         Self::Symbol(s) => Self::Symbol(s.clone()),
    //         Self::Number(n) => Self::Number(n.clone()),
    //         Self::List(list) => Self::List(list.iter().map(|item| item.map(f)).collect()),
    //     }
    // }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    pub fn is_symbol(&self) -> bool {
        matches!(self, Self::Symbol(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, Self::List(_))
    }

    pub fn first_list_item(&self) -> Option<&Spanned<Expr>> {
        match self {
            Self::List(list) => list.first(),
            _ => None,
        }
    }

    pub fn first_list_item_is<F>(&self, f: impl FnOnce(&Expr) -> bool) -> bool {
        self.first_list_item()
            .map(|spanned| f(&spanned.expr))
            .unwrap_or(false)
    }

    pub fn type_of(&self) -> String {
        match self {
            Self::Bool(_) => "Bool".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Symbol(_) => "Symbol".to_string(),
            Self::Number(_) => "Number".to_string(),
            Self::List(_) => "List".to_string(),
        }
    }
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
