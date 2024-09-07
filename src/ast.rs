use crate::environment::Env;

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

#[derive(Clone, PartialEq)]
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

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b:?}"),
            Self::String(s) => write!(f, "{s:?}"),
            Self::Symbol(s) => write!(f, "{s:?}"),
            Self::Number(n) => write!(f, "{n:?}"),
            Self::List(list) => {
                write!(f, "(")?;
                for (i, item) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:?}", item.expr)?;
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
    pub env: Env,
}

impl std::fmt::Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(lambda {} {})", self.params, self.body)
    }
}
