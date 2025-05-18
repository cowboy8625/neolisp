pub type Span = std::ops::Range<usize>;
pub type Ast = Vec<Spanned>;

#[derive(Debug, Clone)]
pub struct Spanned {
    pub expr: Expr,
    pub span: Span,
}

impl Spanned {
    pub fn fake(expr: Expr) -> Spanned {
        Spanned { expr, span: 0..0 }
    }
}

impl PartialEq for Spanned {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl From<(Expr, Span)> for Spanned {
    fn from((expr, span): (Expr, Span)) -> Self {
        Self { expr, span }
    }
}

impl From<(Expr, &Span)> for Spanned {
    fn from((expr, span): (Expr, &Span)) -> Self {
        Self {
            expr,
            span: span.clone(),
        }
    }
}

impl std::fmt::Display for Spanned {
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
    List(Vec<Spanned>),
}

impl Expr {
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

    pub fn first_list_item(&self) -> Option<&Spanned> {
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

fn pretty_print(expr: &Spanned, indent: usize) {
    let pad = "  ".repeat(indent);
    match &expr.expr {
        Expr::Symbol(s) => println!("{}Symbol({:?}) @ {:?}", pad, s, expr.span),
        Expr::String(s) => println!("{}String({:?}) @ {:?}", pad, s, expr.span),
        Expr::List(list) => {
            println!("{}List @ {:?}", pad, expr.span);
            for item in list {
                pretty_print(item, indent + 1);
            }
        }
        Expr::Bool(s) => println!("{}Bool(\"{}\") @ {:?}", pad, s, expr.span),
        Expr::Number(s) => println!("{}Number(\"{}\") @ {:?}", pad, s, expr.span),
    }
}

pub fn print_expr(expr: &Spanned) {
    pretty_print(expr, 0);
}

pub fn print_ast(exprs: &[Spanned]) {
    for expr in exprs.iter() {
        pretty_print(expr, 0);
    }
}

pub fn expand_quasiquote(expr: &Spanned) -> Spanned {
    use Expr::*;

    match &expr.expr {
        Symbol(_) | String(_) | Number(_) | Bool(_) => Spanned::fake(Expr::List(vec![
            Spanned::fake(Expr::Symbol("quote".to_string())),
            expr.clone(),
        ])),

        List(items) => {
            let mut parts = vec![];

            for item in items {
                match &item.expr {
                    List(inner) if !inner.is_empty() => {
                        if let Expr::Symbol(sym) = &inner[0].expr {
                            match sym.as_str() {
                                "unquote" => {
                                    if let Some(val) = inner.get(1) {
                                        parts.push(val.clone());
                                        continue;
                                    }
                                }
                                "unquote-splicing" => {
                                    if let Some(val) = inner.get(1) {
                                        parts.push(Spanned::fake(Expr::List(vec![
                                            Spanned::fake(Expr::Symbol("splice".to_string())),
                                            val.clone(),
                                        ])));
                                        continue;
                                    }
                                }
                                _ => {}
                            }
                        }
                    }

                    _ => {}
                }

                let quoted = Spanned::fake(Expr::List(vec![
                    Spanned::fake(Expr::Symbol("quote".to_string())),
                    item.clone(),
                ]));
                parts.push(quoted);
            }

            let mut full = vec![Spanned::fake(Expr::Symbol("list".to_string()))];
            full.extend(parts);

            Spanned::fake(Expr::List(full))
        }
    }
}

pub fn macro_expand(expr: &Spanned) -> Spanned {
    match &expr.expr {
        Expr::List(items) if !items.is_empty() => {
            if let Expr::Symbol(sym) = &items[0].expr {
                if sym == "quasiquote" {
                    if let Some(inner) = items.get(1) {
                        return expand_quasiquote(inner);
                    }
                }
            }
            let new_items = items.iter().map(macro_expand).collect();
            Spanned::fake(Expr::List(new_items))
        }

        _ => expr.clone(),
    }
}

#[test]
fn test_expand_quasiquote() {
    use chumsky::prelude::*;
    use pretty_assertions::assert_eq;
    let src = r#"
    (fn main ()
    (var n 0)
    `(1 ,n)
)"#;

    let ast = crate::parser::parser().parse(src).unwrap();
    let mut new_ast = vec![];
    for spanned in ast.iter() {
        let expanded = macro_expand(spanned);
        new_ast.push(expanded);
    }
    crate::ast::print_ast(&ast);
    println!("-----------------");
    crate::ast::print_ast(&new_ast);
    assert!(false);
}
