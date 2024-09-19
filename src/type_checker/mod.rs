#![allow(dead_code)]
#![allow(unused)]

#[cfg(test)]
mod test;

use super::{BUILTINS, KEYWORDS, OPERATORS};
use crate::ast::{Expr, Span, Spanned};
use crate::symbol_table::{SymbolTable, SymbolType};

pub fn check(symbol_table: &mut SymbolTable, ast: &[Spanned<Expr>]) -> Result<(), Vec<String>> {
    for spanned in ast {
        check_expr(symbol_table, spanned);
    }
    Ok(())
}

fn check_expr(symbol_table: &mut SymbolTable, spanned: &Spanned<Expr>) -> SymbolType {
    match &spanned.expr {
        Expr::Bool(_) => SymbolType::Bool,
        Expr::String(_) => SymbolType::String,
        Expr::Symbol(v) if KEYWORDS.contains(&v.as_str()) => check_keywords(symbol_table, v),
        Expr::Symbol(v) if OPERATORS.contains(&v.as_str()) => check_operator(symbol_table, v),
        Expr::Symbol(v) if BUILTINS.contains(&v.as_str()) => check_builtins(symbol_table, v),
        Expr::Symbol(v) => symbol_table.lookup(v.as_str()).unwrap().symbol_type,
        Expr::Number(_) => SymbolType::Float,
        Expr::List(v) => check_s_expr(symbol_table, v),
        Expr::Builtin(_, _) => unreachable!(),
        Expr::Func(_) => unreachable!(),
        Expr::Lambda(_) => unreachable!(),
    }
}

fn check_builtins(symbol_table: &mut SymbolTable, v: &str) -> SymbolType {
    todo!()
}

fn check_operator(symbol_table: &mut SymbolTable, v: &str) -> SymbolType {
    todo!()
}

fn check_keywords(symbol_table: &mut SymbolTable, v: &str) -> SymbolType {
    todo!()
}

fn check_s_expr(symbol_table: &mut SymbolTable, s_expr: &[Spanned<Expr>]) -> SymbolType {
    for spanned in s_expr {
        check_expr(symbol_table, spanned);
    }
}
