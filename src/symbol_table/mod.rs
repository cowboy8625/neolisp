use crate::ast::{Expr, Span, Spanned};
use crate::parser::parser;
use chumsky::prelude::Parser;

pub type SymbolTable = std::collections::HashMap<String, Symbol>;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    Parse(String),
    MissingFnName(Span),
    MissingFnParams(Span),
    MissingVarName(Span),
    MissingVarExpression(Span),
    ExpectedFnNameToBeSymbol(Spanned<Expr>),
    ExpectedFnParamsToBeList(Spanned<Expr>),
    ExpectedFnBodyToBeList(Spanned<Expr>),
    ExpectedVarNameToBeSymbol(Spanned<Expr>),
}

#[derive(Debug)]
pub struct Errors {
    pub errors: Vec<ErrorKind>,
}

/// Represents the type of a symbol (could be extended to include more complex types)
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    // Int,
    // Float,
    Function(Vec<SymbolType>, Box<SymbolType>), // (params, return type)
    // String,
    // Bool,
    Dynamic,
}

/// Represents the kind of a symbol (variable, function, etc.)
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Variable,
    Function,
    Test,
    Lambda,
    // Constant,
}

/// Represents the scope of a symbol
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Scope {
    #[default]
    Global,
    Function,
    Test,
    // Block(u32),
}

/// Metadata for a symbol in the lookup table
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub kind: SymbolKind,
    pub scope: Scope,
    pub location: Option<u32>,
}

#[derive(Debug, Default)]
pub struct SymbolWalker {
    table: SymbolTable,
    current_scope: Scope,
    errors: Vec<ErrorKind>,
}

impl SymbolWalker {
    pub fn walk(mut self, ast: &[Spanned<Expr>]) -> Result<SymbolTable, Errors> {
        for spanned in ast.iter() {
            self.walk_expr(spanned);
        }
        if self.errors.len() > 0 {
            return Err(Errors {
                errors: self.errors,
            });
        }
        Ok(self.table.clone())
    }

    fn walk_expr(&mut self, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Symbol(name) if name == "lambda" => {}
            Expr::Symbol(name) => self.walk_symbol(name),
            Expr::List(elements) if starts_with(elements, "var") => {
                self.walk_var(elements, &spanned.span);
                self.current_scope = Scope::Global;
            }
            Expr::List(elements) if starts_with(elements, "fn") => {
                self.walk_fn(elements, &spanned.span);
                self.current_scope = Scope::Global;
            }
            Expr::List(elements) if starts_with(elements, "test") => {
                self.walk_test(elements, &spanned.span);
                self.current_scope = Scope::Global;
            }
            Expr::List(list) if starts_with(list, "lambda") => {
                let old_scope = self.current_scope;
                self.current_scope = Scope::Function;
                self.walk_list(list);
                self.current_scope = old_scope;
            }
            Expr::List(list) => self.walk_list(list),
            _ => {}
        }
    }

    fn walk_list(&mut self, list: &[Spanned<Expr>]) {
        for item in list.iter() {
            self.walk_expr(item);
        }
    }

    fn walk_symbol(&mut self, name: &str) {
        let symbol = Symbol {
            name: name.to_string(),
            symbol_type: SymbolType::Dynamic,
            kind: SymbolKind::Variable,
            scope: self.current_scope,
            location: None,
        };
        self.table.insert(name.to_string(), symbol);
    }

    fn walk_test(&mut self, elements: &[Spanned<Expr>], span: &Span) {
        // NOTE: We know that the first element is the "test" keyword

        let scope = self.current_scope;
        self.current_scope = Scope::Test;
        // Name of the Test
        let Some(name_element) = elements.get(1) else {
            self.errors.push(ErrorKind::MissingVarName(span.clone()));
            return;
        };

        let name = match &name_element.expr {
            Expr::Symbol(name) => name,
            Expr::String(name) => name,
            _ => {
                self.errors
                    .push(ErrorKind::ExpectedVarNameToBeSymbol(name_element.clone()));
                return;
            }
        };

        for element in elements.iter().skip(2) {
            self.walk_expr(element);
        }

        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Dynamic,
            kind: SymbolKind::Test,
            scope,
            location: None,
        };

        self.table.insert(name.clone(), symbol);
    }

    fn walk_var(&mut self, elements: &[Spanned<Expr>], span: &Span) {
        // NOTE: We know that the first element is the "var" keyword

        // Name of the Variable
        let Some(name_element) = elements.get(1) else {
            self.errors.push(ErrorKind::MissingVarName(span.clone()));
            return;
        };
        let Expr::Symbol(name) = &name_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedVarNameToBeSymbol(name_element.clone()));
            return;
        };

        // Body of the Variable
        let Some(body_element) = elements.get(2) else {
            self.errors
                .push(ErrorKind::MissingVarExpression(name_element.span.clone()));
            return;
        };

        self.walk_expr(body_element);

        let kind = if let Expr::List(body) = &body_element.expr {
            if starts_with(body, "lambda") {
                SymbolKind::Lambda
            } else {
                SymbolKind::Variable
            }
        } else {
            SymbolKind::Variable
        };

        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Dynamic,
            kind,
            scope: self.current_scope,
            location: None,
        };

        self.table.insert(name.clone(), symbol);
    }

    fn walk_fn(&mut self, elements: &[Spanned<Expr>], span: &Span) {
        // NOTE: We know that the first element is the "fn" keyword

        let scope = self.current_scope;
        self.current_scope = Scope::Function;

        // Name of the function
        let Some(name_element) = elements.get(1) else {
            self.errors.push(ErrorKind::MissingFnName(span.clone()));
            return;
        };
        let Expr::Symbol(name) = &name_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnNameToBeSymbol(name_element.clone()));
            return;
        };

        // Parameters of the function
        let Some(params_element) = elements.get(2) else {
            self.errors.push(ErrorKind::MissingFnParams(span.clone()));
            return;
        };
        let Expr::List(params) = &params_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnParamsToBeList(params_element.clone()));
            return;
        };

        // Handle the params
        let mut params_symbol_types = Vec::new();
        for param in params.iter() {
            let expr = &param.expr;
            let Expr::Symbol(param_name) = expr else {
                self.errors
                    .push(ErrorKind::ExpectedFnParamsToBeList(params_element.clone()));
                return;
            };
            params_symbol_types.push(SymbolType::Dynamic);
            let symbol = Symbol {
                name: param_name.clone(),
                symbol_type: SymbolType::Dynamic,
                kind: SymbolKind::Variable,
                scope: self.current_scope,
                location: None,
            };
            self.table.insert(param_name.clone(), symbol);
        }

        // Body of the function
        let Some(body_element) = elements.get(3) else {
            self.errors.push(ErrorKind::MissingFnName(span.clone()));
            return;
        };
        let Expr::List(body) = &body_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnBodyToBeList(body_element.clone()));
            return;
        };

        // Handle the body
        for element in body.iter() {
            self.walk_expr(element);
        }
        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Function(params_symbol_types, Box::new(SymbolType::Dynamic)),
            kind: SymbolKind::Function,
            scope,
            location: None,
        };

        self.table.insert(name.clone(), symbol);
    }
}

fn starts_with(elements: &[Spanned<Expr>], start: &str) -> bool {
    if let Some(Spanned {
        expr: Expr::Symbol(symbol),
        ..
    }) = elements.first()
    {
        symbol == start
    } else {
        false
    }
}
