#![allow(dead_code)]
#![allow(unused)]
#[cfg(test)]
mod test;

use super::{BUILTINS, KEYWORDS, OPERATORS};
use crate::ast::{Expr, Span, Spanned};
use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable {
    global_scope: HashMap<String, Symbol>, // Permanent global scope
    function_scopes: HashMap<String, HashMap<String, Symbol>>, // Persistent function scopes
    scope_stack: Vec<HashMap<String, Symbol>>, // Stack for temporary scopes during AST walk
}

impl SymbolTable {
    /// Create a new symbol table with an empty global scope
    pub fn new() -> Self {
        SymbolTable {
            global_scope: HashMap::new(),
            function_scopes: HashMap::new(),
            scope_stack: Vec::new(),
        }
    }

    /// Enter a new scope (pushes a new scope onto the stack)
    pub fn enter_new_scope(&mut self) {
        self.scope_stack.push(HashMap::new());
    }

    /// Exit the current scope (pops the top scope off the stack)
    pub fn exit_new_scope(&mut self, current_function: Option<&str>) {
        let Some(scope) = self.scope_stack.pop() else {
            panic!("No current scope for {}", current_function.unwrap_or("N/A"));
        };
        // If exiting a function, store the scope in `function_scopes`
        if let Some(func_name) = current_function {
            self.function_scopes.insert(func_name.to_string(), scope);
        }
    }

    pub fn enter_scope(&mut self, name: &str) {
        let Some(scope) = self.function_scopes.get(name) else {
            panic!("Can not enter scope {name} as it does not exist");
        };
        self.scope_stack.push(scope.clone());
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn set_location(&mut self, scope_name: Option<&str>, name: &str, location: u32) {
        if let Some(scope_name) = scope_name {
            if let Some(scope) = self.function_scopes.get_mut(scope_name) {
                if let Some(symbol) = scope.get_mut(name) {
                    symbol.location = Some(location);
                    return;
                }
            }
        }

        for scope in self.scope_stack.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                symbol.location = Some(location);
                return;
            }
        }

        // Fall back to global scope
        let Some(symbol) = self.global_scope.get_mut(name) else {
            panic!("location of `{name}` has not initialized {:#?}", self);
        };

        symbol.location = Some(location);
    }

    pub fn get_scope_level(&self) -> usize {
        self.scope_stack.len()
    }

    /// Insert a symbol into the current scope
    pub fn insert(&mut self, name: String, symbol: Symbol) {
        let scope_level = self.scope_stack.len(); // Get the current scope level

        if scope_level == 0 {
            // Insert into global scope if no local scopes exist
            self.global_scope.insert(name, symbol);
        } else {
            // Insert into the top scope (local)
            self.scope_stack
                .last_mut()
                .expect("No current scope to insert into")
                .insert(name, symbol);
        }
    }

    /// Look up a symbol by name, checking local scopes first, then global scope
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // Search through local scopes first (starting from the top)
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }

        if let Some(scope) = self.function_scopes.get(name) {
            if let Some(scope) = scope.get(name) {
                return Some(scope);
            }
        }

        // Fall back to global scope
        self.global_scope.get(name)
    }

    /// Lookup symbols within a specific function scope
    pub fn lookup_in_function(&self, function_name: &str, symbol_name: &str) -> Option<&Symbol> {
        if let Some(func_scope) = self.function_scopes.get(function_name) {
            func_scope.get(symbol_name)
        } else {
            None
        }
    }

    /// List all global symbols (for later stages)
    pub fn get_global_symbols(&self) -> &HashMap<String, Symbol> {
        &self.global_scope
    }

    /// Get the symbol table for a function scope after analysis
    pub fn get_function_scope(&self, function_name: &str) -> Option<&HashMap<String, Symbol>> {
        self.function_scopes.get(function_name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    Parse(String),
    MissingFnName(Span),
    MissingFnParams(Span),
    MissingFnBody(Span),
    MissingVarName(Span),
    MissingVarExpression(Span),
    ExpectedFnNameToBeSymbol(Spanned<Expr>),
    ExpectedFnParamsToBeList(Spanned<Expr>),
    ExpectedFnBodyToBeList(Spanned<Expr>),
    ExpectedVarNameToBeSymbol(Spanned<Expr>),
    Unimplemented(Spanned<Expr>),
}

#[derive(Debug)]
pub struct Errors {
    pub errors: Vec<ErrorKind>,
}

/// Represents the type of a symbol (could be extended to include more complex types)
#[derive(Debug, Clone, PartialEq, Default)]
pub enum SymbolType {
    #[default]
    Dynamic,
    // Bool,
    // Int,
    // Float,
    // String,
    Function(Vec<SymbolType>, Box<SymbolType>), // (params, return type)
}

/// Represents the kind of a symbol (variable, function, etc.)
#[derive(Debug, Clone, PartialEq, Default)]
pub enum SymbolKind {
    #[default]
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
#[derive(Debug, Clone, Default)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub kind: SymbolKind,
    pub scope: Scope,
    pub scope_level: u32,
    pub location: Option<u32>,
}

#[derive(Debug, Default)]
pub struct SymbolWalker {
    current_scope: Scope,
    lambda_counter: u32,
    is_in_lambda: bool,
    errors: Vec<ErrorKind>,
}

impl SymbolWalker {
    pub fn walk(mut self, ast: &[Spanned<Expr>]) -> Result<SymbolTable, Errors> {
        let mut table = SymbolTable::new();

        for spanned in ast.iter() {
            self.walk_expr(&mut table, spanned);
        }
        if self.errors.len() > 0 {
            return Err(Errors {
                errors: self.errors,
            });
        }

        Ok(table)
    }

    fn walk_expr(&mut self, table: &mut SymbolTable, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Symbol(name) if KEYWORDS.contains(&name.as_str()) => {}
            Expr::Symbol(name) if OPERATORS.contains(&name.as_str()) => {}
            Expr::Symbol(name) if BUILTINS.contains(&name.as_str()) => {}
            // Expr::Symbol(name) if self.is_in_lambda => {
            //     let symbol = Symbol {
            //         name: name.clone(),
            //         symbol_type: SymbolType::Dynamic,
            //         kind: SymbolKind::Variable,
            //         scope: self.current_scope,
            //         ..Default::default()
            //     };
            //     table.insert(name.clone(), symbol);
            // }
            Expr::Symbol(name) => {
                if table.lookup(name.as_str()).is_some() {
                    return;
                }
                self.errors.push(ErrorKind::Unimplemented(spanned.clone()));
            }
            Expr::List(elements) if starts_with(elements, "var") => {
                self.walk_var(table, elements, &spanned.span);
                self.current_scope = Scope::Global;
            }
            Expr::List(elements) if starts_with(elements, "fn") => {
                self.walk_fn(table, elements, &spanned.span);
                self.current_scope = Scope::Global;
            }
            Expr::List(elements) if starts_with(elements, "test") => {
                self.walk_test(table, elements, &spanned.span);
                self.current_scope = Scope::Global;
            }
            Expr::List(list) if starts_with(list, "lambda") => {
                let old_scope = self.current_scope;
                self.walk_lambda(table, list, &spanned.span);
                self.current_scope = old_scope;
            }
            Expr::List(list) => self.walk_list(table, list),
            _ => {}
        }
    }

    fn walk_list(&mut self, table: &mut SymbolTable, list: &[Spanned<Expr>]) {
        for item in list.iter() {
            self.walk_expr(table, item);
        }
    }

    fn walk_test(&mut self, table: &mut SymbolTable, elements: &[Spanned<Expr>], span: &Span) {
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
            self.walk_expr(table, element);
        }

        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Dynamic,
            kind: SymbolKind::Test,
            scope,
            ..Default::default()
        };

        table.insert(name.clone(), symbol);
    }

    fn walk_lambda(&mut self, table: &mut SymbolTable, elements: &[Spanned<Expr>], span: &Span) {
        let scope = self.current_scope;
        self.current_scope = Scope::Function;

        let name = format!("lambda_{}", self.lambda_counter);
        eprintln!("symbol table: {}", name);
        self.lambda_counter += 1;

        table.enter_new_scope();

        // Parameters of the function
        let Some(params_element) = elements.get(1) else {
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
                ..Default::default()
            };
            table.insert(param_name.clone(), symbol);
        }

        // Body of the function
        let Some(body_element) = elements.get(2) else {
            self.errors
                .push(ErrorKind::MissingFnBody(params_element.span.clone()));
            return;
        };
        let Expr::List(_) = &body_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnBodyToBeList(body_element.clone()));
            return;
        };

        // Handle the body
        self.walk_expr(table, body_element);

        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Function(params_symbol_types, Box::new(SymbolType::Dynamic)),
            kind: SymbolKind::Lambda,
            scope,
            scope_level: table.get_scope_level() as u32,
            location: None,
        };

        table.insert(name.clone(), symbol);
        table.exit_new_scope(Some(&name));
    }

    fn walk_var(&mut self, table: &mut SymbolTable, elements: &[Spanned<Expr>], span: &Span) {
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

        let kind = if let Expr::List(body) = &body_element.expr {
            if starts_with(body, "lambda") {
                self.is_in_lambda = true;
                SymbolKind::Lambda
            } else {
                SymbolKind::Variable
            }
        } else {
            SymbolKind::Variable
        };

        self.walk_expr(table, body_element);

        if self.is_in_lambda {
            self.is_in_lambda = false;
        }

        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Dynamic,
            kind,
            scope: self.current_scope,
            ..Default::default()
        };

        table.insert(name.clone(), symbol);
    }

    fn walk_fn(&mut self, table: &mut SymbolTable, elements: &[Spanned<Expr>], span: &Span) {
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

        table.enter_new_scope();

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
                ..Default::default()
            };
            table.insert(param_name.clone(), symbol);
        }

        // Body of the function
        let Some(body_element) = elements.get(3) else {
            self.errors
                .push(ErrorKind::MissingFnBody(params_element.span.clone()));
            return;
        };
        let Expr::List(body) = &body_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnBodyToBeList(body_element.clone()));
            return;
        };

        // Handle the body
        for element in body.iter() {
            self.walk_expr(table, element);
        }

        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Function(params_symbol_types, Box::new(SymbolType::Dynamic)),
            kind: SymbolKind::Function,
            scope,
            scope_level: table.get_scope_level() as u32,
            location: None,
        };

        table.insert(name.clone(), symbol);
        table.exit_new_scope(Some(&name))
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
