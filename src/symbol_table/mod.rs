#[cfg(test)]
mod test;

use crate::ast::{Expr, Spanned};
use crate::expr_walker::{
    AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LoopExpr, VarExpr,
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
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

    pub fn get_id(&self) -> usize {
        self.scope_stack
            .iter()
            .fold(0, |acc, scope| acc + scope.len())
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

    /// Get the symbol table for a function scope after analysis
    pub fn get_function_scope(&self, function_name: &str) -> Option<&HashMap<String, Symbol>> {
        self.function_scopes.get(function_name)
    }
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    FreeVariable,
    Variable,
    Parameter,
    Function,
    // Test,
    Lambda,
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum SymbolScope {
    #[default]
    Global,
    Function,
    Free,
    // Test,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// The id of the symbol in the symbol table related to the current scope
    pub id: usize,
    pub name: String,
    pub symbol_type: SymbolType,
    pub kind: SymbolKind,
    pub scope: SymbolScope,
    pub scope_level: u32,
    pub location: Option<u32>,
}

#[derive(Debug, Default)]
pub struct SymbolTableBuilder {
    current_scope: SymbolScope,
    lambda_counter: usize,
    local_counter: usize,
    free_counter: usize,
    is_in_lambda: bool,
}

impl SymbolTableBuilder {
    pub fn build(&mut self, ast: &[Spanned<Expr>]) -> SymbolTable {
        let mut table = SymbolTable::new();
        self.walk(&mut table, ast);
        table
    }

    fn get_free_id(&mut self) -> usize {
        let id = self.free_counter;
        self.free_counter += 1;
        id
    }
}

impl AstWalker<SymbolTable> for SymbolTableBuilder {
    fn get_lambda_name(&mut self) -> String {
        let name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    fn handle_operator(&mut self, table: &mut SymbolTable, _: &str, expr: &[Spanned<Expr>]) {
        for spanned in expr.iter().skip(1) {
            self.walk_expr(table, spanned);
        }
    }

    fn handle_builtin(&mut self, table: &mut SymbolTable, _: &str, expr: &[Spanned<Expr>]) {
        for spanned in expr.iter().skip(1) {
            self.walk_expr(table, spanned);
        }
    }

    fn handle_function(&mut self, table: &mut SymbolTable, function: &FunctionExpr) {
        let scope = self.current_scope;
        self.current_scope = SymbolScope::Function;

        let Expr::Symbol(name) = &function.name.expr else {
            // TODO: REPORT ERROR
            return;
        };

        let old_local_counter = self.local_counter;
        self.local_counter = 0;
        let id = table.get_id();
        table.enter_new_scope();

        // Parameters of the function
        let Expr::List(params) = &function.params.expr else {
            // NOTE: This is unreachable code as we already checked this in AstWalker
            unreachable!();
        };

        // Handle the params
        let mut params_symbol_types = Vec::new();
        for param in params.iter() {
            let expr = &param.expr;
            let Expr::Symbol(param_name) = expr else {
                // TODO: REPORT ERROR
                return;
            };
            params_symbol_types.push(SymbolType::Dynamic);
            let symbol = Symbol {
                id: table.get_id(),
                name: param_name.clone(),
                symbol_type: SymbolType::Dynamic,
                kind: SymbolKind::Parameter,
                scope: self.current_scope,
                scope_level: table.get_scope_level() as u32,
                location: None,
            };
            table.insert(param_name.clone(), symbol);
        }

        // Body of the function
        self.walk_expr(table, function.body);

        let symbol = Symbol {
            id,
            name: name.to_string(),
            symbol_type: SymbolType::Function(params_symbol_types, Box::new(SymbolType::Dynamic)),
            kind: SymbolKind::Function,
            scope,
            scope_level: table.get_scope_level() as u32,
            location: None,
        };

        table.insert(function.name.to_string(), symbol);
        table.exit_new_scope(Some(name));

        self.current_scope = scope;
        self.local_counter = old_local_counter;
    }

    fn handle_lambda(&mut self, table: &mut SymbolTable, lambda: &LambdaExpr) {
        self.is_in_lambda = true;
        let scope = self.current_scope;
        self.current_scope = SymbolScope::Function;

        let name = self.get_lambda_name();

        let old_local_counter = self.local_counter;
        self.local_counter = 0;
        let id = table.get_id();
        table.enter_new_scope();

        // Parameters of the function
        let Expr::List(params) = &lambda.params.expr else {
            // NOTE: This is unreachable code as we already checked this in AstWalker
            unreachable!();
        };

        // Handle the params
        let mut params_symbol_types = Vec::new();
        for param in params.iter() {
            let expr = &param.expr;
            let Expr::Symbol(param_name) = expr else {
                // TODO: REPORT ERROR
                panic!("expected symbol in lambda params, got: {expr:?}");
            };
            params_symbol_types.push(SymbolType::Dynamic);
            let symbol = Symbol {
                id: table.get_id(),
                name: param_name.clone(),
                symbol_type: SymbolType::Dynamic,
                kind: SymbolKind::Parameter,
                scope: self.current_scope,
                scope_level: table.get_scope_level() as u32,
                location: None,
            };
            table.insert(param_name.clone(), symbol);
        }

        // Body of the function
        self.walk_expr(table, lambda.body);

        let symbol = Symbol {
            id,
            name: name.clone(),
            symbol_type: SymbolType::Function(params_symbol_types, Box::new(SymbolType::Dynamic)),
            kind: SymbolKind::Lambda,
            scope,
            scope_level: table.get_scope_level() as u32,
            location: None,
        };

        table.insert(name.clone(), symbol);
        table.exit_new_scope(Some(&name));
        self.current_scope = scope;
        self.local_counter = old_local_counter;
        self.is_in_lambda = false;
    }

    fn handle_call(&mut self, table: &mut SymbolTable, call: &CallExpr) {
        self.walk_expr(table, call.callee);
        for arg in call.args.iter() {
            self.walk_expr(table, arg);
        }
    }

    fn handle_if_else(&mut self, table: &mut SymbolTable, if_else: &IfElseExpr) {
        self.walk_expr(table, if_else.condition);
        self.walk_expr(table, if_else.then);
        if let Some(else_body) = if_else.otherwise {
            self.walk_expr(table, else_body);
        }
    }

    fn handle_var(&mut self, table: &mut SymbolTable, var: &VarExpr) {
        let Expr::Symbol(name) = &var.name.expr else {
            // TODO: REPORT ERROR
            panic!("var name must be a symbol");
        };

        self.walk_expr(table, var.body);

        let symbol = Symbol {
            id: table.get_id(),
            name: name.clone(),
            symbol_type: SymbolType::Dynamic,
            kind: SymbolKind::Variable,
            scope: self.current_scope,
            scope_level: table.get_scope_level() as u32,
            location: None,
        };

        table.insert(name.clone(), symbol);
    }

    fn handle_loop(&mut self, table: &mut SymbolTable, loop_expr: &LoopExpr) {
        self.walk_expr(table, loop_expr.condition);
        self.walk_expr(table, loop_expr.body);
    }

    fn handle_symbol(&mut self, table: &mut SymbolTable, name: &str) {
        let current_scope_level = table.get_scope_level() as u32;
        match table.lookup(name).cloned() {
            Some(symbol) if symbol.scope_level < current_scope_level && self.is_in_lambda => {
                if let Some(last) = table.scope_stack.last_mut() {
                    for (name, other_symbol) in last {
                        if name != &symbol.name && other_symbol.id > 0 {
                            other_symbol.id -= 1;
                        }
                    }
                }
                table.insert(
                    name.to_string(),
                    Symbol {
                        // TODO: I think table.get_id() needs to be on SymbolTableBuilder like
                        // self.get_free_id()
                        id: self.get_free_id(),
                        name: name.to_string(),
                        symbol_type: symbol.symbol_type.clone(),
                        kind: SymbolKind::FreeVariable,
                        scope: SymbolScope::Free,
                        scope_level: current_scope_level,
                        location: None,
                    },
                );
            }
            _ => {}
        }
    }

    // ----------- START NOT USED -----------
    fn handle_bool(&mut self, _: &mut SymbolTable, _: bool) {}
    fn handle_string(&mut self, _: &mut SymbolTable, _: &str) {}
    fn handle_number(&mut self, _: &mut SymbolTable, _: f64) {}
    // -----------  END NOT USED  -----------
}

// fn starts_with(elements: &[Spanned<Expr>], start: &str) -> bool {
//     if let Some(Spanned {
//         expr: Expr::Symbol(symbol),
//         ..
//     }) = elements.first()
//     {
//         symbol == start
//     } else {
//         false
//     }
// }
//
// fn is_lambda(spanned: &Spanned<Expr>) -> bool {
//     let Expr::List(elements) = &spanned.expr else {
//         return false;
//     };
//     starts_with(elements, "lambda")
// }
