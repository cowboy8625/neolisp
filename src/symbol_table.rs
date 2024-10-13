use super::{
    ast::{Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, VarExpr,
    },
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SymbolTable {
    global_scope: HashMap<String, Symbol>, // Permanent global scope
    function_scopes: HashMap<String, HashMap<String, Symbol>>, // Persistent function scopes
    scope_stack: Vec<HashMap<String, Symbol>>, // Stack for temporary scopes during AST walk
    scope_name: Vec<String>,               // Name of the current scope
}

impl SymbolTable {
    /// Enter a new scope (pushes a new scope onto the stack)
    pub fn enter_new_scope(&mut self, name: &str, kind: SymbolKind) {
        let scope = HashMap::from([(
            name.to_string(),
            Symbol {
                id: self.get_id(),
                name: name.to_string(),
                symbol_type: SymbolType::Function {
                    self_reference: false,
                    params: vec![],
                    return_type: Box::new(SymbolType::Dynamic),
                },
                kind,
                scope: SymbolScope::Global,
                scope_level: self.get_scope_level() as u32,
                location: None,
            },
        )]);
        self.scope_name.push(name.to_string());
        self.scope_stack.push(scope);
    }

    // TODO: get ride of the need to pass the name as we are storing the name already.

    /// Exit the current scope (pops the top scope off the stac)
    pub fn exit_new_scope(&mut self, current_function: Option<&str>) {
        let Some(scope) = self.scope_stack.pop() else {
            // TODO: REPORT ERROR
            panic!("No current scope for {}", current_function.unwrap_or("N/A"));
        };
        // If exiting a function, store the scope in `function_scopes`
        if let Some(func_name) = current_function {
            self.function_scopes.insert(func_name.to_string(), scope);
        }
        let name = self.scope_name.pop();
        debug_assert_eq!(name, current_function.map(ToString::to_string));
    }

    pub fn enter_scope(&mut self, name: &str) {
        let Some(scope) = self.function_scopes.get(name) else {
            // TODO: REPORT ERROR
            panic!("Can not enter scope {name} as it does not exist");
        };
        self.scope_name.push(name.to_string());
        self.scope_stack.push(scope.clone());
    }

    pub fn get_current_scope_name(&self) -> Option<&String> {
        self.scope_name.last()
    }

    pub fn exit_scope(&mut self) {
        self.scope_name.pop();
        self.scope_stack.pop();
    }

    /// Set the location of a symbol
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
            // TODO: REPORT ERROR
            panic!("location of `{name}` has not initialized {:#?}", self);
        };

        symbol.location = Some(location);
    }

    pub fn get_scope_level(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn get_id(&self) -> usize {
        self.scope_stack
            .last()
            .map(|scope| scope.len().saturating_sub(1))
            .unwrap_or(0)
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

    pub fn get_mut_current_scope(&mut self, name: &str) -> Option<&mut Symbol> {
        if self.scope_stack.is_empty() {
            return None;
        }
        let length = self.scope_stack.len() - 1;
        self.scope_stack
            .get_mut(length)
            .and_then(|scope| scope.get_mut(name))
    }

    pub fn is_recursive(&self, name: &str) -> bool {
        self.scope_stack.iter().any(|scope| {
            scope.contains_key(name)
                && scope
                    .get(name)
                    .map(|s| matches!(s.kind, SymbolKind::Function | SymbolKind::Lambda))
                    .unwrap_or(false)
        })
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
    Function {
        self_reference: bool,
        params: Vec<SymbolType>,
        return_type: Box<SymbolType>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    FreeVariable,
    Variable,
    Parameter,
    Function,
    // Test,
    Lambda,
    Let,
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum SymbolScope {
    #[default]
    Global,
    Function,
    Free,
    // Test,
    Let,
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

impl Symbol {
    pub fn is_self_reference(&self) -> bool {
        match self.symbol_type {
            SymbolType::Function {
                ref self_reference, ..
            } => *self_reference,
            _ => false,
        }
    }

    pub fn make_recursive(&mut self) {
        match self.symbol_type {
            SymbolType::Function {
                ref mut self_reference,
                ..
            } => {
                *self_reference = true;
            }
            _ => panic!("Cannot make recursive symbol {:#?}", self),
        }
    }

    pub fn params(&mut self, new_params: Vec<SymbolType>) {
        match &mut self.symbol_type {
            SymbolType::Function { ref mut params, .. } => {
                *params = new_params;
            }
            _ => panic!("Cannot make recursive symbol {:#?}", self),
        }
    }

    pub fn return_type(&mut self, new_return_type: SymbolType) {
        match &mut self.symbol_type {
            SymbolType::Function {
                ref mut return_type,
                ..
            } => {
                *return_type = Box::new(new_return_type);
            }
            _ => panic!("Cannot make recursive symbol {:#?}", self),
        }
    }
}

#[derive(Debug, Default)]
pub struct SymbolTableBuilder {
    current_scope: SymbolScope,
    lambda_counter: usize,
    local_counter: usize,
    free_counter: usize,
    is_in_lambda: bool,
    errors: Vec<Error>,
}

impl SymbolTableBuilder {
    pub fn build_from_scope(
        mut self,
        ast: &[Spanned<Expr>],
        table: &mut SymbolTable,
    ) -> std::result::Result<(), Vec<Error>> {
        self.walk(table, ast);
        if !self.errors.is_empty() {
            return Err(self.errors);
        }
        Ok(())
    }

    pub fn build(mut self, ast: &[Spanned<Expr>]) -> std::result::Result<SymbolTable, Vec<Error>> {
        let mut table = SymbolTable::default();
        self.walk(&mut table, ast);
        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        Ok(table)
    }

    fn get_free_id(&mut self) -> usize {
        let id = self.free_counter;
        self.free_counter += 1;
        id
    }
}

impl AstWalker<SymbolTable> for SymbolTableBuilder {
    fn error(&mut self, error: Error) {
        self.errors.push(error);
    }

    fn get_lambda_name(&mut self) -> String {
        let name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    fn handle_operator(&mut self, table: &mut SymbolTable, _: &str, operator_expr: &OperatorExpr) {
        for spanned in operator_expr.args.iter() {
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
        table.enter_new_scope(name, SymbolKind::Function);

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
        for spanned in function.body.iter() {
            self.walk_expr(table, spanned);
        }

        {
            let function_symbol = table
                .get_mut_current_scope(name)
                .expect("This should never fail as we just inserted it at the top of this method");

            function_symbol.params(params_symbol_types);
        }

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
        table.enter_new_scope(&name, SymbolKind::Lambda);

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
        for spanned in lambda.body.iter() {
            self.walk_expr(table, spanned);
        }

        {
            let lambda_symbol = table
                .get_mut_current_scope(&name)
                .expect("This should never fail as we just inserted it at the top of this method");

            lambda_symbol.params(params_symbol_types);
        }
        table.exit_new_scope(Some(&name));
        self.current_scope = scope;
        self.local_counter = old_local_counter;
        self.is_in_lambda = false;
    }

    fn handle_let_binding(&mut self, table: &mut SymbolTable, let_binding: &LetBindingExpr) {
        // NOTE: Let bindings are just lambdas under the hood we are going to treat them as functions
        // This may have performace cost but it's a lot simpler to implement.. I think
        self.is_in_lambda = true;
        let old_local_counter = self.local_counter;
        self.local_counter = 0;
        let scope = self.current_scope;
        self.current_scope = SymbolScope::Let;
        let names = let_binding
            .bindings
            .iter()
            .filter_map(|binding| binding.expr.first_list_item())
            .map(|item| &item.expr)
            .map(|expr| match expr {
                Expr::Symbol(name) => name.clone(),
                _ => panic!("expected symbol in let binding, got: {expr:?}"),
            })
            .collect::<Vec<_>>();
        let scope_name = format!("let_{}|{}", table.get_id(), names.join("|"));
        table.enter_new_scope(&scope_name, SymbolKind::Let);

        for spanned in let_binding.bindings.iter() {
            match &spanned.expr {
                Expr::List(list) => {
                    self.walk_expr(table, &list[1]);

                    let Expr::Symbol(binding_name) = &list[0].expr else {
                        // TODO: REPORT ERROR
                        panic!("expected symbol in let binding, got: {list:?}");
                    };
                    let symbol = Symbol {
                        id: table.get_id(),
                        name: binding_name.clone(),
                        symbol_type: SymbolType::Dynamic,
                        kind: SymbolKind::Parameter,
                        scope: self.current_scope,
                        scope_level: table.get_scope_level() as u32,
                        location: None,
                    };
                    table.insert(binding_name.clone(), symbol);
                }
                _ => {
                    // TODO: REPORT ERROR
                    panic!("expected list for let binding but found {:?}", spanned.expr);
                }
            }
        }

        self.walk_expr(table, let_binding.body);
        table.exit_new_scope(Some(&scope_name));
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

    fn handle_symbol(&mut self, table: &mut SymbolTable, name: &str, _: Span) {
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
        if table.is_recursive(name) {
            let symbol = table.get_mut_current_scope(name).unwrap();
            symbol.make_recursive();
        }
    }

    // ----------- START NOT USED -----------
    fn handle_bool(&mut self, _: &mut SymbolTable, _: bool) {}
    fn handle_string(&mut self, _: &mut SymbolTable, _: &str) {}
    fn handle_number(&mut self, _: &mut SymbolTable, _: f64) {}
    // -----------  END NOT USED  -----------
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parser;
    use chumsky::prelude::Parser;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_table_creation() {
        let src = r#"
(var add (lambda (x) (lambda (y) (+ x y))))
(fn main () (print ((add 123) 321) "\n"))
"#;
        let ast = parser().parse(src).unwrap();
        let symbol_table = SymbolTableBuilder::default().build(&ast).unwrap();
        eprintln!("{:#?}", symbol_table);
        assert_eq!(
            symbol_table.lookup("add"),
            Some(&Symbol {
                id: 0,
                name: "add".to_string(),
                symbol_type: SymbolType::Dynamic,
                kind: SymbolKind::Variable,
                scope: SymbolScope::Global,
                scope_level: 0,
                location: None,
            })
        );
        assert_eq!(
            symbol_table.lookup("main"),
            Some(&Symbol {
                id: 0,
                name: "main".to_string(),
                symbol_type: SymbolType::Function {
                    self_reference: false,
                    params: vec![],
                    return_type: Box::new(SymbolType::Dynamic)
                },
                kind: SymbolKind::Function,
                scope: SymbolScope::Global,
                scope_level: 0,
                location: None,
            })
        );
        assert_eq!(
            symbol_table.lookup("lambda_0"),
            Some(&Symbol {
                id: 0,
                name: "lambda_0".to_string(),
                symbol_type: SymbolType::Function {
                    self_reference: false,
                    params: vec![SymbolType::Dynamic],
                    return_type: Box::new(SymbolType::Dynamic)
                },
                kind: SymbolKind::Lambda,
                scope: SymbolScope::Global,
                scope_level: 0,
                location: None,
            })
        );
        assert_eq!(
            symbol_table.lookup("lambda_1"),
            Some(&Symbol {
                id: 0,
                name: "lambda_1".to_string(),
                symbol_type: SymbolType::Function {
                    self_reference: false,
                    params: vec![SymbolType::Dynamic],
                    return_type: Box::new(SymbolType::Dynamic)
                },
                kind: SymbolKind::Lambda,
                scope: SymbolScope::Global,
                scope_level: 1,
                location: None,
            })
        );
    }

    #[test]
    fn detecting_recursion() {
        let src = r#"
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fn main () (print (fib 42) "\n"))
"#;
        let ast = parser().parse(src).unwrap();
        let symbol_table = SymbolTableBuilder::default().build(&ast).unwrap();
        eprintln!("{:#?}", symbol_table);
        assert_eq!(
            symbol_table.lookup("fib"),
            Some(&Symbol {
                id: 0,
                name: "fib".to_string(),
                symbol_type: SymbolType::Function {
                    self_reference: true,
                    params: vec![SymbolType::Dynamic],
                    return_type: Box::new(SymbolType::Dynamic)
                },
                kind: SymbolKind::Function,
                scope: SymbolScope::Global,
                scope_level: 0,
                location: None,
            })
        );
    }
}
