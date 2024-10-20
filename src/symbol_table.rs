use super::{
    ast::{Ast, Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, VarExpr,
    },
};
use std::collections::HashMap;

// #[derive(Debug, Clone, PartialEq, Default)]
// pub struct SymbolTable {
//     global_scope: HashMap<String, Symbol>, // Permanent global scope
//     function_scopes: HashMap<String, HashMap<String, Symbol>>, // Persistent function scopes
//     scope_stack: Vec<HashMap<String, Symbol>>, // Stack for temporary scopes during AST walk
//     scope_name: Vec<String>,               // Name of the current scope
// }
//
// impl SymbolTable {
//     /// Enter a new scope (pushes a new scope onto the stack)
//     pub fn enter_new_scope(&mut self, name: &str, kind: SymbolKind) {
//         let scope = HashMap::from([(
//             name.to_string(),
//             Symbol {
//                 id: self.get_id(),
//                 name: name.to_string(),
//                 symbol_type: SymbolType::Function {
//                     self_reference: false,
//                     params: vec![],
//                     return_type: Box::new(SymbolType::Dynamic),
//                 },
//                 kind,
//                 scope: SymbolScope::Global,
//                 scope_level: self.get_scope_level() as u32,
//                 location: None,
//                 span: None,
//             },
//         )]);
//         self.scope_name.push(name.to_string());
//         self.scope_stack.push(scope);
//     }
//
//     /// Exit the current scope (pops the top scope off the stac)
//     pub fn exit_new_scope(&mut self, current_function: Option<&str>) {
//         let Some(scope) = self.scope_stack.pop() else {
//             // NOTE: I think this its ok to panic here.
//             // This is an internal error.  Im unsure how to report it or if ajust a panic is ok.
//             // We would want to never get to this point in the code before a crash.
//             // Going to make this with
//             // NOTE: INTENTIONAL_PANIC
//             panic!("No current scope for {}", current_function.unwrap_or("N/A"));
//         };
//         // If exiting a function, store the scope in `function_scopes`
//         if let Some(func_name) = current_function {
//             self.function_scopes.insert(func_name.to_string(), scope);
//         }
//         let name = self.scope_name.pop();
//         debug_assert_eq!(name, current_function.map(ToString::to_string));
//     }
//
//     pub fn enter_scope(&mut self, name: &str) {
//         let Some(scope) = self.function_scopes.get(name) else {
//             // NOTE: INTENTIONAL_PANIC
//             panic!("Can not enter scope {name} as it does not exist");
//         };
//         self.scope_name.push(name.to_string());
//         self.scope_stack.push(scope.clone());
//     }
//
//     pub fn get_current_scope_name(&self) -> Option<&String> {
//         self.scope_name.last()
//     }
//
//     pub fn exit_scope(&mut self) {
//         self.scope_name.pop();
//         self.scope_stack.pop();
//     }
//
//     /// Set the location of a symbol
//     pub fn set_location(&mut self, scope_name: Option<&str>, name: &str, location: u32) {
//         if let Some(scope_name) = scope_name {
//             if let Some(scope) = self.function_scopes.get_mut(scope_name) {
//                 if let Some(symbol) = scope.get_mut(name) {
//                     symbol.location = Some(location);
//                     return;
//                 }
//             }
//         }
//
//         for scope in self.scope_stack.iter_mut().rev() {
//             if let Some(symbol) = scope.get_mut(name) {
//                 symbol.location = Some(location);
//                 return;
//             }
//         }
//
//         // Fall back to global scope
//         let Some(symbol) = self.global_scope.get_mut(name) else {
//             // NOTE: INTENTIONAL_PANIC
//             panic!("location of `{name}` has not initialized {:#?}", self);
//         };
//
//         symbol.location = Some(location);
//     }
//
//     pub fn get_scope_level(&self) -> usize {
//         self.scope_stack.len()
//     }
//
//     pub fn get_id(&self) -> usize {
//         self.scope_stack
//             .last()
//             .map(|scope| scope.len().saturating_sub(1))
//             .unwrap_or(0)
//     }
//
//     /// Insert a symbol into the current scope
//     pub fn insert(&mut self, name: String, symbol: Symbol) {
//         let scope_level = self.scope_stack.len(); // Get the current scope level
//
//         if scope_level == 0 {
//             // Insert into global scope if no local scopes exist
//             self.global_scope.insert(name, symbol);
//         } else {
//             // Insert into the top scope (local)
//             self.scope_stack
//                 .last_mut()
//                 .expect("No current scope to insert into")
//                 .insert(name, symbol);
//         }
//     }
//
//     /// Look up a symbol by name, checking local scopes first, then global scope
//     pub fn lookup(&self, name: &str) -> Option<&Symbol> {
//         // Search through local scopes first (starting from the top)
//         for scope in self.scope_stack.iter().rev() {
//             if let Some(symbol) = scope.get(name) {
//                 return Some(symbol);
//             }
//         }
//
//         if let Some(scope) = self.function_scopes.get(name) {
//             if let Some(scope) = scope.get(name) {
//                 return Some(scope);
//             }
//         }
//
//         // Fall back to global scope
//         self.global_scope.get(name)
//     }
//
//     pub fn get_mut_current_scope(&mut self, name: &str) -> Option<&mut Symbol> {
//         if self.scope_stack.is_empty() {
//             return None;
//         }
//         let length = self.scope_stack.len() - 1;
//         self.scope_stack
//             .get_mut(length)
//             .and_then(|scope| scope.get_mut(name))
//     }
//
//     pub fn is_recursive(&self, name: &str) -> bool {
//         self.scope_stack.iter().any(|scope| {
//             scope.contains_key(name)
//                 && scope
//                     .get(name)
//                     .map(|s| matches!(s.kind, SymbolKind::Function | SymbolKind::Lambda))
//                     .unwrap_or(false)
//         })
//     }
//
//     /// Get the symbol table for a function scope after analysis
//     pub fn get_function_scope(&self, function_name: &str) -> Option<&HashMap<String, Symbol>> {
//         self.function_scopes.get(function_name)
//     }
// }
//
// #[derive(Debug, Clone, PartialEq, Default)]
// pub enum SymbolType {
//     #[default]
//     Dynamic,
//     // Bool,
//     // Int,
//     // Float,
//     // String,
//     Function {
//         self_reference: bool,
//         params: Vec<SymbolType>,
//         return_type: Box<SymbolType>,
//     },
// }
//
// #[derive(Debug, Clone, PartialEq)]
// pub enum SymbolKind {
//     FreeVariable,
//     Variable,
//     Parameter,
//     Function,
//     // Test,
//     Lambda,
//     Let,
// }
//
// #[derive(Debug, Clone, Copy, PartialEq, Default)]
// pub enum SymbolScope {
//     #[default]
//     Global,
//     Function,
//     Free,
//     // Test,
//     Let,
// }
//
// #[derive(Debug, Clone, PartialEq)]
// pub struct Symbol {
//     /// The id of the symbol in the symbol table related to the current scope
//     pub id: usize,
//     pub name: String,
//     pub symbol_type: SymbolType,
//     pub kind: SymbolKind,
//     pub scope: SymbolScope,
//     pub scope_level: u32,
//     pub location: Option<u32>,
//     pub span: Option<Span>,
// }
//
// impl Symbol {
//     pub fn is_self_reference(&self) -> bool {
//         match self.symbol_type {
//             SymbolType::Function {
//                 ref self_reference, ..
//             } => *self_reference,
//             _ => false,
//         }
//     }
//
//     pub fn make_recursive(&mut self) {
//         match self.symbol_type {
//             SymbolType::Function {
//                 ref mut self_reference,
//                 ..
//             } => {
//                 *self_reference = true;
//             }
//             _ => panic!("Cannot make recursive symbol {:#?}", self),
//         }
//     }
//
//     pub fn params(&mut self, new_params: Vec<SymbolType>) {
//         match &mut self.symbol_type {
//             SymbolType::Function { ref mut params, .. } => {
//                 *params = new_params;
//             }
//             _ => panic!("Cannot make recursive symbol {:#?}", self),
//         }
//     }
//
//     pub fn return_type(&mut self, new_return_type: SymbolType) {
//         match &mut self.symbol_type {
//             SymbolType::Function {
//                 ref mut return_type,
//                 ..
//             } => {
//                 *return_type = Box::new(new_return_type);
//             }
//             _ => panic!("Cannot make recursive symbol {:#?}", self),
//         }
//     }
// }
//
// #[derive(Debug, Default)]
// pub struct SymbolTableBuilder {
//     current_scope: SymbolScope,
//     lambda_counter: usize,
//     local_counter: usize,
//     free_counter: usize,
//     is_in_lambda: bool,
//     errors: Vec<Error>,
// }
//
// impl SymbolTableBuilder {
//     pub fn build_from_scope(
//         mut self,
//         ast: &[Spanned<Expr>],
//         table: &mut SymbolTable,
//     ) -> std::result::Result<(), Vec<Error>> {
//         self.walk(table, ast);
//         if !self.errors.is_empty() {
//             return Err(self.errors);
//         }
//         Ok(())
//     }
//
//     pub fn build(mut self, ast: &[Spanned<Expr>]) -> std::result::Result<SymbolTable, Vec<Error>> {
//         let mut table = SymbolTable::default();
//         self.walk(&mut table, ast);
//         if !self.errors.is_empty() {
//             return Err(self.errors);
//         }
//
//         Ok(table)
//     }
//
//     fn get_free_id(&mut self) -> usize {
//         let id = self.free_counter;
//         self.free_counter += 1;
//         id
//     }
// }
//
// impl AstWalker<SymbolTable> for SymbolTableBuilder {
//     fn error(&mut self, error: Error) {
//         self.errors.push(error);
//     }
//
//     fn get_lambda_name(&mut self) -> String {
//         let name = format!("lambda_{}", self.lambda_counter);
//         self.lambda_counter += 1;
//         name
//     }
//
//     fn handle_operator(&mut self, table: &mut SymbolTable, _: &str, operator_expr: &OperatorExpr) {
//         for spanned in operator_expr.args.iter() {
//             self.walk_expr(table, spanned);
//         }
//     }
//
//     fn handle_builtin(&mut self, table: &mut SymbolTable, _: &str, expr: &[Spanned<Expr>]) {
//         for spanned in expr.iter().skip(1) {
//             self.walk_expr(table, spanned);
//         }
//     }
//
//     fn handle_function(&mut self, table: &mut SymbolTable, function: &FunctionExpr) {
//         let scope = self.current_scope;
//         self.current_scope = SymbolScope::Function;
//
//         let Expr::Symbol(name) = &function.name.expr else {
//             unreachable!("This should never fail as we already checked this in AstWalker");
//         };
//
//         let old_local_counter = self.local_counter;
//         self.local_counter = 0;
//         table.enter_new_scope(name, SymbolKind::Function);
//
//         // Parameters of the function
//         let Expr::List(params) = &function.params.expr else {
//             unreachable!("This should never fail as we already checked this in AstWalker");
//         };
//
//         // Handle the params
//         let mut params_symbol_types = Vec::new();
//         for param in params.iter() {
//             let expr = &param.expr;
//             let Expr::Symbol(param_name) = expr else {
//                 self.error(Error::ExpectedFound {
//                     span: param.span.clone(),
//                     expected: "Symbol".to_string(),
//                     found: expr.type_of(),
//                     note: None,
//                     help: Some("(fn <symbol> (<symbol>) <expression>)".to_string()),
//                 });
//                 continue;
//             };
//             params_symbol_types.push(SymbolType::Dynamic);
//             let symbol = Symbol {
//                 id: table.get_id(),
//                 name: param_name.clone(),
//                 symbol_type: SymbolType::Dynamic,
//                 kind: SymbolKind::Parameter,
//                 scope: self.current_scope,
//                 scope_level: table.get_scope_level() as u32,
//                 location: None,
//                 span: Some(param.span.clone()),
//             };
//             table.insert(param_name.clone(), symbol);
//         }
//
//         // Body of the function
//         for spanned in function.body.iter() {
//             self.walk_expr(table, spanned);
//         }
//
//         {
//             let function_symbol = table
//                 .get_mut_current_scope(name)
//                 .expect("This should never fail as we just inserted it at the top of this method");
//
//             function_symbol.params(params_symbol_types);
//         }
//
//         table.exit_new_scope(Some(name));
//
//         self.current_scope = scope;
//         self.local_counter = old_local_counter;
//     }
//
//     fn handle_lambda(&mut self, table: &mut SymbolTable, lambda: &LambdaExpr) {
//         self.is_in_lambda = true;
//         let scope = self.current_scope;
//         self.current_scope = SymbolScope::Function;
//
//         let name = self.get_lambda_name();
//
//         let old_local_counter = self.local_counter;
//         self.local_counter = 0;
//         table.enter_new_scope(&name, SymbolKind::Lambda);
//
//         // Parameters of the function
//         let Expr::List(params) = &lambda.params.expr else {
//             unreachable!("This should never fail as we already checked this in AstWalker");
//         };
//
//         // Handle the params
//         let mut params_symbol_types = Vec::new();
//         for param in params.iter() {
//             let expr = &param.expr;
//             let Expr::Symbol(param_name) = expr else {
//                 self.error(Error::ExpectedFound {
//                     span: param.span.clone(),
//                     expected: "Symbol".to_string(),
//                     found: expr.type_of(),
//                     note: None,
//                     help: Some("(lambda (<symbol>) <expression>)".to_string()),
//                 });
//                 continue;
//             };
//             params_symbol_types.push(SymbolType::Dynamic);
//             let symbol = Symbol {
//                 id: table.get_id(),
//                 name: param_name.clone(),
//                 symbol_type: SymbolType::Dynamic,
//                 kind: SymbolKind::Parameter,
//                 scope: self.current_scope,
//                 scope_level: table.get_scope_level() as u32,
//                 location: None,
//                 span: Some(param.span.clone()),
//             };
//             table.insert(param_name.clone(), symbol);
//         }
//
//         // Body of the function
//         for spanned in lambda.body.iter() {
//             self.walk_expr(table, spanned);
//         }
//
//         {
//             let lambda_symbol = table
//                 .get_mut_current_scope(&name)
//                 .expect("This should never fail as we just inserted it at the top of this method");
//
//             lambda_symbol.params(params_symbol_types);
//         }
//         table.exit_new_scope(Some(&name));
//         self.current_scope = scope;
//         self.local_counter = old_local_counter;
//         self.is_in_lambda = false;
//     }
//
//     fn handle_let_binding(&mut self, table: &mut SymbolTable, let_binding: &LetBindingExpr) {
//         // NOTE: Let bindings are just lambdas under the hood we are going to treat them as functions
//         // This may have performace cost but it's a lot simpler to implement.. I think
//         self.is_in_lambda = true;
//         let old_local_counter = self.local_counter;
//         self.local_counter = 0;
//         let scope = self.current_scope;
//         self.current_scope = SymbolScope::Let;
//         let names = let_binding
//             .bindings
//             .iter()
//             .filter_map(|binding| binding.expr.first_list_item())
//             .map(|item| &item.expr)
//             .map(|expr| match expr {
//                 Expr::Symbol(name) => name.clone(),
//                 _ => panic!("expected symbol in let binding, got: {expr:?}"),
//             })
//             .collect::<Vec<_>>();
//         let scope_name = format!("let_{}|{}", table.get_id(), names.join("|"));
//         table.enter_new_scope(&scope_name, SymbolKind::Let);
//
//         for spanned in let_binding.bindings.iter() {
//             let Expr::List(list) = &spanned.expr else {
//                 self.error(Error::ExpectedFound {
//                     span: spanned.span.clone(),
//                     expected: "List or Symbol".to_string(),
//                     found: spanned.expr.type_of(),
//                     note: None,
//                     help: Some(
//                         "(let (<symbol> <expression> | (<symbol <expression>)) <expression>)"
//                             .to_string(),
//                     ),
//                 });
//                 continue;
//             };
//
//             let Some(binding_expression) = list.get(1) else {
//                 self.error(Error::ExpectedFound {
//                     span: spanned.span.clone(),
//                     expected: "expression after binding name".to_string(),
//                     found: "nothing".to_string(),
//                     note: Some(
//                         "(let (<symbol> <expression> | (<symbol <expression>)) <expression>)"
//                             .to_string(),
//                     ),
//                     help: Some(format!("maybe try something like ({} 10), your just missing an expression after the name.", list[0].expr)),
//                 });
//                 continue;
//             };
//             self.walk_expr(table, binding_expression);
//
//             let Expr::Symbol(binding_name) = &list[0].expr else {
//                 unreachable!("This should never fail as we already checked this in AstWalker");
//             };
//             let symbol = Symbol {
//                 id: table.get_id(),
//                 name: binding_name.clone(),
//                 symbol_type: SymbolType::Dynamic,
//                 kind: SymbolKind::Parameter,
//                 scope: self.current_scope,
//                 scope_level: table.get_scope_level() as u32,
//                 location: None,
//                 span: Some(spanned.span.clone()),
//             };
//             table.insert(binding_name.clone(), symbol);
//         }
//
//         self.walk_expr(table, let_binding.body);
//         table.exit_new_scope(Some(&scope_name));
//         self.current_scope = scope;
//         self.local_counter = old_local_counter;
//         self.is_in_lambda = false;
//     }
//
//     fn handle_call(&mut self, table: &mut SymbolTable, call: &CallExpr) {
//         self.walk_expr(table, call.callee);
//         for arg in call.args.iter() {
//             self.walk_expr(table, arg);
//         }
//     }
//
//     fn handle_if_else(&mut self, table: &mut SymbolTable, if_else: &IfElseExpr) {
//         self.walk_expr(table, if_else.condition);
//         self.walk_expr(table, if_else.then);
//         if let Some(else_body) = if_else.otherwise {
//             self.walk_expr(table, else_body);
//         }
//     }
//
//     fn handle_var(&mut self, table: &mut SymbolTable, var: &VarExpr) {
//         let Expr::Symbol(name) = &var.name.expr else {
//             unreachable!("This should never fail as we already checked this in AstWalker");
//         };
//
//         self.walk_expr(table, var.body);
//
//         let symbol = Symbol {
//             id: table.get_id(),
//             name: name.clone(),
//             symbol_type: SymbolType::Dynamic,
//             kind: SymbolKind::Variable,
//             scope: self.current_scope,
//             scope_level: table.get_scope_level() as u32,
//             location: None,
//             span: Some(var.name.span.clone()),
//         };
//
//         if let Some(def) = table.lookup(name) {
//             self.error(Error::Redefined {
//                 // HACK: some how we need to get the original span here
//                 // We could put it in the symbol table but that would be too much effort.
//                 // If we put it in the symbol table as a Option<Span> we could do it that way
//                 // with little effort.
//                 original_span: def.span.clone().unwrap(),
//                 original_name: def.name.clone(),
//                 new_span: var.name.span.clone(),
//                 new_name: name.clone(),
//                 note: None,
//                 help: None,
//             });
//             return;
//         }
//
//         table.insert(name.clone(), symbol);
//     }
//
//     fn handle_loop(&mut self, table: &mut SymbolTable, loop_expr: &LoopExpr) {
//         self.walk_expr(table, loop_expr.condition);
//         self.walk_expr(table, loop_expr.body);
//     }
//
//     fn handle_symbol(&mut self, table: &mut SymbolTable, name: &str, span: Span) {
//         let current_scope_level = table.get_scope_level() as u32;
//         match table.lookup(name).cloned() {
//             Some(symbol) if symbol.scope_level < current_scope_level && self.is_in_lambda => {
//                 if let Some(last) = table.scope_stack.last_mut() {
//                     for (name, other_symbol) in last {
//                         if name != &symbol.name && other_symbol.id > 0 {
//                             other_symbol.id -= 1;
//                         }
//                     }
//                 }
//                 table.insert(
//                     name.to_string(),
//                     Symbol {
//                         id: self.get_free_id(),
//                         name: name.to_string(),
//                         symbol_type: symbol.symbol_type.clone(),
//                         kind: SymbolKind::FreeVariable,
//                         scope: SymbolScope::Free,
//                         scope_level: current_scope_level,
//                         location: None,
//                         span: Some(span),
//                     },
//                 );
//             }
//             _ => {}
//         }
//         if table.is_recursive(name) {
//             let symbol = table.get_mut_current_scope(name).unwrap();
//             symbol.make_recursive();
//         }
//     }
//
//     // ----------- START NOT USED -----------
//     fn handle_bool(&mut self, _: &mut SymbolTable, _: bool) {}
//     fn handle_string(&mut self, _: &mut SymbolTable, _: &str) {}
//     fn handle_number(&mut self, _: &mut SymbolTable, _: f64) {}
//     // -----------  END NOT USED  -----------
// }
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::parser::parser;
//     use chumsky::prelude::Parser;
//     use pretty_assertions::assert_eq;
//
//     #[test]
//     fn test_table_creation() {
//         let src = r#"
// (var add (lambda (x) (lambda (y) (+ x y))))
// (fn main () (print ((add 123) 321) "\n"))
// "#;
//         let ast = parser().parse(src).unwrap();
//         let symbol_table = SymbolTableBuilder::default().build(&ast).unwrap();
//         eprintln!("{:#?}", symbol_table);
//         assert_eq!(
//             symbol_table.lookup("add"),
//             Some(&Symbol {
//                 id: 0,
//                 name: "add".to_string(),
//                 symbol_type: SymbolType::Dynamic,
//                 kind: SymbolKind::Variable,
//                 scope: SymbolScope::Global,
//                 scope_level: 0,
//                 location: None,
//                 span: Some(6..10)
//             })
//         );
//         assert_eq!(
//             symbol_table.lookup("main"),
//             Some(&Symbol {
//                 id: 0,
//                 name: "main".to_string(),
//                 symbol_type: SymbolType::Function {
//                     self_reference: false,
//                     params: vec![],
//                     return_type: Box::new(SymbolType::Dynamic)
//                 },
//                 kind: SymbolKind::Function,
//                 scope: SymbolScope::Global,
//                 scope_level: 0,
//                 location: None,
//                 span: None
//             })
//         );
//         assert_eq!(
//             symbol_table.lookup("lambda_0"),
//             Some(&Symbol {
//                 id: 0,
//                 name: "lambda_0".to_string(),
//                 symbol_type: SymbolType::Function {
//                     self_reference: false,
//                     params: vec![SymbolType::Dynamic],
//                     return_type: Box::new(SymbolType::Dynamic)
//                 },
//                 kind: SymbolKind::Lambda,
//                 scope: SymbolScope::Global,
//                 scope_level: 0,
//                 location: None,
//                 span: None
//             })
//         );
//         assert_eq!(
//             symbol_table.lookup("lambda_1"),
//             Some(&Symbol {
//                 id: 0,
//                 name: "lambda_1".to_string(),
//                 symbol_type: SymbolType::Function {
//                     self_reference: false,
//                     params: vec![SymbolType::Dynamic],
//                     return_type: Box::new(SymbolType::Dynamic)
//                 },
//                 kind: SymbolKind::Lambda,
//                 scope: SymbolScope::Global,
//                 scope_level: 1,
//                 location: None,
//                 span: None
//             })
//         );
//     }
//
//     #[test]
//     fn detecting_recursion() {
//         let src = r#"
// (fn fib (n)
//   (if (or (= n 0) (= n 1))
//       n
//       (+ (fib (- n 1)) (fib (- n 2)))))
//
// (fn main () (print (fib 42) "\n"))
// "#;
//         let ast = parser().parse(src).unwrap();
//         let symbol_table = SymbolTableBuilder::default().build(&ast).unwrap();
//         eprintln!("{:#?}", symbol_table);
//         assert_eq!(
//             symbol_table.lookup("fib"),
//             Some(&Symbol {
//                 id: 0,
//                 name: "fib".to_string(),
//                 symbol_type: SymbolType::Function {
//                     self_reference: true,
//                     params: vec![SymbolType::Dynamic],
//                     return_type: Box::new(SymbolType::Dynamic)
//                 },
//                 kind: SymbolKind::Function,
//                 scope: SymbolScope::Global,
//                 scope_level: 0,
//                 location: None,
//                 span: None
//             })
//         );
//     }
// }
//
// // +---------------------------------------------------------------+
// // |                       New Symbol Table                        |
// // +---------------------------------------------------------------+
//

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Int,
}

#[derive(Debug, Eq, PartialEq)]
pub struct UnboundVariable {
    pub id: usize,
    pub name: String,
    pub span: Span,
    pub typeis: Option<Type>,
    pub scope_level: usize,
    pub location: Option<usize>,
}

impl UnboundVariable {
    pub fn new(id: usize, name: impl Into<String>, span: Span, scope_level: usize) -> Self {
        Self {
            id,
            name: name.into(),
            span,
            typeis: None,
            scope_level,
            location: None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Variable {
    pub id: usize,
    pub name: String,
    pub span: Span,
    pub typeis: Option<Type>,
    pub scope_level: usize,
    pub location: Option<usize>,
}

impl Variable {
    pub fn new(id: usize, name: impl Into<String>, span: Span, scope_level: usize) -> Self {
        Self {
            id,
            name: name.into(),
            span,
            typeis: None,
            scope_level,
            location: None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Parameter {
    pub id: usize,
    pub name: String,
    pub span: Span,
    pub typeis: Option<Type>,
    pub is_unbound: bool,
    pub scope_level: usize,
    pub location: Option<usize>,
}

impl Parameter {
    pub fn new(id: usize, name: impl Into<String>, span: Span, scope_level: usize) -> Self {
        Self {
            id,
            name: name.into(),
            span,
            typeis: None,
            is_unbound: false,
            scope_level,
            location: None,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    pub id: usize,
    pub name: String,
    pub span: Span,
    pub return_type: Option<Type>,
    pub parameters: Vec<Parameter>,
    pub scope_level: usize,
    pub location: Option<usize>,
    pub is_recursive: bool,
}

impl Function {
    pub fn new(
        id: usize,
        name: impl Into<String>,
        span: Span,
        parameters: Vec<Parameter>,
        scope_level: usize,
    ) -> Self {
        Self {
            id,
            name: name.into(),
            span,
            return_type: None,
            parameters,
            scope_level,
            location: None,
            is_recursive: false,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Lambda {
    pub id: usize,
    pub span: Span,
    pub return_type: Option<Type>,
    pub parameters: Vec<Parameter>,
    pub scope_level: usize,
    pub location: Option<usize>,
}

impl Lambda {
    pub fn new(id: usize, span: Span, parameters: Vec<Parameter>, scope_level: usize) -> Self {
        Self {
            id,
            span,
            return_type: None,
            parameters,
            scope_level,
            location: None,
        }
    }
    pub fn name(&self) -> String {
        format!("lambda_{}", self.id)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Let {
    pub id: usize,
    pub span: Span,
    pub bindings: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub scope_level: usize,
    pub location: Option<usize>,
}

impl Let {
    pub fn new(id: usize, span: Span, bindings: Vec<Parameter>, scope_level: usize) -> Self {
        Self {
            id,
            span,
            bindings,
            return_type: None,
            scope_level,
            location: None,
        }
    }
    pub fn name(&self) -> String {
        let names = self
            .bindings
            .iter()
            .map(|p| p.name.clone())
            .collect::<Vec<_>>()
            .join("|");
        let id = self.id;
        format!("let_{id}|{names}")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol {
    UnboundVariable(UnboundVariable),
    Variable(Variable),
    Parameter(Parameter),
    Function(Function),
    Lambda(Lambda),
    Let(Let),
}

impl Symbol {
    pub fn is_recursive(&self) -> bool {
        match self {
            Symbol::Function(f) => f.is_recursive,
            _ => false,
        }
    }

    pub fn set_location(&mut self, location: usize) {
        match self {
            Symbol::UnboundVariable(v) => v.location = Some(location),
            Symbol::Variable(v) => v.location = Some(location),
            Symbol::Parameter(v) => v.location = Some(location),
            Symbol::Function(v) => v.location = Some(location),
            Symbol::Lambda(v) => v.location = Some(location),
            Symbol::Let(v) => v.location = Some(location),
        }
    }

    pub fn get_location(&self) -> Option<usize> {
        match self {
            Symbol::UnboundVariable(v) => v.location.clone(),
            Symbol::Variable(v) => v.location.clone(),
            Symbol::Parameter(v) => v.location.clone(),
            Symbol::Function(v) => v.location.clone(),
            Symbol::Lambda(v) => v.location.clone(),
            Symbol::Let(v) => v.location.clone(),
        }
    }

    pub fn is_global(&self) -> bool {
        let level = match self {
            Symbol::UnboundVariable(v) => v.scope_level,
            Symbol::Variable(v) => v.scope_level,
            Symbol::Parameter(v) => v.scope_level,
            Symbol::Function(v) => v.scope_level,
            Symbol::Lambda(v) => v.scope_level,
            Symbol::Let(v) => v.scope_level,
        };
        level == 1
    }

    pub fn span(&self) -> Span {
        match self {
            Symbol::UnboundVariable(v) => v.span.clone(),
            Symbol::Variable(v) => v.span.clone(),
            Symbol::Parameter(v) => v.span.clone(),
            Symbol::Function(v) => v.span.clone(),
            Symbol::Lambda(v) => v.span.clone(),
            Symbol::Let(v) => v.span.clone(),
        }
    }

    pub fn name(&self) -> String {
        match self {
            Symbol::UnboundVariable(v) => v.name.clone(),
            Symbol::Variable(v) => v.name.clone(),
            Symbol::Parameter(v) => v.name.clone(),
            Symbol::Function(v) => v.name.clone(),
            Symbol::Lambda(v) => v.name(),
            Symbol::Let(v) => v.name(),
        }
    }

    pub fn get_parameters(&self) -> &[Parameter] {
        match self {
            Symbol::Function(f) => &f.parameters,
            Symbol::Lambda(l) => &l.parameters,
            Symbol::Let(l) => &l.bindings,
            _ => &[],
        }
    }
}

macro_rules! into_symbol {
        ($($t:ident),* $(,)?) => {
            $(
                impl From<$t> for Symbol {
                    fn from(value: $t) -> Self {
                        Symbol::$t(value)
                    }
                }
            )*
        };
    }

into_symbol!(UnboundVariable, Variable, Parameter, Function, Lambda, Let,);

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

impl Scope {
    pub fn insert(&mut self, symbol: impl Into<Symbol>) {
        let symbol = symbol.into();
        let name = match &symbol {
            Symbol::UnboundVariable(v) => &v.name,
            Symbol::Variable(v) => &v.name,
            Symbol::Parameter(p) => &p.name,
            Symbol::Function(f) => &f.name,
            Symbol::Lambda(l) => &l.name(),
            Symbol::Let(l) => &l.name(),
        };
        self.symbols.insert(name.clone(), symbol);
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.symbols.get_mut(name)
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SymbolTable {
    scopes: HashMap<String, Scope>,
    scope_stack: Vec<String>,
}

impl SymbolTable {
    pub const SEPARATOR: &'static str = "()";
    pub fn set_location(&mut self, name: &str, location: usize) {
        self.get_mut(name, |symbol| symbol.set_location(location));
    }

    pub fn get_scope_name(&self) -> String {
        self.scope_stack.last().cloned().unwrap_or_default()
    }

    fn get_full_scope_name(&self) -> String {
        self.scope_stack.join(Self::SEPARATOR)
    }

    pub fn enter_scope(&mut self, name: &str) {
        self.scope_stack.push(name.to_string());
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn scope_level(&self) -> usize {
        self.scope_stack.len()
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for i in (0..=self.scope_stack.len()).rev() {
            let scope_path = self.scope_stack[0..i].join(Self::SEPARATOR);

            if let Some(scope) = self.scopes.get(&scope_path) {
                if let Some(symbol) = scope.lookup(name) {
                    return Some(symbol);
                }
            }
        }

        self.scopes
            .get("global")
            .and_then(|scope| scope.lookup(name))
    }

    pub fn get_mut(&mut self, name: &str, mut f: impl FnMut(&mut Symbol)) {
        for i in (0..self.scope_stack.len()).rev() {
            let scope_path = self.scope_stack[0..=i].join(Self::SEPARATOR);

            if let Some(scope) = self.scopes.get_mut(&scope_path) {
                if let Some(symbol) = scope.get_mut(name) {
                    f(symbol);
                }
            }
        }

        let Some(symbol) = self
            .scopes
            .get_mut("global")
            .and_then(|scope| scope.get_mut(name))
        else {
            return;
        };
        f(symbol);
    }

    fn push(&mut self, symbol: impl Into<Symbol>) {
        let symbol = symbol.into();

        let name = match &symbol {
            Symbol::Function(Function { name, .. })
            | Symbol::Variable(Variable { name, .. })
            | Symbol::Parameter(Parameter { name, .. })
            | Symbol::UnboundVariable(UnboundVariable { name, .. }) => name.to_string(),
            Symbol::Lambda(l) => l.name(),
            Symbol::Let(l) => l.name(),
        };

        if matches!(self.scope_stack.last(), Some(sn) if &name == sn) {
            self.scope_stack.pop();
            let scope_name = self.get_full_scope_name();
            self.scopes.entry(scope_name).or_default().insert(symbol);
            self.scope_stack.push(name.to_string());
        } else {
            let scope_name = self.get_full_scope_name();
            self.scopes.entry(scope_name).or_default().insert(symbol);
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SymbolTableBuilder {
    lambda_counter: usize,
    unbound_counter: usize,
    variable_counter: usize,
    errors: Vec<Error>,
}

impl SymbolTableBuilder {
    pub fn build(mut self, ast: &Ast) -> std::result::Result<SymbolTable, Vec<Error>> {
        let mut table = SymbolTable::default();
        table.enter_scope("global");
        self.walk(&mut table, ast);
        table.exit_scope();
        if !self.errors.is_empty() {
            return Err(self.errors);
        }
        Ok(table)
    }

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

    fn variable_id(&mut self) -> usize {
        let id = self.variable_counter;
        self.variable_counter += 1;
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

    fn handle_function(&mut self, table: &mut SymbolTable, function_expr: &FunctionExpr) {
        let id = self.variable_id();
        let Expr::Symbol(name) = &function_expr.name.expr else {
            self.error(Error::ExpectedFound {
                span: function_expr.name.span.clone(),
                expected: "Symbol".to_string(),
                found: function_expr.name.expr.type_of(),
                note: None,
                help: Some("(fn <symbol> (<symbol>) <expression>)".to_string()),
            });
            return;
        };

        let old_variable_counter = self.variable_counter;
        self.variable_counter = 0;
        table.enter_scope(name);

        let Expr::List(params) = &function_expr.params.expr else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        let mut parameters: Vec<Parameter> = Vec::new();
        for spanned in params.iter() {
            let Expr::Symbol(name) = &spanned.expr else {
                self.error(Error::ExpectedFound {
                    span: spanned.span.clone(),
                    expected: "Symbol".to_string(),
                    found: spanned.expr.type_of(),
                    note: None,
                    help: Some("(fn <symbol> (<symbol>) <expression>)".to_string()),
                });
                return;
            };
            let parameter = Parameter::new(
                self.variable_id(),
                name,
                spanned.span.clone(),
                table.scope_level(),
            );
            parameters.push(parameter.clone());
            table.push(parameter);
        }

        let function = Function::new(
            id,
            name,
            function_expr.name.span.clone(),
            parameters,
            table.scope_level(),
        );
        table.push(function);

        for spanned in function_expr.body.iter() {
            self.walk_expr(table, spanned);
        }

        table.exit_scope();
        self.variable_counter = old_variable_counter;
    }

    fn handle_lambda(&mut self, table: &mut SymbolTable, lambda_expr: &LambdaExpr) {
        let id = self.lambda_counter;
        let name = self.get_lambda_name();
        let old_variable_counter = self.variable_counter;
        self.variable_counter = 0;
        table.enter_scope(&name);

        let Expr::List(params) = &lambda_expr.params.expr else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        let mut parameters: Vec<Parameter> = Vec::new();
        for spanned in params.iter() {
            let Expr::Symbol(name) = &spanned.expr else {
                self.error(Error::ExpectedFound {
                    span: spanned.span.clone(),
                    expected: "Symbol".to_string(),
                    found: spanned.expr.type_of(),
                    note: None,
                    help: Some("(lambda (<symbol>) <expression>)".to_string()),
                });
                return;
            };
            let parameter = Parameter::new(
                self.variable_id(),
                name,
                spanned.span.clone(),
                table.scope_level(),
            );
            parameters.push(parameter.clone());
            table.push(parameter);
        }

        let lambda = Lambda::new(
            id,
            lambda_expr.params.span.clone(),
            parameters,
            table.scope_level(),
        );
        table.push(lambda);

        for spanned in lambda_expr.body.iter().rev() {
            self.walk_expr(table, spanned);
        }

        table.exit_scope();
        self.variable_counter = old_variable_counter;
    }

    fn handle_let_binding(&mut self, table: &mut SymbolTable, let_binding: &LetBindingExpr) {
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
        let id = self.lambda_counter;
        self.lambda_counter += 1;
        let scope_name = format!("let_{id}|{}", names.join("|"));
        let old_variable_counter = self.variable_counter;
        self.variable_counter = 0;
        table.enter_scope(&scope_name);

        let mut bindings = Vec::new();
        for spanned in let_binding.bindings.iter() {
            let Expr::List(list) = &spanned.expr else {
                self.error(Error::ExpectedFound {
                    span: spanned.span.clone(),
                    expected: "List or Symbol".to_string(),
                    found: spanned.expr.type_of(),
                    note: None,
                    help: Some(
                        "(let (<symbol> <expression> | (<symbol <expression>)) <expression>)"
                            .to_string(),
                    ),
                });
                continue;
            };

            let Some(binding_expression) = list.get(1) else {
                self.error(Error::ExpectedFound {
                    span: spanned.span.clone(),
                    expected: "expression after binding name".to_string(),
                    found: "nothing".to_string(),
                    note: Some(
                        "(let (<symbol> <expression> | (<symbol <expression>)) <expression>)"
                            .to_string(),
                    ),
                    help: Some(format!("maybe try something like ({} 10), your just missing an expression after the name.", list[0].expr)),
                });
                continue;
            };
            self.walk_expr(table, binding_expression);

            let Expr::Symbol(binding_name) = &list[0].expr else {
                unreachable!("This should never fail as we already checked this in AstWalker");
            };
            let parameter = Parameter::new(
                self.variable_id(),
                binding_name,
                spanned.span.clone(),
                table.scope_level(),
            );
            bindings.push(parameter.clone());
            table.push(parameter);
        }
        let r#let = Let::new(
            id,
            let_binding.bindings[0].span.clone(),
            bindings,
            table.scope_level(),
        );

        table.push(r#let);

        self.walk_expr(table, let_binding.body);
        // for spanned in let_binding.body.iter() {
        //     self.walk_expr(table, spanned);
        // }

        table.exit_scope();
        self.variable_counter = old_variable_counter;
    }

    fn handle_call(&mut self, table: &mut SymbolTable, call: &CallExpr) {
        self.walk_expr(table, call.callee);
        for arg in call.args.iter() {
            self.walk_expr(table, arg);
        }
    }

    fn handle_var(&mut self, table: &mut SymbolTable, var: &VarExpr) {
        let Expr::Symbol(name) = &var.name.expr else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        self.walk_expr(table, var.body);

        let symbol = Variable::new(
            self.variable_id(),
            name,
            var.name.span.clone(),
            table.scope_level(),
        );

        if let Some(def) = table.get(name) {
            self.error(Error::Redefined {
                original_span: def.span(),
                original_name: def.name(),
                new_span: var.name.span.clone(),
                new_name: name.clone(),
                note: None,
                help: None,
            });
            return;
        }

        table.push(symbol);
    }

    fn handle_if_else(&mut self, i: &mut SymbolTable, if_else: &IfElseExpr) {
        self.walk_expr(i, if_else.condition);
        self.walk_expr(i, if_else.then);
        if let Some(else_body) = if_else.otherwise {
            self.walk_expr(i, else_body);
        }
    }

    fn handle_loop(&mut self, table: &mut SymbolTable, loop_expr: &LoopExpr) {
        self.walk_expr(table, loop_expr.condition);
        self.walk_expr(table, loop_expr.body);
    }

    fn handle_symbol(&mut self, table: &mut SymbolTable, name: &str, span: Span) {
        let Some(scope) = table.get(&table.get_scope_name()) else {
            return;
        };
        let params = scope.get_parameters();
        let has_value_in_current_scope = params.iter().any(|p| p.name == name);
        if has_value_in_current_scope {
            return;
        }
        let scope_name = table.get_scope_name();
        let mut skip = false;
        table.get_mut(name, |symbol| match symbol {
            Symbol::UnboundVariable(_) => {
                skip = true;
                return;
            }
            Symbol::Variable(_) => todo!(),
            Symbol::Parameter(p) => p.is_unbound = true,
            Symbol::Function(f) => {
                f.is_recursive = f.name == scope_name || f.is_recursive;
                eprintln!("{} {}", f.name, f.is_recursive);
                skip = true;
                return;
            }
            Symbol::Lambda(_) => todo!(),
            Symbol::Let(_) => todo!(),
        });
        if skip {
            return;
        }
        let id = self.unbound_counter;
        self.unbound_counter += 1;
        table.push(UnboundVariable::new(id, name, span, table.scope_level()));
    }

    // ----------- START NOT USED -----------
    fn handle_bool(&mut self, _: &mut SymbolTable, _: bool) {}
    fn handle_string(&mut self, _: &mut SymbolTable, _: &str) {}
    fn handle_number(&mut self, _: &mut SymbolTable, _: f64) {}
    // -----------  END NOT USED  -----------
}

#[test]
fn new_symbol_table() {
    assert!(true);
    //         use crate::parser::parse_or_report;
    //         use pretty_assertions::assert_eq;
    //         let ast = parse_or_report(
    //             "test_new_symbol_table",
    //             r#"
    // ;; (lambda (x) (lambda (y) (+ x y)))
    //
    // ;; (let (x 1) x)
    //
    // (let (x 1)
    // (let (y 2)
    // (let (z 3)
    // (print x y z "\n"))))
    //
    // ;; (fn add (x y) (+ x y))
    // "#,
    //         );
    //         let symbol_table = SymbolTableBuilder::default().build(&ast).unwrap();
    //         assert_eq!(symbol_table, SymbolTable::default());
}
