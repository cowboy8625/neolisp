use num_derive::{FromPrimitive, ToPrimitive};

use crate::{
    compiler::CompilerOptions,
    expr_walker::{FfiBindExpr, SetExpr},
};

use super::{
    ast::{Ast, Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, QuoteExpr, TestExpr, VarExpr,
    },
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq, FromPrimitive, ToPrimitive)]
#[repr(u8)]
pub enum Type {
    Nil,
    Bool,
    Int,
    String,
}

impl TryFrom<&str> for Type {
    type Error = ();
    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "int" => Ok(Type::Int),
            "string" => Ok(Type::String),
            "bool" => Ok(Type::Bool),
            "nil" => Ok(Type::Nil),
            _ => Err(()),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Type::Nil => "nil",
            Type::Bool => "bool",
            Type::Int => "int",
            Type::String => "string",
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Test {
    pub id: usize,
    pub span: Span,
    pub name: String,
    pub scope_level: usize,
    pub location: Option<std::ops::Range<usize>>,
}

impl Test {
    pub fn new(id: usize, name: impl Into<String>, span: Span, scope_level: usize) -> Self {
        Self {
            id,
            name: name.into(),
            span,
            scope_level,
            location: None,
        }
    }

    pub fn name(&self) -> String {
        self.name.to_string()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnboundVariable {
    pub id: usize,
    pub name: String,
    pub span: Span,
    pub typeis: Option<Type>,
    pub scope_level: usize,
    pub location: Option<std::ops::Range<usize>>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Variable {
    pub id: usize,
    pub name: String,
    pub span: Span,
    pub typeis: Option<Type>,
    pub scope_level: usize,
    pub location: Option<std::ops::Range<usize>>,
    pub is_recursive: bool,
}

impl Variable {
    pub fn new(
        id: usize,
        name: impl Into<String>,
        span: Span,
        scope_level: usize,
        is_recursive: bool,
    ) -> Self {
        Self {
            id,
            name: name.into(),
            span,
            typeis: None,
            scope_level,
            location: None,
            is_recursive,
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
    pub location: Option<std::ops::Range<usize>>,
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

    pub fn with_type(mut self, typeis: Type) -> Self {
        self.typeis = Some(typeis);
        self
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub id: usize,
    pub name: String,
    pub span: Span,
    pub return_type: Option<Type>,
    pub parameters: Vec<Parameter>,
    pub scope_level: usize,
    pub location: Option<std::ops::Range<usize>>,
    pub is_recursive: bool,
}

impl Function {
    pub fn new(
        id: usize,
        name: impl Into<String>,
        span: Span,
        parameters: Vec<Parameter>,
        scope_level: usize,
        is_recursive: bool,
    ) -> Self {
        Self {
            id,
            name: name.into(),
            span,
            return_type: None,
            parameters,
            scope_level,
            location: None,
            is_recursive,
        }
    }

    pub fn with_return_type(mut self, return_type: Type) -> Self {
        self.return_type = Some(return_type);
        self
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Lambda {
    pub id: usize,
    pub span: Span,
    pub return_type: Option<Type>,
    pub parameters: Vec<Parameter>,
    pub scope_level: usize,
    pub location: Option<std::ops::Range<usize>>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Let {
    pub id: usize,
    pub span: Span,
    pub bindings: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub scope_level: usize,
    pub location: Option<std::ops::Range<usize>>,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Symbol {
    UnboundVariable(UnboundVariable),
    Variable(Variable),
    Parameter(Parameter),
    Function(Function),
    Lambda(Lambda),
    Let(Let),
    Test(Test),
}

impl Symbol {
    pub fn id(&self) -> usize {
        match self {
            Symbol::UnboundVariable(v) => v.id,
            Symbol::Variable(v) => v.id,
            Symbol::Parameter(v) => v.id,
            Symbol::Function(v) => v.id,
            Symbol::Lambda(v) => v.id,
            Symbol::Let(v) => v.id,
            Symbol::Test(v) => v.id,
        }
    }

    pub fn is_recursive(&self) -> bool {
        match self {
            Symbol::Function(f) => f.is_recursive,
            Symbol::Variable(v) => v.is_recursive,
            _ => false,
        }
    }

    pub fn set_location(&mut self, location: std::ops::Range<usize>) {
        match self {
            Symbol::UnboundVariable(v) => v.location = Some(location),
            Symbol::Variable(v) => v.location = Some(location),
            Symbol::Parameter(v) => v.location = Some(location),
            Symbol::Function(v) => v.location = Some(location),
            Symbol::Lambda(v) => v.location = Some(location),
            Symbol::Let(v) => v.location = Some(location),
            Symbol::Test(v) => v.location = Some(location),
        }
    }

    pub fn get_location(&self) -> Option<std::ops::Range<usize>> {
        match self {
            Symbol::UnboundVariable(v) => v.location.clone(),
            Symbol::Variable(v) => v.location.clone(),
            Symbol::Parameter(v) => v.location.clone(),
            Symbol::Function(v) => v.location.clone(),
            Symbol::Lambda(v) => v.location.clone(),
            Symbol::Let(v) => v.location.clone(),
            Symbol::Test(v) => v.location.clone(),
        }
    }

    pub fn is_global(&self) -> bool {
        let level = match self {
            Symbol::UnboundVariable(v) => v.scope_level,
            Symbol::Variable(v) => v.scope_level,
            Symbol::Parameter(v) => v.scope_level,
            Symbol::Function(_) => return true,
            Symbol::Lambda(v) => v.scope_level,
            Symbol::Let(v) => v.scope_level,
            Symbol::Test(_) => return true,
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
            Symbol::Test(v) => v.span.clone(),
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
            Symbol::Test(v) => v.name(),
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

into_symbol!(
    UnboundVariable,
    Variable,
    Parameter,
    Function,
    Lambda,
    Let,
    Test
);

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

impl Scope {
    pub fn insert(&mut self, symbol: impl Into<Symbol>) {
        let symbol = symbol.into();
        let name = symbol.name();
        self.symbols.insert(name.clone(), symbol);
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        self.symbols.get_mut(name)
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    pub scopes: HashMap<String, Scope>,
    pub scope_stack: Vec<String>,
}

impl SymbolTable {
    pub const SEPARATOR: &'static str = "()";
    pub fn set_location(&mut self, name: &str, location: std::ops::Range<usize>) {
        self.get_mut(name, |symbol| symbol.set_location(location.clone()));
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

        let name = symbol.name();

        if self.scope_stack.last().map_or(false, |sn| &name == sn) {
            self.scope_stack.pop();
            let scope_name = self.get_full_scope_name();
            self.scopes.entry(scope_name).or_default().insert(symbol);
            self.scope_stack.push(name.to_string());
        } else {
            let scope_name = self.get_full_scope_name();
            self.scopes.entry(scope_name).or_default().insert(symbol);
        }
    }

    pub fn extend(&mut self, other: SymbolTable) {
        self.scopes.extend(other.scopes);
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SymbolTableBuilder {
    lambda_counter: usize,
    unbound_counter: usize,
    variable_counter: usize,
    errors: Vec<Error>,
    options: CompilerOptions,
}

impl SymbolTableBuilder {
    pub fn with_options(mut self, options: CompilerOptions) -> Self {
        self.options = options;
        self
    }

    pub fn build(
        mut self,
        ast: &Ast,
        table: &mut SymbolTable,
    ) -> std::result::Result<(), Vec<Error>> {
        table.enter_scope("global");
        self.walk(table, ast);
        table.exit_scope();
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

    fn handle_builtin(
        &mut self,
        table: &mut SymbolTable,
        _: &str,
        _: Span,
        expr: &[Spanned<Expr>],
    ) {
        for spanned in expr.iter().skip(1) {
            self.walk_expr(table, spanned);
        }
    }

    fn handle_test(&mut self, table: &mut SymbolTable, test_expr: &TestExpr) {
        if !self.options.test {
            return;
        }
        let id = self.variable_id();
        let name = format!("test(){}", test_expr.name);
        let old_variable_counter = self.variable_counter;
        self.variable_counter = 0;
        table.enter_scope(&name);

        for spanned in test_expr.body.iter() {
            self.walk_expr(table, spanned);
        }

        table.exit_scope();
        self.variable_counter = old_variable_counter;

        let test = Test::new(id, name, test_expr.span.clone(), table.scope_level());
        table.push(test);
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

        let is_recursive = function_expr
            .body
            .iter()
            .any(|spanned| is_recursive(name, spanned));
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
            is_recursive,
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
        // let names = let_binding
        //     .bindings
        //     .iter()
        //     .filter_map(|binding| binding.expr.first_list_item())
        //     .map(|item| &item.expr)
        //     .map(|expr| match expr {
        //         Expr::Symbol(name) => name.clone(),
        //         _ => panic!("expected symbol in let binding, got: {expr:?}"),
        //     })
        //     .collect::<Vec<_>>();
        let id = self.lambda_counter;
        self.lambda_counter += 1;
        // let scope_name = format!("let_{id}|{}", names.join("|"));
        // table.enter_scope(&scope_name);

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

        for spanned in let_binding.body.iter() {
            self.walk_expr(table, spanned);
        }

        // table.exit_scope();
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

        let is_recursive = is_recursive(name, var.body);
        self.walk_expr(table, var.body);

        let symbol = Variable::new(
            self.variable_id(),
            name,
            var.name.span.clone(),
            table.scope_level(),
            is_recursive,
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

    fn handle_set(&mut self, table: &mut SymbolTable, var: &SetExpr) {
        self.walk_expr(table, var.name);
        self.walk_expr(table, var.body);
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
        for spanned in loop_expr.body.iter() {
            self.walk_expr(table, spanned);
        }
    }

    fn handle_symbol(&mut self, table: &mut SymbolTable, name: &str, span: Span) {
        let Some(scope) = table.get(&table.get_scope_name()) else {
            return;
        };
        let params = scope.get_parameters();
        let has_value_in_current_scope = params.iter().any(|p| p.name == name);
        if has_value_in_current_scope || scope.is_global() {
            return;
        }
        let scope_name = table.get_scope_name();
        let mut skip = false;
        table.get_mut(name, |symbol| match symbol {
            Symbol::UnboundVariable(_) => {
                skip = true;
            }
            Symbol::Variable(_) => {
                skip = true;
            }
            Symbol::Parameter(p) => {
                p.is_unbound = true;
            }
            Symbol::Function(f) => {
                f.is_recursive = f.name == scope_name || f.is_recursive;
                skip = true;
            }
            Symbol::Lambda(_) => todo!("Lambdas are not supported yet"),
            Symbol::Let(_) => todo!("Lets are not supported yet"),
            Symbol::Test(_) => todo!("Tests are not supported yet"),
        });
        if skip {
            return;
        }
        table.push(UnboundVariable::new(
            self.unbound_counter,
            name,
            span,
            table.scope_level(),
        ));
        self.unbound_counter += 1;
    }

    fn handle_ffi_bind(&mut self, table: &mut SymbolTable, ffi_bind_expr: &FfiBindExpr) {
        let id = self.variable_id();
        let Expr::Symbol(name) = &ffi_bind_expr.fn_symbol.1.expr else {
            self.error(Error::ExpectedFound {
                span: ffi_bind_expr.fn_symbol.1.span.clone(),
                expected: "Symbol".to_string(),
                found: ffi_bind_expr.fn_symbol.1.expr.type_of(),
                note: None,
                help: None,
            });
            return;
        };

        let Expr::List(params) = &ffi_bind_expr.args.1.expr else {
            self.error(Error::ExpectedFound {
                span: ffi_bind_expr.args.1.span.clone(),
                expected: "Symbol".to_string(),
                found: ffi_bind_expr.fn_symbol.1.expr.type_of(),
                note: None,
                help: None,
            });
            return;
        };

        let old_variable_counter = self.variable_counter;
        self.variable_counter = 0;
        let mut parameters = Vec::new();

        for spanned in params.iter() {
            let Expr::Symbol(name) = &spanned.expr else {
                self.error(Error::ExpectedFound {
                    span: spanned.span.clone(),
                    expected: "Symbol".to_string(),
                    found: spanned.expr.type_of(),
                    note: None,
                    help: Some("(ffi-bind .. :args (<symbol>*) ..)".to_string()),
                });
                return;
            };

            let Ok(typeis) = Type::try_from(name.as_str()) else {
                self.error(Error::ExpectedFound {
                    span: spanned.span.clone(),
                    expected: "Type".to_string(),
                    found: spanned.expr.type_of(),
                    note: Some("int, float, string, bool".to_string()),
                    help: None,
                });
                return;
            };
            let parameter = Parameter::new(
                self.variable_id(),
                // HACK: This is the type name not the function param name.
                // Maybe it should be `:args ((name int) (name float) (name string) (name bool))`
                // currently it is `:args (int float string bool)`
                name,
                spanned.span.clone(),
                table.scope_level(),
            )
            .with_type(typeis);
            parameters.push(parameter.clone());
            table.push(parameter);
        }

        let return_type = match &ffi_bind_expr.return_type.1.expr {
            Expr::Symbol(name) => {
                let Ok(typeis) = Type::try_from(name.as_str()) else {
                    self.error(Error::ExpectedFound {
                        span: ffi_bind_expr.return_type.1.span.clone(),
                        expected: "Type".to_string(),
                        found: ffi_bind_expr.return_type.1.expr.type_of(),
                        note: None,
                        help: None,
                    });
                    return;
                };
                typeis
            }
            Expr::Nil => Type::Nil,
            t => panic!("expected symbol {t:?}"),
        };

        let function = Function::new(
            id,
            name,
            ffi_bind_expr.fn_symbol.1.span.clone(),
            parameters,
            table.scope_level(),
            false,
        )
        .with_return_type(return_type);
        self.variable_counter = old_variable_counter;
        table.push(function);
    }

    // ----------- START NOT USED -----------
    fn handle_nil(&mut self, _: &mut SymbolTable) {}
    fn handle_bool(&mut self, _: &mut SymbolTable, _: bool) {}
    fn handle_string(&mut self, _: &mut SymbolTable, _: &str) {}
    fn handle_number(&mut self, _: &mut SymbolTable, _: f64) {}
    fn handle_quote(&mut self, _: &mut SymbolTable, _: &QuoteExpr) {}
    fn handle_keyword(&mut self, _: &mut SymbolTable, _: &str, _: Span) {}
    // -----------  END NOT USED  -----------
}

fn is_recursive(lambda_name: &str, body: &Spanned<Expr>) -> bool {
    match &body.expr {
        Expr::Symbol(name) => name == lambda_name,
        Expr::List(list) => list
            .iter()
            .any(|sub_expr| is_recursive(lambda_name, sub_expr)),
        _ => false,
    }
}
