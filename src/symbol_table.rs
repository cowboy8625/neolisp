use super::{
    ast::{Ast, Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, QuoteExpr, VarExpr,
    },
};
use std::collections::HashMap;

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
    pub fn id(&self) -> usize {
        match self {
            Symbol::UnboundVariable(v) => v.id,
            Symbol::Variable(v) => v.id,
            Symbol::Parameter(v) => v.id,
            Symbol::Function(v) => v.id,
            Symbol::Lambda(v) => v.id,
            Symbol::Let(v) => v.id,
        }
    }

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
            Symbol::UnboundVariable(v) => v.location,
            Symbol::Variable(v) => v.location,
            Symbol::Parameter(v) => v.location,
            Symbol::Function(v) => v.location,
            Symbol::Lambda(v) => v.location,
            Symbol::Let(v) => v.location,
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
            }
            Symbol::Variable(_) => todo!(),
            Symbol::Parameter(p) => p.is_unbound = true,
            Symbol::Function(f) => {
                f.is_recursive = f.name == scope_name || f.is_recursive;
                skip = true;
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
    fn handle_quote(&mut self, _: &mut SymbolTable, _: &QuoteExpr) {}
    fn handle_keyword(&mut self, _: &mut SymbolTable, _: &str, _: Span) {}
    // -----------  END NOT USED  -----------
}
