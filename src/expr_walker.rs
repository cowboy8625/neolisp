use super::{
    ast::{Expr, Span, Spanned},
    error::Error,
    BUILTINS, KEYWORDS, OPERATORS,
};

#[derive(Debug)]
pub struct OperatorExpr<'a> {
    pub operator: &'a Spanned<Expr>,
    pub args: &'a [&'a Spanned<Expr>],
}

#[derive(Debug)]
pub struct VarExpr<'a> {
    pub name: &'a Spanned<Expr>,
    pub body: &'a Spanned<Expr>,
}

impl VarExpr<'_> {
    pub fn span(&self) -> Span {
        let start = self.name.span.start;
        let end = self.body.span.end;
        start..end
    }
}

#[derive(Debug)]
pub struct SetExpr<'a> {
    pub name: &'a Spanned<Expr>,
    pub body: &'a Spanned<Expr>,
}

#[derive(Debug)]
pub struct TestExpr<'a> {
    pub keyword: &'a Spanned<Expr>,
    pub name: &'a Spanned<Expr>,
    pub body: &'a [&'a Spanned<Expr>],
    pub span: Span,
}

impl TestExpr<'_> {
    pub fn name(&self) -> String {
        format!("test_{}", self.name)
    }

    pub fn as_expr(&self) -> Spanned<Expr> {
        let mut list = vec![self.keyword.clone(), self.name.clone()];

        list.extend(self.body.iter().map(|&expr| expr.clone()));

        Spanned::from((Expr::List(list), self.span.clone()))
    }
}

#[derive(Debug)]
pub struct LoopExpr<'a> {
    pub condition: &'a Spanned<Expr>,
    pub body: &'a [&'a Spanned<Expr>],
}

#[derive(Debug)]
pub struct LambdaExpr<'a> {
    pub params: &'a Spanned<Expr>,
    pub body: &'a [&'a Spanned<Expr>],
}

impl LambdaExpr<'_> {
    pub fn span(&self) -> Span {
        let start_span = self.params.span.clone();
        let start = start_span.start;
        let end = self
            .body
            .last()
            .map(|expr| expr.span.end)
            .unwrap_or(start_span.end);

        start..end
    }
}

#[derive(Debug)]
pub struct FunctionExpr<'a> {
    pub fn_symbol: &'a Spanned<Expr>,
    pub name: &'a Spanned<Expr>,
    pub params: &'a Spanned<Expr>,
    pub body: &'a [&'a Spanned<Expr>],
    pub span: Span,
}

impl FunctionExpr<'_> {
    pub fn as_expr(&self) -> Spanned<Expr> {
        let mut list = vec![
            self.fn_symbol.clone(),
            self.name.clone(),
            self.params.clone(),
        ];

        list.extend(self.body.iter().map(|&expr| expr.clone()));

        Spanned::from((Expr::List(list), self.span.clone()))
    }
}

#[derive(Debug)]
pub struct CallExpr<'a> {
    pub callee: &'a Spanned<Expr>,
    pub args: &'a [&'a Spanned<Expr>],
}

#[derive(Debug)]
pub struct IfElseExpr<'a> {
    pub condition: &'a Spanned<Expr>,
    pub then: &'a Spanned<Expr>,
    pub otherwise: Option<&'a Spanned<Expr>>,
}

#[derive(Debug)]
pub struct LetBindingExpr<'a> {
    pub bindings: &'a [&'a Spanned<Expr>],
    pub body: &'a [&'a Spanned<Expr>],
}

#[derive(Debug)]
pub struct QuoteExpr<'a> {
    pub quoted: &'a Spanned<Expr>,
    pub expr: &'a Spanned<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct FfiBindExpr<'a> {
    /// ffi-bind keyword
    pub name: &'a Spanned<Expr>,
    /// library name symbol is in
    /// :library "libname"
    pub lib: (&'a Spanned<Expr>, &'a Spanned<Expr>),
    /// symbol name in the library
    /// :symbol "symbolname"
    pub symbol: (&'a Spanned<Expr>, &'a Spanned<Expr>),
    /// arguments to the function in library
    /// :args '(int int)
    pub args: (&'a Spanned<Expr>, &'a Spanned<Expr>),
    /// return type of the function
    /// :return 'int
    pub return_type: (&'a Spanned<Expr>, &'a Spanned<Expr>),
    /// name of function in source code aka neolisp
    /// :fn 'function-name-in-source
    pub fn_symbol: (&'a Spanned<Expr>, &'a Spanned<Expr>),
    pub span: Span,
}

pub trait AstWalker<T> {
    fn error(&mut self, _: Error);
    fn get_lambda_name(&mut self) -> String;
    fn handle_operator(&mut self, _: &mut T, _: &str, _: &OperatorExpr);
    fn handle_builtin(&mut self, _: &mut T, _: &str, _: Span, _: &[Spanned<Expr>]);
    fn handle_test(&mut self, _: &mut T, _: &TestExpr);
    fn handle_function(&mut self, _: &mut T, _: &FunctionExpr);
    fn handle_lambda(&mut self, _: &mut T, _: &LambdaExpr);
    fn handle_let_binding(&mut self, _: &mut T, _: &LetBindingExpr);
    fn handle_call(&mut self, _: &mut T, _: &CallExpr);
    fn handle_var(&mut self, _: &mut T, _: &VarExpr);
    fn handle_set(&mut self, _: &mut T, _: &SetExpr);
    fn handle_if_else(&mut self, _: &mut T, _: &IfElseExpr);
    fn handle_loop(&mut self, _: &mut T, _: &LoopExpr);
    fn handle_bool(&mut self, _: &mut T, _: bool);
    fn handle_nil(&mut self, _: &mut T);
    fn handle_string(&mut self, _: &mut T, _: &str);
    fn handle_number(&mut self, _: &mut T, _: f64);
    fn handle_symbol(&mut self, _: &mut T, _: &str, _: Span);
    fn handle_quote(&mut self, _: &mut T, _: &QuoteExpr);
    fn handle_ffi_bind(&mut self, _: &mut T, _: &FfiBindExpr);
    fn handle_keyword(&mut self, _: &mut T, _: &str, _: Span);

    fn walk_list(&mut self, t: &mut T, exprs: &[Spanned<Expr>], span: Span) {
        let Some(first) = exprs.first() else {
            // TODO: Should this be an error?
            return;
        };

        match &first.expr {
            Expr::Symbol(symbol) if OPERATORS.contains(&symbol.as_str()) => {
                self.walk_operator(t, symbol, exprs)
            }
            Expr::Symbol(symbol) if KEYWORDS.contains(&symbol.as_str()) => {
                self.walk_keyword(t, symbol, exprs, span)
            }
            Expr::Symbol(symbol) if BUILTINS.contains(&symbol.as_str()) => {
                self.handle_builtin(t, symbol, first.span.clone(), exprs)
            }
            Expr::Bool(_) | Expr::String(_) | Expr::Number(_) => {
                self.error(Error::NotCallable(first.span.clone(), first.expr.type_of()))
            }
            _ => self.walk_callable(t, exprs),
        }
    }

    fn walk_operator(&mut self, t: &mut T, symbol: &str, exprs: &[Spanned<Expr>]) {
        const OPERATOR: usize = 0;
        const ARGS: usize = 1;

        let Some(operator) = exprs.get(OPERATOR) else {
            unreachable!("checked before walk_operator was called");
        };

        let args = exprs.iter().skip(ARGS).collect::<Vec<_>>();

        let operator = OperatorExpr {
            operator,
            args: &args,
        };

        self.handle_operator(t, symbol, &operator)
    }

    fn walk_callable(&mut self, t: &mut T, exprs: &[Spanned<Expr>]) {
        const ARGS: usize = 1;
        let Some(first) = exprs.first() else {
            // TODO: Should this be an error?
            panic!("expected callable");
        };

        let args = exprs.iter().skip(ARGS).collect::<Vec<_>>();
        self.handle_call(
            t,
            &CallExpr {
                callee: first,
                args: &args,
            },
        )
    }

    fn walk_keyword(&mut self, t: &mut T, name: &str, exprs: &[Spanned<Expr>], span: Span) {
        match name {
            "var" => self.walk_var(t, exprs),
            "set" => self.walk_set(t, exprs),
            "test" => self.walk_test(t, exprs, span),
            "loop" => self.walk_loop(t, exprs),
            "lambda" => self.walk_lambda(t, exprs),
            "fn" => self.walk_fn(t, exprs, span),
            "if" => self.walk_if(t, exprs),
            "let" => self.walk_let_binding(t, exprs),
            "quote" => self.walk_quote(t, exprs, span),
            "ffi-bind" => self.walk_ffi_bind(t, exprs, span),
            _ => panic!("Unknown keyword: {name}"),
        }
    }

    fn walk_ffi_bind(&mut self, t: &mut T, elements: &[Spanned<Expr>], span: Span) {
        // NOTE: This is hard coding the structure of the ffi-bind expression
        // This makes it easier to parse but not as flexible in user land.
        const FFI_BIND_NAME: usize = 0;
        const LIB_KEYWORD: usize = FFI_BIND_NAME + 1;
        const LIB_VALUE: usize = LIB_KEYWORD + 1;
        const SYMBOL_KEYWORD: usize = LIB_VALUE + 1;
        const SYMBOL_VALUE: usize = SYMBOL_KEYWORD + 1;
        const FN_KEYWORD: usize = SYMBOL_VALUE + 1;
        const FN_VALUE: usize = FN_KEYWORD + 1;
        const ARGS_KEYWORD: usize = FN_VALUE + 1;
        const ARGS_VALUE: usize = ARGS_KEYWORD + 1;
        const RET_KEYWORD: usize = ARGS_VALUE + 1;
        const RET_VALUE: usize = RET_KEYWORD + 1;
        const HELP : &str = "(ffi-bind :library <string> :symbol <string>  :fn <symbol> <args> '(<symbol>) :return <symbol>)";

        let Some(name) = elements.get(FFI_BIND_NAME) else {
            unreachable!("checked before walk_ffi_bind was called");
        };

        let Some(lib_keyword) = elements.get(LIB_KEYWORD) else {
            self.error(Error::ExpectedFound {
                span: name.span.clone(),
                expected: ":library".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(lib_value) = elements.get(LIB_VALUE) else {
            self.error(Error::ExpectedFound {
                span: lib_keyword.span.clone(),
                expected: "library name".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(symbol_keyword) = elements.get(SYMBOL_KEYWORD) else {
            self.error(Error::ExpectedFound {
                span: lib_value.span.clone(),
                expected: ":symbol".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(symbol_value) = elements.get(SYMBOL_VALUE) else {
            self.error(Error::ExpectedFound {
                span: symbol_keyword.span.clone(),
                expected: "symbol name".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(fn_keyword) = elements.get(FN_KEYWORD) else {
            self.error(Error::ExpectedFound {
                span: symbol_value.span.clone(),
                expected: ":fn".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(fn_value) = elements.get(FN_VALUE) else {
            self.error(Error::ExpectedFound {
                span: fn_keyword.span.clone(),
                expected: "function name".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(args_keyword) = elements.get(ARGS_KEYWORD) else {
            self.error(Error::ExpectedFound {
                span: fn_value.span.clone(),
                expected: ":args".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(args_value) = elements.get(ARGS_VALUE) else {
            self.error(Error::ExpectedFound {
                span: args_keyword.span.clone(),
                expected: "arguments".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(ret_keyword) = elements.get(RET_KEYWORD) else {
            self.error(Error::ExpectedFound {
                span: args_value.span.clone(),
                expected: ":return".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let Some(ret_value) = elements.get(RET_VALUE) else {
            self.error(Error::ExpectedFound {
                span: ret_keyword.span.clone(),
                expected: "return type".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let ffi_bind_expr = FfiBindExpr {
            name,
            lib: (lib_keyword, lib_value),
            symbol: (symbol_keyword, symbol_value),
            fn_symbol: (fn_keyword, fn_value),
            args: (args_keyword, args_value),
            return_type: (ret_keyword, ret_value),
            span,
        };

        self.handle_ffi_bind(t, &ffi_bind_expr);
    }

    fn walk_quote(&mut self, t: &mut T, elements: &[Spanned<Expr>], span: Span) {
        const QUOTED: usize = 0;
        const BODY: usize = 1;

        let Some(quoted) = elements.get(QUOTED) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: "quoted expression".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(quote <expression>)".to_string()),
            });
            return;
        };

        let Some(body) = elements.get(BODY) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: "body after quote but".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(quote <expression>)".to_string()),
            });
            return;
        };

        let quote = QuoteExpr {
            quoted,
            expr: body,
            span,
        };

        self.handle_quote(t, &quote);
    }

    fn walk_if(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const CONDITION: usize = 1;
        const THEN: usize = 2;
        const OTHERWISE: usize = 3;

        let starting_span = elements[0].span.clone();
        let Some(condition_spanned) = &elements.get(CONDITION) else {
            self.error(Error::ExpectedFound {
                span: starting_span,
                expected: "condition after if but".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(if <condition> <then-branch> <otherwise-branch>)".to_string()),
            });
            return;
        };

        let Some(then_spanned) = &elements.get(THEN) else {
            self.error(Error::ExpectedFound {
                span: condition_spanned.span.clone(),
                expected: "then branch after condition but".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(if <condition> <then-branch> <otherwise-branch>)".to_string()),
            });
            return;
        };

        let if_else = IfElseExpr {
            condition: condition_spanned,
            then: then_spanned,
            otherwise: elements.get(OTHERWISE),
        };

        self.handle_if_else(t, &if_else);
    }

    fn walk_let_binding(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const BINDINGS: usize = 1;
        const BODY: usize = 2;

        let starting_span = elements[0].span.clone();
        let Some(bindings_spanned) = elements.get(BINDINGS) else {
            self.error(Error::ExpectedFound {
                span: starting_span,
                expected: "bindings after let but".to_string(),
                found: "nothing".to_string(),
                note: Some("bindings can be a single list or a list of list. Example (x 1) or ((x 1) (y 2))".to_string()),
                help: Some("(let <bindings> <body>)".to_string()),
            });
            return;
        };

        let Expr::List(mabindings) = &bindings_spanned.expr else {
            self.error(Error::ExpectedFound {
                span: bindings_spanned.span.clone(),
                expected: "List".to_string(),
                found: bindings_spanned.expr.type_of(),
                note: None,
                help: None,
            });
            return;
        };
        let is_single_binding = bindings_spanned
            .expr
            .first_list_item()
            .map(|item| item.expr.is_symbol())
            .unwrap_or(false);

        let bindings = if is_single_binding {
            &vec![bindings_spanned]
        } else {
            &mabindings.iter().collect::<Vec<_>>()
        };

        let body_list = &elements.iter().skip(BODY).collect::<Vec<_>>();
        let let_binding_expr = LetBindingExpr {
            bindings,
            body: body_list,
        };
        self.handle_let_binding(t, &let_binding_expr);
    }

    fn walk_lambda(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const PARAMS: usize = 1;
        const BODY: usize = 2;

        let starting_span = elements[0].span.clone();
        let Some(params_spanned) = &elements.get(PARAMS) else {
            self.error(Error::ExpectedFound {
                span: starting_span,
                expected: "params after lambda but".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(lambda (<symbol>) <body>)".to_string()),
            });
            return;
        };

        if !params_spanned.expr.is_list() {
            self.error(Error::ExpectedFound {
                span: params_spanned.span.clone(),
                expected: "List".to_string(),
                found: params_spanned.expr.type_of(),
                note: None,
                help: Some("(lambda (<symbol>) <body>)".to_string()),
            });
            return;
        };

        let body_list = &elements.iter().skip(BODY).collect::<Vec<_>>();

        let function = LambdaExpr {
            params: params_spanned,
            body: body_list,
        };

        self.handle_lambda(t, &function);
    }

    fn walk_fn(&mut self, t: &mut T, elements: &[Spanned<Expr>], span: Span) {
        const NAME: usize = 1;
        const PARAMS: usize = 2;
        const BODY: usize = 3;

        let starting_span = elements[0].span.clone();
        let Some(name_spanned) = elements.get(NAME) else {
            self.error(Error::ExpectedFound {
                span: starting_span,
                expected: "name after fn but".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(fn <symbol> (<symbol>) <body>)".to_string()),
            });
            return;
        };

        let Some(params_spanned) = &elements.get(PARAMS) else {
            self.error(Error::ExpectedFound {
                span: name_spanned.span.clone(),
                expected: "params after function name but".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(fn <symbol> (<symbol>) <body>)".to_string()),
            });
            return;
        };

        if !params_spanned.expr.is_list() {
            self.error(Error::ExpectedFound {
                span: params_spanned.span.clone(),
                expected: "List".to_string(),
                found: params_spanned.expr.type_of(),
                note: None,
                help: Some("(fn <symbol> (<symbol>) <body>)".to_string()),
            });
            return;
        }

        let body_list = &elements.iter().skip(BODY).collect::<Vec<_>>();

        let function = FunctionExpr {
            // NOTE: INTENTIONAL UNWRAP
            // this is safe as we already checked that the name is a symbol
            fn_symbol: elements.first().unwrap(),
            name: name_spanned,
            params: params_spanned,
            body: body_list,
            span,
        };

        self.handle_function(t, &function);
    }

    fn walk_var(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const NAME: usize = 1;
        const BODY: usize = 2;

        let starting_span = elements[0].span.clone();
        let Some(name_spanned) = elements.get(NAME) else {
            self.error(Error::ExpectedFound {
                span: starting_span,
                expected: "name after var".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(var <name> <expression>)".to_string()),
            });
            return;
        };
        let Expr::Symbol(_) = &name_spanned.expr else {
            self.error(Error::ExpectedFound {
                span: name_spanned.span.clone(),
                expected: "Symbol".to_string(),
                found: name_spanned.expr.type_of(),
                note: None,
                help: Some("(var <name> <expression>)".to_string()),
            });
            return;
        };

        let Some(body_spanned) = elements.get(BODY) else {
            self.error(Error::ExpectedFound {
                span: name_spanned.span.clone(),
                expected: "expression after name".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(var <name> <expression>)".to_string()),
            });
            return;
        };

        let var = VarExpr {
            name: name_spanned,
            body: body_spanned,
        };
        self.handle_var(t, &var);
    }

    fn walk_set(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const NAME: usize = 1;
        const BODY: usize = 2;

        let starting_span = elements[0].span.clone();
        let Some(name_spanned) = elements.get(NAME) else {
            self.error(Error::ExpectedFound {
                span: starting_span,
                expected: "name after set".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(set <name> <expression>)".to_string()),
            });
            return;
        };
        let Expr::Symbol(_) = &name_spanned.expr else {
            self.error(Error::ExpectedFound {
                span: name_spanned.span.clone(),
                expected: "Symbol".to_string(),
                found: name_spanned.expr.type_of(),
                note: None,
                help: Some("(set <name> <expression>)".to_string()),
            });
            return;
        };

        let Some(body_spanned) = elements.get(BODY) else {
            self.error(Error::ExpectedFound {
                span: name_spanned.span.clone(),
                expected: "expression after name".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(set <name> <expression>)".to_string()),
            });
            return;
        };

        let set = SetExpr {
            name: name_spanned,
            body: body_spanned,
        };
        self.handle_set(t, &set);
    }

    fn walk_test(&mut self, t: &mut T, elements: &[Spanned<Expr>], span: Span) {
        const NAME: usize = 1;
        const BODY: usize = 2;

        let Some(name_spanned) = elements.get(NAME) else {
            self.error(Error::ExpectedFound {
                span: elements[0].span.clone(),
                expected: "name after test".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(test <name> <expression>)".to_string()),
            });
            return;
        };

        let Expr::Symbol(_) = &name_spanned.expr else {
            self.error(Error::ExpectedFound {
                span: name_spanned.span.clone(),
                expected: "Symbol".to_string(),
                found: name_spanned.expr.type_of(),
                note: None,
                help: Some("(test <name> <expression>)".to_string()),
            });
            return;
        };

        let body = &elements.iter().skip(BODY).collect::<Vec<_>>();

        let test = TestExpr {
            // NOTE: INTENTIONAL UNWRAP
            // this is safe as we already checked that the name is a symbol
            keyword: elements.first().unwrap(),
            name: name_spanned,
            body,
            span,
        };
        self.handle_test(t, &test);
    }

    fn walk_loop(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const CONDTION: usize = 1;
        const BODY: usize = 2;
        let starting_span = elements[0].span.clone();
        let Some(condtion_spanned) = elements.get(CONDTION) else {
            self.error(Error::ExpectedFound {
                span: starting_span,
                expected: "condition after loop".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(loop <condition> <expression>)".to_string()),
            });
            return;
        };

        let body_list = &elements.iter().skip(BODY).collect::<Vec<_>>();
        // TODO: Is a empty body valid?
        // (fn foo ())
        // I think to answer this question I would as is this useful?
        // I don't think so? But I will leave it for now till I am sure.
        if body_list.is_empty() {
            self.error(Error::ExpectedFound {
                span: condtion_spanned.span.clone(),
                expected: "expression after condition".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some("(loop <condition> <expression>)".to_string()),
            });
            return;
        };

        let loop_expr = LoopExpr {
            condition: condtion_spanned,
            body: body_list,
        };
        self.handle_loop(t, &loop_expr);
    }

    fn walk_expr(&mut self, t: &mut T, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Nil => self.handle_nil(t),
            Expr::Bool(value) => self.handle_bool(t, *value),
            Expr::String(string) => self.handle_string(t, string),
            Expr::Symbol(symbol) if symbol.starts_with(':') => {
                self.handle_keyword(t, symbol, spanned.span.clone())
            }
            Expr::Symbol(symbol) => self.handle_symbol(t, symbol, spanned.span.clone()),
            Expr::Number(number) => self.handle_number(t, *number),
            Expr::List(vec) => self.walk_list(t, vec, spanned.span.clone()),
        }
    }
    fn walk(&mut self, t: &mut T, exprs: &[Spanned<Expr>]) {
        for expr in exprs {
            self.walk_expr(t, expr);
        }
    }
}
