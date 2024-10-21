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

// #[derive(Debug)]
// pub struct TestExpr;

#[derive(Debug)]
pub struct LoopExpr<'a> {
    pub condition: &'a Spanned<Expr>,
    pub body: &'a Spanned<Expr>,
}

#[derive(Debug)]
pub struct LambdaExpr<'a> {
    pub params: &'a Spanned<Expr>,
    pub body: &'a [&'a Spanned<Expr>],
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
    pub body: &'a Spanned<Expr>,
}

#[derive(Debug)]
pub struct QuoteExpr<'a> {
    pub quoted: &'a Spanned<Expr>,
    pub expr: &'a Spanned<Expr>,
    pub span: Span,
}

pub trait AstWalker<T> {
    fn error(&mut self, _: Error);
    fn get_lambda_name(&mut self) -> String;
    fn handle_operator(&mut self, _: &mut T, _: &str, _: &OperatorExpr);
    fn handle_builtin(&mut self, _: &mut T, _: &str, _: &[Spanned<Expr>]);
    fn handle_function(&mut self, _: &mut T, _: &FunctionExpr);
    fn handle_lambda(&mut self, _: &mut T, _: &LambdaExpr);
    fn handle_let_binding(&mut self, _: &mut T, _: &LetBindingExpr);
    fn handle_call(&mut self, _: &mut T, _: &CallExpr);
    fn handle_var(&mut self, _: &mut T, _: &VarExpr);
    fn handle_if_else(&mut self, _: &mut T, _: &IfElseExpr);
    fn handle_loop(&mut self, _: &mut T, _: &LoopExpr);
    fn handle_bool(&mut self, _: &mut T, _: bool);
    fn handle_string(&mut self, _: &mut T, _: &str);
    fn handle_number(&mut self, _: &mut T, _: f64);
    fn handle_symbol(&mut self, _: &mut T, _: &str, _: Span);
    fn handle_quote(&mut self, _: &mut T, _: &QuoteExpr);
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
                self.handle_builtin(t, symbol, exprs)
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
            "test" => self.walk_test(t, exprs),
            "loop" => self.walk_loop(t, exprs),
            "lambda" => self.walk_lambda(t, exprs),
            "fn" => self.walk_fn(t, exprs, span),
            "if" => self.walk_if(t, exprs),
            "let" => self.walk_let_binding(t, exprs),
            "quote" => self.walk_quote(t, exprs, span),
            _ => panic!("Unknown keyword: {name}"),
        }
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

        let Some(body_spanned) = &elements.get(BODY) else {
            self.error(Error::ExpectedFound {
                span: bindings_spanned.span.clone(),
                expected: "body after bindings but".to_string(),
                found: "nothing".to_string(),
                note: Some("body can be a list or a single expression".to_string()),
                help: Some("(let <bindings> <body>)".to_string()),
            });
            return;
        };

        let let_binding_expr = LetBindingExpr {
            bindings,
            body: body_spanned,
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
            fn_symbol: elements.get(0).unwrap(),
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

    fn walk_test(&mut self, _: &mut T, _: &[Spanned<Expr>]) {
        todo!()
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

        let Some(body_spanned) = elements.get(BODY) else {
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
            body: body_spanned,
        };
        self.handle_loop(t, &loop_expr);
    }

    fn walk_expr(&mut self, t: &mut T, spanned: &Spanned<Expr>) {
        match &spanned.expr {
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
