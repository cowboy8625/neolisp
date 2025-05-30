use std::collections::HashMap;

use super::{
    ast::{Expr, Span, Spanned},
    error::Error,
    BUILTINS, KEYWORDS, OPERATORS,
};

#[derive(Debug)]
pub struct OperatorExpr<'a> {
    pub operator: &'a Spanned,
    pub args: &'a [&'a Spanned],
}

#[derive(Debug)]
pub struct VarExpr<'a> {
    pub name: &'a Spanned,
    pub body: &'a Spanned,
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
    pub name: &'a Spanned,
    pub body: &'a Spanned,
}

#[derive(Debug)]
pub struct TestExpr<'a> {
    pub keyword: &'a Spanned,
    pub name: &'a Spanned,
    pub body: &'a [&'a Spanned],
    pub span: Span,
}

impl TestExpr<'_> {
    pub fn name(&self) -> String {
        format!("test_{}", self.name)
    }

    pub fn as_expr(&self) -> Spanned {
        let mut list = vec![self.keyword.clone(), self.name.clone()];

        list.extend(self.body.iter().map(|&expr| expr.clone()));

        Spanned::from((Expr::List(list), self.span.clone()))
    }
}

#[derive(Debug)]
pub struct LoopExpr<'a> {
    pub condition: &'a Spanned,
    pub body: &'a [&'a Spanned],
}

#[derive(Debug)]
pub struct LambdaExpr<'a> {
    pub params: &'a Spanned,
    pub body: &'a [&'a Spanned],
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
    pub fn_symbol: &'a Spanned,
    pub name: &'a Spanned,
    pub params: &'a Spanned,
    pub body: &'a [&'a Spanned],
    pub span: Span,
}

impl FunctionExpr<'_> {
    pub fn as_expr(&self) -> Spanned {
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
    pub callee: &'a Spanned,
    pub args: &'a [&'a Spanned],
}

#[derive(Debug)]
pub struct IfElseExpr<'a> {
    pub condition: &'a Spanned,
    pub then: &'a Spanned,
    pub otherwise: Option<&'a Spanned>,
}

#[derive(Debug)]
pub struct LetBindingExpr<'a> {
    pub bindings: &'a [&'a Spanned],
    pub body: &'a [&'a Spanned],
}

#[derive(Debug)]
pub struct QuoteExpr<'a> {
    pub quoted: &'a Spanned,
    pub expr: &'a Spanned,
    pub span: Span,
}

#[derive(Debug)]
pub struct FfiBindFnExpr<'a> {
    /// ffi-bind keyword
    pub name: &'a Spanned,
    /// library name symbol is in
    /// :library "libname"
    pub lib: &'a Spanned,
    /// symbol name in the library
    /// :symbol "symbolname"
    pub symbol: &'a Spanned,
    /// name of function in source code aka neolisp
    /// :fn 'function-name-in-source
    pub fn_symbol: &'a Spanned,
    /// arguments to the function in library
    /// :args '(int int)
    pub args: &'a Spanned,
    /// return type of the function
    /// :return 'int
    pub return_type: &'a Spanned,
    pub span: Span,
}

#[derive(Debug)]
pub struct FfiBindStructExpr<'a> {
    /// ffi-bind keyword
    pub name: &'a Spanned,
    /// library name symbol is in
    /// :library "libname"
    pub lib: &'a Spanned,
    /// symbol name in the library
    /// :symbol "symbolname"
    pub symbol: &'a Spanned,
    /// name of function in source code aka neolisp
    /// :struct 'struct-name-in-source
    pub struct_symbol: &'a Spanned,
    /// arguments to the function in library
    /// :args '(int int)
    pub fields: &'a Spanned,
    pub span: Span,
}

#[derive(Debug)]
pub struct StructExpr<'a> {
    /// struct keyword
    pub struct_keyword: &'a Spanned,
    /// struct name
    pub name: &'a Spanned,
    /// struct fields
    pub fields: &'a [&'a Spanned],
    pub span: Span,
}

#[derive(Debug)]
pub struct ReturnExpr<'a> {
    pub return_keyword: &'a Spanned,
    pub expr: Option<&'a Spanned>,
    pub span: Span,
}

pub trait AstWalker<T> {
    fn error(&mut self, _: Error);
    fn get_lambda_name(&mut self) -> String;
    fn handle_bool(&mut self, _: &mut T, _: bool);
    fn handle_builtin(&mut self, _: &mut T, _: &str, _: Span, _: &[Spanned]);
    fn handle_call(&mut self, _: &mut T, _: &CallExpr);
    fn handle_ffi_bind_fn(&mut self, _: &mut T, _: &FfiBindFnExpr);
    fn handle_ffi_bind_struct(&mut self, _: &mut T, _: &FfiBindStructExpr);
    fn handle_function(&mut self, _: &mut T, _: &FunctionExpr);
    fn handle_if_else(&mut self, _: &mut T, _: &IfElseExpr);
    fn handle_keyword(&mut self, _: &mut T, _: &str, _: Span);
    fn handle_lambda(&mut self, _: &mut T, _: &LambdaExpr);
    fn handle_let_binding(&mut self, _: &mut T, _: &LetBindingExpr);
    fn handle_loop(&mut self, _: &mut T, _: &LoopExpr);
    fn handle_nil(&mut self, _: &mut T);
    fn handle_number(&mut self, _: &mut T, _: f64);
    fn handle_operator(&mut self, _: &mut T, _: &str, _: &OperatorExpr);
    fn handle_quote(&mut self, _: &mut T, _: &QuoteExpr);
    fn handle_return(&mut self, _: &mut T, _: &ReturnExpr);
    fn handle_set(&mut self, _: &mut T, _: &SetExpr);
    fn handle_string(&mut self, _: &mut T, _: &str);
    fn handle_struct(&mut self, _: &mut T, _: &StructExpr);
    fn handle_symbol(&mut self, _: &mut T, _: &str, _: Span);
    fn handle_test(&mut self, _: &mut T, _: &TestExpr);
    fn handle_var(&mut self, _: &mut T, _: &VarExpr);

    fn walk_list(&mut self, t: &mut T, exprs: &[Spanned], span: Span) {
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

    fn walk_operator(&mut self, t: &mut T, symbol: &str, exprs: &[Spanned]) {
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

    fn walk_callable(&mut self, t: &mut T, exprs: &[Spanned]) {
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

    fn walk_keyword(&mut self, t: &mut T, name: &str, exprs: &[Spanned], span: Span) {
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
            "struct" => self.walk_struct(t, exprs),
            "return" => self.walk_return(t, exprs, span),
            "unquote" => unreachable!("unquote"),
            "quasiquote" => unreachable!("quasiquote"),
            _ => panic!("Unknown keyword: {name}"),
        }
    }

    fn walk_struct(&mut self, t: &mut T, elements: &[Spanned]) {
        const STRUCT_KEYWORD: usize = 0;
        const STRUCT_NAME: usize = STRUCT_KEYWORD + 1;
        const FIELDS: usize = STRUCT_NAME + 1;
        const HELP: &str = "(struct <symbol> <(<symbol> <type>)*>)";

        let struct_keyword = &elements[STRUCT_KEYWORD];
        let start_span = struct_keyword.span.clone();
        let Some(struct_name) = elements.get(STRUCT_NAME) else {
            self.error(Error::ExpectedFound {
                span: start_span.clone(),
                expected: "<symbol>".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let fields = &elements.iter().skip(FIELDS).collect::<Vec<_>>();
        let end_span = fields
            .last()
            .map(|f| f.span.clone())
            .unwrap_or(start_span.clone());

        let struct_expr = StructExpr {
            struct_keyword,
            name: struct_name,
            fields,
            span: start_span.start..end_span.end,
        };
        self.handle_struct(t, &struct_expr);
    }

    fn walk_ffi_bind(&mut self, t: &mut T, elements: &[Spanned], span: Span) {
        let mut items = HashMap::new();
        let mut element_iters = elements.iter().skip(1);
        while let Some(ele) = element_iters.next() {
            let Expr::Symbol(symbol) = &ele.expr else {
                self.error(Error::ExpectedFound {
                    span: ele.span.clone(),
                    expected: "Symbol".to_string(),
                    found: ele.expr.type_of(),
                    note: None,
                    help: None,
                });
                return;
            };
            let Some(value) = element_iters.next() else {
                self.error(Error::ExpectedFound {
                    span: ele.span.clone(),
                    expected: "Value".to_string(),
                    found: ele.expr.type_of(),
                    note: None,
                    help: None,
                });
                return;
            };
            items.insert(symbol, value);
        }

        let name = elements.first().unwrap();

        #[allow(clippy::unnecessary_to_owned)]
        if items.contains_key(&":struct".to_string()) {
            self.walk_ffi_bind_struct(t, &items, name, span);
            return;
        }
        self.walk_ffi_bind_fn(t, &items, name, span);
    }

    fn walk_ffi_bind_fn(
        &mut self,
        t: &mut T,
        items: &HashMap<&String, &Spanned>,
        name: &Spanned,
        span: Span,
    ) {
        const HELP : &str = "(ffi-bind :library <string> :symbol <string>  :fn <symbol> <args> (<symbol>*) :return <symbol>)";
        #[allow(clippy::unnecessary_to_owned)]
        let Some(lib) = items.get(&":library".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":library".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };
        #[allow(clippy::unnecessary_to_owned)]
        let Some(symbol) = items.get(&":symbol".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":symbol".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };
        #[allow(clippy::unnecessary_to_owned)]
        let Some(fn_symbol) = items.get(&":fn".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":fn".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };
        #[allow(clippy::unnecessary_to_owned)]
        let Some(args) = items.get(&":args".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":args".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };
        #[allow(clippy::unnecessary_to_owned)]
        let Some(return_type) = items.get(&":return".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":args".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let ffi_bind_expr = FfiBindFnExpr {
            name,
            lib,
            symbol,
            fn_symbol,
            args,
            return_type,
            span,
        };
        self.handle_ffi_bind_fn(t, &ffi_bind_expr);
    }

    fn walk_ffi_bind_struct(
        &mut self,
        t: &mut T,
        items: &HashMap<&String, &Spanned>,
        name: &Spanned,
        span: Span,
    ) {
        const HELP : &str = "(ffi-bind :library <string> :symbol <string>  :struct <symbol> :fields (<symbol> <type>))";
        #[allow(clippy::unnecessary_to_owned)]
        let Some(lib) = items.get(&":library".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":library".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };
        #[allow(clippy::unnecessary_to_owned)]
        let Some(symbol) = items.get(&":symbol".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":symbol".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };
        #[allow(clippy::unnecessary_to_owned)]
        let Some(struct_symbol) = items.get(&":struct".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":struct".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };
        #[allow(clippy::unnecessary_to_owned)]
        let Some(fields) = items.get(&":fields".to_string()) else {
            self.error(Error::ExpectedFound {
                span: span.clone(),
                expected: ":fields".to_string(),
                found: "nothing".to_string(),
                note: None,
                help: Some(HELP.to_string()),
            });
            return;
        };

        let ffi_bind_expr = FfiBindStructExpr {
            name,
            lib,
            symbol,
            struct_symbol,
            fields,
            span,
        };
        self.handle_ffi_bind_struct(t, &ffi_bind_expr);
    }

    fn walk_quote(&mut self, t: &mut T, elements: &[Spanned], span: Span) {
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

    fn walk_if(&mut self, t: &mut T, elements: &[Spanned]) {
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

    fn walk_let_binding(&mut self, t: &mut T, elements: &[Spanned]) {
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

    fn walk_lambda(&mut self, t: &mut T, elements: &[Spanned]) {
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

    fn walk_fn(&mut self, t: &mut T, elements: &[Spanned], span: Span) {
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

    fn walk_var(&mut self, t: &mut T, elements: &[Spanned]) {
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

    fn walk_set(&mut self, t: &mut T, elements: &[Spanned]) {
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

    fn walk_test(&mut self, t: &mut T, elements: &[Spanned], span: Span) {
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

    fn walk_loop(&mut self, t: &mut T, elements: &[Spanned]) {
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
        // I think to answer this question I would ask is this useful?
        // I don't think so? But I will leave it for now till I am sure.
        // You can always just write (fn foo nil) to do nothing
        // So maybe this is the answer?
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

    fn walk_return(&mut self, t: &mut T, exprs: &[Spanned], span: Span) {
        const KEYWORD: usize = 0;
        const EXPRS: usize = 1;
        let return_expr = ReturnExpr {
            return_keyword: &exprs[KEYWORD],
            expr: exprs.get(EXPRS),
            span,
        };
        self.handle_return(t, &return_expr);
    }

    fn walk_expr(&mut self, t: &mut T, spanned: &Spanned) {
        match &spanned.expr {
            Expr::Bool(value) => self.handle_bool(t, *value),
            Expr::String(string) => self.handle_string(t, string),
            Expr::Symbol(symbol) if symbol.starts_with(':') => {
                self.handle_keyword(t, symbol, spanned.span.clone())
            }
            Expr::Symbol(symbol) if symbol == "nil" => self.handle_nil(t),
            Expr::Symbol(symbol) => self.handle_symbol(t, symbol, spanned.span.clone()),
            Expr::Number(number) => self.handle_number(t, *number),
            Expr::List(vec) => self.walk_list(t, vec, spanned.span.clone()),
        }
    }
    fn walk(&mut self, t: &mut T, exprs: &[Spanned]) {
        for expr in exprs {
            self.walk_expr(t, expr);
        }
    }
}
