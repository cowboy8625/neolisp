use super::{BUILTINS, KEYWORDS, OPERATORS};
use crate::ast::{Expr, Spanned};

#[derive(Debug)]
pub struct VarExpr<'a> {
    name: &'a Spanned<Expr>,
    body: &'a Spanned<Expr>,
}

#[derive(Debug)]
pub struct TestExpr;

#[derive(Debug)]
pub struct LoopExpr<'a> {
    condition: &'a Spanned<Expr>,
    body: &'a Spanned<Expr>,
}

#[derive(Debug)]
pub struct LambdaExpr<'a> {
    params: &'a Spanned<Expr>,
    body: &'a Spanned<Expr>,
}

#[derive(Debug)]
pub struct FunctionExpr<'a> {
    name: &'a Spanned<Expr>,
    params: &'a Spanned<Expr>,
    body: &'a Spanned<Expr>,
}

pub trait AstWalker {
    fn get_lambda_name(&mut self) -> String;
    fn handle_operator(&mut self, _: &str, _: &[Spanned<Expr>]);
    fn handle_builtin(&mut self, _: &str, _: &[Spanned<Expr>]);
    fn handle_function(&mut self, _: &FunctionExpr<'_>);
    fn handle_lambda(&mut self, _: &LambdaExpr<'_>);
    fn handle_var(&mut self, _: &VarExpr<'_>);
    fn handle_loop(&mut self, _: &LoopExpr<'_>);
    fn handle_bool(&mut self, _: bool);
    fn handle_string(&mut self, _: &str);
    fn handle_number(&mut self, _: f64);
    fn handle_symbol(&mut self, _: &str);
    fn enter_scope(&mut self, _: &str);
    fn leave_scope(&mut self, _: &str);

    fn walk_list(&mut self, exprs: &[Spanned<Expr>]) {
        let Some(first) = exprs.first() else {
            // TODO: Should this be an error?
            return;
        };

        match &first.expr {
            Expr::Symbol(symbol) if OPERATORS.contains(&symbol.as_str()) => {
                self.handle_operator(symbol, exprs)
            }
            Expr::Symbol(symbol) if KEYWORDS.contains(&symbol.as_str()) => {
                self.walk_keyword(symbol, exprs)
            }
            Expr::Symbol(symbol) if BUILTINS.contains(&symbol.as_str()) => {
                self.handle_builtin(symbol, exprs)
            }
            _ => {
                for spanned in exprs {
                    self.walk_expr(&spanned);
                }
            }
        }
    }

    fn walk_keyword(&mut self, name: &str, exprs: &[Spanned<Expr>]) {
        match name {
            "var" => self.walk_var(exprs),
            "test" => self.walk_test(exprs),
            "loop" => self.walk_loop(exprs),
            "lambda" => self.walk_lambda(exprs),
            "fn" => self.walk_fn(exprs),
            _ => panic!("Unknown keyword: {name}"),
        }
    }

    fn walk_lambda(&mut self, elements: &[Spanned<Expr>]) {
        const PARAMS: usize = 1;
        const BODY: usize = 2;

        debug_assert!(elements.len() == 4, "(lambda (<params>) ( <body> ) | expr)");

        let name = self.get_lambda_name();

        self.enter_scope(&name);

        let Some(params_spanned) = &elements.get(PARAMS) else {
            // TODO: REPORT ERROR
            panic!("expected list for params");
        };

        let Expr::List(_) = &params_spanned.expr else {
            // TODO: REPORT ERROR
            panic!(
                "expected list for params but found {:?}",
                params_spanned.expr
            );
        };

        let Some(body_spanned) = &elements.get(BODY) else {
            // TODO: REPORT ERROR
            panic!("expected expr for body");
        };

        self.leave_scope(&name);

        let function = LambdaExpr {
            params: params_spanned,
            body: body_spanned,
        };

        self.handle_lambda(&function);
    }

    fn walk_fn(&mut self, elements: &[Spanned<Expr>]) {
        const NAME: usize = 1;
        const PARAMS: usize = 2;
        const BODY: usize = 3;

        debug_assert!(
            elements.len() == 4,
            "(fn <name> (<params>) ( <body> ) | expr)"
        );

        let Some(name_spanned) = elements.get(NAME) else {
            // TODO: REPORT ERROR: This should call a self.report_error(FunctionMissingName(span))
            unreachable!();
        };

        let Expr::Symbol(name) = &name_spanned.expr else {
            // TODO: REPORT ERROR: This should call a self.report_error(FunctionMissingName(span))
            unreachable!();
        };

        self.enter_scope(name);

        let Some(params_spanned) = &elements.get(PARAMS) else {
            // TODO: REPORT ERROR
            panic!("expected list for params");
        };

        let Expr::List(_) = &params_spanned.expr else {
            // TODO: REPORT ERROR
            panic!(
                "expected list for params but found {:?}",
                params_spanned.expr
            );
        };

        let Some(body_spanned) = &elements.get(BODY) else {
            // TODO: REPORT ERROR
            panic!("expected expr for body");
        };

        self.leave_scope(name);

        let function = FunctionExpr {
            name: name_spanned,
            params: params_spanned,
            body: body_spanned,
        };

        self.handle_function(&function);
    }

    fn walk_var(&mut self, elements: &[Spanned<Expr>]) {
        const NAME: usize = 1;
        const BODY: usize = 2;

        let Some(name_spanned) = elements.get(NAME) else {
            // TODO: REPORT ERROR
            unreachable!();
        };
        let Expr::Symbol(_) = &name_spanned.expr else {
            // TODO: REPORT ERROR
            unreachable!();
        };

        let Some(body_spanned) = elements.get(BODY) else {
            // TODO: REPORT ERROR
            unreachable!();
        };

        let var = VarExpr {
            name: name_spanned,
            body: body_spanned,
        };
        self.handle_var(&var);
    }

    fn walk_test(&mut self, _: &[Spanned<Expr>]) {
        todo!()
    }

    fn walk_loop(&mut self, elements: &[Spanned<Expr>]) {
        const CONDTION: usize = 1;
        const BODY: usize = 2;
        let Some(condtion_spanned) = elements.get(CONDTION) else {
            // TODO: REPORT ERROR
            unreachable!();
        };

        let Some(body_spanned) = elements.get(BODY) else {
            // TODO: REPORT ERROR
            unreachable!();
        };

        let loop_expr = LoopExpr {
            condition: &condtion_spanned,
            body: &body_spanned,
        };
        self.handle_loop(&loop_expr);
    }

    fn walk_expr(&mut self, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(value) => self.handle_bool(*value),
            Expr::String(string) => self.handle_string(&string),
            Expr::Symbol(symbol) => self.handle_symbol(&symbol),
            Expr::Number(number) => self.handle_number(*number),
            Expr::List(vec) => self.walk_list(vec),
        }
    }
    fn walk(&mut self, exprs: &[Spanned<Expr>]) {
        for expr in exprs {
            self.walk_expr(expr);
        }
    }
}
