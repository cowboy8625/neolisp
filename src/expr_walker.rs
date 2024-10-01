use super::{BUILTINS, KEYWORDS, OPERATORS};
use crate::ast::{Expr, Spanned};

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
    pub body: &'a Spanned<Expr>,
}

#[derive(Debug)]
pub struct FunctionExpr<'a> {
    pub name: &'a Spanned<Expr>,
    pub params: &'a Spanned<Expr>,
    pub body: &'a Spanned<Expr>,
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

pub trait AstWalker<T> {
    fn get_lambda_name(&mut self) -> String;
    fn handle_operator(&mut self, _: &mut T, _: &str, _: &[Spanned<Expr>]);
    fn handle_builtin(&mut self, _: &mut T, _: &str, _: &[Spanned<Expr>]);
    fn handle_function(&mut self, _: &mut T, _: &FunctionExpr);
    fn handle_lambda(&mut self, _: &mut T, _: &LambdaExpr);
    fn handle_call(&mut self, _: &mut T, _: &CallExpr);
    fn handle_var(&mut self, _: &mut T, _: &VarExpr);
    fn handle_if_else(&mut self, _: &mut T, _: &IfElseExpr);
    fn handle_loop(&mut self, _: &mut T, _: &LoopExpr);
    fn handle_bool(&mut self, _: &mut T, _: bool);
    fn handle_string(&mut self, _: &mut T, _: &str);
    fn handle_number(&mut self, _: &mut T, _: f64);
    fn handle_symbol(&mut self, _: &mut T, _: &str);

    fn walk_list(&mut self, t: &mut T, exprs: &[Spanned<Expr>]) {
        let Some(first) = exprs.first() else {
            // TODO: Should this be an error?
            return;
        };

        match &first.expr {
            Expr::Symbol(symbol) if OPERATORS.contains(&symbol.as_str()) => {
                self.handle_operator(t, symbol, exprs)
            }
            Expr::Symbol(symbol) if KEYWORDS.contains(&symbol.as_str()) => {
                self.walk_keyword(t, symbol, exprs)
            }
            Expr::Symbol(symbol) if BUILTINS.contains(&symbol.as_str()) => {
                self.handle_builtin(t, symbol, exprs)
            }
            // TODO: REPORT ERROR
            Expr::Bool(_) => panic!("bool are not callable"),
            // TODO: REPORT ERROR
            Expr::String(_) => panic!("strings are not callable"),
            // TODO: REPORT ERROR
            Expr::Number(_) => panic!("numbers are not callable"),
            _ => self.walk_callable(t, exprs),
        }
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

    fn walk_keyword(&mut self, t: &mut T, name: &str, exprs: &[Spanned<Expr>]) {
        match name {
            "var" => self.walk_var(t, exprs),
            "test" => self.walk_test(t, exprs),
            "loop" => self.walk_loop(t, exprs),
            "lambda" => self.walk_lambda(t, exprs),
            "fn" => self.walk_fn(t, exprs),
            "if" => self.walk_if(t, exprs),
            _ => panic!("Unknown keyword: {name}"),
        }
    }

    fn walk_if(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const CONDITION: usize = 1;
        const THEN: usize = 2;
        const OTHERWISE: usize = 3;

        let Some(condition_spanned) = &elements.get(CONDITION) else {
            // TODO: REPORT ERROR
            panic!("expected expr for condition");
        };

        let Some(then_spanned) = &elements.get(THEN) else {
            // TODO: REPORT ERROR
            panic!("expected expr for then");
        };

        let if_else = IfElseExpr {
            condition: condition_spanned,
            then: then_spanned,
            otherwise: elements.get(OTHERWISE),
        };

        self.handle_if_else(t, &if_else);
    }

    fn walk_lambda(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
        const PARAMS: usize = 1;
        const BODY: usize = 2;

        debug_assert!(elements.len() == 3, "(lambda (<params>) ( <body> ) | expr)");

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

        let function = LambdaExpr {
            params: params_spanned,
            body: body_spanned,
        };

        self.handle_lambda(t, &function);
    }

    fn walk_fn(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
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

        let function = FunctionExpr {
            name: name_spanned,
            params: params_spanned,
            body: body_spanned,
        };

        self.handle_function(t, &function);
    }

    fn walk_var(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
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
        self.handle_var(t, &var);
    }

    fn walk_test(&mut self, _: &mut T, _: &[Spanned<Expr>]) {
        todo!()
    }

    fn walk_loop(&mut self, t: &mut T, elements: &[Spanned<Expr>]) {
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
            condition: condtion_spanned,
            body: body_spanned,
        };
        self.handle_loop(t, &loop_expr);
    }

    fn walk_expr(&mut self, t: &mut T, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(value) => self.handle_bool(t, *value),
            Expr::String(string) => self.handle_string(t, string),
            Expr::Symbol(symbol) => self.handle_symbol(t, symbol),
            Expr::Number(number) => self.handle_number(t, *number),
            Expr::List(vec) => self.walk_list(t, vec),
        }
    }
    fn walk(&mut self, t: &mut T, exprs: &[Spanned<Expr>]) {
        for expr in exprs {
            self.walk_expr(t, expr);
        }
    }
}
