use super::{
    ast::{Ast, Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, QuoteExpr, VarExpr,
    },
};
#[derive(Debug, Default)]
pub struct MacroExpansion {
    errors: Vec<Error>,
}

impl MacroExpansion {
    pub fn build(&mut self, ast: &Ast) -> Result<Ast, Vec<Error>> {
        let mut new_ast = Vec::new();
        self.walk(&mut new_ast, ast);
        if self.errors.is_empty() {
            Ok(new_ast)
        } else {
            Err(self.errors.clone())
        }
    }
}

impl AstWalker<Ast> for MacroExpansion {
    fn error(&mut self, error: Error) {
        self.errors.push(error);
    }

    fn get_lambda_name(&mut self) -> String {
        unreachable!()
    }

    fn handle_operator(&mut self, _: &mut Ast, _: &str, _: &OperatorExpr) {
        todo!()
    }

    fn handle_builtin(&mut self, _: &mut Ast, _: &str, _: &[Spanned<Expr>]) {
        todo!()
    }

    fn handle_function(&mut self, new_ast: &mut Ast, function_expr: &FunctionExpr) {
        new_ast.push(function_expr.as_expr());
    }

    fn handle_lambda(&mut self, _: &mut Ast, _: &LambdaExpr) {
        todo!()
    }

    fn handle_let_binding(&mut self, _: &mut Ast, _: &LetBindingExpr) {
        todo!()
    }

    fn handle_call(&mut self, new_ast: &mut Ast, call_expr: &CallExpr) {
        let Expr::Symbol(name) = &call_expr.callee.expr else {
            return;
        };
        if name != "quote" {
            return;
        }
        for spanned in call_expr.args.iter() {
            self.walk_expr(new_ast, spanned);
        }
    }

    fn handle_var(&mut self, _: &mut Ast, _: &VarExpr) {
        todo!()
    }

    fn handle_quote(&mut self, _: &mut Ast, _: &QuoteExpr) {
        todo!("handle_quote in macro_expansion.rs")
    }

    fn handle_if_else(&mut self, _: &mut Ast, _: &IfElseExpr) {
        todo!()
    }

    fn handle_loop(&mut self, _: &mut Ast, _: &LoopExpr) {
        todo!()
    }

    fn handle_bool(&mut self, _: &mut Ast, _: bool) {
        todo!()
    }

    fn handle_string(&mut self, _: &mut Ast, _: &str) {
        todo!()
    }

    fn handle_number(&mut self, _: &mut Ast, _: f64) {
        todo!()
    }

    fn handle_symbol(&mut self, new_ast: &mut Ast, name: &str, span: Span) {
        new_ast.push(Spanned::from((Expr::Symbol(name.to_string()), span)));
    }

    fn handle_keyword(&mut self, _: &mut Ast, _: &str, _: Span) {
        todo!()
    }
}
