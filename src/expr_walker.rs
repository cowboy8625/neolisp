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

pub trait AstWalker<T> {
    fn get_lambda_name(&mut self) -> String;
    fn handle_operator(&mut self, _: &mut T, _: &str, _: &[Spanned<Expr>]);
    fn handle_builtin(&mut self, _: &mut T, _: &str, _: &[Spanned<Expr>]);
    fn handle_function(&mut self, _: &mut T, _: &FunctionExpr);
    fn handle_lambda(&mut self, _: &mut T, _: &LambdaExpr);
    fn handle_call(&mut self, _: &mut T, _: &CallExpr);
    fn handle_var(&mut self, _: &mut T, _: &VarExpr);
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
            "if" => todo!("if expresion not covered"),
            _ => panic!("Unknown keyword: {name}"),
        }
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
            condition: &condtion_spanned,
            body: &body_spanned,
        };
        self.handle_loop(t, &loop_expr);
    }

    fn walk_expr(&mut self, t: &mut T, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(value) => self.handle_bool(t, *value),
            Expr::String(string) => self.handle_string(t, &string),
            Expr::Symbol(symbol) => self.handle_symbol(t, &symbol),
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

// #[cfg(test)]
// mod test {
//     use super::*;
//     use crate::compiler::{
//         Chunk, IState, Stage1Callee, Stage1Data, Stage1Function, Stage1Instruction, Stage1Lambda,
//         Stage1Value,
//     };
//     use crate::parser::parser;
//     use crate::symbol_table::{
//         SymbolKind, SymbolScope, SymbolTable, SymbolTableBuilder, SymbolType,
//     };
//     use crate::vm::Direction;
//     use chumsky::prelude::Parser;
//
//     #[derive(Debug)]
//     struct Stage1Compiler<'a> {
//         symbol_table: &'a mut SymbolTable,
//         functions: Vec<Stage1Function>,
//         lambdas: Vec<Stage1Lambda>,
//         lambda_counter: usize,
//     }
//
//     impl<'a> Stage1Compiler<'a> {
//         fn new(symbol_table: &'a mut SymbolTable) -> Self {
//             Self {
//                 symbol_table,
//                 functions: Vec::new(),
//                 lambdas: Vec::new(),
//                 lambda_counter: 0,
//             }
//         }
//
//         fn compile_to_chunk(&mut self, ast: &[Spanned<Expr>]) -> Chunk {
//             let mut chunk = Chunk::new();
//             self.walk(&mut chunk, ast);
//             chunk
//         }
//
//         fn compile(mut self, ast: &[Spanned<Expr>]) -> Stage1Data {
//             let mut chunk = Chunk::new();
//             self.walk(&mut chunk, ast);
//             for function in self.functions.iter_mut() {
//                 if function.name == "main" {
//                     function.prelude.extend(chunk);
//                     break;
//                 }
//             }
//
//             Stage1Data {
//                 functions: self.functions,
//                 lambdas: self.lambdas,
//             }
//         }
//     }
//
//     impl<'a> AstWalker<Chunk> for Stage1Compiler<'a> {
//         fn get_lambda_name(&mut self) -> String {
//             let name = format!("lambda_{}", self.lambda_counter);
//             self.lambda_counter += 1;
//             name
//         }
//
//         fn handle_operator(&mut self, chunk: &mut Chunk, operator: &str, list: &[Spanned<Expr>]) {
//             const ARGS: usize = 1;
//             let Some(op) = get_operator_opcode(operator) else {
//                 // TODO: REPORT ERROR
//                 panic!("unknown operator: {}", operator);
//             };
//
//             let count = list.len() - 1;
//             for spanned in list.iter().skip(ARGS) {
//                 self.walk_expr(chunk, spanned);
//             }
//             for _ in 0..count - 1 {
//                 chunk.push(op.clone());
//             }
//         }
//
//         fn handle_builtin(&mut self, chunk: &mut Chunk, name: &str, spanned: &[Spanned<Expr>]) {
//             const ARGS: usize = 1;
//             for spanned in spanned.iter().skip(ARGS) {
//                 self.walk_expr(chunk, spanned);
//             }
//             chunk.push(Stage1Instruction::Call(
//                 Stage1Callee::Builtin(name.to_string()),
//                 (spanned.len() - 1) as u8,
//             ));
//         }
//
//         fn handle_function(&mut self, _: &mut Chunk, function: &FunctionExpr) {
//             let Expr::Symbol(name) = &function.name.expr else {
//                 // TODO: REPORT ERROR
//                 panic!(
//                     "expected symbol for function name but found {:?}",
//                     function.name.expr
//                 );
//             };
//
//             self.symbol_table.enter_scope(name);
//
//             let Expr::List(expr_params) = &function.params.expr else {
//                 // TODO: REPORT ERROR
//                 panic!(
//                     "expected list for params but found {:?}",
//                     &function.params.expr
//                 );
//             };
//
//             let mut params = Chunk::new();
//             for param in expr_params.iter() {
//                 let Expr::Symbol(_) = &param.expr else {
//                     panic!("expected symbol for param");
//                 };
//                 params.push(Stage1Instruction::Rot);
//                 params.push(Stage1Instruction::LoadLocal);
//             }
//
//             let mut body = Chunk::new();
//             self.walk_expr(&mut body, &function.body);
//
//             if name == "main" {
//                 body.push(Stage1Instruction::Halt);
//             } else {
//                 body.push(Stage1Instruction::Rot);
//                 body.push(Stage1Instruction::Return);
//             }
//
//             self.symbol_table.exit_scope();
//
//             self.functions.push(Stage1Function {
//                 name: name.to_string(),
//                 prelude: Chunk::new(),
//                 params,
//                 body,
//             });
//         }
//
//         fn handle_lambda(&mut self, chunk: &mut Chunk, lambda: &LambdaExpr) {
//             let name = self.get_lambda_name();
//
//             let Expr::List(expr_params) = &lambda.params.expr else {
//                 // TODO: REPORT ERROR
//                 panic!(
//                     "expected list for params but found {:?}",
//                     lambda.params.expr
//                 );
//             };
//
//             let mut params = Vec::new();
//             for param in expr_params.iter() {
//                 let Expr::Symbol(_) = &param.expr else {
//                     panic!("expected symbol for param");
//                 };
//                 params.push(Stage1Instruction::Rot);
//                 params.push(Stage1Instruction::LoadLocal);
//             }
//
//             let mut body = Chunk::new();
//             self.symbol_table.enter_scope(&name);
//             self.walk_expr(&mut body, &lambda.body);
//             self.symbol_table.exit_scope();
//
//             let Some(local_scope) = self.symbol_table.get_function_scope(&name) else {
//                 // TODO: REPORT ERROR
//                 panic!("no scope for {name:?}");
//             };
//
//             for (name, symbol) in local_scope.iter() {
//                 if symbol.kind == SymbolKind::FreeVariable {
//                     let instruction = if symbol.scope == SymbolScope::Global {
//                         Stage1Instruction::GetGlobal(IState::Unset(name.to_string()))
//                     } else {
//                         Stage1Instruction::GetLocal(IState::Unset(name.to_string()))
//                     };
//                     chunk.push(instruction);
//                     chunk.push(Stage1Instruction::LoadFree);
//                 }
//             }
//
//             body.push(Stage1Instruction::Rot);
//             body.push(Stage1Instruction::Return);
//
//             self.symbol_table.exit_scope();
//
//             chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
//                 IState::Unset(name.to_string()),
//             )));
//
//             self.lambdas.push(Stage1Lambda {
//                 name: name.to_string(),
//                 params: Chunk::from(params),
//                 body,
//             });
//         }
//
//         fn handle_var(&mut self, chunk: &mut Chunk, var: &VarExpr) {
//             let Expr::Symbol(name) = &var.name.expr else {
//                 // TODO: REPORT ERROR
//                 panic!("var name must be a symbol but found {:?}", var.name.expr);
//             };
//
//             self.walk_expr(chunk, var.body);
//
//             let Some(symbol) = self.symbol_table.lookup(name) else {
//                 // TODO: REPORT ERROR
//                 panic!("unknown symbol: {}", name);
//             };
//
//             let instruction = if symbol.scope == SymbolScope::Global {
//                 Stage1Instruction::LoadGlobal
//             } else {
//                 Stage1Instruction::LoadLocal
//             };
//
//             chunk.push(instruction);
//         }
//
//         fn handle_loop(&mut self, chunk: &mut Chunk, r#loop: &LoopExpr) {
//             let mut chunk_condition = Chunk::new();
//             self.walk_expr(&mut chunk_condition, r#loop.condition);
//
//             let mut chunk_body = Chunk::new();
//             self.walk_expr(&mut chunk_body, r#loop.body);
//
//             let body_offset = chunk_body.size();
//             let start_offset = body_offset
//                 + chunk_condition.size()
//                 + Stage1Instruction::JumpIf(0).size()
//                 + Stage1Instruction::Jump(Direction::Backward(0)).size();
//             chunk.extend(chunk_condition);
//             chunk.push(Stage1Instruction::JumpIf(body_offset));
//             chunk.extend(chunk_body);
//             chunk.push(Stage1Instruction::Jump(Direction::Backward(start_offset)));
//         }
//
//         fn handle_symbol(&mut self, chunk: &mut Chunk, name: &str) {
//             let Some(symbol) = self.symbol_table.lookup(name) else {
//                 // TODO: REPORT ERROR
//                 panic!("Variable `{}` is not defined", name,);
//             };
//
//             match symbol.kind {
//                 SymbolKind::Parameter => {
//                     chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
//                 }
//                 SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
//                     chunk.push(Stage1Instruction::GetGlobal(IState::Set(symbol.id)));
//                 }
//                 SymbolKind::Variable if symbol.scope == SymbolScope::Function => {
//                     chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
//                 }
//                 SymbolKind::Variable => todo!("Variable: {}, id: {}", name, symbol.id),
//                 SymbolKind::Function => {
//                     chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
//                         IState::Unset(name.to_string()),
//                     )));
//                 }
//                 // SymbolKind::Test => todo!("Test {}", name),
//                 SymbolKind::Lambda => todo!("Lambda {}", name),
//                 SymbolKind::FreeVariable => {
//                     chunk.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
//                 }
//             }
//         }
//
//         fn handle_bool(&mut self, chunk: &mut Chunk, value: bool) {
//             chunk.push(Stage1Instruction::Push(Stage1Value::Bool(value)));
//         }
//
//         fn handle_string(&mut self, chunk: &mut Chunk, value: &str) {
//             chunk.push(Stage1Instruction::Push(Stage1Value::String(
//                 value.to_string(),
//             )));
//         }
//
//         fn handle_number(&mut self, chunk: &mut Chunk, value: f64) {
//             chunk.push(Stage1Instruction::Push(Stage1Value::F64(value)));
//         }
//     }
//
//     fn get_operator_opcode(op: &str) -> Option<Stage1Instruction> {
//         // NOTE: When implementing a new operator if this step is skipped the compiler will crash
//         // here reminding you to add the new operator to this list. ONLY if you added the operator
//         // to the OPERATORS list in main.rs
//         match op {
//             "+" => Some(Stage1Instruction::Add),
//             "-" => Some(Stage1Instruction::Sub),
//             "*" => Some(Stage1Instruction::Mul),
//             "/" => Some(Stage1Instruction::Div),
//             "=" => Some(Stage1Instruction::Eq),
//             ">" => Some(Stage1Instruction::GreaterThan),
//             "<" => Some(Stage1Instruction::LessThan),
//             ">=" => Some(Stage1Instruction::GreaterThanOrEqual),
//             "<=" => Some(Stage1Instruction::LessThanOrEqual),
//             "and" => Some(Stage1Instruction::And),
//             "or" => Some(Stage1Instruction::Or),
//             "not" => Some(Stage1Instruction::Not),
//             "mod" => Some(Stage1Instruction::Mod),
//             _ => None,
//         }
//     }
//
//     #[test]
//     fn test_walk() {
//         let ast = parser()
//             .parse(
//                 r#"
//             (loop true (+ 123 321))
//             "#,
//             )
//             .unwrap();
//         let mut table = SymbolTableBuilder::default().build(&ast);
//         // let data = Stage1Compiler::new(&mut table).compile(&ast);
//         // eprintln!("{:#?}", data);
//         let chunk = Stage1Compiler::new(&mut table).compile_to_chunk(&ast);
//         eprintln!("{:#?}", chunk);
//
//         assert_eq!(table, SymbolTable::new());
//     }
// }
