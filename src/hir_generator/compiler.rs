#![allow(dead_code)]
#![allow(unused)]
use super::hir::{CompiledHir, Function, Hir, If, Lambda, Operator, Test, Value, Var};
use super::{BUILTINS, OPERATORS};
use crate::ast::{Expr, Spanned};
use crate::symbol_table::{SymbolKind, SymbolTable};

#[derive(Debug)]
pub struct Compiler {
    functions: Vec<Function>,
    lambdas: Vec<Lambda>,
    vars: Vec<Var>,
    tests: Vec<Test>,
    symbol_table: SymbolTable,
    lambda_counter: u32,
}

impl Compiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            functions: Vec::new(),
            lambdas: Vec::new(),
            vars: Vec::new(),
            tests: Vec::new(),
            symbol_table,
            lambda_counter: 0,
        }
    }

    pub fn compile(mut self, ast: &[Spanned<Expr>]) -> Result<Vec<CompiledHir>, Vec<String>> {
        let mut global_ir_code = Vec::new();

        for spanned in ast {
            self.compile_expr(&mut global_ir_code, spanned);
        }

        Ok(vec![
            CompiledHir::Functions(self.functions),
            CompiledHir::Lambdas(self.lambdas),
            CompiledHir::Tests(self.tests),
            CompiledHir::GlobalVar(self.vars),
        ])
    }

    fn compile_expr(&mut self, ir_code: &mut Vec<Hir>, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(v) => ir_code.push(Hir::Value(Value::Bool(*v))),
            Expr::String(v) => ir_code.push(Hir::Value(Value::String(v.clone()))),
            Expr::Symbol(v) => ir_code.push(Hir::Value(Value::Id(v.clone()))),
            Expr::Number(v) => ir_code.push(Hir::Value(Value::F64(*v))),
            Expr::List(v) => self.compile_s_expr(ir_code, v),
            Expr::Builtin(_, _) => unreachable!(),
            Expr::Func(_) => unreachable!(),
            Expr::Lambda(_) => unreachable!(),
        }
    }

    fn compile_s_expr(&mut self, ir_code: &mut Vec<Hir>, s_expr: &[Spanned<Expr>]) {
        match s_expr.first() {
            Some(spanned) => match &spanned.expr {
                Expr::Symbol(v) if v.as_str() == "fn" => self.compile_function(s_expr),
                Expr::Symbol(v) if v.as_str() == "var" => self.compile_var(s_expr),
                Expr::Symbol(v) if v.as_str() == "test" => self.compile_test(s_expr),
                Expr::Symbol(v) if v.as_str() == "if" => self.compile_if(ir_code, s_expr),
                Expr::Symbol(v) if v.as_str() == "lambda" => self.compile_lambda(ir_code, s_expr),
                Expr::Symbol(v) if OPERATORS.contains(&v.as_str()) => {
                    self.compile_operator(ir_code, s_expr, v.as_str())
                }
                Expr::Symbol(_) => self.compile_call(ir_code, s_expr),
                Expr::List(items) if matches!(items.first(), Some(Spanned { expr: Expr::Symbol(v), .. }) if v.as_str() == "lambda") =>
                {
                    self.compile_expr(ir_code, &spanned);
                    let args = s_expr.iter().skip(1).fold(Vec::new(), |mut acc, s| {
                        self.compile_expr(&mut acc, s);
                        acc
                    });

                    let name = format!("lambda_{}", self.lambda_counter - 1);
                    ir_code.push(Hir::Call(name, args))
                }
                _ => self.compile_expr(ir_code, spanned),
            },
            None => unreachable!(),
        }
    }

    fn compile_operator(
        &mut self,
        ir_code: &mut Vec<Hir>,
        s_expr: &[Spanned<Expr>],
        operator: &str,
    ) {
        let mut args = Vec::new();
        for spanned in s_expr.iter().skip(1) {
            self.compile_expr(&mut args, spanned);
        }
        ir_code.push(Hir::Operator(Operator::from(operator), args));
    }

    fn compile_function(&mut self, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(1)
        else {
            panic!("expected function name");
        };

        let Some(Spanned {
            expr: Expr::List(params),
            ..
        }) = s_expr.get(2)
        else {
            panic!("expected function params");
        };

        let params = params
            .iter()
            .map(|param| match &param.expr {
                Expr::Symbol(v) => v.to_string(),
                v => panic!("expected function param to be a symbols found {:?}", v),
            })
            .collect();

        let Some(Spanned {
            expr: Expr::List(body),
            ..
        }) = s_expr.get(3)
        else {
            panic!("expected function body");
        };

        let mut instruction = Vec::new();

        self.compile_s_expr(&mut instruction, body);

        if name == "main" {
            instruction.push(Hir::Halt);
        } else {
            instruction.push(Hir::Return);
        }

        let function = Function {
            name: name.to_string(),
            params,
            body: instruction,
        };

        self.functions.push(function);
    }

    fn compile_lambda(&mut self, ir_code: &mut Vec<Hir>, s_expr: &[Spanned<Expr>]) {
        debug_assert_eq!(s_expr.len(), 3, "expected lambda expression {s_expr:#?}");
        let Some(Spanned {
            expr: Expr::List(params_expr),
            ..
        }) = s_expr.get(1)
        else {
            panic!("expected lambda args {:#?}", s_expr);
        };

        let mut params = Vec::new();
        for spanned in params_expr.iter() {
            let Expr::Symbol(v) = &spanned.expr else {
                panic!("expected parameter name");
            };
            params.push(v.clone());
        }

        let id = self.lambda_counter;
        self.lambda_counter += 1;

        let mut body = Vec::new();
        for spanned in s_expr.iter().skip(2) {
            self.compile_expr(&mut body, spanned);
        }
        body.push(Hir::Return);
        let name = format!("lambda_{}", id);

        let captured: Vec<String> = get_variables(&body)
            .into_iter()
            .filter(|v| !params.contains(v))
            .collect();

        for capture in captured.iter() {
            ir_code.push(Hir::Value(Value::Id(capture.clone())));
        }

        ir_code.push(Hir::LoadLambda(name.clone()));

        self.lambdas.push(Lambda {
            name: name.to_string(),
            params: params.clone(),
            instruction: body,
            captured,
        });
    }

    fn compile_test(&mut self, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned { expr, .. }) = s_expr.get(1) else {
            panic!("expected test name");
        };
        let name = match expr {
            Expr::Symbol(v) => v.to_string(),
            Expr::String(v) => v.to_string(),
            _ => panic!(
                "expected test name either a string or symbol found {:?}",
                expr
            ),
        };

        let mut ir_code = Vec::new();
        for spanned in s_expr.iter().skip(2) {
            self.compile_expr(&mut ir_code, spanned);
        }
        ir_code.push(Hir::Return);

        self.tests.push(Test {
            name,
            instruction: ir_code,
        });
    }

    fn compile_var(&mut self, s_expr: &[Spanned<Expr>]) {
        let mut instruction = Vec::new();
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(1)
        else {
            panic!("expected variable name");
        };

        for spanned in s_expr.iter().skip(2) {
            self.compile_expr(&mut instruction, spanned);
        }

        let Some(symbol) = self.symbol_table.lookup(name) else {
            panic!("variable `{name}` not found")
        };

        instruction.push(Hir::LoadGlobal(name.to_string(), symbol.id));

        self.vars.push(Var {
            name: name.to_string(),
            instruction,
        });
    }

    fn compile_call(&mut self, ir_code: &mut Vec<Hir>, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(0)
        else {
            panic!("expected function name");
        };

        let mut args = Vec::new();
        for spanned in s_expr.iter().skip(1) {
            self.compile_expr(&mut args, spanned);
        }

        if BUILTINS.contains(&name.as_str()) {
            ir_code.push(Hir::BuiltIn(name.clone(), args));
            return;
        }
        let Some(symbol) = self.symbol_table.lookup(name) else {
            panic!("function `{name}` not found")
        };
        ir_code.push(Hir::Call(name.clone(), args))
    }

    fn compile_if(&mut self, ir_code: &mut Vec<Hir>, s_expr: &[Spanned<Expr>]) {
        if s_expr.len() != 4 {
            panic!("expected if condition, then expression and else expression");
        }
        let Some(condition) = s_expr.get(1) else {
            panic!("expected if condition");
        };

        let Some(then_expr) = s_expr.get(2) else {
            panic!("expected if then expression");
        };

        let Some(else_expr) = s_expr.get(3) else {
            panic!("expected if else expression");
        };

        let mut condition_ir = Vec::new();
        self.compile_expr(&mut condition_ir, &condition);
        let mut then_ir = Vec::new();
        self.compile_expr(&mut then_ir, &then_expr);
        let mut else_ir = Vec::new();
        self.compile_expr(&mut else_ir, &else_expr);

        ir_code.push(Hir::If(If {
            condition: condition_ir,
            then_block: then_ir,
            else_block: else_ir,
        }));
    }
}

fn get_variables(ir_code: &[Hir]) -> Vec<String> {
    let mut variables = Vec::new();
    fn visit(ir_code: &Hir, variables: &mut Vec<String>) {
        match ir_code {
            Hir::LoadGlobal(name, _) => variables.push(name.clone()),
            Hir::Operator(_, args) => {
                let v = get_variables(&args);
                variables.extend(v);
            }
            Hir::Value(value) => match value {
                Value::Id(name) => variables.push(name.clone()),
                _ => {}
            },
            Hir::If(r#if) => {
                let v = get_variables(&r#if.condition);
                variables.extend(v);
                let v = get_variables(&r#if.then_block);
                variables.extend(v);
                let v = get_variables(&r#if.else_block);
                variables.extend(v);
            }
            Hir::BuiltIn(name, args) => {
                variables.push(name.clone());
                let v = get_variables(&args);
                variables.extend(v);
            }
            Hir::Call(name, args) => {
                variables.push(name.clone());
                let v = get_variables(&args);
                variables.extend(v);
            }
            Hir::LoadTest(_, _) => todo!(),
            Hir::LoadLambda(name) => variables.push(name.clone()),
            Hir::Return => {} // nothing to do with Return
            Hir::Halt => {}   // nothing to do with Halt
        }
    }

    for ir in ir_code.iter() {
        visit(ir, &mut variables);
    }
    variables
}
