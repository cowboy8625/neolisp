use super::ir::{Function, If, Ir, Lambda, LookupTable, Operator, Scope, Test, Value};
use super::Header;
use crate::ast::{Expr, Spanned};
use crate::symbol_table::{SymbolTable, SymbolWalker};
use crate::vm::OpCode;

const OPERATORS: &[&str] = &["+", "="];
const BUILTINS: &[&str] = &["print", "nth", "length", "assert-eq", "list", "cons", "car"];

#[derive(Debug)]
pub(crate) struct Compiler {
    pub header: Option<Header>,
    pub functions: Vec<Function>,
    pub tests: Vec<Test>,
    pub lookup_table: LookupTable,
    pub test_lookup_table: Vec<(String, u32)>,
    pub symbol_table: SymbolTable,
    pub offset: u32,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            header: Some(Header::default()),
            functions: Vec::new(),
            tests: Vec::new(),
            lookup_table: LookupTable::new(),
            test_lookup_table: Vec::new(),
            offset: Header::SIZE,
            symbol_table: SymbolTable::default(),
        }
    }
}

impl Compiler {
    pub fn new(header: Option<Header>) -> Self {
        let offset = if header.is_some() { Header::SIZE } else { 0 };
        Self {
            header,
            offset,
            ..Default::default()
        }
    }

    pub fn compile(mut self, ast: &[Spanned<Expr>]) -> Result<Vec<u8>, Vec<String>> {
        // TODO: Handle errors
        self.symbol_table = SymbolWalker::default().walk(&ast).unwrap();
        let mut global_ir_code = self.generate_ir_code(ast);
        self.create_lookup_table(&global_ir_code);
        self.append_ir_code_to_main(&mut global_ir_code);
        let mut program = Vec::new();

        for test in self.tests.iter() {
            let bytecode = test.to_bytecode(&self.lookup_table);
            debug_assert_eq!(
                bytecode.len() as u32,
                test.size(),
                "test {} size does not match its bytecode size",
                test.name
            );
            program.extend(bytecode);
        }

        for function in self.functions.iter() {
            let bytecode = function.to_bytecode(&self.lookup_table);
            // display_chunk(&bytecode);
            debug_assert_eq!(
                bytecode.len() as u32,
                function.size(),
                "function {} size does not match its bytecode size",
                function.name
            );
            program.extend(bytecode);

            if self.header.is_none() {
                continue;
            }
            if function.name == "main" {
                let Some(Scope::Global(main_offset)) = self.lookup_table.get("main").copied()
                else {
                    if self.header.is_none() {
                        continue;
                    }
                    panic!("main function not found");
                };
                let Some(header) = self.header.as_mut() else {
                    continue;
                };
                header.set_start(main_offset)
            }
        }
        // eprintln!("{:#?}", self.lookup_table);

        let header_bytes = match self.header {
            Some(h) => h.to_bytecode(),
            None => vec![],
        };

        Ok(vec![header_bytes, program].into_iter().flatten().collect())
    }

    pub fn generate_ir_code(&mut self, ast: &[Spanned<Expr>]) -> Vec<Ir> {
        let mut global_ir_code = Vec::new();
        for spanned in ast.iter() {
            self.compile_expr(&mut global_ir_code, spanned);
        }

        global_ir_code
    }

    fn create_lookup_table(&mut self, global_ir_code: &[Ir]) {
        for test in self.tests.iter() {
            self.lookup_table
                .insert(test.name.clone(), Scope::Global(self.offset));
            self.test_lookup_table
                .push((test.name.clone(), self.offset));
            let Some(symbol) = self.symbol_table.get_mut(&test.name) else {
                panic!("test `{}` not found", test.name);
            };
            symbol.location = Some(self.offset);
            self.offset += test.size();
        }

        for function in self.functions.iter() {
            self.lookup_table
                .insert(function.name.clone(), Scope::Global(self.offset));
            let test_offset = if function.name == "main" {
                self.tests
                    .iter()
                    .fold(0, |acc, test| acc + test.call_size())
            } else {
                0
            };

            let Some(symbol) = self.symbol_table.get_mut(&function.name) else {
                panic!("function `{}` not found", function.name);
            };
            symbol.location = Some(self.offset);

            self.offset += function.size() + test_offset;
        }

        let mut global_var_index = 0;
        for code in global_ir_code.iter() {
            if let Ir::LoadGlobalVar(name) = &code {
                self.lookup_table
                    .insert(name.clone(), Scope::Global(global_var_index));

                let Some(symbol) = self.symbol_table.get_mut(name) else {
                    panic!("global variable `{}` not found", name);
                };
                symbol.location = Some(self.offset);
                global_var_index += 1;
            }
        }
    }

    fn append_ir_code_to_main(&mut self, ir_code: &mut Vec<Ir>) {
        for (name, value) in self.test_lookup_table.iter() {
            ir_code.push(Ir::LoadTest(name.clone(), *value));
        }

        if self.header.is_some() {
            let Some(main) = self.functions.iter_mut().find(|f| f.name == "main") else {
                panic!("main function not found");
            };
            for instruction in ir_code.iter().rev() {
                main.instruction.insert(0, instruction.clone());
            }
        }
    }

    fn compile_expr(&mut self, ir_code: &mut Vec<Ir>, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(v) => ir_code.push(Ir::Value(Value::Bool(*v))),
            Expr::String(v) => ir_code.push(Ir::Value(Value::String(v.clone()))),
            Expr::Symbol(v) => match v.as_str() {
                _ => ir_code.push(Ir::Value(Value::Id(v.clone()))),
            },
            Expr::Number(v) => ir_code.push(Ir::Value(Value::F64(*v))),
            Expr::List(v) => self.compile_s_expr(ir_code, v),
            Expr::Builtin(_, _) => unreachable!(),
            Expr::Func(_) => unreachable!(),
            Expr::Lambda(_) => unreachable!(),
        }
    }

    fn compile_s_expr(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
        match s_expr.first() {
            Some(spanned) => match &spanned.expr {
                Expr::Symbol(v) if v.as_str() == "fn" => self.compile_function(s_expr),
                Expr::Symbol(v) if v.as_str() == "var" => self.compile_var(ir_code, s_expr),
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

                    ir_code.push(Ir::CallLambda(args))
                }
                _ => self.compile_expr(ir_code, spanned),
            },
            None => {}
        }
    }

    fn compile_lambda(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
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
        let mut body = Vec::new();
        for spanned in s_expr.iter().skip(2) {
            self.compile_expr(&mut body, spanned);
        }
        body.push(Ir::Return);
        ir_code.push(Ir::Lambda(Lambda { params, body }));
    }

    fn compile_operator(
        &mut self,
        ir_code: &mut Vec<Ir>,
        s_expr: &[Spanned<Expr>],
        operator: &str,
    ) {
        let mut args = Vec::new();
        for spanned in s_expr.iter().skip(1) {
            self.compile_expr(&mut args, spanned);
        }
        ir_code.push(Ir::Operator(Operator::from(operator), args));
    }

    fn compile_if(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
        if (s_expr.len() != 4) {
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

        ir_code.push(Ir::If(If {
            condition: condition_ir,
            then_block: then_ir,
            else_block: else_ir,
        }));
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
        ir_code.push(Ir::Return);

        self.tests.push(Test {
            name,
            instruction: ir_code,
        });
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
            instruction.push(Ir::Halt);
        } else {
            instruction.push(Ir::Return);
        }

        let function = Function {
            name: name.to_string(),
            params,
            instruction,
        };

        self.functions.push(function);
    }

    fn compile_var(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(1)
        else {
            panic!("expected variable name");
        };
        for spanned in s_expr.iter().skip(2) {
            self.compile_expr(ir_code, spanned);
        }
        ir_code.push(Ir::LoadGlobalVar(name.to_string()));
    }

    fn compile_call(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(0)
        else {
            panic!("expected function name");
        };

        let args = s_expr.iter().skip(1).fold(Vec::new(), |mut acc, s| {
            self.compile_expr(&mut acc, s);
            acc
        });

        if BUILTINS.contains(&name.as_str()) {
            ir_code.push(Ir::BuiltIn(name.clone(), args));
            return;
        }
        ir_code.push(Ir::Call(name.clone(), args))
    }
}
