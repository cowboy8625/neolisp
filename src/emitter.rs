use crate::instruction::RuntimeMetadata;
use crate::{compiler::CompilerOptions, expr_walker::SetExpr};

use super::{
    ast::{Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, QuoteExpr, TestExpr, VarExpr,
    },
    instruction::{Callable, Instruction, Value},
    symbol_table::{
        Function, Lambda, Let, Parameter, Symbol, SymbolTable, Test, UnboundVariable, Variable,
    },
    BUILTINS,
};

type Program = Vec<Instruction>;

trait ProgramSize {
    fn program_size(&self) -> usize;
}

impl ProgramSize for Program {
    fn program_size(&self) -> usize {
        self.iter().map(|i| i.size()).sum::<usize>()
    }
}

#[derive(Debug)]
pub struct Emitter<'a> {
    symbol_table: &'a mut SymbolTable,
    lambda_counter: usize,
    options: CompilerOptions,
    offset: usize,
    tests: Vec<(String, Span)>,
    errors: Vec<Error>,
}

impl<'a> Emitter<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable, options: CompilerOptions) -> Self {
        symbol_table.enter_scope("global");
        Self {
            symbol_table,
            lambda_counter: 0,
            options,
            offset: 0,
            tests: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn with_offset(mut self, offset: usize) -> Self {
        self.offset = offset;
        self
    }

    pub fn compile(
        mut self,
        ast: &[Spanned<Expr>],
    ) -> std::result::Result<Vec<Instruction>, Vec<Error>> {
        let mut program = Vec::new();
        self.walk(&mut program, ast);
        if !self.options.no_main {
            let Some(symbol) = self.symbol_table.get("main") else {
                self.error(Error::MainNotDefined);
                return Err(self.errors);
            };
            let Some(location) = symbol.get_location() else {
                unreachable!("Main function location should always be set at this point");
            };
            program.push(Instruction::Jump(location));
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        self.symbol_table.exit_scope();

        self.compile_tests(&mut program)?;
        Ok(program)
    }

    fn get_operator_opcode(op: &str, count: usize) -> Option<Instruction> {
        // NOTE: When implementing a new operator if this step is skipped the compiler will crash
        // here reminding you to add the new operator to this list. ONLY if you added the operator
        // to the OPERATORS list in lib.rs
        match op {
            "+" => Some(Instruction::Add(count)),
            "-" => Some(Instruction::Sub(count)),
            "*" => Some(Instruction::Mul(count)),
            "/" => Some(Instruction::Div(count)),
            "=" => Some(Instruction::Eq(count)),
            ">" => Some(Instruction::GreaterThan(count)),
            "<" => Some(Instruction::LessThan(count)),
            ">=" => Some(Instruction::GreaterThanOrEqual(count)),
            "<=" => Some(Instruction::LessThanOrEqual(count)),
            "and" => Some(Instruction::And(count)),
            "or" => Some(Instruction::Or(count)),
            "not" => Some(Instruction::Not),
            "mod" => Some(Instruction::Mod),
            _ => None,
        }
    }

    fn get_program_size(&self, program: &Program) -> usize {
        self.offset + program.program_size()
    }

    fn emit_set_instruction(program: &mut Program, symbol: &Symbol) {
        use Instruction::*;
        let metadata = RuntimeMetadata::new(symbol.id(), symbol.name());
        let instruction = match symbol {
            Symbol::UnboundVariable(UnboundVariable { .. }) => SetFree(metadata),
            Symbol::Variable(Variable { .. }) if symbol.is_global() => SetGlobal(metadata),
            Symbol::Variable(Variable { .. }) => SetLocal(metadata),
            Symbol::Parameter(Parameter { .. }) => SetLocal(metadata),
            Symbol::Function(Function { .. }) => {
                todo!("Function not implemented in emit_set_instruction")
            }
            Symbol::Lambda(Lambda { .. }) => {
                todo!("Lambda not implemented in emit_set_instruction")
            }
            Symbol::Let(Let { .. }) => todo!("Let not implemented in emit_set_instruction"),
            Symbol::Test(Test { .. }) => todo!("Test not implemented in emit_set_instruction"),
        };
        program.push(instruction);
    }

    fn tail_call_optimization(&self, program: &mut Program, name: &str) {
        use Instruction::*;
        let Some(symbol) = self.symbol_table.get(name) else {
            return;
        };

        if self.symbol_table.get_scope_name() != *name {
            return;
        }

        if !symbol.is_recursive() {
            return;
        }

        let end = program.len();

        let [GetGlobal(value) | GetLocal(value), Call(count), Return] = &program[end - 3..end]
        else {
            return;
        };

        if value.data != symbol.id() {
            return;
        }

        program[end - 2] = Instruction::TailCall(*count);
    }

    fn compile_tests(
        &mut self,
        program: &mut Vec<Instruction>,
    ) -> std::result::Result<(), Vec<Error>> {
        if !self.options.test {
            return Ok(());
        }
        for (name, span) in self.tests.clone().into_iter() {
            let Some(symbol) = self.symbol_table.get(&name) else {
                // NOTE: This probably will never happen.
                self.error(Error::TestNotDefined { name, span });
                continue;
            };
            let metadata = RuntimeMetadata::new(symbol.id(), name);
            program.push(Instruction::GetGlobal(metadata));
            program.push(Instruction::CallTest);
        }
        Ok(())
    }
}

impl AstWalker<Program> for Emitter<'_> {
    fn error(&mut self, error: Error) {
        self.errors.push(error);
    }

    fn get_lambda_name(&mut self) -> String {
        let name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    fn handle_operator(
        &mut self,
        program: &mut Program,
        operator_name: &str,
        oper_expr: &OperatorExpr,
    ) {
        for arg in oper_expr.args.iter() {
            self.walk_expr(program, arg);
        }
        let Some(instruction) = Self::get_operator_opcode(operator_name, oper_expr.args.len())
        else {
            // NOTE: INTENTIONAL_PANIC
            panic!("Unknown operator: {operator_name}");
        };

        program.push(instruction);
    }

    fn handle_test(&mut self, program: &mut Program, test_expr: &TestExpr) {
        if !self.options.test {
            return;
        }

        program.push(Instruction::Jump(usize::MAX));
        let index = program.len() - 1;
        let address = self.get_program_size(program);

        let name = format!("test(){}", test_expr.name);
        self.symbol_table.enter_scope(&name);

        let Some(id) = self.symbol_table.get(&name).map(|s| s.id()) else {
            // NOTE: This probably will never happen.
            self.error(Error::TestNotDefined {
                name: name.clone(),
                span: test_expr.span.clone(),
            });
            return;
        };

        for spanned in test_expr.body.iter() {
            self.walk_expr(program, spanned);
        }

        program.push(Instruction::ReturnFromTest);

        self.symbol_table.exit_scope();
        let body_size = self.get_program_size(program) - address;
        program[index] = Instruction::Jump(address + body_size);
        let callable = Callable::new(address, name.to_string());
        let value = Value::Callable(Box::new(callable));
        program.push(Instruction::Push(Box::new(value)));
        let metadata = RuntimeMetadata::new(id, &name);
        program.push(Instruction::SetGlobal(metadata));

        self.tests.push((name, test_expr.span.clone()));
    }

    fn handle_builtin(&mut self, program: &mut Program, name: &str, args: &[Spanned<Expr>]) {
        const ARGS: usize = 1;
        for arg in args.iter().skip(ARGS).rev() {
            self.walk_expr(program, arg);
        }
        let Some(id) = BUILTINS.iter().position(|b| b == &name) else {
            // NOTE: INTENTIONAL_PANIC
            panic!("Unknown builtin: {name}");
        };
        program.push(Instruction::Push(Box::new(Value::Builtin(id))));
        program.push(Instruction::Call(args.len() - ARGS));
    }

    fn handle_function(&mut self, program: &mut Program, function: &FunctionExpr) {
        program.push(Instruction::Jump(usize::MAX));
        let index = program.len() - 1;
        let start = self.get_program_size(program);
        let Expr::Symbol(name) = &function.name.expr else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        self.symbol_table.set_location(name, start);

        self.symbol_table.enter_scope(name);

        let Some(id) = self.symbol_table.get(name).map(|s| s.id()) else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        if !function.params.expr.is_list() {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        let scope_name = self.symbol_table.get_scope_name();

        let params = self
            .symbol_table
            .get(&scope_name)
            .map(|s| s.get_parameters())
            .unwrap_or_default();

        for param in params.iter() {
            let Some(symbol) = self.symbol_table.get(&param.name) else {
                panic!("Variable `{}` should be known at this point", param.name);
            };
            let Symbol::Parameter(Parameter {
                is_unbound: true, ..
            }) = symbol
            else {
                continue;
            };
            let metadata = RuntimeMetadata::new(param.id, &param.name);
            program.push(Instruction::GetLocal(metadata.clone()));
            program.push(Instruction::SetFree(metadata));
        }

        for spanned in function.body.iter() {
            self.walk_expr(program, spanned);
        }

        if name == "main" {
            program.push(Instruction::Halt);
        } else {
            program.push(Instruction::Return);
        }

        self.tail_call_optimization(program, name);

        self.symbol_table.exit_scope();

        let body_size = self.get_program_size(program) - start;
        program[index] = Instruction::Jump(start + body_size);

        let callable = Callable::new(start, name.to_string());
        let value = Value::Callable(Box::new(callable));
        program.push(Instruction::Push(Box::new(value)));
        let metadata = RuntimeMetadata::new(id, name);
        program.push(Instruction::SetGlobal(metadata));
    }

    fn handle_lambda(&mut self, program: &mut Program, lambda: &LambdaExpr) {
        program.push(Instruction::Jump(usize::MAX));
        let index = program.len() - 1;
        let start = self.get_program_size(program);
        let name = self.get_lambda_name();
        self.symbol_table.set_location(&name, start);

        self.symbol_table.enter_scope(&name);

        let scope_name = self.symbol_table.get_scope_name();

        let params = self
            .symbol_table
            .get(&scope_name)
            .map(|s| s.get_parameters())
            .unwrap_or_default();

        for param in params.iter() {
            let Some(symbol) = self.symbol_table.get(&param.name) else {
                panic!("Variable `{}` should be known at this point", param.name,);
            };
            let Symbol::Parameter(Parameter {
                is_unbound: true, ..
            }) = symbol
            else {
                continue;
            };
            let metadata = RuntimeMetadata::new(param.id, &param.name);
            program.push(Instruction::GetLocal(metadata.clone()));
            program.push(Instruction::SetFree(metadata));
        }

        if !lambda.params.expr.is_list() {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        for spanned in lambda.body.iter() {
            self.walk_expr(program, spanned);
        }
        program.push(Instruction::Return);

        self.symbol_table.exit_scope();

        let body_size = self.get_program_size(program) - start;

        program[index] = Instruction::Jump(start + body_size);

        let callable = Callable::new(start, name);
        let value = Value::Callable(Box::new(callable));
        program.push(Instruction::Push(Box::new(value)));
    }

    fn handle_let_binding(&mut self, program: &mut Program, let_binding: &LetBindingExpr) {
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
        self.symbol_table.enter_scope(&scope_name);

        for spanned in let_binding.bindings.iter() {
            let Expr::List(list) = &spanned.expr else {
                unreachable!("This should never fail as we already checked this in AstWalker");
            };
            let Some(binding_expression) = list.get(1) else {
                unreachable!("This should never fail as we already checked this in AstWalker");
            };
            self.walk_expr(program, binding_expression);

            let Expr::Symbol(binding_name) = &list[0].expr else {
                unreachable!("This should never fail as we already checked this in AstWalker");
            };
            let Some(symbol) = self.symbol_table.get(binding_name) else {
                // NOTE: INTENTIONAL_PANIC
                panic!("expected symbol in let binding, got: {list:?}");
            };
            Self::emit_set_instruction(program, symbol);
        }

        let scope_name = self.symbol_table.get_scope_name();

        let params = self
            .symbol_table
            .get(&scope_name)
            .map(|s| s.get_parameters())
            .unwrap_or_default();

        for param in params.iter() {
            let Some(symbol) = self.symbol_table.get(&param.name) else {
                panic!("Variable `{}` should be known at this point", param.name,);
            };
            let Symbol::Parameter(Parameter {
                is_unbound: true, ..
            }) = symbol
            else {
                continue;
            };
            let metadata = RuntimeMetadata::new(param.id, &param.name);
            program.push(Instruction::GetLocal(metadata.clone()));
            program.push(Instruction::SetFree(metadata));
        }

        self.walk_expr(program, let_binding.body);
        // for spanned in lambda.body.iter() {
        //     self.walk_expr(program, spanned);
        // }

        self.symbol_table.exit_scope();
    }

    fn handle_call(&mut self, program: &mut Program, call: &CallExpr) {
        for arg in call.args.iter().rev() {
            self.walk_expr(program, arg);
        }

        self.walk_expr(program, call.callee);
        program.push(Instruction::Call(call.args.len()));
    }

    fn handle_var(&mut self, program: &mut Program, var: &VarExpr) {
        let Expr::Symbol(name) = &var.name.expr else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        self.walk_expr(program, var.body);

        let Some(symbol) = self.symbol_table.get(name) else {
            // NOTE: INTENTIONAL_PANIC
            panic!("unknown symbol: {}", name);
        };

        let metadata = RuntimeMetadata::new(symbol.id(), name);
        let set_instruction = if symbol.is_global() {
            Instruction::SetGlobal(metadata.clone())
        } else {
            Instruction::SetLocal(metadata.clone())
        };

        program.push(set_instruction);

        if !symbol.is_recursive() {
            return;
        }

        let get_instruction = if symbol.is_global() {
            Instruction::GetGlobal(metadata.clone())
        } else {
            Instruction::GetLocal(metadata.clone())
        };
        program.push(get_instruction);
        program.push(Instruction::SetFree(metadata));
    }

    fn handle_set(&mut self, program: &mut Program, set: &SetExpr) {
        let Expr::Symbol(name) = &set.name.expr else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        self.walk_expr(program, set.body);

        let Some(symbol) = self.symbol_table.get(name) else {
            // NOTE: Probably need to have a user error message
            panic!("unknown symbol: {}", name);
        };

        Self::emit_set_instruction(program, symbol);
    }

    fn handle_quote(&mut self, program: &mut Program, quote: &QuoteExpr) {
        fn emit_values(values: &mut Vec<Value>, spanned: &Spanned<Expr>) {
            match &spanned.expr {
                Expr::Bool(b) => {
                    values.push(Value::Bool(*b));
                }
                Expr::String(string) => {
                    values.push(Value::String(Box::new(string.clone())));
                }
                Expr::Symbol(value) if value.starts_with(':') => {
                    values.push(Value::Keyword(Box::new(value.clone())));
                }
                Expr::Symbol(value) => {
                    values.push(Value::Symbol(Box::new(value.clone())));
                }
                Expr::Number(number) => {
                    values.push(Value::F64(*number));
                }
                Expr::List(vec) => {
                    let mut list = Vec::new();
                    for spanned in vec.iter() {
                        emit_values(&mut list, spanned);
                    }
                    values.push(Value::List(Box::new(list)));
                }
            }
        }
        let mut values = Vec::new();
        emit_values(&mut values, quote.expr);

        for value in values.into_iter() {
            program.push(Instruction::Push(Box::new(value)));
        }
    }

    fn handle_if_else(&mut self, program: &mut Program, if_else: &IfElseExpr) {
        self.walk_expr(program, if_else.condition);

        let mut then_chunk = Vec::new();
        self.walk_expr(&mut then_chunk, if_else.then);
        let then_offset = self.get_program_size(&then_chunk) + Instruction::JumpForward(0).size();

        let mut else_chunk = Vec::new();
        if let Some(else_spanned) = if_else.otherwise.as_ref() {
            self.walk_expr(&mut else_chunk, else_spanned);
        };
        let else_offset = self.get_program_size(&else_chunk);
        program.push(Instruction::JumpIf(then_offset));
        program.extend(then_chunk);
        program.push(Instruction::JumpForward(else_offset));
        program.extend(else_chunk);
    }

    fn handle_loop(&mut self, program: &mut Program, r#loop: &LoopExpr) {
        let start = self.get_program_size(program);
        self.walk_expr(program, r#loop.condition);
        let jump_instruction_index = program.len();
        program.push(Instruction::JumpIf(usize::MAX));

        for stmt in r#loop.body.iter() {
            self.walk_expr(program, stmt);
        }
        program.push(Instruction::Jump(start));

        program[jump_instruction_index] = Instruction::JumpIf(self.get_program_size(program));
    }

    fn handle_bool(&mut self, program: &mut Program, b: bool) {
        let value = Value::Bool(b);
        program.push(Instruction::Push(Box::new(value)));
    }

    fn handle_string(&mut self, program: &mut Program, string: &str) {
        let value = Value::String(Box::new(string.to_string()));
        program.push(Instruction::Push(Box::new(value)));
    }

    fn handle_number(&mut self, program: &mut Program, float64: f64) {
        let value = Value::F64(float64);
        let instruction = Instruction::Push(Box::new(value));
        program.push(instruction);
    }

    fn handle_symbol(&mut self, program: &mut Program, name: &str, span: Span) {
        use Instruction::*;
        let Some(symbol) = self.symbol_table.get(name) else {
            self.error(Error::SymbolNotDefined(span, name.to_string()));
            return;
        };
        let metadata = RuntimeMetadata::new(symbol.id(), name);
        let instruction = match symbol {
            Symbol::UnboundVariable(UnboundVariable { .. }) => GetFree(metadata),
            Symbol::Variable(Variable { .. }) if symbol.is_global() => GetGlobal(metadata),
            Symbol::Variable(Variable { .. }) => GetLocal(metadata),
            Symbol::Parameter(Parameter { .. }) => GetLocal(metadata),
            Symbol::Function(Function { .. }) => GetGlobal(metadata),
            Symbol::Lambda(Lambda { .. }) => todo!("Lambdas are not supported yet in symbol"),
            Symbol::Let(Let { .. }) => todo!("Lets are not supported yet in symbol"),
            Symbol::Test(Test { .. }) => todo!("Tests are not supported yet in symbol"),
        };
        program.push(instruction);
    }

    fn handle_keyword(&mut self, program: &mut Program, name: &str, _: Span) {
        program.push(Instruction::Push(Box::new(Value::Keyword(Box::new(
            name.to_string(),
        )))));
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::instruction::{
        Callable as CallableData,
        Instruction::{self, *},
        RuntimeMetadata,
        Value::*,
    };
    use crate::symbol_table::SymbolTable;
    use pretty_assertions::assert_eq;

    fn compiler(src: &str) -> Vec<Instruction> {
        let mut st = SymbolTable::default();
        Compiler::default()
            .no_main(true)
            .compile(src, &mut st)
            .ok()
            .flatten()
            .unwrap()
    }

    fn compile_test(src: &str) -> Vec<Instruction> {
        let mut st = SymbolTable::default();
        Compiler::default()
            .no_main(true)
            .with_test(true)
            .compile(src, &mut st)
            .ok()
            .flatten()
            .unwrap()
    }

    #[test]
    fn test_let_binding() {
        let src = r#"
    (let (x 1)
    (let (y 2)
    (let (z 3)
    (print x y z "\n"))))"#;
        let instructions = compiler(src);

        assert_eq!(
            instructions,
            vec![
                Push(Box::new(F64(1.0))),
                SetLocal(RuntimeMetadata::new(0, "x")),
                GetLocal(RuntimeMetadata::new(0, "x")),
                SetFree(RuntimeMetadata::new(0, "x")),
                Push(Box::new(F64(2.0))),
                SetLocal(RuntimeMetadata::new(0, "y")),
                GetLocal(RuntimeMetadata::new(0, "y")),
                SetFree(RuntimeMetadata::new(0, "y")),
                Push(Box::new(F64(3.0))),
                SetLocal(RuntimeMetadata::new(0, "z")),
                GetFree(RuntimeMetadata::new(0, "x")),
                GetFree(RuntimeMetadata::new(1, "y")),
                GetLocal(RuntimeMetadata::new(0, "z")),
                Push(Box::new(String(Box::new("\n".to_string())))),
                Push(Box::new(Builtin(17))),
                Call(4),
            ]
        );
    }

    #[test]
    fn test_if_else() {
        let src = r#"
(if true "then\n" "else\n")
"#;
        let instructions = compiler(src);

        assert_eq!(
            instructions,
            vec![
                Push(Box::new(Bool(true))),
                JumpIf(16),
                Push(Box::new(String(Box::new("then\n".to_string())))),
                JumpForward(11),
                Push(Box::new(String(Box::new("else\n".to_string()))))
            ]
        );
    }

    // HACK: This is messed up. Some times it passes some times it doesn't.
    #[test]
    fn test_applying_function() {
        let src = r#"
 (fn apply (f x) (f x))
 (apply (lambda (x) (+ x 321)) 123)
 "#;
        let instructions = compiler(src);

        assert_eq!(
            instructions,
            vec![
                Jump(22),
                GetLocal(RuntimeMetadata::new(1, "x")),
                GetLocal(RuntimeMetadata::new(0, "f")),
                Call(1),
                Return,
                Push(Box::new(Callable(Box::new(CallableData::new(5, "apply"))))),
                SetGlobal(RuntimeMetadata::new(0, "apply")),
                Push(Box::new(F64(123.0))),
                Jump(83),
                GetLocal(RuntimeMetadata::new(0, "x")),
                Push(Box::new(F64(321.0))),
                Add(2),
                Return,
                Push(Box::new(Callable(Box::new(CallableData::new(
                    63, "lambda_0"
                ))))),
                GetGlobal(RuntimeMetadata::new(0, "apply")),
                Call(2)
            ]
        );
    }

    #[test]
    fn test_tail_call_optimization() {
        let src = r#"
(fn fib (n a b)
  (if (= n 0)
      a
      (fib (- n 1) b (+ a b))))
 "#;
        let instructions = compiler(src);

        assert_eq!(
            instructions,
            vec![
                Jump(95),
                GetLocal(RuntimeMetadata::new(0, "n")),
                Push(Box::new(F64(0.0))),
                Eq(2),
                JumpIf(12),
                GetLocal(RuntimeMetadata::new(1, "a")),
                JumpForward(53),
                GetLocal(RuntimeMetadata::new(1, "a")),
                GetLocal(RuntimeMetadata::new(2, "b")),
                Add(2),
                GetLocal(RuntimeMetadata::new(2, "b")),
                GetLocal(RuntimeMetadata::new(0, "n")),
                Push(Box::new(F64(1.0))),
                Sub(2),
                GetGlobal(RuntimeMetadata::new(0, "fib")),
                TailCall(3),
                Return,
                Push(Box::new(Callable(Box::new(CallableData::new(5, "fib"))))),
                SetGlobal(RuntimeMetadata::new(0, "fib")),
            ]
        );
    }
    #[test]
    fn test_testing() {
        let src = r#"
 (test test-testing (assert (= (+ 1 2) 3)))
 "#;
        let instructions = compile_test(src);

        assert_eq!(
            instructions,
            vec![
                Jump(48),
                Push(Box::new(F64(1.0))),
                Push(Box::new(F64(2.0))),
                Add(2),
                Push(Box::new(F64(3.0))),
                Eq(2),
                Push(Box::new(Builtin(20))),
                Call(1),
                ReturnFromTest,
                Push(Box::new(Callable(Box::new(CallableData::new(
                    5,
                    "test()test-testing"
                ))))),
                SetGlobal(RuntimeMetadata::new(0, "test()test-testing")),
                GetGlobal(RuntimeMetadata::new(0, "test()test-testing")),
                CallTest,
            ]
        );
    }
}
