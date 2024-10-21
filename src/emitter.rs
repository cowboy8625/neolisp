use super::{
    ast::{Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, VarExpr,
    },
    instruction::{Instruction, Value},
    symbol_table::{
        Function, Lambda, Let, Parameter, Symbol, SymbolTable, UnboundVariable, Variable,
    },
    BUILTINS,
};

#[derive(Debug, Default)]
pub struct EmitterOptions {
    pub no_main: bool,
}

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
    options: EmitterOptions,
    offset: usize,
    errors: Vec<Error>,
}

impl<'a> Emitter<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable, options: EmitterOptions) -> Self {
        symbol_table.enter_scope("global");
        Self {
            symbol_table,
            lambda_counter: 0,
            options,
            offset: 0,
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
        match symbol {
            Symbol::UnboundVariable(UnboundVariable { .. }) => {
                program.push(Instruction::SetFree);
            }
            Symbol::Variable(Variable { .. }) => {
                if symbol.is_global() {
                    program.push(Instruction::SetGlobal);
                } else {
                    program.push(Instruction::SetLocal);
                }
            }
            Symbol::Parameter(Parameter { .. }) => program.push(Instruction::SetLocal),
            Symbol::Function(Function { .. }) => todo!(),
            Symbol::Lambda(Lambda { .. }) => todo!(),
            Symbol::Let(Let { .. }) => todo!(),
        }
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

    fn handle_builtin(&mut self, program: &mut Program, name: &str, args: &[Spanned<Expr>]) {
        const ARGS: usize = 1;
        for arg in args.iter().skip(ARGS) {
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

        if !function.params.expr.is_list() {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        for spanned in function.body.iter() {
            self.walk_expr(program, spanned);
        }

        if name == "main" {
            program.push(Instruction::Halt);
        } else {
            program.push(Instruction::Return);
        }

        self.symbol_table.exit_scope();

        let body_size = self.get_program_size(program) - start;
        program[index] = Instruction::Jump(start + body_size);

        program.push(Instruction::Push(Box::new(Value::Callable(start))));
        program.push(Instruction::SetGlobal);
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
            program.push(Instruction::GetLocal(param.id));
            program.push(Instruction::SetFree);
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

        program.push(Instruction::Push(Box::new(Value::Callable(start))));
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
            program.push(Instruction::GetLocal(param.id));
            program.push(Instruction::SetFree);
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

        if let Expr::Symbol(name) = &call.callee.expr {
            let Some(symbol) = self.symbol_table.get(name) else {
                program.push(Instruction::Call(call.args.len()));
                return;
            };

            if self.symbol_table.get_scope_name() != *name {
                program.push(Instruction::Call(call.args.len()));
                return;
            }
            if !symbol.is_recursive() {
                program.push(Instruction::Call(call.args.len()));
                return;
            }
            eprintln!(
                "{} {} {}",
                symbol.is_recursive(),
                name,
                self.symbol_table.get_scope_name()
            );
            program.push(Instruction::TailCall(call.args.len()));
            return;
        }
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

        let instruction = if symbol.is_global() {
            Instruction::SetGlobal
        } else {
            Instruction::SetLocal
        };

        program.push(instruction);
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

        self.walk_expr(program, r#loop.body);
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
        let Some(symbol) = self.symbol_table.get(name) else {
            self.error(Error::SymbolNotDefined(span, name.to_string()));
            return;
        };
        match symbol {
            Symbol::UnboundVariable(UnboundVariable { id, .. }) => {
                program.push(Instruction::GetFree(*id));
            }
            Symbol::Variable(Variable { id, .. }) => {
                // OK This is where you left off.
                // You need some kind of id or index of how many variables are in scope so we
                // know where to index.
                if symbol.is_global() {
                    program.push(Instruction::GetGlobal(*id));
                } else {
                    program.push(Instruction::GetLocal(*id));
                }
            }
            Symbol::Parameter(Parameter { id, .. }) => {
                program.push(Instruction::GetLocal(*id));
            }
            Symbol::Function(Function { id, .. }) => {
                program.push(Instruction::GetGlobal(*id));
            }
            Symbol::Lambda(Lambda { .. }) => todo!(),
            Symbol::Let(Let { .. }) => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::instruction::{Instruction::*, Value::*};
    use pretty_assertions::assert_eq;

    // HACK: Going to try this test once the new symbol table is in place
    #[test]
    fn test_let_binding() {
        let src = r#"
    (let (x 1)
    (let (y 2)
    (let (z 3)
    (print x y z "\n"))))"#;
        let (_, instructions) = Compiler::default()
            .no_main(true)
            .compile(src)
            .ok()
            .flatten()
            .unwrap();

        // HACK: This is the problem child
        assert_eq!(
            instructions,
            vec![
                Push(Box::new(F64(1.0))),
                SetLocal,
                GetLocal(0),
                SetFree,
                Push(Box::new(F64(2.0))),
                SetLocal,
                GetLocal(0),
                SetFree,
                Push(Box::new(F64(3.0))),
                SetLocal,
                GetFree(0),
                GetFree(1),
                GetLocal(0),
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
        let (_, instructions) = Compiler::default()
            .no_main(true)
            .compile(src)
            .ok()
            .flatten()
            .unwrap();

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
        let (_, instructions) = Compiler::default()
            .no_main(true)
            .compile(src)
            .ok()
            .flatten()
            .unwrap();

        assert_eq!(
            instructions,
            vec![
                Jump(18),
                GetLocal(1),
                GetLocal(0),
                Call(1),
                Return,
                Push(Box::new(Callable(5))),
                SetGlobal,
                Push(Box::new(F64(123.0))),
                Jump(58),
                GetLocal(0),
                Push(Box::new(F64(321.0))),
                Add(2),
                Return,
                Push(Box::new(Callable(40))),
                GetGlobal(0),
                Call(2)
            ]
        );
    }
}
