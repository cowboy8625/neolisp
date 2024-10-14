use super::{
    ast::{Expr, Span, Spanned},
    error::Error,
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, VarExpr,
    },
    instruction::{Instruction, Value},
    parser::parser,
    symbol_table::{Symbol, SymbolKind, SymbolScope, SymbolTable, SymbolTableBuilder},
    BUILTINS,
};

use chumsky::prelude::Parser;

pub fn compile(
    src: &str,
    options: CompilerOptions,
) -> std::result::Result<Vec<Instruction>, Vec<Error>> {
    if src.is_empty() {
        return Err(vec![Error::EmptyFile]);
    }
    let ast = match parser().parse(src) {
        Ok(ast) => ast,
        Err(errors) => return Err(errors),
    };

    let mut symbol_table = SymbolTableBuilder::default().build(&ast)?;
    Compiler::new(&mut symbol_table, options).compile(&ast)
}

#[derive(Debug, Default)]
pub struct CompilerOptions {
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
struct UnsetLocation {
    index: usize,
    name: String,
}

#[derive(Debug)]
pub struct Compiler<'a> {
    symbol_table: &'a mut SymbolTable,
    lambda_counter: usize,
    options: CompilerOptions,
    offset: usize,
    unset_locations: Vec<UnsetLocation>,
    errors: Vec<Error>,
}

impl<'a> Compiler<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable, options: CompilerOptions) -> Self {
        Self {
            symbol_table,
            lambda_counter: 0,
            options,
            offset: 0,
            unset_locations: Vec::new(),
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
            let Some(symbol) = self.symbol_table.lookup("main") else {
                self.error(Error::MainNotDefined);
                return Err(self.errors);
            };
            let Some(location) = symbol.location else {
                unreachable!("Main function location should always be set at this point");
            };
            program.push(Instruction::Jump(location as usize));
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        for UnsetLocation { index, name } in self.unset_locations.iter() {
            let Some(symbol) = self.symbol_table.lookup(name) else {
                unreachable!("Variable `{}` should be known at this point", name,);
            };
            let Some(location) = symbol.location else {
                unreachable!(
                    "{} {:?} location should always be set at this point",
                    name, symbol.kind
                );
            };
            let instruction = &mut program[*index];
            match instruction {
                Instruction::Push(value) => match value.as_mut() {
                    Value::Callable(i) => *i = location as usize,
                    _ => {
                        // NOTE: INTENTIONAL_PANIC
                        panic!("expected push but found {instruction:?}");
                    }
                },
                _ => {
                    // NOTE: INTENTIONAL_PANIC
                    panic!("expected push but found {instruction:?}");
                }
            }
        }
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

    fn emit_set_instruction(&mut self, program: &mut Program, symbol: &Symbol) {
        match symbol.kind {
            SymbolKind::FreeVariable => program.push(Instruction::SetFree),
            SymbolKind::Variable => match symbol.scope {
                SymbolScope::Global => program.push(Instruction::SetGlobal),
                SymbolScope::Function => program.push(Instruction::SetLocal),
                SymbolScope::Free => program.push(Instruction::SetFree),
                SymbolScope::Let => program.push(Instruction::SetLocal),
            },
            SymbolKind::Parameter => program.push(Instruction::SetLocal),
            // NOTE: Unsure if this will ever be hit.
            SymbolKind::Function => todo!("Function"),
            SymbolKind::Lambda => todo!("Lambda"),
            SymbolKind::Let => todo!("Let"),
        }
    }

    fn emit_get_instruction(&mut self, program: &mut Program, symbol: Symbol) {
        let id = symbol.id;
        match symbol.kind {
            SymbolKind::FreeVariable => program.push(Instruction::GetFree(id)),
            SymbolKind::Variable => match symbol.scope {
                SymbolScope::Global => program.push(Instruction::GetGlobal(id)),
                SymbolScope::Function => program.push(Instruction::GetLocal(id)),
                SymbolScope::Free => program.push(Instruction::GetFree(id)),
                SymbolScope::Let => program.push(Instruction::GetLocal(id)),
            },
            SymbolKind::Parameter => program.push(Instruction::GetLocal(id)),
            SymbolKind::Function if symbol.location.is_some() => {
                let location = symbol.location.unwrap();
                let value = Value::Callable(location as usize);
                program.push(Instruction::Push(Box::new(value)));
            }
            SymbolKind::Function if BUILTINS.contains(&symbol.name.as_str()) => {
                todo!("BUILTINS {}", symbol.name)
            }
            SymbolKind::Function => {
                let location = usize::MAX;
                let value = Value::Callable(location);
                program.push(Instruction::Push(Box::new(value)));
                let index = program.len();
                self.unset_locations.push(UnsetLocation {
                    index,
                    name: symbol.name.clone(),
                });
            }
            // NOTE: Unsure if this will ever be hit.
            SymbolKind::Lambda => todo!("Lambda"),
            SymbolKind::Let => todo!("Let"),
        }
    }

    fn get_program_size(&self, program: &Program) -> usize {
        self.offset + program.program_size()
    }
}

impl AstWalker<Program> for Compiler<'_> {
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

        self.symbol_table
            .set_location(Some(name), name, start as u32);

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
        self.symbol_table
            .set_location(Some(&name), &name, start as u32);

        self.symbol_table.enter_scope(&name);

        if !lambda.params.expr.is_list() {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        for spanned in lambda.body.iter() {
            self.walk_expr(program, spanned);
        }
        program.push(Instruction::Return);

        self.symbol_table.exit_scope();

        let Some(local_scope) = self.symbol_table.get_function_scope(&name) else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        let body_size = self.get_program_size(program) - start;

        for (_, symbol) in local_scope.iter() {
            if symbol.kind == SymbolKind::FreeVariable {
                let instruction = if symbol.scope == SymbolScope::Global {
                    Instruction::GetGlobal(symbol.id)
                } else {
                    Instruction::GetLocal(symbol.id)
                };
                program.push(instruction);
                program.push(Instruction::SetFree);
            }
        }

        program[index] = Instruction::Jump(start + body_size);

        program.push(Instruction::Push(Box::new(Value::Callable(start))));
    }

    fn handle_let_binding(&mut self, program: &mut Program, let_binding: &LetBindingExpr) {
        let id = self.symbol_table.get_id();

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
        let name = format!("let_{}|{}", id, names.join("|"));
        let Some(local_scope) = self.symbol_table.get_function_scope(&name) else {
            // NOTE: INTENTIONAL_PANIC
            panic!("no scope for let binding {name:?}");
        };

        for (_, symbol) in local_scope.iter() {
            if symbol.kind == SymbolKind::FreeVariable {
                let instruction = if symbol.scope == SymbolScope::Global {
                    Instruction::GetGlobal(symbol.id)
                } else {
                    Instruction::GetLocal(symbol.id)
                };
                program.push(instruction);
                program.push(Instruction::SetFree);
            }
        }

        self.symbol_table.enter_scope(&name);

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
            let Some(symbol) = self.symbol_table.lookup(binding_name).cloned() else {
                // NOTE: INTENTIONAL_PANIC
                panic!("expected symbol in let binding, got: {list:?}");
            };
            self.emit_set_instruction(program, &symbol);
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
            let Some(symbol) = self.symbol_table.lookup(name) else {
                return;
            };

            let has_same_scope_name = matches!(
                self.symbol_table.get_current_scope_name(),
                Some(name) if name == &symbol.name
            );
            let last_instruction_is_call = matches!(
                program.last(),
                Some(Instruction::Call(_)) | Some(Instruction::TailCall(_))
            );
            if symbol.is_self_reference() && has_same_scope_name && last_instruction_is_call {
                program.push(Instruction::TailCall(call.args.len()));
                return;
            }
        }
        program.push(Instruction::Call(call.args.len()));
    }

    fn handle_var(&mut self, program: &mut Program, var: &VarExpr) {
        let Expr::Symbol(name) = &var.name.expr else {
            unreachable!("This should never fail as we already checked this in AstWalker");
        };

        self.walk_expr(program, var.body);

        let Some(symbol) = self.symbol_table.lookup(name) else {
            // NOTE: INTENTIONAL_PANIC
            panic!("unknown symbol: {}", name);
        };

        let instruction = if symbol.scope == SymbolScope::Global {
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
        let mut chunk_condition = Vec::new();
        self.walk_expr(&mut chunk_condition, r#loop.condition);

        let mut chunk_body = Vec::new();
        self.walk_expr(&mut chunk_body, r#loop.body);

        let body_offset = chunk_body.program_size();
        let start_offset = body_offset
            + chunk_condition.program_size()
            + Instruction::JumpIf(0).size()
            + Instruction::JumpBackward(0).size();
        program.extend(chunk_condition);
        program.push(Instruction::JumpIf(body_offset));
        program.extend(chunk_body);
        program.push(Instruction::JumpBackward(start_offset));
    }

    fn handle_bool(&mut self, program: &mut Program, b: bool) {
        let value = Value::Bool(b);
        program.push(Instruction::Push(Box::new(value)));
    }

    fn handle_string(&mut self, program: &mut Program, string: &str) {
        let value = Value::String(Box::new(string.to_string()));
        program.push(Instruction::Push(Box::new(value)));
    }

    fn handle_number(&mut self, program: &mut Program, value: f64) {
        program.push(Instruction::Push(Box::new(Value::F64(value))));
    }

    fn handle_symbol(&mut self, program: &mut Program, name: &str, span: Span) {
        let Some(symbol) = self.symbol_table.lookup(name).cloned() else {
            self.error(Error::SymbolNotDefined(span, name.to_string()));
            return;
        };
        self.emit_get_instruction(program, symbol);
    }
}