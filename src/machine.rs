use super::{
    ast::{Expr, Spanned},
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LetBindingExpr, LoopExpr,
        OperatorExpr, VarExpr,
    },
    parser::{parse_or_report, parser},
    symbol_table::{Symbol, SymbolKind, SymbolScope, SymbolTable, SymbolTableBuilder},
    BUILTINS,
};
use anyhow::{anyhow, Result};
use chumsky::prelude::Parser;
pub use instruction::{Instruction, OpCode, Value};
use intrinsic::Intrinsic;
use num_traits::FromPrimitive;

mod instruction {
    use super::BUILTINS;
    use num_derive::{FromPrimitive, ToPrimitive};
    #[derive(Debug, Clone, PartialEq, FromPrimitive, ToPrimitive)]
    #[repr(u8)]
    pub enum OpCode {
        Halt,
        Return,
        Push,
        Add,
        Sub,
        Mul,
        Div,
        Eq,
        GreaterThan,
        LessThan,
        GreaterThanOrEqual,
        LessThanOrEqual,
        And,
        Or,
        Not,
        Mod,
        Rot,
        Call,
        TailCall,
        SetLocal,
        SetGlobal,
        SetFree,
        GetLocal,
        GetGlobal,
        GetFree,
        JumpIf,
        JumpForward,
        JumpBackward,
        Jump,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Instruction {
        Halt,
        Return,
        Push(Box<Value>),
        Add(usize),
        Sub(usize),
        Mul(usize),
        Div(usize),
        Eq(usize),
        GreaterThan(usize),
        LessThan(usize),
        GreaterThanOrEqual(usize),
        LessThanOrEqual(usize),
        And(usize),
        Or(usize),
        Not,
        Mod,
        Rot,
        Call(usize),
        TailCall(usize),
        SetLocal,
        SetGlobal,
        SetFree,
        GetLocal(usize),
        GetGlobal(usize),
        GetFree(usize),
        JumpIf(usize),
        JumpForward(usize),
        JumpBackward(usize),
        Jump(usize),
    }

    impl Instruction {
        pub fn size(&self) -> usize {
            match self {
                Self::Halt => 1,
                Self::Return => 1,
                Self::Push(value) => 1 + value.size(),
                Self::Add(_) => 2,
                Self::Sub(_) => 2,
                Self::Mul(_) => 2,
                Self::Div(_) => 2,
                Self::Eq(_) => 2,
                Self::GreaterThan(_) => 2,
                Self::LessThan(_) => 2,
                Self::GreaterThanOrEqual(_) => 2,
                Self::LessThanOrEqual(_) => 2,
                Self::And(_) => 2,
                Self::Or(_) => 2,
                Self::Not => 1,
                Self::Mod => 1,
                Self::Rot => 1,
                Self::Call(_) => 2,
                Self::TailCall(_) => 2,
                Self::SetLocal => 1,
                Self::SetGlobal => 1,
                Self::SetFree => 1,
                Self::GetLocal(_) => 5,
                Self::GetGlobal(_) => 5,
                Self::GetFree(_) => 5,
                Self::JumpIf(_) => 5,
                Self::JumpForward(_) => 5,
                Self::JumpBackward(_) => 5,
                Self::Jump(_) => 5,
            }
        }

        pub fn opcode(&self) -> OpCode {
            match self {
                Self::Halt => OpCode::Halt,
                Self::Return => OpCode::Return,
                Self::Push(_) => OpCode::Push,
                Self::Add(_) => OpCode::Add,
                Self::Sub(_) => OpCode::Sub,
                Self::Mul(_) => OpCode::Mul,
                Self::Div(_) => OpCode::Div,
                Self::Eq(_) => OpCode::Eq,
                Self::GreaterThan(_) => OpCode::GreaterThan,
                Self::LessThan(_) => OpCode::LessThan,
                Self::GreaterThanOrEqual(_) => OpCode::GreaterThanOrEqual,
                Self::LessThanOrEqual(_) => OpCode::LessThanOrEqual,
                Self::And(_) => OpCode::And,
                Self::Or(_) => OpCode::Or,
                Self::Not => OpCode::Not,
                Self::Mod => OpCode::Mod,
                Self::Rot => OpCode::Rot,
                Self::Call(..) => OpCode::Call,
                Self::TailCall(..) => OpCode::TailCall,
                Self::SetLocal => OpCode::SetLocal,
                Self::SetGlobal => OpCode::SetGlobal,
                Self::SetFree => OpCode::SetFree,
                Self::GetLocal(_) => OpCode::GetLocal,
                Self::GetGlobal(_) => OpCode::GetGlobal,
                Self::GetFree(_) => OpCode::GetFree,
                Self::JumpIf(_) => OpCode::JumpIf,
                Self::JumpForward(_) => OpCode::JumpForward,
                Self::JumpBackward(_) => OpCode::JumpBackward,
                Self::Jump(_) => OpCode::Jump,
            }
        }

        pub fn to_bytecode(&self) -> Vec<u8> {
            let mut bytes = Vec::new();
            match self {
                Self::Halt => bytes.push(OpCode::Halt as u8),
                Self::Return => bytes.push(OpCode::Return as u8),
                Self::Push(value) => {
                    bytes.push(OpCode::Push as u8);
                    bytes.extend(&value.to_bytecode());
                }
                Self::Add(count)
                | Self::Sub(count)
                | Self::Mul(count)
                | Self::Div(count)
                | Self::Eq(count)
                | Self::GreaterThan(count)
                | Self::LessThan(count)
                | Self::GreaterThanOrEqual(count)
                | Self::LessThanOrEqual(count)
                | Self::And(count)
                | Self::Or(count) => {
                    bytes.push(self.opcode() as u8);
                    bytes.push(*count as u8);
                }
                Self::Not
                | Self::Mod
                | Self::Rot
                | Self::SetLocal
                | Self::SetGlobal
                | Self::SetFree => bytes.push(self.opcode() as u8),
                Self::Call(count) | Self::TailCall(count) => {
                    bytes.push(self.opcode() as u8);
                    bytes.push(*count as u8);
                }
                Self::GetLocal(address)
                | Self::GetGlobal(address)
                | Self::GetFree(address)
                | Self::JumpIf(address)
                | Self::JumpForward(address)
                | Self::JumpBackward(address)
                | Self::Jump(address) => {
                    bytes.push(self.opcode() as u8);
                    bytes.extend(&(*address as u32).to_le_bytes());
                }
            }

            bytes
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Value {
        U8(u8),
        I32(i32),
        U32(u32),
        F32(f32),
        F64(f64),
        String(Box<String>),
        Bool(bool),
        List(Box<Vec<Value>>),
        Callable(usize),
        Builtin(usize),
    }

    impl Value {
        pub const CODE_U8: u8 = 0x00;
        pub const CODE_I32: u8 = 0x01;
        pub const CODE_U32: u8 = 0x02;
        pub const CODE_F32: u8 = 0x03;
        pub const CODE_F64: u8 = 0x04;
        pub const CODE_STRING: u8 = 0x05;
        pub const CODE_BOOL: u8 = 0x06;
        pub const CODE_LIST: u8 = 0x07;
        pub const CODE_CALLABLE: u8 = 0x08;
        pub const CODE_BUILTIN: u8 = 0x09;

        pub fn to_bytecode(&self) -> Vec<u8> {
            match self {
                Value::U8(v) => vec![Self::CODE_U8, *v],
                Value::I32(v) => {
                    let mut bytes = vec![Self::CODE_I32];
                    bytes.extend_from_slice(&v.to_le_bytes());
                    bytes
                }
                Value::U32(v) => {
                    let mut bytes = vec![Self::CODE_U32];
                    bytes.extend_from_slice(&v.to_le_bytes());
                    bytes
                }
                Value::F32(v) => {
                    let mut bytes = vec![Self::CODE_F32];
                    bytes.extend_from_slice(&v.to_le_bytes());
                    bytes
                }
                Value::F64(v) => {
                    let mut bytes = vec![Self::CODE_F64];
                    bytes.extend_from_slice(&v.to_le_bytes());
                    bytes
                }
                Value::String(v) => {
                    let mut bytes = vec![Self::CODE_STRING];
                    bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                    bytes.extend_from_slice(v.as_bytes());
                    bytes
                }
                Value::Bool(v) => vec![Self::CODE_BOOL, if *v { 1 } else { 0 }],
                Value::List(vec) => {
                    let mut bytes = vec![Self::CODE_LIST];
                    bytes.extend_from_slice(&(vec.len() as u32).to_le_bytes());
                    for v in vec.iter() {
                        bytes.extend(v.to_bytecode());
                    }
                    bytes
                }
                Value::Callable(index) => {
                    let mut bytes = vec![Self::CODE_CALLABLE];
                    bytes.extend_from_slice(&(*index as u32).to_le_bytes());
                    bytes
                }
                Value::Builtin(index) => {
                    let mut bytes = vec![Self::CODE_BUILTIN];
                    bytes.extend_from_slice(&(*index as u32).to_le_bytes());
                    bytes
                }
            }
        }

        pub fn size(&self) -> usize {
            let conent_size = match self {
                Value::U8(_) => 1,
                Value::I32(_) => 4,
                Value::U32(_) => 4,
                Value::F32(_) => 4,
                Value::F64(_) => 8,
                Value::String(v) => 4 + v.len(),
                Value::Bool(_) => 1,
                Value::List(vec) => 4 + vec.iter().map(|v| v.size()).sum::<usize>(),
                // 4 bytes    4 bytes
                // start......end
                Value::Callable(_) => 4,
                Value::Builtin(_) => 4,
            };
            // opcode + content
            1 + conent_size
        }

        pub fn type_of(&self) -> String {
            match self {
                Self::U8(_) => "u8".to_string(),
                Self::I32(_) => "i32".to_string(),
                Self::U32(_) => "u32".to_string(),
                Self::F32(_) => "f32".to_string(),
                Self::F64(_) => "f64".to_string(),
                Self::String(_) => "String".to_string(),
                Self::Bool(_) => "Bool".to_string(),
                Self::List(_) => "List".to_string(),
                Self::Callable(_) => "Function".to_string(),
                Self::Builtin(_) => "Builtin".to_string(),
            }
        }
    }

    impl std::fmt::Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::U8(value) => write!(f, "{value}"),
                Self::I32(value) => write!(f, "{value}"),
                Self::U32(value) => write!(f, "{value}"),
                Self::F32(value) => write!(f, "{value}"),
                Self::F64(value) => write!(f, "{value}"),
                Self::String(value) => write!(f, "{value}"),
                Self::Bool(value) => write!(f, "{value}"),
                Self::List(value) => {
                    write!(
                        f,
                        "({})",
                        value
                            .iter()
                            .map(|v| format!("{}", v))
                            .collect::<Vec<String>>()
                            .join(" ")
                    )
                }
                Self::Callable(index) => write!(f, "<function {index:?}>"),
                Self::Builtin(index) => write!(f, "<function {:?}>", BUILTINS[*index]),
            }
        }
    }
}

pub fn compile(src: &str, options: CompilerOptions) -> Result<Vec<Instruction>> {
    let ast = parser()
        .parse(src)
        .map_err(|e| anyhow::anyhow!(e.iter().map(|e| e.to_string() + "\n").collect::<String>()))?;

    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
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
}

impl<'a> Compiler<'a> {
    fn new(symbol_table: &'a mut SymbolTable, options: CompilerOptions) -> Self {
        Self {
            symbol_table,
            lambda_counter: 0,
            options,
            offset: 0,
            unset_locations: Vec::new(),
        }
    }

    fn with_offset(mut self, offset: usize) -> Self {
        self.offset = offset;
        self
    }

    fn compile(&mut self, ast: &[Spanned<Expr>]) -> Result<Vec<Instruction>> {
        let mut program = Vec::new();
        self.walk(&mut program, ast);
        if !self.options.no_main {
            let Some(symbol) = self.symbol_table.lookup("main") else {
                // TODO: REPORT ERROR
                panic!("Main function is not defined");
            };
            let Some(location) = symbol.location else {
                // TODO: REPORT ERROR
                panic!("Main function location is unknown");
            };
            program.push(Instruction::Jump(location as usize));
        }

        for UnsetLocation { index, name } in self.unset_locations.iter() {
            let Some(symbol) = self.symbol_table.lookup(name) else {
                // TODO: REPORT ERROR
                panic!("Variable `{}` is not defined", name,);
            };
            let Some(location) = symbol.location else {
                // TODO: REPORT ERROR
                panic!("Variable `{}` location is unknown", name,);
            };
            let instruction = &mut program[*index];
            match instruction {
                Instruction::Push(value) => match value.as_mut() {
                    Value::Callable(i) => *i = location as usize,
                    _ => {
                        // TODO: REPORT ERROR
                        panic!("expected push but found {instruction:?}");
                    }
                },
                _ => {
                    // TODO: REPORT ERROR
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
            SymbolKind::Lambda => todo!("Lambda"),
            SymbolKind::Let => todo!("Let"),
        }
    }

    fn get_program_size(&self, program: &Program) -> usize {
        self.offset + program.program_size()
    }
}

impl AstWalker<Program> for Compiler<'_> {
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
            // TODO: ERROR REPORTING
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
            // TODO: ERROR REPORTING
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
            // TODO: REPORT ERROR
            panic!(
                "expected symbol for function name but found {:?}",
                function.name.expr
            );
        };

        self.symbol_table
            .set_location(Some(name), name, start as u32);

        self.symbol_table.enter_scope(name);

        if !function.params.expr.is_list() {
            // TODO: REPORT ERROR
            panic!(
                "expected list for params but found {:?}",
                &function.params.expr
            );
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
            // TODO: REPORT ERROR
            panic!(
                "expected list for params but found {:?}",
                &lambda.params.expr
            );
        };

        for spanned in lambda.body.iter() {
            self.walk_expr(program, spanned);
        }
        program.push(Instruction::Return);

        self.symbol_table.exit_scope();

        let Some(local_scope) = self.symbol_table.get_function_scope(&name) else {
            // TODO: REPORT ERROR
            panic!("no scope for {name:?}");
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
            // TODO: REPORT ERROR
            panic!("no scope for {name:?}");
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
            match &spanned.expr {
                Expr::List(list) => {
                    self.walk_expr(program, &list[1]);

                    let Expr::Symbol(binding_name) = &list[0].expr else {
                        // TODO: REPORT ERROR
                        panic!("expected symbol in let binding, got: {list:?}");
                    };
                    let Some(symbol) = self.symbol_table.lookup(binding_name).cloned() else {
                        // TODO: REPORT ERROR
                        panic!("expected symbol in let binding, got: {list:?}");
                    };
                    self.emit_set_instruction(program, &symbol);
                }
                _ => {
                    // TODO: REPORT ERROR
                    panic!("expected list for let binding but found {:?}", spanned.expr);
                }
            }
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
                // TODO: REPORT ERROR
                panic!("unknown symbol: {}", name);
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
            // TODO: REPORT ERROR
            panic!("var name must be a symbol but found {:?}", var.name.expr);
        };

        self.walk_expr(program, var.body);

        let Some(symbol) = self.symbol_table.lookup(name) else {
            // TODO: REPORT ERROR
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

    fn handle_symbol(&mut self, program: &mut Program, name: &str) {
        let Some(symbol) = self.symbol_table.lookup(name).cloned() else {
            // TODO: REPORT ERROR
            panic!("Variable `{}` is not defined", name,);
        };
        self.emit_get_instruction(program, symbol);
    }
}

const INSTRUCTION_CALL: [fn(&mut Machine) -> Result<()>; 29] = [
    Machine::instruction_halt,
    Machine::instruction_return,
    Machine::instruction_push,
    Machine::instruction_add,
    Machine::instruction_sub,
    Machine::instruction_mul,
    Machine::instruction_div,
    Machine::instruction_eq,
    Machine::instruction_greater_than,
    Machine::instruction_less_than,
    Machine::instruction_greater_than_or_equal,
    Machine::instruction_less_than_or_equal,
    Machine::instruction_and,
    Machine::instruction_or,
    Machine::instruction_not,
    Machine::instruction_mod,
    Machine::instruction_rot,
    Machine::instruction_call_function,
    Machine::instruction_tail_call_function,
    Machine::instruction_set_local,
    Machine::instruction_set_global,
    Machine::instruction_set_free,
    Machine::instruction_get_local,
    Machine::instruction_get_global,
    Machine::instruction_get_free,
    Machine::instruction_jump_if,
    Machine::instruction_jump_forward,
    Machine::instruction_jump_backward,
    Machine::instruction_jump,
];

const INTRISICS: [fn(&mut Machine, u8) -> Result<()>; 25] = [
    Intrinsic::intrinsic_sleep,
    Intrinsic::intrinsic_is_atom,
    Intrinsic::intrinsic_is_number,
    Intrinsic::intrinsic_slice,
    Intrinsic::intrinsic_join,
    Intrinsic::intrinsic_split,
    Intrinsic::intrinsic_to_string,
    Intrinsic::intrinsic_filter,
    Intrinsic::intrinsic_fold_right,
    Intrinsic::intrinsic_fold,
    Intrinsic::intrinsic_map,
    Intrinsic::intrinsic_nth,
    Intrinsic::intrinsic_reverse,
    Intrinsic::intrinsic_append,
    Intrinsic::intrinsic_last,
    Intrinsic::intrinsic_cdr,
    Intrinsic::intrinsic_typeof,
    Intrinsic::intrinsic_print,
    Intrinsic::intrinsic_nth,
    Intrinsic::intrinsic_length,
    Intrinsic::intrinsic_assert_eq,
    Intrinsic::intrinsic_assert,
    Intrinsic::intrinsic_create_list,
    Intrinsic::intrinsic_cons,
    Intrinsic::intrinsic_car,
];

#[derive(Debug)]
pub(crate) struct Frame {
    pub return_address: Option<usize>,
    pub args: Vec<Value>,
    pub stack: Vec<Value>,
}

impl Frame {
    fn new(return_address: usize, args: Vec<Value>) -> Self {
        Self {
            return_address: Some(return_address),
            args,
            stack: Vec::new(),
        }
    }

    fn bring_to_top_of_stack(&mut self, count: usize) {
        let length = self.stack.len();
        self.stack[length - count..].rotate_left(count.saturating_sub(1));
    }
}

impl TryFrom<(&str, CompilerOptions)> for Machine {
    type Error = anyhow::Error;
    fn try_from((src, options): (&str, CompilerOptions)) -> std::result::Result<Self, Self::Error> {
        let instructions = compile(src, options)?;
        let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
        Ok(Self::new(program))
    }
}

#[derive(Debug, Default)]
pub struct MachineOptions {
    pub quiet: bool,
}

#[derive(Debug)]
pub struct Machine {
    pub(crate) options: MachineOptions,
    pub(crate) program: Vec<u8>,
    pub(crate) global: Vec<Value>,
    pub(crate) free: Vec<Value>,
    pub(crate) stack: Vec<Frame>,
    pub(crate) ip: usize,
    pub(crate) is_running: bool,
    pub(crate) symbol_table: Option<SymbolTable>,
    #[cfg(any(debug_assertions, test))]
    pub(crate) cycle_count: usize,
}

impl Default for Machine {
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

// Constants
impl Machine {
    const MAX_STACK_FRAME_SIZE: usize = 1024;
}

// Public
impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        let mut stack = Vec::with_capacity(1024);
        stack.push(Frame {
            return_address: None,
            args: Vec::with_capacity(256),
            stack: Vec::with_capacity(1024),
        });
        Self {
            options: MachineOptions::default(),
            program,
            global: Vec::with_capacity(1024),
            free: Vec::with_capacity(1024),
            stack,
            ip: 0,
            is_running: true,
            symbol_table: None,
            #[cfg(any(debug_assertions, test))]
            cycle_count: 0,
        }
    }

    pub fn set_options(&mut self, options: MachineOptions) {
        self.options = options;
    }

    pub fn is_running(&self) -> bool {
        self.is_running
    }

    pub fn load_from_string(&mut self, src: &str) -> Result<()> {
        self.is_running = true;

        // let ast = parser().parse(src).map_err(|e| {
        //     anyhow::anyhow!(e.iter().map(|e| e.to_string() + "\n").collect::<String>())
        // })?;
        let ast = parse_or_report("none", src);
        eprintln!("{ast:?}");
        let mut symbol_table = match self.symbol_table.take() {
            Some(mut table) => {
                SymbolTableBuilder::default().build_from_scope(&ast, &mut table);
                table
            }
            None => SymbolTableBuilder::default().build(&ast),
        };
        let instructions = Compiler::new(&mut symbol_table, CompilerOptions { no_main: true })
            .with_offset(self.program.len())
            .compile(&ast)?;
        self.symbol_table = Some(symbol_table);

        let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
        self.program.extend(program);
        Ok(())
    }

    pub fn run_from_string(&mut self, src: &str) -> Result<()> {
        self.load_from_string(src)?;
        self.run()?;
        Ok(())
    }

    pub fn peek_last_stack_value(&self) -> Option<&Value> {
        let frame = self.get_current_frame().ok()?;
        frame.stack.last()
    }

    pub fn push(&mut self, value: Value) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    pub fn pop(&mut self) -> Result<Option<Value>> {
        let frame = self.get_current_frame_mut()?;
        Ok(frame.stack.pop())
    }

    pub fn call_from_address(&mut self, address: usize, count: u8) -> Result<()> {
        let param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count as usize)
        };

        let new_frame = Frame::new(self.ip, param_values);
        self.stack.push(new_frame);
        self.ip = address;

        while self.ip < self.program.len() {
            let Some(byte) = self.peek_u8() else {
                // TODO: ERROR REPORTING
                panic!("missing opcode on stack");
            };
            if let Some(OpCode::Return) = OpCode::from_u8(*byte) {
                self.instruction_return()?;
                break;
            }
            self.run_once()?;
        }
        Ok(())
    }

    pub fn run_once(&mut self) -> Result<()> {
        #[cfg(any(debug_assertions, test))]
        {
            self.cycle_count += 1;
        }

        let Ok(opcode) = self.get_u8() else {
            self.shutdown();
            #[cfg(debug_assertions)]
            if !self.options.quiet {
                eprintln!("END PROGRAM {}..{}", self.ip, self.program.len());
            }
            return Ok(());
        };
        INSTRUCTION_CALL[opcode as usize](self)?;

        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        while self.is_running {
            self.run_once()?;
        }
        #[cfg(any(debug_assertions, test))]
        if !self.options.quiet {
            eprintln!("cycles: {}", self.cycle_count);
            eprintln!("stack {:#?}", self.stack);
        }
        Ok(())
    }
}

// Private
impl Machine {
    fn shutdown(&mut self) {
        self.is_running = false;
    }

    fn get_value(&mut self) -> Result<Value> {
        let value_opcode = self.get_u8()?;
        match value_opcode {
            Value::CODE_U8 => {
                let value = self.get_u8()?;
                Ok(Value::U8(value))
            }
            Value::CODE_I32 => {
                let value = self.get_i32()?;
                Ok(Value::I32(value))
            }
            Value::CODE_U32 => {
                let value = self.get_u32()?;
                Ok(Value::U32(value))
            }
            Value::CODE_F32 => {
                let value = self.get_f32()?;
                Ok(Value::F32(value))
            }
            Value::CODE_F64 => {
                let value = self.get_f64()?;
                Ok(Value::F64(value))
            }
            Value::CODE_STRING => {
                let value = self.get_string()?;
                Ok(Value::String(Box::new(value)))
            }
            Value::CODE_BOOL => {
                let value = self.get_u8()? != 0;
                Ok(Value::Bool(value))
            }
            Value::CODE_LIST => {
                let length = self.get_u32()?;
                let mut values = Vec::new();
                for _ in 0..length {
                    values.push(self.get_value()?);
                }
                Ok(Value::List(Box::new(values)))
            }
            Value::CODE_CALLABLE => {
                let index = self.get_u32()? as usize;
                Ok(Value::Callable(index))
            }
            Value::CODE_BUILTIN => {
                let index = self.get_u32()? as usize;
                Ok(Value::Builtin(index))
            }
            _ => Err(anyhow!("Unknown value `{}`", self.program[self.ip])),
        }
    }

    fn peek_u8(&mut self) -> Option<&u8> {
        self.program.get(self.ip)
    }

    fn get_u8(&mut self) -> Result<u8> {
        let Some(byte) = self.program.get(self.ip) else {
            return Err(anyhow!("index out of bounds"));
        };
        self.ip += 1;
        Ok(*byte)
    }

    #[allow(dead_code)]
    fn get_u16(&mut self) -> Result<u16> {
        let byte2 = self.get_u8()? as u16;
        let byte1 = self.get_u8()? as u16;
        Ok(byte1 << 8 | byte2)
    }

    fn get_i32(&mut self) -> Result<i32> {
        let byte4 = self.get_u8()? as i32;
        let byte3 = self.get_u8()? as i32;
        let byte2 = self.get_u8()? as i32;
        let byte1 = self.get_u8()? as i32;
        Ok(byte1 << 24 | byte2 << 16 | byte3 << 8 | byte4)
    }

    fn get_u32(&mut self) -> Result<u32> {
        let byte4 = self.get_u8()? as u32;
        let byte3 = self.get_u8()? as u32;
        let byte2 = self.get_u8()? as u32;
        let byte1 = self.get_u8()? as u32;
        Ok(byte1 << 24 | byte2 << 16 | byte3 << 8 | byte4)
    }

    #[allow(dead_code)]
    fn get_u64(&mut self) -> Result<u64> {
        let byte8 = self.get_u8()? as u64;
        let byte7 = self.get_u8()? as u64;
        let byte6 = self.get_u8()? as u64;
        let byte5 = self.get_u8()? as u64;
        let byte4 = self.get_u8()? as u64;
        let byte3 = self.get_u8()? as u64;
        let byte2 = self.get_u8()? as u64;
        let byte1 = self.get_u8()? as u64;
        Ok(byte1 << 56
            | byte2 << 48
            | byte3 << 40
            | byte4 << 32
            | byte5 << 24
            | byte6 << 16
            | byte7 << 8
            | byte8)
    }

    fn get_f32(&mut self) -> Result<f32> {
        let byte1 = self.get_u8()? as u32;
        let byte2 = self.get_u8()? as u32;
        let byte3 = self.get_u8()? as u32;
        let byte4 = self.get_u8()? as u32;
        Ok(f32::from_le_bytes([
            byte1 as u8,
            byte2 as u8,
            byte3 as u8,
            byte4 as u8,
        ]))
    }

    fn get_f64(&mut self) -> Result<f64> {
        let byte1 = self.get_u8()? as u64;
        let byte2 = self.get_u8()? as u64;
        let byte3 = self.get_u8()? as u64;
        let byte4 = self.get_u8()? as u64;
        let byte5 = self.get_u8()? as u64;
        let byte6 = self.get_u8()? as u64;
        let byte7 = self.get_u8()? as u64;
        let byte8 = self.get_u8()? as u64;
        Ok(f64::from_le_bytes([
            byte1 as u8,
            byte2 as u8,
            byte3 as u8,
            byte4 as u8,
            byte5 as u8,
            byte6 as u8,
            byte7 as u8,
            byte8 as u8,
        ]))
    }

    fn get_string(&mut self) -> Result<String> {
        let len = self.get_u32()? as usize;
        let bytes = self.program[self.ip..self.ip + len].to_vec();
        self.ip += len;
        Ok(String::from_utf8(bytes)?)
    }

    pub(crate) fn get_current_frame(&self) -> Result<&Frame> {
        self.stack
            .last()
            .map_or_else(|| Err(anyhow!("Stack is empty")), Ok)
    }

    fn get_current_frame_mut(&mut self) -> Result<&mut Frame> {
        self.stack
            .last_mut()
            .map_or_else(|| Err(anyhow!("Stack is empty")), Ok)
    }
}

impl Machine {
    fn instruction_halt(&mut self) -> Result<()> {
        self.is_running = false;
        Ok(())
    }

    fn instruction_return(&mut self) -> Result<()> {
        let value = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORTING
                panic!("missing return value on stack");
            };
            if let Some(address) = frame.return_address {
                self.ip = address;
            }
            value
        };
        self.stack.pop();
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_push(&mut self) -> Result<()> {
        let value = self.get_value()?;
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_add(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l + r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l + r);
                }
                _ => panic!("invalid types for Add"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_sub(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l - r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l - r);
                }
                _ => panic!("invalid types for Sub"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_mul(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l * r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l * r);
                }
                _ => panic!("invalid types for Mul"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_div(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l / r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l / r);
                }
                _ => panic!("invalid types for Div"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_eq(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l == r,
            (Value::F64(l), Value::F64(r)) => l == r,
            _ => panic!("invalid types for Less Than"),
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_greater_than(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l > r,
            (Value::F64(l), Value::F64(r)) => l > r,
            _ => panic!("invalid types for Less Than"),
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_less_than(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l < r,
            (Value::F64(l), Value::F64(r)) => l < r,
            _ => panic!("invalid types for Less Than"),
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_greater_than_or_equal(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l >= r,
            (Value::F64(l), Value::F64(r)) => l >= r,
            _ => panic!("invalid types for Greater Than Or Equal"),
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_less_than_or_equal(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l <= r,
            (Value::F64(l), Value::F64(r)) => l <= r,
            _ => panic!("invalid types for Greater Than Or Equal"),
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_and(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::Bool(l), Value::Bool(r)) => {
                    left = Value::Bool(l && *r);
                }
                _ => panic!("invalid types for Or"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_or(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::Bool(true), Value::Bool(_)) => {
                    left = Value::Bool(true);
                    break;
                }
                (Value::Bool(_), Value::Bool(true)) => {
                    left = Value::Bool(true);
                    break;
                }
                (Value::Bool(l), Value::Bool(r)) => {
                    left = Value::Bool(l || *r);
                }
                _ => panic!("invalid types for And"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_not(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            panic!("expected value on stack for Not")
        };
        match value {
            Value::Bool(b) => {
                frame.stack.push(Value::Bool(!b));
            }
            _ => panic!("invalid types for Not"),
        }
        Ok(())
    }

    fn instruction_mod(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(right) = frame.stack.pop() else {
            panic!("expected value on stack for Mod")
        };
        let Some(left) = frame.stack.last() else {
            panic!("expected value on stack for Mod")
        };
        let last_index = frame.stack.len() - 1;
        match (left, right) {
            (Value::I32(left), Value::I32(right)) => {
                frame.stack[last_index] = Value::I32(left % right);
            }
            (Value::F64(left), Value::F64(right)) => {
                frame.stack[last_index] = Value::F64(left % right);
            }
            _ => panic!("invalid types for Mod"),
        }
        Ok(())
    }

    fn instruction_rot(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        frame.bring_to_top_of_stack(2);
        Ok(())
    }

    fn instruction_call_function(&mut self) -> Result<()> {
        if self.stack.len() >= Self::MAX_STACK_FRAME_SIZE {
            return Err(anyhow!("stack frame overflow at {}", self.ip));
        }
        let count = self.get_u8()? as usize;
        let value = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORT;
                panic!("missing callable value on stack");
            };
            value
        };
        let address = match value {
            Value::Builtin(index) => {
                INTRISICS[index](self, count as u8)?;
                return Ok(());
            }
            Value::Callable(address) => address,
            _ => {
                // TODO: ERROR REPORT;
                panic!("can not call '{}' type", value);
            }
        };
        let mut param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count)
        };

        param_values.reverse();

        let new_frame = Frame::new(self.ip, param_values);
        self.stack.push(new_frame);
        self.ip = address;
        Ok(())
    }

    fn instruction_tail_call_function(&mut self) -> Result<()> {
        if self.stack.len() >= Self::MAX_STACK_FRAME_SIZE {
            return Err(anyhow!("stack frame overflow at {}", self.ip));
        }
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;

        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORT;
            panic!("missing callable value on stack");
        };
        let Value::Callable(address) = value else {
            // TODO: ERROR REPORT;
            panic!("can not call '{}' type", value);
        };

        let length = frame.stack.len();
        let mut param_values = frame.stack.split_off(length - count);
        param_values.reverse();

        frame.args = param_values;
        self.ip = address;
        Ok(())
    }

    fn instruction_set_local(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORT;
            panic!("no value on stack for SetLocal");
        };
        frame.args.push(value);
        Ok(())
    }

    fn instruction_get_local(&mut self) -> Result<()> {
        let index = self.get_u32()? as usize;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.args.get(index) else {
            // TODO: ERROR REPORTING
            panic!("no args on the arg stack");
        };
        frame.stack.push(value.clone());
        Ok(())
    }

    fn instruction_set_global(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORTING
            panic!("missing value on stack frame for SetGlobal instruction")
        };

        self.global.push(value);

        Ok(())
    }

    fn instruction_get_global(&mut self) -> Result<()> {
        let index = self.get_u32()? as usize;
        let Some(value) = self.global.get(index).cloned() else {
            // TODO: ERROR REPORTING
            panic!("no value on the global stack");
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_set_free(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORTING
            panic!("no value on the stack for SetFree");
        };
        self.free.push(value);
        Ok(())
    }

    fn instruction_get_free(&mut self) -> Result<()> {
        let index = self.get_u32()? as usize;
        let Some(value) = self.free.get(index).cloned() else {
            // TODO: ERROR REPORTING
            panic!("no value on the free stack at index {index}");
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_jump_if(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            panic!("expected value on stack for JumpIf")
        };
        if value == Value::Bool(false) {
            self.ip += address;
        }
        Ok(())
    }

    fn instruction_jump_forward(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        self.ip += address;
        Ok(())
    }

    fn instruction_jump_backward(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        self.ip -= address;
        Ok(())
    }

    fn instruction_jump(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        self.ip = address;
        Ok(())
    }
}

// --------------------- Decompiler ---------------------
impl Machine {
    pub fn decompile(&mut self) -> Result<Vec<Instruction>> {
        let mut instructions = Vec::new();
        let program_size = self.program.len();
        let ip_address = self.ip;
        self.ip = 0;
        while self.ip < program_size {
            let byte = self.get_u8()?;
            let opcode = OpCode::from_u8(byte).ok_or(anyhow!("Unknown opcode"))?;
            match opcode {
                OpCode::Halt => instructions.push(Instruction::Halt),
                OpCode::Return => instructions.push(Instruction::Return),
                OpCode::Push => instructions.push(Instruction::Push(Box::new(self.get_value()?))),
                OpCode::Add => instructions.push(Instruction::Add(self.get_u8()? as usize)),
                OpCode::Sub => instructions.push(Instruction::Sub(self.get_u8()? as usize)),
                OpCode::Mul => instructions.push(Instruction::Mul(self.get_u8()? as usize)),
                OpCode::Div => instructions.push(Instruction::Div(self.get_u8()? as usize)),
                OpCode::Eq => instructions.push(Instruction::Eq(self.get_u8()? as usize)),
                OpCode::GreaterThan => {
                    instructions.push(Instruction::GreaterThan(self.get_u8()? as usize))
                }
                OpCode::LessThan => {
                    instructions.push(Instruction::LessThan(self.get_u8()? as usize))
                }
                OpCode::GreaterThanOrEqual => {
                    instructions.push(Instruction::GreaterThanOrEqual(self.get_u8()? as usize))
                }
                OpCode::LessThanOrEqual => {
                    instructions.push(Instruction::LessThanOrEqual(self.get_u8()? as usize))
                }
                OpCode::And => instructions.push(Instruction::And(self.get_u8()? as usize)),
                OpCode::Or => instructions.push(Instruction::Or(self.get_u8()? as usize)),
                OpCode::Not => instructions.push(Instruction::Not),
                OpCode::Mod => instructions.push(Instruction::Mod),
                OpCode::Rot => instructions.push(Instruction::Rot),
                OpCode::Call => instructions.push(Instruction::Call(self.get_u8()? as usize)),
                OpCode::TailCall => {
                    instructions.push(Instruction::TailCall(self.get_u8()? as usize))
                }
                OpCode::SetLocal => instructions.push(Instruction::SetLocal),
                OpCode::SetGlobal => instructions.push(Instruction::SetGlobal),
                OpCode::SetFree => instructions.push(Instruction::SetFree),
                OpCode::GetLocal => {
                    instructions.push(Instruction::GetLocal(self.get_u32()? as usize))
                }
                OpCode::GetGlobal => {
                    instructions.push(Instruction::GetGlobal(self.get_u32()? as usize))
                }
                OpCode::GetFree => {
                    instructions.push(Instruction::GetFree(self.get_u32()? as usize))
                }
                OpCode::JumpIf => instructions.push(Instruction::JumpIf(self.get_u32()? as usize)),
                OpCode::JumpForward => {
                    instructions.push(Instruction::JumpForward(self.get_u32()? as usize))
                }
                OpCode::JumpBackward => {
                    instructions.push(Instruction::JumpBackward(self.get_u32()? as usize))
                }
                OpCode::Jump => instructions.push(Instruction::Jump(self.get_u32()? as usize)),
            }
        }
        self.ip = ip_address;
        Ok(instructions)
    }
}

#[cfg(test)]
mod tests {
    use super::{Machine, Value};
    use anyhow::Result;
    #[test]
    fn test_single_value() -> Result<()> {
        let mut machine = Machine::default();
        machine.run_from_string(r#"12345"#)?;
        machine.run()?;
        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(12345.0));
        assert_eq!(machine.cycle_count, 2);

        let mut machine = Machine::default();
        machine.run_from_string(r#"321.123"#)?;
        machine.run()?;
        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(321.123));

        let mut machine = Machine::default();
        machine.run_from_string(r#""string :)""#)?;
        machine.run()?;
        let frame = machine.get_current_frame()?;
        assert_eq!(
            frame.stack[0],
            Value::String(Box::new(String::from("string :)")))
        );
        assert_eq!(machine.cycle_count, 2);
        assert_eq!(machine.cycle_count, 2);
        Ok(())
    }

    #[test]
    fn test_add_function() -> Result<()> {
        let src = r#"(+ 1 2)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(3.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_sub_function() -> Result<()> {
        let src = r#"(- 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(1.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_mul_function() -> Result<()> {
        let src = r#"(* 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(2.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_div_function() -> Result<()> {
        let src = r#"(/ 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(2.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_eq_function() -> Result<()> {
        let src = r#"(= 2 2 2)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
        Ok(())
    }

    #[test]
    fn test_gt_function() -> Result<()> {
        let src = r#"(> 3 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
        Ok(())
    }

    #[test]
    fn test_lt_function() -> Result<()> {
        let src = r#"(< 2 3 4)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
        Ok(())
    }

    #[test]
    fn test_gte_function() -> Result<()> {
        let src = r#"(>= 3 3 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 6);
        Ok(())
    }

    #[test]
    fn test_lte_function() -> Result<()> {
        let src = r#"(<= 1 3 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 6);
        Ok(())
    }

    #[test]
    fn test_and_function() -> Result<()> {
        let src = r#"(and true true)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_or_function() -> Result<()> {
        let src = r#"(or false true)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_not_function() -> Result<()> {
        let src = r#"(not false)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 3);
        Ok(())
    }

    #[test]
    fn test_mod_function() -> Result<()> {
        let src = r#"(mod 4 2)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(0.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_calling_a_function() -> Result<()> {
        let src = r#"
        (fn add (x y) (+ x y))
        (add 123 321)
        "#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(444.0));
        assert_eq!(machine.cycle_count, 12);
        Ok(())
    }

    #[test]
    fn test_calling_a_lambda() -> Result<()> {
        let src = r#"
        (var add (lambda (x y) (+ x y)))
        (add 123 321)
        "#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(444.0));
        assert_eq!(machine.cycle_count, 12);
        Ok(())
    }

    #[test]
    fn test_var_function() -> Result<()> {
        let src = r#"
        (var X 100)
        (var Y 300)
        (fn add (x y) (+ x y))
        (add X Y)
        "#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(200.0));
        assert_eq!(machine.cycle_count, 16);
        Ok(())
    }

    #[test]
    fn test_if_else_function() -> Result<()> {
        let mut machine = Machine::default();
        machine.run_from_string("(if true 1 3)")?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(1.0));
        assert_eq!(machine.cycle_count, 5);

        let mut machine = Machine::default();
        machine.run_from_string("(if false 1 3)")?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(5.0));
        assert_eq!(machine.cycle_count, 5);
        Ok(())
    }

    #[test]
    fn test_let_function() -> Result<()> {
        let mut machine = Machine::default();
        machine.run_from_string("(let (x 10) x)")?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(10.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }
}

// --------------------- Intrinsics ---------------------
mod intrinsic {
    use super::{Machine, Value};
    use anyhow::Result;
    use std::io::Write;

    pub(crate) struct Intrinsic;
    impl Intrinsic {
        pub(crate) fn intrinsic_sleep(machine: &mut Machine, count: u8) -> Result<()> {
            // (sleep 1000) ; -> false
            if count != 1 {
                anyhow::bail!("sleep only support 1 arg");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::F64(value)) = frame.stack.pop() else {
                anyhow::bail!("expected number on stack for sleep")
            };
            std::thread::sleep(std::time::Duration::from_millis(value as u64));
            Ok(())
        }

        pub(crate) fn intrinsic_is_atom(machine: &mut Machine, count: u8) -> Result<()> {
            // (atom? 10) ; -> true
            // (atom? "10") ; -> true
            // (atom? "abc") ; -> true
            // (atom? '(1 2 3)) ; -> false
            if count != 1 {
                anyhow::bail!("atom? only support 1 arg");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(item) = frame.stack.pop() else {
                anyhow::bail!("expected atom on stack for atom?")
            };
            let result = !matches!(item, Value::List(_) | Value::Callable(_));
            frame.stack.push(Value::Bool(result));
            Ok(())
        }

        pub(crate) fn intrinsic_is_number(machine: &mut Machine, count: u8) -> Result<()> {
            // (number? 10) ; -> true
            // (number? "10") ; -> true
            // (number? "abc") ; -> false
            // (number? '(1 2 3)) ; -> false
            if count != 1 {
                anyhow::bail!("number? only support 1 arg");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(item) = frame.stack.pop() else {
                anyhow::bail!("expected number on stack for number?")
            };
            let result = matches!(
                item,
                Value::F64(_) | Value::U32(_) | Value::I32(_) | Value::F32(_) | Value::U8(_)
            );
            frame.stack.push(Value::Bool(result));
            Ok(())
        }

        pub(crate) fn intrinsic_slice(machine: &mut Machine, count: u8) -> Result<()> {
            // (slice "abc" 1 2) ; -> "b"
            // (slice (list 1 2 3) 1 2) ; -> (2)
            if count != 3 {
                anyhow::bail!("slice only support 3 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::F64(end)) = frame.stack.pop() else {
                anyhow::bail!("expected int on stack for slice")
            };
            let Some(Value::F64(start)) = frame.stack.pop() else {
                anyhow::bail!("expected int on stack for slice")
            };
            let Some(item) = frame.stack.pop() else {
                anyhow::bail!("expected string or list on stack for slice")
            };

            match item {
                Value::String(string) => {
                    let result = string
                        .chars()
                        .skip(start as usize)
                        .take((end - start) as usize)
                        .collect::<String>();
                    frame.stack.push(Value::String(Box::new(result)));
                }
                Value::List(list) => {
                    let result = list
                        .iter()
                        .skip(start as usize)
                        .take((end - start) as usize)
                        .cloned()
                        .collect::<Vec<_>>();
                    frame.stack.push(Value::List(Box::new(result)));
                }
                _ => panic!("expected string or list on stack for slice"),
            }
            Ok(())
        }

        pub(crate) fn intrinsic_join(machine: &mut Machine, count: u8) -> Result<()> {
            // (join " " "abc") ; -> "abc"
            // (join " " (list "1" "2" "3")) ; -> "1 2 3"
            if count != 2 {
                anyhow::bail!("join only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(list)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for join")
            };
            let Some(Value::String(string)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for join")
            };
            let result = list
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(&string);
            frame.stack.push(Value::String(Box::new(result)));
            Ok(())
        }

        pub(crate) fn intrinsic_split(machine: &mut Machine, count: u8) -> Result<()> {
            // (split " " "abc") ; -> ("a" "b" "c")
            // (split " " "(+ 1 1)") ; ->  ("(+" "1" "1)")
            if count != 2 {
                anyhow::bail!("split only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::String(string)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for split")
            };
            let Some(Value::String(item)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for split")
            };
            let result = string
                .split(&*item)
                .map(|i| Value::String(Box::new(i.to_string())))
                .collect::<Vec<_>>();
            frame.stack.push(Value::List(Box::new(result)));

            Ok(())
        }
        pub(crate) fn intrinsic_to_string(machine: &mut Machine, count: u8) -> Result<()> {
            // (to-string 1000) => "1000"
            if count != 1 {
                anyhow::bail!("to-string only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for to-string")
            };
            frame.stack.push(Value::String(Box::new(value.to_string())));

            Ok(())
        }

        pub(crate) fn intrinsic_filter(machine: &mut Machine, count: u8) -> Result<()> {
            // (filter (lambda (x) (> x 1)) (list 1 2 3)) ; -> (2 3)

            if count != 2 {
                anyhow::bail!("filter only support 2 args");
            }

            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for filter")
            };
            let Some(Value::Callable(address)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for filter")
            };
            let mut result = Vec::new();
            for value in list.drain(..) {
                machine.push(value.clone())?;
                machine.call_from_address(address, 1)?;
                let Some(Value::Bool(true)) = machine.pop()? else {
                    continue;
                };
                result.push(value);
            }
            machine.push(Value::List(Box::new(result)))?;
            Ok(())
        }
        pub(crate) fn intrinsic_fold_right(machine: &mut Machine, count: u8) -> Result<()> {
            // (fold-right 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
            if count != 3 {
                anyhow::bail!("fold only support 3 args");
            }
            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for fold")
            };
            let Some(Value::Callable(address)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for fold")
            };
            let Some(initial) = machine.pop()? else {
                anyhow::bail!("expected number on stack for fold")
            };
            let mut result = initial;
            for value in list.drain(..).rev() {
                machine.push(result)?;
                machine.push(value)?;
                machine.call_from_address(address, 2)?;
                let Some(v) = machine.pop()? else {
                    // TODO: ERROR REPORTING
                    anyhow::bail!("expected value on stack for fold right")
                };
                result = v;
            }
            machine.push(result)?;
            Ok(())
        }

        pub(crate) fn intrinsic_fold(machine: &mut Machine, count: u8) -> Result<()> {
            // (fold 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
            if count != 3 {
                anyhow::bail!("fold only support 3 args");
            }
            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for fold")
            };
            let Some(Value::Callable(address)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for fold")
            };
            let Some(initial) = machine.pop()? else {
                anyhow::bail!("expected number on stack for fold")
            };
            let mut result = initial;
            for value in list.drain(..) {
                machine.push(result)?;
                machine.push(value)?;
                machine.call_from_address(address, 2)?;
                let Some(v) = machine.pop()? else {
                    // TODO: ERROR REPORTING
                    anyhow::bail!("expected value on stack for fold")
                };
                result = v;
            }
            machine.push(result)?;
            Ok(())
        }

        pub(crate) fn intrinsic_map(machine: &mut Machine, count: u8) -> Result<()> {
            // (map (lambda (x) (+ x 1)) (list 1 2 3)) => (2 3 4)
            if count != 2 {
                anyhow::bail!("map only support 2 args");
            }

            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for map")
            };

            let Some(Value::Callable(address)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for map")
            };
            let mut result = Vec::new();
            for value in list.drain(..) {
                machine.push(value)?;
                machine.call_from_address(address, 1)?;
                let Some(v) = machine.pop()? else {
                    // TODO: ERROR REPORTING
                    anyhow::bail!("expected value on stack for map")
                };
                result.push(v);
            }

            machine.push(Value::List(Box::new(result)))?;
            Ok(())
        }

        pub(crate) fn intrinsic_nth(machine: &mut Machine, count: u8) -> Result<()> {
            // (nth (list 1 2 3) 1) => 2
            if count != 2 {
                anyhow::bail!("nth only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::F64(index)) = frame.stack.pop() else {
                anyhow::bail!("expected number on stack for nth")
            };
            let Some(Value::List(mut list)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for nth")
            };
            let index = index as usize;
            if index >= list.len() {
                anyhow::bail!("nth index out of range");
            }
            let value = list.remove(index);
            frame.stack.push(value);
            Ok(())
        }

        pub(crate) fn intrinsic_reverse(machine: &mut Machine, count: u8) -> Result<()> {
            // (reverse (list 1 2 3)) => (3 2 1)
            if count != 1 {
                anyhow::bail!("reverse only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(Value::List(mut list)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for reverse")
            };
            list.reverse();
            frame.stack.push(Value::List(list));
            Ok(())
        }

        pub(crate) fn intrinsic_append(machine: &mut Machine, count: u8) -> Result<()> {
            // (append (list 1 2) (list 3 4)) => (1 2 3 4)
            if count != 2 {
                anyhow::bail!("append only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(mut list2)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for append")
            };
            let Some(Value::List(mut list1)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for append")
            };

            list1.append(&mut list2);
            frame.stack.push(Value::List(list1));
            Ok(())
        }

        pub(crate) fn intrinsic_last(machine: &mut Machine, count: u8) -> Result<()> {
            // (last (list 1 2 3 4)) => 4
            if count != 1 {
                anyhow::bail!("last only support 1 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for last")
            };
            match &value {
                Value::List(list) if list.is_empty() => {
                    frame.stack.push(Value::List(Box::default()));
                }
                Value::List(list) => {
                    if let Some(value) = list.last() {
                        frame.stack.push(value.clone());
                        return Ok(());
                    }
                    anyhow::bail!("expected list to have at least 1 for `last`");
                }
                _ => anyhow::bail!("expected list on stack for last"),
            }
            Ok(())
        }

        pub(crate) fn intrinsic_cdr(machine: &mut Machine, count: u8) -> Result<()> {
            // (cdr (list 1 2)) => (2)
            if count != 1 {
                anyhow::bail!("cdr only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for cdr")
            };
            match &value {
                Value::List(list) if list.is_empty() => {
                    frame.stack.push(Value::List(Box::default()));
                }
                Value::List(list) => {
                    frame.stack.push(Value::List(Box::new(list[1..].to_vec())));
                }
                _ => anyhow::bail!("expected list on stack for cdr"),
            }

            if let Value::List(list) = value {
                if let Some(Value::List(list)) = list.last() {
                    frame.stack.push(list[0].clone());
                }
            }
            Ok(())
        }

        pub(crate) fn intrinsic_typeof(machine: &mut Machine, count: u8) -> Result<()> {
            // (typeof 1) => "Number"
            if count != 1 {
                anyhow::bail!("typeof only support 1 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for typeof")
            };
            frame.stack.push(Value::String(Box::new(value.type_of())));
            Ok(())
        }

        pub(crate) fn intrinsic_print(machine: &mut Machine, count: u8) -> Result<()> {
            use std::io::Write;
            let lock = std::io::stdout().lock();
            let mut writer = std::io::BufWriter::new(lock);
            let mut output = String::new();

            let mut items = Vec::new();
            let frame = machine.get_current_frame_mut()?;
            for i in 0..count {
                let Some(value) = frame.stack.pop() else {
                    panic!("expected value on stack for print")
                };
                output.insert_str(0, &format!("{}", value));
                if i == count - 1 {
                    items.push(value);
                }
            }
            writer.write_all(output.as_bytes())?;
            writer.flush()?;

            for items in items.into_iter().rev() {
                frame.stack.push(items);
            }

            Ok(())
        }

        pub(crate) fn intrinsic_length(machine: &mut Machine, count: u8) -> Result<()> {
            // (length (list 1 2 3)) -> 3
            if count != 1 {
                anyhow::bail!("length only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(list)) = frame.stack.pop() else {
                anyhow::bail!("expected a List on stack for nth");
            };

            frame.stack.push(Value::F64(list.len() as f64));
            Ok(())
        }

        pub(crate) fn intrinsic_assert_eq(machine: &mut Machine, count: u8) -> Result<()> {
            // (assert-eq 1 2)
            if count < 2 {
                anyhow::bail!("assert-eq at least 2 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(result) = frame.stack.pop() else {
                anyhow::bail!("expected a value on stack for assert-eq");
            };

            let mut failed = false;
            let mut message = String::new();
            for _ in 0..count - 1 {
                let Some(value) = frame.stack.pop() else {
                    anyhow::bail!("expected a value on stack for assert-eq");
                };
                failed = result != value;
                if failed {
                    message = format!("assert-eq failed expected {} got {}", result, value);
                    break;
                }
            }
            if failed {
                if let Some(Value::String(result)) = frame.stack.pop() {
                    eprintln!("TEST: {}", result);
                }
                anyhow::bail!(message);
            }

            Ok(())
        }

        pub(crate) fn intrinsic_assert(machine: &mut Machine, count: u8) -> Result<()> {
            // (assert (> 1 2) "1 is not greater than 2")
            if count != 2 {
                anyhow::bail!("assert only support 2 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(Value::String(message)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for assert")
            };
            let Some(Value::Bool(value)) = frame.stack.pop() else {
                anyhow::bail!("expected boolean on stack for assert")
            };
            if !value {
                let _ = writeln!(std::io::stderr(), "{message}");
                std::process::exit(1);
            }

            Ok(())
        }

        pub(crate) fn intrinsic_create_list(machine: &mut Machine, count: u8) -> Result<()> {
            // (create-list 1 2 3) => (1 2 3)
            let mut items = Vec::new();
            let frame = machine.get_current_frame_mut()?;
            for _ in 0..count {
                let Some(item) = frame.stack.pop() else {
                    panic!("expected value on stack for CreateList")
                };
                items.insert(0, item);
            }
            frame.stack.push(Value::List(Box::new(items)));
            Ok(())
        }

        pub(crate) fn intrinsic_cons(machine: &mut Machine, count: u8) -> Result<()> {
            // (cons 1 (list 2 3)) => (1 2 3)
            if count != 2 {
                anyhow::bail!("cons only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(mut list)) = frame.stack.pop() else {
                panic!("expected a List on stack for cons")
            };
            let Some(item) = frame.stack.pop() else {
                panic!("expected value on stack for cons")
            };
            list.insert(0, item);
            frame.stack.push(Value::List(Box::new(*list)));
            Ok(())
        }

        pub(crate) fn intrinsic_car(machine: &mut Machine, count: u8) -> Result<()> {
            // (car (list 1 2)) => 1
            if count != 1 {
                anyhow::bail!("car only support 2 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(Value::List(list)) = frame.stack.pop() else {
                panic!("expected a List on stack for car")
            };
            let item = list.first().cloned().unwrap_or(Value::Bool(false));

            frame.stack.push(item);
            Ok(())
        }
    }
}
