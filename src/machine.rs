use super::{
    ast::{Expr, Spanned},
    expr_walker::{
        AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LoopExpr, OperatorExpr, VarExpr,
    },
    parser::parser,
    symbol_table::{Symbol, SymbolKind, SymbolScope, SymbolTable, SymbolTableBuilder},
    BUILTINS,
};
use anyhow::{anyhow, Result};
use chumsky::prelude::Parser;
use core::panic;
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;

#[cfg(debug_assertions)]
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const UNDERLINE: &str = "\x1b[4m";
const RESET: &str = "\x1b[0m";

#[cfg(debug_assertions)]
#[derive(Debug, Default)]
pub enum DebugMode {
    #[default]
    Off,
    Pause,
    PauseAndDisplay,
    Step,
    StepAndDisplay,
    Continue,
    ContinueStart,
}

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
            Self::Call(count) => {
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

/*


(fn add (a b) (+ a b))

LoadLocal a
LoadLocal b
GetLocal 0
GetLocal 1
Add
Return

DefineGlobal add 0 0..6


*/

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
pub struct Compiler<'a> {
    symbol_table: &'a mut SymbolTable,
    lambda_counter: usize,
    options: CompilerOptions,
    offset: usize,
}

impl<'a> Compiler<'a> {
    fn new(symbol_table: &'a mut SymbolTable, options: CompilerOptions) -> Self {
        Self {
            symbol_table,
            lambda_counter: 0,
            options,
            offset: 0,
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

    #[allow(dead_code)]
    fn emit_set_instruction(program: &mut Program, symbol: &Symbol) {
        match symbol.kind {
            SymbolKind::FreeVariable => program.push(Instruction::SetFree),
            SymbolKind::Variable => match symbol.scope {
                SymbolScope::Global => program.push(Instruction::SetGlobal),
                SymbolScope::Function => program.push(Instruction::SetLocal),
                SymbolScope::Free => program.push(Instruction::SetFree),
            },
            SymbolKind::Parameter => program.push(Instruction::SetLocal),
            SymbolKind::Function => todo!(),
            SymbolKind::Lambda => todo!(),
        }
    }

    fn emit_get_instruction(program: &mut Program, symbol: &Symbol) {
        let id = symbol.id;
        match symbol.kind {
            SymbolKind::FreeVariable => program.push(Instruction::GetFree(id)),
            SymbolKind::Variable => match symbol.scope {
                SymbolScope::Global => program.push(Instruction::GetGlobal(id)),
                SymbolScope::Function => program.push(Instruction::GetLocal(id)),
                SymbolScope::Free => program.push(Instruction::GetFree(id)),
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
            SymbolKind::Function => todo!("Function {}", symbol.name),
            SymbolKind::Lambda => todo!(),
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
        let Some(_id) = BUILTINS.iter().position(|b| b == &name) else {
            // TODO: ERROR REPORTING
            panic!("Unknown builtin: {name}");
        };
        program.push(Instruction::Push(Box::new(Value::Builtin(0))));
        program.push(Instruction::Call(args.len() - ARGS));
    }

    fn handle_function(&mut self, program: &mut Program, function: &FunctionExpr) {
        let jump_forward_instruciion_size = Instruction::Jump(0).size();
        let Expr::Symbol(name) = &function.name.expr else {
            // TODO: REPORT ERROR
            panic!(
                "expected symbol for function name but found {:?}",
                function.name.expr
            );
        };

        let start = self.get_program_size(program) + jump_forward_instruciion_size;
        self.symbol_table
            .set_location(Some(name), name, start as u32);

        self.symbol_table.enter_scope(name);

        let Expr::List(_expr_params) = &function.params.expr else {
            // TODO: REPORT ERROR
            panic!(
                "expected list for params but found {:?}",
                &function.params.expr
            );
        };

        let mut body = Vec::new();
        self.walk_expr(&mut body, function.body);

        if name == "main" {
            body.push(Instruction::Halt);
        } else {
            body.push(Instruction::Return);
        }

        self.symbol_table.exit_scope();

        let body_size = self.get_program_size(&body);
        program.push(Instruction::Jump(start + body_size));
        program.extend(body);

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

        self.walk_expr(program, lambda.body);
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

    fn handle_call(&mut self, program: &mut Program, call: &CallExpr) {
        for arg in call.args.iter().rev() {
            self.walk_expr(program, arg);
        }
        self.walk_expr(program, call.callee);
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
        let then_offset = self.get_program_size(&then_chunk);

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
        let Some(symbol) = self.symbol_table.lookup(name) else {
            // TODO: REPORT ERROR
            panic!("Variable `{}` is not defined", name,);
        };
        Self::emit_get_instruction(program, symbol);
    }
}

const INSTRUCTION_CALL: [fn(&mut Machine) -> Result<()>; 28] = [
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

const INTRISICS: [fn(&mut Machine, u8) -> Result<()>; 1] = [Intrinsic::intrinsic_print];

#[derive(Debug)]
struct Frame {
    pub return_address: Option<usize>,
    pub args: Vec<Value>,
    pub stack: Vec<Value>,
}

impl Frame {
    fn bring_to_top_of_stack(&mut self, count: usize) {
        let length = self.stack.len();
        self.stack[length - count..].rotate_left(count.saturating_sub(1));
    }
}

impl Frame {
    fn new(return_address: usize, args: Vec<Value>) -> Self {
        Self {
            return_address: Some(return_address),
            args,
            stack: Vec::new(),
        }
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

#[derive(Debug)]
pub struct Machine {
    program: Vec<u8>,
    global: Vec<Value>,
    free: Vec<Value>,
    stack: Vec<Frame>,
    ip: usize,
    is_running: bool,
    symbol_table: Option<SymbolTable>,
    breakpoints: Vec<usize>,
    #[cfg(debug_assertions)]
    debug_mode: DebugMode,
    #[cfg(any(debug_assertions, test))]
    cycle_count: usize,
}

impl Default for Machine {
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        let mut stack = Vec::with_capacity(1024);
        stack.push(Frame {
            return_address: None,
            args: Vec::with_capacity(256),
            stack: Vec::with_capacity(1024),
        });
        Self {
            program,
            global: Vec::with_capacity(1024),
            free: Vec::with_capacity(1024),
            stack,
            ip: 0,
            is_running: true,
            symbol_table: None,
            breakpoints: Vec::new(),
            #[cfg(debug_assertions)]
            debug_mode: DebugMode::Off,
            #[cfg(any(debug_assertions, test))]
            cycle_count: 0,
        }
    }

    pub fn run_from_string(&mut self, src: &str) -> Result<()> {
        self.is_running = true;

        let ast = parser().parse(src).map_err(|e| {
            anyhow::anyhow!(e.iter().map(|e| e.to_string() + "\n").collect::<String>())
        })?;
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

        self.run()?;
        Ok(())
    }

    pub fn peek_last_stack_value(&self) -> Option<&Value> {
        let frame = self.get_current_frame().ok()?;
        frame.stack.last()
    }

    pub fn pop(&mut self) -> Option<Value> {
        let frame = self.get_current_frame_mut().ok()?;
        frame.stack.pop()
    }

    #[cfg(debug_assertions)]
    pub fn add_breakpoint(&mut self, ip: usize) {
        self.breakpoints.push(ip);
    }

    pub fn run_once(&mut self) -> Result<()> {
        #[cfg(any(debug_assertions, test))]
        {
            self.cycle_count += 1;
        }

        #[cfg(debug_assertions)]
        match self.debug_mode {
            DebugMode::Off if self.breakpoints.contains(&self.ip) => {
                self.debug_mode = DebugMode::PauseAndDisplay;
                return Ok(());
            }
            DebugMode::Off => {}
            DebugMode::Pause => {
                self.debugger()?;
                return Ok(());
            }
            DebugMode::PauseAndDisplay => {
                self.debug_mode = DebugMode::Pause;
                self.debug()?;
                self.debugger()?;
                return Ok(());
            }
            DebugMode::Step => self.debug_mode = DebugMode::Pause,
            DebugMode::StepAndDisplay => self.debug_mode = DebugMode::PauseAndDisplay,
            DebugMode::Continue if self.breakpoints.contains(&self.ip) => {
                self.debug_mode = DebugMode::PauseAndDisplay;
                return Ok(());
            }
            DebugMode::Continue => {}
            DebugMode::ContinueStart => self.debug_mode = DebugMode::Continue,
        }

        let Ok(opcode) = self.get_u8() else {
            self.shutdown();
            #[cfg(debug_assertions)]
            eprintln!("END PROGRAM {}..{}", self.ip, self.program.len());
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
        eprintln!("cycles: {}", self.cycle_count);
        #[cfg(any(debug_assertions, test))]
        eprintln!("stack {:#?}", self.stack);
        Ok(())
    }
}

impl Machine {
    #[cfg(debug_assertions)]
    fn debugger(&mut self) -> Result<()> {
        use std::io::Write;
        let mut input = String::new();
        let mut result = Vec::new();
        while input.is_empty() {
            print!("{RED}debugger> {RESET}");
            std::io::stdout().flush().unwrap();
            std::io::stdin().read_line(&mut input).unwrap();
            result = input.trim().split(' ').collect::<Vec<_>>();
        }
        match result[0] {
            "ni" => {
                let temp = self.ip;
                eprintln!("{:?}", OpCode::from_u8(self.get_u8()?));
                self.ip = temp;
            }
            "set-ip" if result.len() == 2 => {
                let Ok(ip) = result[1].parse::<usize>() else {
                    println!("set-ip <ip> not {RED}{}{RESET}", result[1]);
                    return Ok(());
                };
                self.ip = ip;
            }
            "d" | "display" => self.debug()?,
            "h" | "help" => {
                println!("h  help");
                println!("pfs  print-free-stack");
                println!("pls  print-local-stack");
                println!("pgs  print-global-stack");
                println!("ps   print-stack");
                println!("p    print <index>");
                println!("b    breakpoint <ip>");
                println!("c    continue");
                println!("n    next");
                println!("rot  rotate <count>");
                println!("ip   shows ip address");
                println!("nd   run <next> and <display>");
                println!("q    quit");
            }
            "pfs" | "print-free-stack" => {
                println!("FREE STACK:");
                for (i, item) in self.free.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            "pls" | "print-local-stack" => {
                let frame = self.get_current_frame()?;
                for (i, item) in frame.stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item:?}");
                }
            }
            "pgs" | "print-global-stack" => {
                println!("GLOBAL STACK:");
                for (i, item) in self.global.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            "ps" | "print-stack" => {
                println!("STACK:");
                for (i, item) in self.stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item:#?}");
                }
            }
            "p" | "print" if result.len() == 2 => {
                let Ok(stack_index) = result[1].parse::<usize>() else {
                    println!("print <index> not {RED}{}{RESET}", result[1]);
                    return Ok(());
                };

                if stack_index >= self.stack.len() {
                    println!(
                        "The stack size is {} but your index is out of range {}",
                        self.stack.len(),
                        stack_index
                    );
                    return Ok(());
                }

                let frame = self.get_current_frame()?;
                let item = &frame.stack[self.stack.len() - 1 - stack_index];
                println!("{item}");
            }
            "b" | "breakpoint" if result.len() == 2 => {
                let Ok(ip) = result[1].parse::<usize>() else {
                    println!("breakpoint <ip> not {RED}{}{RESET}", result[1]);
                    return Ok(());
                };
                self.add_breakpoint(ip);
            }
            "p" | "print" => eprintln!("print <index>"),
            "c" | "continue" => self.debug_mode = DebugMode::ContinueStart,
            "n" | "next" => self.debug_mode = DebugMode::Step,
            "rot" if result.len() == 2 => {
                let Ok(count) = result[1].parse::<usize>() else {
                    println!("rotate <count> not {RED}{}{RESET}", result[1]);
                    return Ok(());
                };
                println!("ROT BY: {count}");
                println!("{:?}", self.stack);
                let mut temp = Vec::new();
                for _ in 0..count {
                    temp.push(self.stack.pop().unwrap());
                }
                for item in temp.into_iter() {
                    self.stack.push(item);
                }
                println!("{:?}", self.stack);
            }
            "ip" => println!("ip: 0x{:02X}, {0}", self.ip),
            "nd" => self.debug_mode = DebugMode::StepAndDisplay,
            "q" | "quit" => self.shutdown(),
            _ => println!("{RED}unknown command{RESET} {input}"),
        }
        Ok(())
    }

    pub fn debug(&mut self) -> Result<()> {
        let instructions = self.decompile()?;
        let mut offset = 0;
        for int in instructions.iter() {
            let selected = if self.ip == offset {
                format!("{GREEN}{UNDERLINE}0x{offset:02X} {offset:>3} ")
            } else {
                format!("0x{offset:02X} {offset:>3} ")
            };
            let breakpoint = if self.breakpoints.contains(&offset) {
                "🔴".to_string()
            } else {
                "  ".to_string()
            };
            let bytecode = int
                .to_bytecode()
                .into_iter()
                .fold(String::new(), |acc, i| format!("{acc}{i:02X} "));
            let debug_int = format!("{int:?}");
            eprintln!(
                "{breakpoint}{selected} {debug_int:<20} {}{RESET}",
                bytecode.trim()
            );
            offset += int.size();
        }
        Ok(())
    }
}

// Helpers
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

    fn get_current_frame(&self) -> Result<&Frame> {
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
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::Bool(l == *r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::Bool(l == *r);
                }
                _ => panic!("invalid types for Eq"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_greater_than(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::Bool(l > *r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::Bool(l > *r);
                }
                _ => panic!("invalid types for GreaterThan"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_less_than(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::Bool(l < *r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::Bool(l < *r);
                }
                _ => panic!("invalid types for LessThan"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_greater_than_or_equal(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::Bool(l >= *r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::Bool(l >= *r);
                }
                _ => panic!("invalid types for GreaterThanOrEqual"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_less_than_or_equal(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::Bool(l <= *r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::Bool(l <= *r);
                }
                _ => panic!("invalid types for LessThanOrEqual"),
            }
        }
        frame.stack.push(left);
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
        todo!()
    }

    fn instruction_rot(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        frame.bring_to_top_of_stack(2);
        Ok(())
    }

    fn instruction_call_function(&mut self) -> Result<()> {
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
        let param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count)
        };

        let new_frame = Frame::new(self.ip, param_values);
        self.stack.push(new_frame);
        self.ip = address;
        Ok(())
    }

    fn instruction_set_local(&mut self) -> Result<()> {
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
    fn decompile(&mut self) -> Result<Vec<Instruction>> {
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

struct Intrinsic;
impl Intrinsic {
    fn intrinsic_print(machine: &mut Machine, count: u8) -> Result<()> {
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
}
