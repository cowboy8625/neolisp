use std::{collections::HashMap, rc::Rc};

pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub function_id: usize,
    pub pc: usize,
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    StackUnderflow,
    TypeMismatch {
        expected: &'static str,
        lhs: &'static str,
        rhs: &'static str,
        sign: &'static str,
    },
    DivisionByZero,
    InvalidInstructionPointer,
    UndefinedFunction(String),
    LabelNotFound(String),
    NotEnoughArguments,
}

impl std::fmt::Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeErrorKind::StackUnderflow => write!(f, "stack underflow"),
            RuntimeErrorKind::TypeMismatch {
                expected,
                lhs,
                rhs,
                sign,
            } => write!(
                f,
                "type mismatch: {} {} {}\nexpected: {}",
                lhs, sign, rhs, expected
            ),
            RuntimeErrorKind::DivisionByZero => write!(f, "division by zero"),
            RuntimeErrorKind::InvalidInstructionPointer => write!(f, "invalid instruction pointer"),
            RuntimeErrorKind::UndefinedFunction(name) => write!(f, "undefined function: {}", name),
            RuntimeErrorKind::LabelNotFound(name) => write!(f, "label not found: {}", name),
            RuntimeErrorKind::NotEnoughArguments => write!(f, "not enough arguments"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Integer(i32),
    Float(f64),
    String(Rc<String>),
    Void,
}

impl Value {
    #[inline(always)]
    fn type_name(&self) -> &'static str {
        match self {
            Value::Bool(_) => "Bool",
            Value::Integer(_) => "Integer",
            Value::Float(_) => "Float",
            Value::String(_) => "String",
            Value::Void => "Void",
        }
    }

    #[inline(always)]
    fn add(self, other: Self) -> Result<Self, RuntimeErrorKind> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (lhs, rhs) => Err(RuntimeErrorKind::TypeMismatch {
                expected: "integer or float",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
                sign: "+",
            }),
        }
    }

    #[inline(always)]
    fn sub(self, other: Self) -> Result<Self, RuntimeErrorKind> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (lhs, rhs) => Err(RuntimeErrorKind::TypeMismatch {
                expected: "integer or float",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
                sign: "-",
            }),
        }
    }

    #[inline(always)]
    fn mul(self, other: Self) -> Result<Self, RuntimeErrorKind> {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (lhs, rhs) => Err(RuntimeErrorKind::TypeMismatch {
                expected: "integer or float",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
                sign: "*",
            }),
        }
    }

    #[inline(always)]
    fn div(self, other: Self) -> Result<Self, RuntimeErrorKind> {
        match (self, other) {
            (Value::Integer(_), Value::Integer(0)) => Err(RuntimeErrorKind::DivisionByZero),
            (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a / b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
            (lhs, rhs) => Err(RuntimeErrorKind::TypeMismatch {
                expected: "integer or float",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
                sign: "/",
            }),
        }
    }

    #[inline(always)]
    fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Integer(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Void => false,
        }
    }

    #[inline(always)]
    fn le(&self, rhs: Value) -> Result<Self, RuntimeErrorKind> {
        match (self, rhs) {
            (Value::Integer(a), Value::Integer(b)) => Ok(Self::Bool(*a <= b)),
            (Value::Float(a), Value::Float(b)) => Ok(Self::Bool(*a <= b)),
            (lhs, rhs) => Err(RuntimeErrorKind::TypeMismatch {
                expected: "integer or float",
                lhs: lhs.type_name(),
                rhs: rhs.type_name(),
                sign: "<=",
            }),
        }
    }

    #[inline(always)]
    fn sub_imm(&self, imm: i32) -> Value {
        match self {
            Value::Integer(i) => Value::Integer(i - imm),
            Value::Float(f) => Value::Float(f - imm as f64),
            _ => unreachable!("should not happen"),
        }
    }

    #[inline(always)]
    fn add_imm(&self, imm: i32) -> Value {
        match self {
            Value::Integer(i) => Value::Integer(i + imm),
            Value::Float(f) => Value::Float(f + imm as f64),
            _ => unreachable!("should not happen"),
        }
    }

    #[inline(always)]
    fn mul_imm(&self, imm: i32) -> Value {
        match self {
            Value::Integer(i) => Value::Integer(i * imm),
            Value::Float(f) => Value::Float(f * imm as f64),
            _ => unreachable!("should not happen"),
        }
    }

    #[inline(always)]
    fn div_imm(&self, imm: i32) -> Value {
        match self {
            Value::Integer(i) => Value::Integer(i / imm),
            Value::Float(f) => Value::Float(f / imm as f64),
            _ => unreachable!("should not happen"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(i) => write!(f, "{i}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(i) => write!(f, "{i}"),
            Value::String(i) => write!(f, "{i}"),
            Value::Void => write!(f, "void"),
        }
    }
}

type Number = i32;

#[derive(Debug)]
pub enum Instruction {
    Nop,
    StoreLocal(usize),
    LoadLocal(usize),
    Dup,
    Push(Value),
    Add,
    Sub,
    Mul,
    Div,
    Print,
    Return,
    ReturnImm(Number),
    ReturnLocal(usize),
    JumpIfTrue(usize),
    Jump(usize),
    Call(usize),
    LessThanEqual,
    SubLocalImm(usize, Number),
    AddLocalImm(usize, Number),
    MulLocalImm(usize, Number),
    DivLocalImm(usize, Number),
    AddImm(Number),
    SubImm(Number),
    MulImm(Number),
    DivImm(Number),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Nop => write!(f, "nop"),
            Instruction::StoreLocal(i) => write!(f, "store-local {i}"),
            Instruction::LoadLocal(i) => write!(f, "load-local {i}"),
            Instruction::Dup => write!(f, "dup"),
            Instruction::Push(i) => write!(f, "push {i}"),
            Instruction::Add => write!(f, "add"),
            Instruction::Sub => write!(f, "sub"),
            Instruction::Mul => write!(f, "mul"),
            Instruction::Div => write!(f, "div"),
            Instruction::Print => write!(f, "print"),
            Instruction::Return => write!(f, "return"),
            Instruction::ReturnImm(imm) => write!(f, "return-imm {imm}"),
            Instruction::ReturnLocal(i) => write!(f, "return-local {i}"),
            Instruction::JumpIfTrue(i) => write!(f, "jump-if-true {i}"),
            Instruction::Jump(i) => write!(f, "jump {i}"),
            Instruction::Call(i) => write!(f, "call {i}"),
            Instruction::LessThanEqual => write!(f, "less-than-equal"),
            Instruction::SubLocalImm(i, imm) => write!(f, "sub-local-imm {i} {imm}"),
            Instruction::AddLocalImm(i, imm) => write!(f, "add-local-imm {i} {imm}"),
            Instruction::MulLocalImm(i, imm) => write!(f, "mul-local-imm {i} {imm}"),
            Instruction::DivLocalImm(i, imm) => write!(f, "div-local-imm {i} {imm}"),
            Instruction::AddImm(imm) => write!(f, "add-imm {imm}"),
            Instruction::SubImm(imm) => write!(f, "sub-imm {imm}"),
            Instruction::MulImm(imm) => write!(f, "mul-imm {imm}"),
            Instruction::DivImm(imm) => write!(f, "div-imm {imm}"),
        }
    }
}

#[derive(Debug)]
pub struct FunctionBuilder {
    arity: usize,
    instruction: Vec<Instruction>,
    name: String,
    labels: HashMap<String, usize>,
    links: Vec<(usize, String)>,
}

impl FunctionBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            instruction: Vec::new(),
            name: name.into(),
            arity: 0,
            labels: HashMap::new(),
            links: Vec::new(),
        }
    }

    pub fn arity(&mut self, arity: usize) -> &mut Self {
        self.arity = arity;
        self
    }

    pub fn label(&mut self, name: impl Into<String>) -> &mut Self {
        self.labels.insert(name.into(), self.instruction.len());
        self
    }

    pub fn store_local(&mut self, index: usize) -> &mut Self {
        self.instruction.push(Instruction::StoreLocal(index));
        self
    }

    pub fn load_local(&mut self, index: usize) -> &mut Self {
        self.instruction.push(Instruction::LoadLocal(index));
        self
    }

    pub fn dup(&mut self) -> &mut Self {
        self.instruction.push(Instruction::Dup);
        self
    }

    pub fn push(&mut self, value: Value) -> &mut Self {
        self.instruction.push(Instruction::Push(value));
        self
    }

    pub fn add(&mut self) -> &mut Self {
        self.instruction.push(Instruction::Add);
        self
    }

    pub fn sub(&mut self) -> &mut Self {
        self.instruction.push(Instruction::Sub);
        self
    }

    pub fn mul(&mut self) -> &mut Self {
        self.instruction.push(Instruction::Mul);
        self
    }

    pub fn div(&mut self) -> &mut Self {
        self.instruction.push(Instruction::Div);
        self
    }

    pub fn print(&mut self) -> &mut Self {
        self.instruction.push(Instruction::Print);
        self
    }

    pub fn ret(&mut self) -> &mut Self {
        self.instruction.push(Instruction::Return);
        self
    }

    pub fn jump_if_true(&mut self, label: impl Into<String>) -> &mut Self {
        self.links.push((self.instruction.len(), label.into()));
        self.instruction.push(Instruction::JumpIfTrue(usize::MAX));
        self
    }

    pub fn jump(&mut self, label: impl Into<String>) -> &mut Self {
        self.links.push((self.instruction.len(), label.into()));
        self.instruction.push(Instruction::Jump(usize::MAX));
        self
    }

    pub fn call(&mut self, name: impl Into<String>) -> &mut Self {
        self.links.push((self.instruction.len(), name.into()));
        self.instruction.push(Instruction::Call(usize::MAX));
        self
    }

    pub fn le(&mut self) -> &mut Self {
        self.instruction.push(Instruction::LessThanEqual);
        self
    }

    pub fn optimize(&mut self) {
        let mut i = 0;
        while i < self.instruction.len() {
            let removed = match &self.instruction[i..] {
                [
                    Instruction::LoadLocal(n),
                    Instruction::Push(Value::Integer(value)),
                    Instruction::Sub,
                    ..,
                ] => {
                    self.instruction[i] = Instruction::SubLocalImm(*n, *value);
                    self.instruction.drain(i + 1..i + 3);
                    2
                }
                [
                    Instruction::LoadLocal(n),
                    Instruction::Push(Value::Integer(value)),
                    Instruction::Add,
                    ..,
                ] => {
                    self.instruction[i] = Instruction::AddLocalImm(*n, *value);
                    self.instruction.drain(i + 1..i + 3);
                    2
                }
                [
                    Instruction::LoadLocal(n),
                    Instruction::Push(Value::Integer(value)),
                    Instruction::Mul,
                    ..,
                ] => {
                    self.instruction[i] = Instruction::MulLocalImm(*n, *value);
                    self.instruction.drain(i + 1..i + 3);
                    2
                }
                [
                    Instruction::LoadLocal(n),
                    Instruction::Push(Value::Integer(value)),
                    Instruction::Div,
                    ..,
                ] => {
                    self.instruction[i] = Instruction::DivLocalImm(*n, *value);
                    self.instruction.drain(i + 1..i + 3);
                    2
                }
                [Instruction::Push(Value::Integer(k)), Instruction::Add, ..] => {
                    self.instruction[i] = Instruction::AddImm(*k);
                    self.instruction.remove(i + 1);
                    1
                }
                [Instruction::Push(Value::Integer(k)), Instruction::Sub, ..] => {
                    self.instruction[i] = Instruction::SubImm(*k);
                    self.instruction.remove(i + 1);
                    1
                }
                [Instruction::Push(Value::Integer(k)), Instruction::Mul, ..] => {
                    self.instruction[i] = Instruction::MulImm(*k);
                    self.instruction.remove(i + 1);
                    1
                }
                [Instruction::Push(Value::Integer(k)), Instruction::Div, ..] => {
                    self.instruction[i] = Instruction::DivImm(*k);
                    self.instruction.remove(i + 1);
                    1
                }
                [
                    Instruction::Push(Value::Integer(k)),
                    Instruction::Return,
                    ..,
                ] => {
                    self.instruction[i] = Instruction::ReturnImm(*k);
                    self.instruction.remove(i + 1);
                    1
                }
                [Instruction::LoadLocal(k), Instruction::Return, ..] => {
                    self.instruction[i] = Instruction::ReturnLocal(*k);
                    self.instruction.remove(i + 1);
                    1
                }
                _ => {
                    i += 1;
                    continue;
                }
            };

            for (pos, _) in self.links.iter_mut() {
                if *pos > i {
                    *pos -= removed;
                }
            }
            for value in self.labels.values_mut() {
                if *value > i {
                    *value -= removed;
                }
            }
        }
    }

    pub fn build(mut self) -> Function {
        self.optimize();
        for (index, instruction) in self.instruction.iter_mut().enumerate() {
            if let Instruction::JumpIfTrue(old_pc) = instruction {
                let (_, label) = self
                    .links
                    .iter()
                    .find(|(x, _)| *x == index)
                    .expect("Label not found");
                if let Some(pc) = self.labels.get(label) {
                    *old_pc = *pc;
                } else {
                    eprintln!("Label for JumpIfTrue {label} not found");
                    std::process::exit(1);
                }
            }

            if let Instruction::Jump(old_pc) = instruction {
                let (_, label) = self
                    .links
                    .iter()
                    .find(|(x, _)| *x == index)
                    .expect("Label not found");
                if let Some(pc) = self.labels.get(label) {
                    *old_pc = *pc;
                } else {
                    eprintln!("Label for Jump {label} not found");
                    std::process::exit(1);
                }
            }
        }
        Function {
            name: self.name,
            arity: self.arity,
            instruction: self.instruction,
            links: HashMap::from_iter(self.links),
        }
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
    arity: usize,
    instruction: Vec<Instruction>,
    links: HashMap<usize, String>,
}

impl Function {
    fn link(&mut self, function_table: &HashMap<Rc<String>, usize>) {
        for (index, instruction) in self.instruction.iter_mut().enumerate() {
            if let Instruction::Call(old_pc) = instruction {
                let name = self.links.get(&index).unwrap_or_else(|| {
                    panic!("{} | Function name not found {:?}", index, self.links)
                });
                if let Some(pc) = function_table.get(name) {
                    *old_pc = *pc
                } else {
                    eprintln!("Function {name} not found");
                    std::process::exit(1);
                }
            }
        }
    }
}

#[derive(Debug)]
struct Frame {
    function_id: usize,
    locals: Vec<Value>,
    stack: Vec<Value>,
    prev_pc: Option<usize>,
    pc: usize,
}

impl Frame {
    fn new(function_id: usize) -> Self {
        Self {
            function_id,
            locals: Vec::new(),
            stack: Vec::with_capacity(16),
            prev_pc: None,
            pc: 0,
        }
    }

    #[inline(always)]
    fn pop(&mut self) -> Result<Value, RuntimeErrorKind> {
        self.stack.pop().ok_or(RuntimeErrorKind::StackUnderflow)
    }

    pub fn execute(&mut self, function: &Function) -> Result<FrameSignal, RuntimeErrorKind> {
        if self.pc >= function.instruction.len() {
            return Ok(FrameSignal::Return);
        }
        let instruction = &function.instruction[self.pc];
        self.prev_pc = Some(self.pc);
        self.pc += 1;

        let signal = match instruction {
            Instruction::Nop => FrameSignal::Continue,
            Instruction::StoreLocal(index) => {
                let size = self.locals.len();
                if size <= *index {
                    self.locals.resize(size * 2, Value::Void);
                }
                self.locals[*index] = self.pop()?;
                FrameSignal::Continue
            }
            Instruction::LoadLocal(index) => {
                self.stack.push(self.locals[*index].clone());
                FrameSignal::Continue
            }
            Instruction::Dup => {
                let value = self
                    .stack
                    .last()
                    .cloned()
                    .ok_or(RuntimeErrorKind::StackUnderflow)?;
                self.stack.push(value.clone());
                FrameSignal::Continue
            }
            Instruction::Push(value) => {
                self.stack.push(value.clone());
                FrameSignal::Continue
            }
            Instruction::Add => {
                let rhs = self.pop()?;
                let lhs = self.pop()?;
                self.stack.push(lhs.add(rhs)?);
                FrameSignal::Continue
            }
            Instruction::Sub => {
                let rhs = self.pop()?;
                let lhs = self.pop()?;
                self.stack.push(lhs.sub(rhs)?);
                FrameSignal::Continue
            }
            Instruction::Mul => {
                let rhs = self.pop()?;
                let lhs = self.pop()?;
                self.stack.push(lhs.mul(rhs)?);
                FrameSignal::Continue
            }
            Instruction::Div => {
                let rhs = self.pop()?;
                let lhs = self.pop()?;
                self.stack.push(lhs.div(rhs)?);
                FrameSignal::Continue
            }
            Instruction::Print => {
                let value = self.pop()?;
                println!("{value}");
                FrameSignal::Continue
            }

            Instruction::Return => FrameSignal::Return,
            Instruction::ReturnImm(imm) => {
                self.stack.push(Value::Integer(*imm));
                FrameSignal::Return
            }
            Instruction::ReturnLocal(index) => {
                self.stack.push(self.locals[*index].clone());
                FrameSignal::Return
            }
            Instruction::JumpIfTrue(next_pc) => {
                let value = self.pop()?;
                if value.is_truthy() {
                    self.pc = *next_pc;
                }
                FrameSignal::Continue
            }
            Instruction::Jump(next_pc) => {
                self.pc = *next_pc;
                FrameSignal::Continue
            }
            Instruction::Call(id) => FrameSignal::CallFunction(*id),
            Instruction::LessThanEqual => {
                let rhs = self.pop()?;
                let lhs = self.pop()?;
                self.stack.push(lhs.le(rhs)?);
                FrameSignal::Continue
            }
            Instruction::SubLocalImm(index, imm) => {
                let value = self
                    .locals
                    .get_mut(*index)
                    .ok_or(RuntimeErrorKind::StackUnderflow)?
                    .sub_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
            Instruction::AddLocalImm(index, imm) => {
                let value = self
                    .locals
                    .get_mut(*index)
                    .ok_or(RuntimeErrorKind::StackUnderflow)?
                    .add_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
            Instruction::MulLocalImm(index, imm) => {
                let value = self
                    .locals
                    .get_mut(*index)
                    .ok_or(RuntimeErrorKind::StackUnderflow)?
                    .mul_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
            Instruction::DivLocalImm(index, imm) => {
                let value = self
                    .locals
                    .get_mut(*index)
                    .ok_or(RuntimeErrorKind::StackUnderflow)?
                    .div_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
            Instruction::AddImm(imm) => {
                let value = self.pop()?.add_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
            Instruction::SubImm(imm) => {
                let value = self.pop()?.sub_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
            Instruction::MulImm(imm) => {
                let value = self.pop()?.mul_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
            Instruction::DivImm(imm) => {
                let value = self.pop()?.div_imm(*imm);
                self.stack.push(value);
                FrameSignal::Continue
            }
        };
        Ok(signal)
    }

    fn reset(&mut self, function_id: usize) {
        self.function_id = function_id;
        self.locals.clear();
        self.stack.clear();
        self.prev_pc = None;
        self.pc = 0;
    }
}

#[derive(Debug)]
enum FrameSignal {
    Continue,
    Return,
    CallFunction(usize),
}

#[derive(Debug)]
pub struct Machine {
    _memory: Vec<Value>,
    function_table: HashMap<Rc<String>, usize>,
    functions: Vec<Function>,
    old_frames: Vec<Frame>,
    call_stack: Vec<Frame>,
}

impl Default for Machine {
    fn default() -> Self {
        Self {
            _memory: Vec::new(),
            function_table: HashMap::new(),
            functions: Vec::new(),
            old_frames: vec![
                Frame::new(usize::MAX),
                Frame::new(usize::MAX),
                Frame::new(usize::MAX),
                Frame::new(usize::MAX),
                Frame::new(usize::MAX),
                Frame::new(usize::MAX),
                Frame::new(usize::MAX),
                Frame::new(usize::MAX),
            ],
            call_stack: Vec::new(),
        }
    }
}

impl Machine {
    pub fn step(&mut self) -> Result<(), RuntimeError> {
        let Some(frame) = self.call_stack.last_mut() else {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidInstructionPointer,
                function_id: 0,
                pc: 0,
            });
        };

        let function = &self.functions[frame.function_id];
        match frame.execute(function) {
            Ok(FrameSignal::CallFunction(function_id)) => {
                if function_id >= self.functions.len() {
                    let name = function
                        .links
                        .get(&frame.pc)
                        .expect("Function name not found");
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::UndefinedFunction(name.to_string()),
                        function_id: frame.function_id,
                        pc: frame.prev_pc.unwrap_or(0),
                    });
                }

                let arity = &self.functions[function_id].arity;
                if frame.stack.len() < *arity {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::NotEnoughArguments,
                        function_id: frame.function_id,
                        pc: frame.prev_pc.unwrap_or(0),
                    });
                }

                let func = &self.functions[frame.function_id];
                let is_tail = if frame.pc < func.instruction.len() {
                    matches!(
                        func.instruction[frame.pc],
                        Instruction::Return
                            | Instruction::ReturnImm(_)
                            | Instruction::ReturnLocal(_)
                    )
                } else {
                    true
                };

                if is_tail && function_id == frame.function_id {
                    let end = frame.stack.len();
                    let args = frame.stack.drain((end - *arity)..end);
                    frame.locals.clear();
                    frame.locals.extend(args);
                    frame.stack.clear();
                    frame.function_id = function_id;
                    frame.pc = 0;
                } else {
                    let mut new_frame = if let Some(mut old_frame) = self.old_frames.pop() {
                        old_frame.reset(function_id);
                        old_frame
                    } else {
                        Frame::new(function_id)
                    };
                    let end = frame.stack.len();
                    new_frame
                        .locals
                        .extend(frame.stack.drain((end - *arity)..end));
                    self.call_stack.push(new_frame);
                }
            }
            Ok(FrameSignal::Return) => {
                let return_value = frame.pop().ok();

                if let Some(old_frame) = self.call_stack.pop() {
                    self.old_frames.push(old_frame);
                }

                if let Some(caller) = self.call_stack.last_mut() {
                    if let Some(return_value) = return_value {
                        caller.stack.push(return_value);
                    } else {
                        caller.stack.push(Value::Void);
                    }
                }
            }
            Ok(FrameSignal::Continue) => (),
            Err(kind) => {
                return Err(RuntimeError {
                    kind,
                    function_id: frame.function_id,
                    pc: frame.prev_pc.unwrap_or(0),
                });
            }
        }
        Ok(())
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        self.link();
        while !self.call_stack.is_empty() {
            self.step()?;
        }
        Ok(())
    }

    pub fn load_function(&mut self, build: Function) -> &mut Self {
        if let Some(id) = self.function_table.get(&build.name) {
            self.functions[*id] = build;
            return self;
        }
        let id = self.functions.len();
        self.function_table.insert(Rc::new(build.name.clone()), id);
        self.functions.push(build);
        self
    }

    fn link(&mut self) {
        for function in self.functions.iter_mut() {
            function.link(&self.function_table);
        }
    }

    pub fn call_function(&mut self, name: &str) -> Result<&mut Self, RuntimeErrorKind> {
        let name = Rc::new(name.to_string());
        let id = self
            .function_table
            .get(&name)
            .ok_or(RuntimeErrorKind::UndefinedFunction(name.to_string()))?;
        let frame = Frame::new(*id);
        self.call_stack.push(frame);
        Ok(self)
    }

    pub fn report(&self, err: RuntimeError) {
        let function_name = if let Some(function) = &self.functions.get(err.function_id) {
            &function.name
        } else {
            "unknown"
        };
        let instruction = &self
            .functions
            .get(err.function_id)
            .map(|f| &f.instruction[err.pc]);
        eprintln!("ERROR: {function_name} - {} at instruction", err.kind);
        eprintln!("{} | {:?}", err.pc, instruction);
        eprintln!("{:?}", self.call_stack.last());
    }

    pub fn dump_instructions(&mut self) {
        self.link();
        for function in self.functions.iter() {
            eprintln!("{}:", function.name);
            for (i, instruction) in function.instruction.iter().enumerate() {
                eprintln!("  {i:02} | {instruction}");
            }
        }
    }
}
