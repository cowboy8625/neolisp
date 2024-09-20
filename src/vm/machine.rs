use super::builtin;
use super::instruction::{Callee, Instruction};
use super::value::Value;

pub struct Machine {
    pub program: Vec<Instruction>,
    pub ip: usize,
    pub is_running: bool,
    pub stack: Vec<Value>,
    pub local_stack: Vec<Value>,
    pub breakpoints: Vec<usize>,
}

impl Machine {
    pub fn new(program: Vec<Instruction>) -> Self {
        Self {
            program,
            ip: 0,
            is_running: true,
            stack: Vec::new(),
            local_stack: Vec::new(),
            breakpoints: Vec::new(),
        }
    }

    pub fn add_breakpoint(&mut self, ip: usize) {
        self.breakpoints.push(ip);
    }

    pub fn run_once(&mut self) {
        eprintln!("ip: {}", self.ip);
        let Some(instruction) = &self.program.get(self.ip) else {
            panic!("ip out of bounds")
        };
        self.ip += 1;
        eprintln!("{:#?}", instruction);
        match instruction {
            Instruction::StartAt(address) => self.ip = *address as usize,
            Instruction::Noop => {}
            Instruction::Halt => self.is_running = false,
            Instruction::Return => {
                let Some(Value::U32(address)) = self.stack.pop() else {
                    panic!("expected value on stack for return")
                };
                self.ip = address as usize;
                self.local_stack.clear();
            }
            Instruction::Push(value) => {
                self.stack.push(value.clone());
            }
            Instruction::Add => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Add")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Add")
                };
                eprintln!("{:?} + {:?} = ", left, right);
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::I32(left + right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::F64(left + right));
                    }
                    (Value::String(left), Value::String(right)) => {
                        self.stack.push(Value::String(format!("{}{}", left, right)));
                    }
                    _ => panic!("invalid types for Add"),
                }
            }
            Instruction::Rot => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for rot")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for rot")
                };
                self.stack.push(right);
                self.stack.push(left);
            }
            Instruction::Call(callee, arg_count) => match callee {
                Callee::Function(address) => {
                    self.stack.push(Value::U32(self.ip as u32));
                    self.ip = *address;
                    self.bring_to_top_of_stack(*arg_count as usize);
                }
                Callee::Builtin(name) => self.builtins(name.clone(), *arg_count),
                _ => todo!(),
            },
            Instruction::PopIntoLocalStack => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for PopIntoLocalStack")
                };
                self.local_stack.push(value);
            }
            Instruction::LoadLocal(index) => {
                let Some(value) = self.local_stack.get(*index as usize) else {
                    panic!("expected value on stack for LoadLocal")
                };
                self.stack.push(value.clone());
            }
        }
    }

    pub fn run(&mut self) {
        while self.is_running {
            self.run_once();
        }
    }
}

impl Machine {
    fn bring_to_top_of_stack(&mut self, count: usize) {
        let length = self.stack.len();
        self.stack[length - count..].rotate_left(count);
    }

    fn builtins(&mut self, name: String, arg_count: u8) {
        match name.as_str() {
            "print" => builtin::nlvm_print(self, arg_count).unwrap(),
            "nth" => builtin::nth(self, arg_count).unwrap(),
            "length" => builtin::length(self, arg_count).unwrap(),
            "assert-eq" => builtin::nlvm_assert_eq(self, arg_count).unwrap(),
            "list" => builtin::list(self, arg_count).unwrap(),
            "cons" => builtin::cons(self, arg_count).unwrap(),
            "car" => builtin::car(self, arg_count).unwrap(),
            _ => panic!("unknown builtin: {}", name),
        }
    }
}
