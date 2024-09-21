use super::builtin;
use super::instruction::{Callee, IState, Instruction};
use super::value::Value;

pub struct Machine {
    pub program: Vec<Instruction>,
    pub ip: usize,
    pub is_running: bool,
    pub call_stack: usize,
    pub stack: Vec<Value>,
    pub local_stack: Vec<Vec<Value>>,
    pub global_stack: Vec<Value>,
    pub breakpoints: Vec<usize>,
}

impl Machine {
    pub fn new(program: Vec<Instruction>) -> Self {
        Self {
            program,
            ip: 0,
            is_running: true,
            call_stack: 0,
            stack: Vec::new(),
            local_stack: vec![Vec::new()],
            global_stack: Vec::new(),
            breakpoints: Vec::new(),
        }
    }

    pub fn add_breakpoint(&mut self, ip: usize) {
        self.breakpoints.push(ip);
    }

    pub fn run_once(&mut self) {
        let Some(instruction) = &self.program.get(self.ip) else {
            panic!("ip out of bounds")
        };
        self.ip += 1;
        match instruction {
            Instruction::StartAt(address) => self.ip = *address as usize,
            Instruction::Noop => {}
            Instruction::Halt => self.is_running = false,
            Instruction::Return => {
                let Some(Value::U32(address)) = self.stack.pop() else {
                    panic!("expected value on stack for return")
                };
                self.ip = address as usize;
                self.leaving_local_stack();
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
            Instruction::Call(callee, arg_count) => match callee.clone() {
                Callee::Function(IState::Set(address)) => {
                    let count = *arg_count as usize;
                    self.new_local_stack();
                    self.stack.push(Value::U32(self.ip as u32));
                    self.ip = address;
                    self.bring_to_top_of_stack(count);
                }
                Callee::Lambda(IState::Set(local_address)) => {
                    let Some(Value::Lambda(IState::Set(address))) =
                        self.get_local(local_address).cloned()
                    else {
                        panic!("expected lambda on stack for lambda call")
                    };
                    self.new_local_stack();
                    self.stack.push(Value::U32(self.ip as u32));
                    self.ip = address;
                }
                Callee::Builtin(name) => self.builtins(name.clone(), *arg_count),
                Callee::Function(IState::Unset(name)) => {
                    unreachable!("ERROR IN COMPILER: function location not set: {name}")
                }
                Callee::Lambda(IState::Unset(name)) => {
                    unreachable!("ERROR IN COMPILER: lambda location not set: {name}")
                }
            },
            Instruction::LoadLocal => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for LoadLocal")
                };

                self.push_local(value);
            }
            Instruction::GetLocal(IState::Set(index)) => {
                let Some(value) = self.get_local(*index) else {
                    panic!("expected value on stack for PopIntoLocalStack")
                };
                self.stack.push(value.clone());
            }
            Instruction::LoadGlobal => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for LoadGlobal")
                };

                self.global_stack.push(value);
            }
            Instruction::GetGlobal(IState::Set(index)) => {
                let Some(value) = self.global_stack.get(*index) else {
                    panic!("expected value on stack for GetGlobal")
                };
                self.stack.push(value.clone());
            }
            _ => unimplemented!("unimplemented instruction: {instruction:?}"),
        }
    }

    pub fn run(&mut self) {
        while self.is_running {
            self.run_once();
        }
    }
}

impl Machine {
    fn new_local_stack(&mut self) {
        self.local_stack.push(Vec::new());
        self.call_stack += 1;
    }
    fn leaving_local_stack(&mut self) {
        self.local_stack.pop();
        self.call_stack -= 1;
    }
    fn push_local(&mut self, value: Value) {
        self.local_stack[self.call_stack].push(value);
    }

    fn get_local(&self, index: usize) -> Option<&Value> {
        self.local_stack[self.call_stack].get(index)
    }

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
