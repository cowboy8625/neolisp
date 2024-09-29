use core::panic;

use super::builtin;
use super::decompile::{decompile, get_instruction};
use super::instruction::{Callee, Instruction};
use super::value::Value;

const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const UNDERLINE: &str = "\x1b[4m";
const RESET: &str = "\x1b[0m";

#[derive(Debug, Default)]
pub enum DebugMode {
    #[default]
    Off,
    Pause,
    Step,
    Continue,
}

pub struct Machine {
    pub program: Vec<u8>,
    pub ip: usize,
    pub is_running: bool,
    pub call_stack: usize,
    pub stack: Vec<Value>,
    pub local_stack: Vec<Vec<Value>>,
    pub global_stack: Vec<Value>,
    pub free_stack: Vec<Value>,
    pub breakpoints: Vec<usize>,
    pub debug_mode: DebugMode,
}

impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        Self {
            program,
            ip: 0,
            is_running: true,
            call_stack: 0,
            stack: Vec::new(),
            local_stack: vec![Vec::new()],
            global_stack: Vec::new(),
            free_stack: Vec::new(),
            breakpoints: Vec::new(),
            debug_mode: DebugMode::Off,
        }
    }

    pub fn add_breakpoint(&mut self, ip: usize) {
        self.breakpoints.push(ip);
    }

    pub fn run_once(&mut self) {
        match self.debug_mode {
            DebugMode::Off if self.breakpoints.contains(&self.ip) => {
                self.debug_mode = DebugMode::Pause;
                return;
            }
            DebugMode::Pause => {
                self.debugger();
                return;
            }
            DebugMode::Step => self.debug_mode = DebugMode::Pause,
            _ => (),
        }

        let Some(instruction) = get_instruction(&self.program, &mut self.ip).ok() else {
            panic!("ip out of bounds")
        };

        match instruction {
            Instruction::StartAt(address) => self.ip = address as usize,
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
            Instruction::Sub => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Sub")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Sub")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::I32(left - right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::F64(left - right));
                    }
                    _ => panic!("invalid types for Add"),
                }
            }
            Instruction::Mul => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Mul")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Mul")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::I32(left * right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::F64(left * right));
                    }
                    _ => panic!("invalid types for Mul"),
                }
            }
            Instruction::Div => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Div")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Div")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::I32(left / right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::F64(left / right));
                    }
                    _ => panic!("invalid types for Div"),
                }
            }
            Instruction::Eq => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Eq")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Eq")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::Bool(left == right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::Bool(left == right));
                    }
                    (Value::String(left), Value::String(right)) => {
                        self.stack.push(Value::Bool(left == right));
                    }
                    _ => panic!("invalid types for Add"),
                }
            }
            Instruction::GreaterThan => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Greater Than")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Greater Than")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::Bool(left > right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::Bool(left > right));
                    }
                    _ => panic!("invalid types for Greater Than"),
                }
            }
            Instruction::LessThan => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Less Than")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Less Than")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::Bool(left < right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::Bool(left < right));
                    }
                    _ => panic!("invalid types for Less Than"),
                }
            }
            Instruction::GreaterThanOrEqual => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Greater Than or Equal")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Greater Than or Equal")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::Bool(left >= right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::Bool(left >= right));
                    }
                    _ => panic!("invalid types for Greater Than or Equal"),
                }
            }
            Instruction::LessThanOrEqual => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Less Than or Equal")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Less Than or Equal")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::Bool(left <= right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::Bool(left <= right));
                    }
                    _ => panic!("invalid types for Greater Less or Equal"),
                }
            }
            Instruction::And => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Or")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Or")
                };
                match (left, right) {
                    (Value::Bool(left), Value::Bool(right)) => {
                        self.stack.push(Value::Bool(left && right));
                    }
                    _ => panic!("invalid types for Or"),
                }
            }
            Instruction::Or => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Or")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Or")
                };
                match (left, right) {
                    (Value::Bool(left), Value::Bool(right)) => {
                        self.stack.push(Value::Bool(left || right));
                    }
                    _ => panic!("invalid types for Or"),
                }
            }
            Instruction::Not => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for Not")
                };
                match value {
                    Value::Bool(b) => {
                        self.stack.push(Value::Bool(!b));
                    }
                    _ => panic!("invalid types for Not"),
                }
            }
            Instruction::Mod => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for Mod")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for Mod")
                };
                match (left, right) {
                    (Value::I32(left), Value::I32(right)) => {
                        self.stack.push(Value::I32(left % right));
                    }
                    (Value::F64(left), Value::F64(right)) => {
                        self.stack.push(Value::F64(left % right));
                    }
                    _ => panic!("invalid types for Mod"),
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
                Callee::Function => {
                    let count = arg_count as usize;
                    let address = match self.stack.pop() {
                        Some(Value::Callable(address)) => address,
                        None => {
                            panic!("expected value on stack for function call but stack is empty")
                        }
                        value => {
                            panic!(
                                "expected value on stack for function call but got {:?}",
                                value
                            )
                        }
                    };

                    self.new_local_stack();
                    self.stack.push(Value::U32(self.ip as u32));
                    self.ip = address;
                    self.bring_to_top_of_stack(count);
                }
                Callee::Builtin(name) => self.builtins(name.clone(), arg_count),
            },
            Instruction::LoadLocal => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for LoadLocal")
                };

                self.push_local(value);
            }
            Instruction::GetLocal(index) => {
                let Some(value) = self.get_local(index) else {
                    panic!("expected value on stack for GetLocal")
                };
                self.stack.push(value.clone());
            }
            Instruction::LoadGlobal => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for LoadGlobal")
                };

                self.global_stack.push(value);
            }
            Instruction::GetGlobal(index) => {
                let Some(value) = self.global_stack.get(index) else {
                    panic!("expected value on stack for GetGlobal")
                };
                self.stack.push(value.clone());
            }
            Instruction::LoadFree => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for LoadFree")
                };

                self.free_stack.push(value);
            }
            Instruction::GetFree(index) => {
                let Some(value) = self.free_stack.get(index) else {
                    panic!("expected value on stack for GetFree")
                };
                self.stack.push(value.clone());
            }
            Instruction::JumpIf(address) => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for JumpIf")
                };
                if value == Value::Bool(false) {
                    self.ip += address;
                }
            }
            Instruction::Jump(address) => {
                self.ip += address;
            }
        }
    }

    pub fn run(&mut self) {
        while self.is_running {
            self.run_once();
        }
    }

    pub fn call(&mut self, address: usize, arg_count: usize) {
        self.new_local_stack();
        self.stack.push(Value::U32(self.ip as u32));
        self.ip = address;
        self.bring_to_top_of_stack(arg_count);
        loop {
            let mut ip = self.ip;
            let Some(Instruction::Return) = get_instruction(&self.program, &mut ip).ok() else {
                self.run_once();
                continue;
            };
            self.run_once();
            break;
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
            "sleep" => builtin::nlvm_sleep(self, arg_count).unwrap(),
            "atom?" => builtin::nlvm_is_atom(self, arg_count).unwrap(),
            "number?" => builtin::nlvm_is_number(self, arg_count).unwrap(),
            "slice" => builtin::nlvm_slice(self, arg_count).unwrap(),
            "join" => builtin::nlvm_join(self, arg_count).unwrap(),
            "split" => builtin::nlvm_split(self, arg_count).unwrap(),
            "to-string" => builtin::nlvm_to_string(self, arg_count).unwrap(),
            "filter" => builtin::nlvm_filter(self, arg_count).unwrap(),
            "fold-right" => builtin::nlvm_fold_right(self, arg_count).unwrap(),
            "fold" => builtin::nlvm_fold(self, arg_count).unwrap(),
            "map" => builtin::nlvm_map(self, arg_count).unwrap(),
            "nth" => builtin::nlvm_nth(self, arg_count).unwrap(),
            "reverse" => builtin::nlvm_reverse(self, arg_count).unwrap(),
            "append" => builtin::nlvm_append(self, arg_count).unwrap(),
            "last" => builtin::nlvm_last(self, arg_count).unwrap(),
            "cdr" => builtin::nlvm_cdr(self, arg_count).unwrap(),
            "typeof" => builtin::nlvm_typeof(self, arg_count).unwrap(),
            "print" => builtin::nlvm_print(self, arg_count).unwrap(),
            "length" => builtin::length(self, arg_count).unwrap(),
            "assert-eq" => builtin::nlvm_assert_eq(self, arg_count).unwrap(),
            "list" => builtin::list(self, arg_count).unwrap(),
            "cons" => builtin::cons(self, arg_count).unwrap(),
            "car" => builtin::car(self, arg_count).unwrap(),
            _ => panic!("unknown builtin: {}", name),
        }
    }

    fn debugger(&mut self) {
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
            "d" | "display" => self.debug(),
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
                println!("q    quit");
            }
            "pfs" | "print-free-stack" => {
                println!("FREE STACK:");
                for (i, item) in self.free_stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            // HACK: the local stack should be shared with the main stack.
            "pls" | "print-local-stack" => {
                println!("LOCAL STACK:");
                for (i, item) in self.local_stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item:?}");
                }
            }
            "pgs" | "print-global-stack" => {
                println!("GLOBAL STACK:");
                for (i, item) in self.global_stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            "ps" | "print-stack" => {
                println!("STACK:");
                for (i, item) in self.stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            "p" | "print" if result.len() == 2 => {
                let Ok(stack_index) = result[1].parse::<usize>() else {
                    println!("print <index> not {RED}{}{RESET}", result[1]);
                    return;
                };

                if stack_index >= self.stack.len() {
                    println!(
                        "The stack size is {} but your index is out of range {}",
                        self.stack.len(),
                        stack_index
                    );
                    return;
                }

                let item = &self.stack[self.stack.len() - 1 - stack_index];
                println!("{item}");
            }
            "b" | "breakpoint" if result.len() == 2 => {
                let Ok(ip) = result[1].parse::<usize>() else {
                    println!("breakpoint <ip> not {RED}{}{RESET}", result[1]);
                    return;
                };
                self.add_breakpoint(ip);
            }
            "p" | "print" => eprintln!("print <index>"),
            "c" | "continue" => self.debug_mode = DebugMode::Continue,
            "n" | "next" => self.debug_mode = DebugMode::Step,
            "rot" if result.len() == 2 => {
                let Ok(count) = result[1].parse::<usize>() else {
                    println!("rotate <count> not {RED}{}{RESET}", result[1]);
                    return;
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
            "q" | "quit" => self.shutdown(),
            _ => println!("{RED}unknown command{RESET} {input}"),
        }
    }

    fn debug(&self) {
        let instructions = decompile(&self.program);
        let mut offset = 0;
        for int in instructions.iter() {
            let selected = if self.ip == offset {
                format!("{GREEN}{UNDERLINE}0x{offset:02X} {offset:>2} ")
            } else {
                format!("0x{offset:02X} {offset:>2} ")
            };
            let breakpoint = if self.breakpoints.contains(&offset) {
                "🔴".to_string()
            } else {
                "  ".to_string()
            };
            eprintln!("{breakpoint}{selected} {int:?}{RESET}");
            offset += int.size();
        }
    }

    fn shutdown(&mut self) {
        self.is_running = false;
    }
}
