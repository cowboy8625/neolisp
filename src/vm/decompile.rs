use super::{Callee, Direction, Instruction, OpCode, Value};
use num_traits::FromPrimitive;
pub fn decompile(bytes: &[u8]) -> Vec<Instruction> {
    let mut ip = 0;
    let mut instructions = Vec::new();

    while ip < bytes.len() {
        let instruction = match get_instruction(bytes, &mut ip) {
            Ok(instruction) => instruction,
            Err(e) => panic!("{}\n{:#?}", e, instructions),
        };
        instructions.push(instruction);
    }
    instructions
}

pub fn get_instruction(bytes: &[u8], ip: &mut usize) -> Result<Instruction, String> {
    let Some(opcode) = OpCode::from_u8(bytes[*ip]) else {
        return Err(format!(
            "Unknown instruction `{} {:?}`",
            bytes[*ip],
            &bytes[*ip..]
                .iter()
                .map(|x| format!("{x:02x}"))
                .collect::<Vec<_>>()
        ));
    };
    *ip += 1;

    match opcode {
        OpCode::StartAt => {
            let address = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Instruction::StartAt(address as usize))
        }
        OpCode::Noop => Ok(Instruction::Noop),
        OpCode::Halt => Ok(Instruction::Halt),
        OpCode::Return => Ok(Instruction::Return),
        OpCode::Push => {
            let value = get_value(bytes, ip)?;
            Ok(Instruction::Push(value))
        }
        OpCode::Add => {
            let count = bytes[*ip];
            *ip += 1;
            Ok(Instruction::Add(count))
        }
        OpCode::Sub => {
            let count = bytes[*ip];
            *ip += 1;
            Ok(Instruction::Sub(count))
        }
        OpCode::Mul => {
            let count = bytes[*ip];
            *ip += 1;
            Ok(Instruction::Mul(count))
        }
        OpCode::Div => {
            let count = bytes[*ip];
            *ip += 1;
            Ok(Instruction::Div(count))
        }
        OpCode::Eq => {
            let count = bytes[*ip];
            *ip += 1;
            Ok(Instruction::Eq(count))
        }
        OpCode::GreaterThan => Ok(Instruction::GreaterThan),
        OpCode::LessThan => Ok(Instruction::LessThan),
        OpCode::GreaterThanOrEqual => Ok(Instruction::GreaterThanOrEqual),
        OpCode::LessThanOrEqual => Ok(Instruction::LessThanOrEqual),
        OpCode::And => Ok(Instruction::And),
        OpCode::Or => Ok(Instruction::Or),
        OpCode::Not => Ok(Instruction::Not),
        OpCode::Mod => Ok(Instruction::Mod),
        OpCode::Rot => Ok(Instruction::Rot),
        OpCode::Call => {
            let callee = get_callee(bytes, ip)?;
            let count = bytes[*ip];
            *ip += 1;
            Ok(Instruction::Call(callee, count))
        }
        OpCode::LoadLocal => Ok(Instruction::LoadLocal),
        OpCode::GetLocal => {
            let index = bytes[*ip];
            *ip += 1;
            Ok(Instruction::GetLocal(index as usize))
        }
        OpCode::LoadGlobal => Ok(Instruction::LoadGlobal),
        OpCode::GetGlobal => {
            // FIXME: GetGlobaL probably sould have a u32
            let index = bytes[*ip];
            *ip += 1;
            Ok(Instruction::GetGlobal(index as usize))
        }
        OpCode::LoadFree => Ok(Instruction::LoadFree),
        OpCode::GetFree => {
            let index = bytes[*ip];
            *ip += 1;
            Ok(Instruction::GetFree(index as usize))
        }
        OpCode::JumpIf => {
            let address = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Instruction::JumpIf(address as usize))
        }
        OpCode::Jump => match bytes[*ip] {
            Direction::OPCODE_FORWARD => {
                *ip += 1;
                let address = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
                *ip += 4;
                Ok(Instruction::Jump(Direction::Forward(address as usize)))
            }
            Direction::OPCODE_BACKWARD => {
                *ip += 1;
                let address = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
                *ip += 4;
                Ok(Instruction::Jump(Direction::Backward(address as usize)))
            }
            _ => unreachable!(),
        },
    }
}

fn get_value(bytes: &[u8], ip: &mut usize) -> Result<Value, String> {
    match bytes[*ip] {
        Value::CODE_U8 => {
            *ip += 1;
            let value = bytes[*ip];
            *ip += 1;
            Ok(Value::U8(value))
        }
        Value::CODE_I32 => {
            *ip += 1;
            let value = i32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Value::I32(value))
        }
        Value::CODE_U32 => {
            *ip += 1;
            let value = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Value::U32(value))
        }
        Value::CODE_F32 => {
            *ip += 1;
            let value = f32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Value::F32(value))
        }
        Value::CODE_F64 => {
            *ip += 1;
            let value = f64::from_le_bytes(bytes[*ip..*ip + 8].try_into().unwrap());
            *ip += 8;
            Ok(Value::F64(value))
        }
        Value::CODE_STRING => {
            *ip += 1;
            let length = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            let value = String::from_utf8(bytes[*ip..*ip + length as usize].to_vec()).unwrap();
            *ip += length as usize;
            Ok(Value::String(value))
        }
        Value::CODE_BOOL => {
            *ip += 1;
            let value = bytes[*ip] == 1;
            *ip += 1;
            Ok(Value::Bool(value))
        }
        Value::CODE_LIST => {
            *ip += 1;
            let length = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            let mut values = Vec::new();
            for _ in 0..length {
                values.push(get_value(bytes, ip)?);
            }
            Ok(Value::List(values))
        }
        Value::CODE_CALLABLE => {
            *ip += 1;
            let value = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Value::Callable(value as usize))
        }
        _ => Err(format!("Unknown value `{}`", bytes[*ip])),
    }
}

fn get_callee(bytes: &[u8], ip: &mut usize) -> Result<Callee, String> {
    match bytes[*ip] {
        // Function
        0x00 => {
            *ip += 1;
            Ok(Callee::Function)
        }
        // Builtin(String),
        0x01 => {
            *ip += 1;
            let length = bytes[*ip];
            *ip += 1;
            let name = String::from_utf8_lossy(&bytes[*ip..*ip + length as usize]).to_string();
            *ip += length as usize;
            Ok(Callee::Builtin(name))
        }
        _ => Err(format!("Unknown callee type: {}", bytes[*ip])),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_decompile_start_at() {
        let bytes = vec![OpCode::StartAt as u8, 0x01, 0x00, 0x00, 0x00];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::StartAt(1)]);
    }

    #[test]
    fn test_decompile_noop() {
        let bytes = vec![OpCode::Noop as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Noop]);
    }

    #[test]
    fn test_decompile_halt() {
        let bytes = vec![OpCode::Halt as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Halt]);
    }

    #[test]
    fn test_decompile_return() {
        let bytes = vec![OpCode::Return as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Return]);
    }

    #[test]
    fn test_decompile_push() {
        let bytes = vec![OpCode::Push as u8, 0x00, 0x04];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Push(Value::U8(4))]);
    }

    #[test]
    fn test_decompile_add() {
        let bytes = vec![OpCode::Add as u8, 0x02];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Add(2)]);
    }

    #[test]
    fn test_decompile_sub() {
        let bytes = vec![OpCode::Sub as u8, 0x03];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Sub(3)]);
    }

    #[test]
    fn test_decompile_eq() {
        let bytes = vec![OpCode::Eq as u8, 0x03];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Eq(3)]);
    }

    #[test]
    fn test_decompile_or() {
        let bytes = vec![OpCode::Or as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Or]);
    }

    #[test]
    fn test_decompile_rot() {
        let bytes = vec![OpCode::Rot as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::Rot]);
    }

    #[test]
    fn test_decompile_callable_builtin_print() {
        #[rustfmt::skip]
    let bytes = vec![
        OpCode::Call as u8, // Callable type
        0x01, // Builtin type
        0x05, // Builtin name length
        0x70, 0x72, 0x69, 0x6E, 0x74, // "print"
        0x01, // count
    ];

        let instructions = decompile(&bytes);
        assert_eq!(
            instructions,
            vec![Instruction::Call(Callee::Builtin("print".to_string()), 1)]
        );
    }

    #[test]
    fn test_decompile_load_local() {
        let bytes = vec![OpCode::LoadLocal as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::LoadLocal]);
    }

    #[test]
    fn test_decompile_get_local() {
        let bytes = vec![OpCode::GetLocal as u8, 0x10];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::GetLocal(16)]);
    }

    #[test]
    fn test_decompile_load_global() {
        let bytes = vec![OpCode::LoadGlobal as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::LoadGlobal]);
    }

    #[test]
    fn test_decompile_get_global() {
        let bytes = vec![OpCode::GetGlobal as u8, 0x10];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::GetGlobal(16)]);
    }

    #[test]
    fn test_decompile_load_free() {
        let bytes = vec![OpCode::LoadFree as u8];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::LoadFree]);
    }

    #[test]
    fn test_decompile_get_free() {
        let bytes = vec![OpCode::GetFree as u8, 0x10];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::GetFree(16)]);
    }

    #[test]
    fn test_decompile_jump_if() {
        let bytes = vec![OpCode::JumpIf as u8, 0x10, 0x00, 0x00, 0x00];

        let instructions = decompile(&bytes);
        assert_eq!(instructions, vec![Instruction::JumpIf(16)]);
    }

    #[test]
    fn test_decompile_jump() {
        let bytes = vec![
            OpCode::Jump as u8,
            Direction::OPCODE_FORWARD,
            0x10,
            0x00,
            0x00,
            0x00,
        ];

        let instructions = decompile(&bytes);
        assert_eq!(
            instructions,
            vec![Instruction::Jump(Direction::Forward(16))]
        );

        let bytes = vec![
            OpCode::Jump as u8,
            Direction::OPCODE_BACKWARD,
            0x10,
            0x00,
            0x00,
            0x00,
        ];

        let instructions = decompile(&bytes);
        assert_eq!(
            instructions,
            vec![Instruction::Jump(Direction::Backward(16))]
        );
    }

    #[test]
    fn test_decompile_value_u8() {
        let bytes = vec![0x00, 0x01];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::U8(1));
        assert_eq!(ip, 2);
    }

    #[test]
    fn test_decompile_value_i32() {
        let bytes = vec![0x01, 0x01, 0x00, 0x00, 0x00];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::I32(1));
        assert_eq!(ip, 5);
    }

    #[test]
    fn test_decompile_value_u32() {
        let bytes = vec![0x02, 0x01, 0x00, 0x00, 0x00];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::U32(1));
        assert_eq!(ip, 5);
    }

    #[test]
    fn test_decompile_value_f32() {
        let bytes = vec![0x03, 0x00, 0x00, 0x80, 0x3F];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::F32(1.));
        assert_eq!(ip, 5);
    }

    #[test]
    fn test_decompile_value_f64() {
        let bytes = vec![0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x3F];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::F64(1.));
        assert_eq!(ip, 9);
    }

    #[test]
    fn test_decompile_value_string() {
        let bytes = vec![0x05, 0x05, 0x00, 0x00, 0x00, 0x61, 0x62, 0x63, 0x64, 0x65];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::String("abcde".to_string()));
        assert_eq!(ip, 10);
    }

    #[test]
    fn test_decompile_value_boolean() {
        let bytes = vec![0x06, 0x01];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::Bool(true));
        assert_eq!(ip, 2);
    }

    #[test]
    fn test_decompile_value_list() {
        let bytes = vec![
            0x07, // List type
            0x04, 0x00, 0x00, 0x00, // List length
            0x00, 0x05, // u8 type with a value of 5
            0x01, 0x02, 0x00, 0x00, 0x00, // i32 type with a value of 2
            0x00, 0x01, // u8 type with a value of 1
            0x02, 0x03, 0x00, 0x00, 0x00, // u32 type with a value of 3
        ];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(
            value,
            Value::List(vec![
                Value::U8(5),
                Value::I32(2),
                Value::U8(1),
                Value::U32(3)
            ])
        );
        assert_eq!(ip, 19);
    }

    #[test]
    fn test_decompile_value_callable() {
        let bytes = vec![
            0x08, // Callable type
            0x10, 0x00, 0x00, 0x00, // Callable index
        ];

        let mut ip = 0;
        let value = get_value(&bytes, &mut ip).unwrap();
        assert_eq!(value, Value::Callable(16));
        assert_eq!(ip, 5);
    }

    #[test]
    fn test_decompile_callee_function() {
        let bytes = vec![0x00];
        let mut ip = 0;
        let callee = get_callee(&bytes, &mut ip).unwrap();
        assert_eq!(callee, Callee::Function);
        assert_eq!(ip, 1);
    }

    #[test]
    fn test_decompile_callee_builtin_print() {
        let bytes = vec![0x01, 0x05, 0x70, 0x72, 0x69, 0x6E, 0x74];
        let mut ip = 0;
        let callee = get_callee(&bytes, &mut ip).unwrap();
        assert_eq!(callee, Callee::Builtin("print".to_string()));
        assert_eq!(ip, 7);
    }
}
