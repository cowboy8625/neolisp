use super::{Callee, Instruction, Value};
pub fn decompile(bytes: &[u8]) -> Vec<Instruction> {
    let mut ip = 0;
    let mut instructions = Vec::new();

    while ip < bytes.len() {
        let instruction = match get_instructions(&bytes, &mut ip) {
            Ok(instruction) => instruction,
            Err(e) => panic!("{}", e),
        };
        instructions.push(instruction);
    }
    instructions
}

fn get_instructions(bytes: &[u8], ip: &mut usize) -> Result<Instruction, String> {
    match bytes[*ip] {
        Instruction::START_AT => {
            debug_assert!(bytes[*ip..].len() >= 5);
            *ip += 1;
            let address = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Instruction::StartAt(address as usize))
        }
        Instruction::NOOP => {
            debug_assert!(bytes[*ip..].len() >= 1);
            *ip += 1;
            Ok(Instruction::Noop)
        }
        Instruction::HALT => {
            debug_assert!(bytes[*ip..].len() >= 1);
            *ip += 1;
            Ok(Instruction::Halt)
        }
        Instruction::RETURN => {
            debug_assert!(bytes[*ip..].len() >= 1);
            *ip += 1;
            Ok(Instruction::Return)
        }
        Instruction::PUSH => {
            *ip += 1;
            let value = get_value(bytes, ip)?;
            Ok(Instruction::Push(value))
        }
        Instruction::ADD => {
            *ip += 1;
            Ok(Instruction::Add)
        }
        Instruction::SUB => {
            *ip += 1;
            Ok(Instruction::Sub)
        }
        Instruction::EQ => {
            *ip += 1;
            Ok(Instruction::Eq)
        }
        Instruction::OR => {
            *ip += 1;
            Ok(Instruction::Or)
        }
        Instruction::ROT => {
            *ip += 1;
            Ok(Instruction::Rot)
        }
        Instruction::CALL => {
            *ip += 1;
            let callee = get_callee(bytes, ip)?;
            let count = bytes[*ip];
            *ip += 1;
            Ok(Instruction::Call(callee, count))
        }
        Instruction::LOAD_LOCAL => {
            *ip += 1;
            Ok(Instruction::LoadLocal)
        }
        Instruction::GET_LOCAL => {
            *ip += 1;
            let index = bytes[*ip];
            *ip += 1;
            Ok(Instruction::GetLocal(index as usize))
        }
        Instruction::LOAD_GLOBAL => {
            *ip += 1;
            Ok(Instruction::LoadGlobal)
        }
        Instruction::GET_GLOBAL => {
            // FIXME: GetGlobaL probably sould have a u32
            *ip += 1;
            let index = bytes[*ip];
            *ip += 1;
            Ok(Instruction::GetGlobal(index as usize))
        }
        Instruction::LOAD_FREE => {
            *ip += 1;
            Ok(Instruction::LoadFree)
        }
        Instruction::GET_FREE => {
            *ip += 1;
            let index = bytes[*ip];
            *ip += 1;
            Ok(Instruction::GetFree(index as usize))
        }
        Instruction::JUMP_IF => {
            *ip += 1;
            let address = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Instruction::JumpIf(address as usize))
        }
        Instruction::JUMP => {
            *ip += 1;
            let address = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Ok(Instruction::Jump(address as usize))
        }
        value => {
            return Err(format!(
                "Unknown instruction `{value} {:?}`",
                &bytes[*ip..]
                    .iter()
                    .map(|x| format!("{x:02x}"))
                    .collect::<Vec<_>>()
            ))
        }
    }
}

#[test]
fn test_decompile_start_at() {
    let bytes = vec![Instruction::START_AT, 0x01, 0x00, 0x00, 0x00];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::StartAt(1)]);
}

#[test]
fn test_decompile_noop() {
    let bytes = vec![Instruction::NOOP];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Noop]);
}

#[test]
fn test_decompile_halt() {
    let bytes = vec![Instruction::HALT];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Halt]);
}

#[test]
fn test_decompile_return() {
    let bytes = vec![Instruction::RETURN];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Return]);
}

#[test]
fn test_decompile_push() {
    let bytes = vec![Instruction::PUSH, 0x00, 0x04];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Push(Value::U8(4))]);
}

#[test]
fn test_decompile_add() {
    let bytes = vec![Instruction::ADD];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Add]);
}

#[test]
fn test_decompile_sub() {
    let bytes = vec![Instruction::SUB];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Sub]);
}

#[test]
fn test_decompile_eq() {
    let bytes = vec![Instruction::EQ];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Eq]);
}

#[test]
fn test_decompile_or() {
    let bytes = vec![Instruction::OR];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Or]);
}

#[test]
fn test_decompile_rot() {
    let bytes = vec![Instruction::ROT];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Rot]);
}

#[test]
fn test_decompile_callable_builtin_print() {
    #[rustfmt::skip]
    let bytes = vec![
        Instruction::CALL, // Callable type
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
    let bytes = vec![Instruction::LOAD_LOCAL];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::LoadLocal]);
}

#[test]
fn test_decompile_get_local() {
    let bytes = vec![Instruction::GET_LOCAL, 0x10];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::GetLocal(16)]);
}

#[test]
fn test_decompile_load_global() {
    let bytes = vec![Instruction::LOAD_GLOBAL];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::LoadGlobal]);
}

#[test]
fn test_decompile_get_global() {
    let bytes = vec![Instruction::GET_GLOBAL, 0x10];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::GetGlobal(16)]);
}

#[test]
fn test_decompile_load_free() {
    let bytes = vec![Instruction::LOAD_FREE];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::LoadFree]);
}

#[test]
fn test_decompile_get_free() {
    let bytes = vec![Instruction::GET_FREE, 0x10];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::GetFree(16)]);
}

#[test]
fn test_decompile_jump_if() {
    let bytes = vec![Instruction::JUMP_IF, 0x10, 0x00, 0x00, 0x00];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::JumpIf(16)]);
}

#[test]
fn test_decompile_jump() {
    let bytes = vec![Instruction::JUMP, 0x10, 0x00, 0x00, 0x00];

    let instructions = decompile(&bytes);
    assert_eq!(instructions, vec![Instruction::Jump(16)]);
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
