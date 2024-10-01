use crate::compiler::compile_to_instructions;
use crate::vm::OpCode;
use pretty_assertions::assert_eq;

#[test]
fn test_to_bytecode() {
    let src = r#"(fn main () (print (* 1 2)))"#;

    let instructions = compile_to_instructions(src).unwrap();
    assert_eq!(
        instructions[0].to_bytecode(),
        vec![OpCode::StartAt as u8, 0x05, 0x00, 0x00, 0x00],
        "{:?}",
        instructions[0]
    );
    assert_eq!(
        instructions[1].to_bytecode(),
        vec![OpCode::Push as u8, 4, 0, 0, 0, 0, 0, 0, 240, 63],
        "{:?}",
        instructions[1]
    );
    assert_eq!(
        instructions[2].to_bytecode(),
        vec![OpCode::Push as u8, 4, 0, 0, 0, 0, 0, 0, 0, 64],
        "{:?}",
        instructions[2]
    );
    assert_eq!(
        instructions[3].to_bytecode(),
        vec![OpCode::Mul as u8],
        "{:?}",
        instructions[3]
    );
    #[rustfmt::skip]
    assert_eq!(
        instructions[4].to_bytecode(),
        vec![
            OpCode::Call as u8, // Callable type
            0x01, // Builtin type
            0x05, // Builtin name length
            0x70, 0x72, 0x69, 0x6E, 0x74, // "print"
            0x01, // count
        ],
        "{:?}",
        instructions[4]
    );
}
