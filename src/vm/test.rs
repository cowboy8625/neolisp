use crate::compiler::{compile, compile_to_instructions, CompilerOptions};
use crate::vm::OpCode;
use pretty_assertions::assert_eq;

use super::{Machine, Value};

fn test_compile(src: &str) -> Vec<u8> {
    let options = CompilerOptions { no_main: true };
    compile(src, &options).unwrap()
}

#[test]
fn test_add_instrction() {
    let src = r#"
    (+ 123 321)
    "#;

    let program = test_compile(src);
    let mut machine = Machine::new(program);

    machine.run();
    assert_eq!(machine.stack.len(), 1);
    assert_eq!(machine.stack[0], Value::F64(444.));
}

#[test]
fn test_sub_instrction() {
    let src = r#"
    (- 123 321)
    "#;

    let program = test_compile(src);
    let mut machine = Machine::new(program);

    machine.run();
    assert_eq!(machine.stack.len(), 1);
    assert_eq!(machine.stack[0], Value::F64(-198.));
}

#[test]
fn test_mul_instrction() {
    let src = r#"
    (* 123 321)
    "#;

    let program = test_compile(src);
    let mut machine = Machine::new(program);

    machine.run();
    assert_eq!(machine.stack.len(), 1);
    assert_eq!(machine.stack[0], Value::F64(39483.));
}

#[test]
fn test_div_instrction() {
    let src = r#"
    (/ 222 2)
    "#;

    let program = test_compile(src);
    let mut machine = Machine::new(program);

    machine.run();
    assert_eq!(machine.stack.len(), 1);
    assert_eq!(machine.stack[0], Value::F64(111.));
}

#[test]
fn test_lambda() {
    let src = r#"
    (((lambda (x) (lambda (y) (+ x y))) 1) 3)
    "#;
    let program = test_compile(src);
    let mut machine = Machine::new(program);

    machine.run();
    eprintln!("{:#?}", machine.stack);
    assert_eq!(machine.stack.len(), 1);
    assert_eq!(machine.stack[0], Value::F64(4.));
}

// #[test]
// fn test_lambda_calculus() {
//     let src = r#"
// (var TRUE (lambda (x) (lambda (y) x)))
// (var FALSE (lambda (x) (lambda (y) x)))
// (var AND (lambda (x) (lambda (y) ((x y) x))))
//
// (fn main () (print ((((AND TRUE) FALSE) "LEFT") "RIGHT") "\n"))
//     "#;
//     let program = compile(src, &Default::default()).unwrap();
//     let mut machine = Machine::new(program);
//
//     machine.run();
//     eprintln!("{:#?}", machine.stack);
//     assert_eq!(machine.stack.len(), 1);
//     assert_eq!(machine.stack[0], Value::F64(4.));
// }

#[test]
fn test_to_bytecode() {
    let src = r#"(fn main () (print (* 1 2)))"#;

    let instructions = compile_to_instructions(src, &Default::default()).unwrap();
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
