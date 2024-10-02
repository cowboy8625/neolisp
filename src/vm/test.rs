use crate::compiler::{compile, compile_to_instructions, CompilerOptions};
use crate::vm::OpCode;
use pretty_assertions::assert_eq;

use super::{Machine, Value};

fn test_compile(src: &str) -> Vec<u8> {
    let options = CompilerOptions { no_main: true };
    compile(src, &options).unwrap()
}

macro_rules! check_return_when {
    ($name:ident, $src:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let program = test_compile($src);
            let mut machine = Machine::new(program);

            machine.run();
            assert_eq!(machine.stack.len(), 1);
            assert_eq!(machine.stack[0], $expected);
        }
    };
}

check_return_when!(add_instrction, "(+ 123 321)", Value::F64(444.));
check_return_when!(sub_instrction, "(- 123 321)", Value::F64(-198.));
check_return_when!(multiply_instrction, "(* 123 321)", Value::F64(39483.));
check_return_when!(divide_instrction, "(/ 444 2 2)", Value::F64(111.));
check_return_when!(equals_instrction, "(= 2 2 2)", Value::Bool(true));
check_return_when!(greater_than_instrction, "(> 5 3 2)", Value::Bool(true));
check_return_when!(less_than_instrction, "(< 2 3 5)", Value::Bool(true));

check_return_when!(
    greater_than_or_equal_instrction,
    "(>= 9 9 6 7 8)",
    Value::Bool(true)
);

check_return_when!(
    less_than_or_eqal_instrction,
    "(<= 2 2 3 5)",
    Value::Bool(true)
);

check_return_when!(
    lambda_is_called,
    "(((lambda (x) (lambda (y) (+ x y))) 1) 3)",
    Value::F64(4.)
);

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
        vec![OpCode::Mul as u8, 0x02],
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
