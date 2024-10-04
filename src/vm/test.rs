use crate::compiler::{compile, compile_to_instructions, CompilerOptions};
use crate::vm::OpCode;
use pretty_assertions::assert_eq;

use super::{Machine, Value};

fn test_compile(src: &str) -> Vec<u8> {
    let options = CompilerOptions { no_main: true };
    compile(src, &options).unwrap()
}

macro_rules! check_return_when {
    ($name:ident, $src:expr, $expected:expr $(, cycles = $expected_cycles:expr )?) => {
        #[test]
        fn $name() -> anyhow::Result<()> {
            let program = test_compile($src);
            let mut machine = Machine::new(program);

            machine.debug();

            machine.run()?;
            assert_eq!(machine.stack.len(), 1);
            assert_eq!(machine.stack[0], $expected);
            $(
                assert_eq!(machine.cycle_count, $expected_cycles);
            )?
            Ok(())
        }
    };
}

check_return_when!(add_instrction, "(+ 123 321)", Value::F64(444.), cycles = 5);
check_return_when!(sub_instrction, "(- 123 321)", Value::F64(-198.), cycles = 5);
check_return_when!(
    multiply_instrction,
    "(* 123 321)",
    Value::F64(39483.),
    cycles = 5
);
check_return_when!(
    divide_instrction,
    "(/ 444 2 2)",
    Value::F64(111.),
    cycles = 6
);
check_return_when!(
    equals_instrction,
    "(= 2 2 2)",
    Value::Bool(true),
    cycles = 6
);
check_return_when!(
    greater_than_instrction,
    "(> 5 3 2)",
    Value::Bool(true),
    cycles = 6
);
check_return_when!(
    less_than_instrction,
    "(< 2 3 5)",
    Value::Bool(true),
    cycles = 6
);
check_return_when!(
    and_instruction_true,
    "(and true true true)",
    Value::Bool(true),
    cycles = 6
);
check_return_when!(
    and_instruction_false,
    "(and false true true)",
    Value::Bool(false),
    cycles = 6
);
check_return_when!(
    or_instruction_true,
    "(or false false true)",
    Value::Bool(true),
    cycles = 6
);
check_return_when!(
    or_instruction_false,
    "(or false false false)",
    Value::Bool(false),
    cycles = 6
);

check_return_when!(
    greater_than_or_equal_instrction,
    "(>= 9 9 6 7 8)",
    Value::Bool(true),
    cycles = 8
);

check_return_when!(
    less_than_or_eqal_instrction,
    "(<= 2 2 3 5)",
    Value::Bool(true),
    cycles = 7
);

check_return_when!(
    not_instruction_true,
    "(not false)",
    Value::Bool(true),
    cycles = 4
);
check_return_when!(
    not_instruction_false,
    "(not true)",
    Value::Bool(false),
    cycles = 4
);
check_return_when!(mod_instruction, "(mod 4 2)", Value::F64(0.), cycles = 5);

check_return_when!(
    lambda_is_called,
    "(((lambda (x) (lambda (y) (+ x y))) 1) 3)",
    Value::F64(4.),
    cycles = 19
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

#[test]
fn test_cycles_for_fib() {
    let src = r#"
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 30)
"#;

    let program = compile(src, &CompilerOptions { no_main: true }).unwrap();
    let mut machine = Machine::new(program);
    machine.run().unwrap();
    // eprintln!("{:#?}", machine.stack);
    assert_eq!(machine.stack.len(), 1);
    assert_eq!(machine.stack[0], Value::F64(832040.));
    // FIXME: This seems really really really high!
    assert_eq!(machine.cycle_count, 47119398);
}

#[test]
fn testing_function_call_instructions() {
    // NOTE: This is just to make sure a bench mark runs correctly
    // Just delete later if I forget
    use super::instruction::{Callee, Instruction};
    let program = vec![
        // 0x00
        Instruction::Push(Value::F64(1.)), // 0x0A
        // 0x0A
        Instruction::Push(Value::F64(1.)), // 0x0A
        // 0x14
        Instruction::Push(Value::Callable(0x1E)), // 0x06
        // 0x1A
        Instruction::Call(Callee::Function, 2), // 0x03
        // 0x1D
        Instruction::Halt, // 0x01
        // 0x1E
        Instruction::LoadLocal,   // 0x01
        Instruction::LoadLocal,   // 0x01
        Instruction::GetLocal(0), // 0x05?
        Instruction::GetLocal(1), // 0x05?
        Instruction::Add(2),      // 0x02
        Instruction::Rot,         // 0x01
        Instruction::Return,      // 0x01
    ]
    .iter()
    .flat_map(|i| i.to_bytecode())
    .collect::<Vec<u8>>();

    let mut machine = Machine::new(program);

    machine.run().unwrap();
}
