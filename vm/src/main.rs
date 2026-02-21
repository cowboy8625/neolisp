use vm;

fn main() {
    let mut machine = vm::Machine::default();

    let mut fb = vm::FunctionBuilder::new("fib");
    // fb.arity(1)
    //     // if n <= 1 return n
    //     .load_local(0) // n
    //     .dup() // n, n
    //     .push(vm::Value::Integer(1))
    //     .le() // n <= 1
    //     .jump_if_true("base")
    //     // fib(n - 1)
    //     .load_local(0)
    //     .push(vm::Value::Integer(1))
    //     .sub()
    //     .call("fib")
    //     // fib(n - 2)
    //     .load_local(0)
    //     .push(vm::Value::Integer(2))
    //     .sub()
    //     .call("fib")
    //     // add results
    //     .add()
    //     .ret()
    //     // base case
    //     .label("base")
    //     .load_local(0)
    //     .ret();
    fb.arity(3)
        .load_local(0)
        .push(vm::Value::Integer(0))
        .le()
        .jump_if_true("ret_a")
        .load_local(0)
        .push(vm::Value::Integer(1))
        .le()
        .jump_if_true("ret_b")
        .load_local(0)
        .push(vm::Value::Integer(1))
        .sub()
        .load_local(2)
        .load_local(1)
        .load_local(2)
        .add()
        .call("fib")
        .ret()
        .label("ret_a")
        .load_local(1)
        .ret()
        .label("ret_b")
        .load_local(2)
        .ret();
    machine.load_function(fb.build());

    let mut fb = vm::FunctionBuilder::new("main");
    // fb.push(vm::Value::Integer(30)).call("fib").print().ret();
    fb.push(vm::Value::Integer(46))
        .push(vm::Value::Integer(0))
        .push(vm::Value::Integer(1))
        .call("fib")
        .print()
        .ret();
    machine.load_function(fb.build());

    machine.call_function("main").expect("failed to call main");
    if std::env::args().any(|arg| arg == "--debug") {
        machine.dump_instructions();
        std::process::exit(0);
    }
    if let Err(err) = machine.run() {
        machine.report(err);
    }
}
