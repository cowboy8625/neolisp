use criterion::{Criterion, criterion_group, criterion_main};
use vm;

pub fn fibbonacci_recursive(c: &mut Criterion) {
    c.bench_function("vm run", |b| {
        b.iter(|| {
            let mut fib = vm::FunctionBuilder::new("fib");
            fib.arity(1)
                // if n <= 1 return n
                .load_local(0) // n
                .dup() // n, n
                .push(vm::Value::Integer(1))
                .le() // n <= 1
                .jump_if_true("base")
                // fib(n - 1)
                .load_local(0)
                .push(vm::Value::Integer(1))
                .sub()
                .call("fib")
                // fib(n - 2)
                .load_local(0)
                .push(vm::Value::Integer(2))
                .sub()
                .call("fib")
                // add results
                .add()
                .ret()
                // base case
                .label("base")
                .load_local(0)
                .ret();

            let mut main = vm::FunctionBuilder::new("main");
            main.push(vm::Value::Integer(32)).call("fib").print().ret();

            let mut machine = vm::Machine::default();
            machine.load_function(fib.build());
            machine.load_function(main.build());
            machine.call_function("main").expect("failed to call main");

            if let Err(err) = machine.run() {
                machine.report(err);
            }
        })
    });
}

criterion_group!(benches, fibbonacci_recursive,);
criterion_main!(benches);
