**cargo**
```shell
cargo bench
```

**hyperfind**
```shell
hyperfine --warmup 30 --runs 200 'cargo run -r -- run samples/fib.nl' 'neolisp run samples/fib.nl'
```

**flamegraph**
```shell
cargo flamegraph --dev -- run
```

**perf**
```shell
curl cheat.sh/perf
```
