# NeoLisp

NeoLisp is a lightweight, Lisp-inspired programming language designed with simplicity and extensibility in mind. It is implemented in Rust, offering a balance between performance and safety. NeoLisp is a functional programming language, featuring first-class functions, closures, and easy extensibility.

## Features

- **FFI**: NeoLisp supports foreign function interface (FFI) for calling external libraries and APIs from within the language.
- **Simple Syntax**: NeoLisp features a clean and simple Lisp-inspired syntax, making it easy to learn and use.
- **Functional Programming**: Supports functional programming paradigms, with first-class functions, recursion, and closures.
- **Procedural Programming**: Offers a procedural programming style, allowing for easy integration with existing codebases.
- **Lightweight**: Designed for simplicity and ease of use, making it suitable for quick prototyping and small projects.
- **Extensibility**: Easily extendable, allowing users to define custom functions and expand the language's features.
- **Efficient**: Written in Rust for performance and memory safety.

## Getting Started

To get started with NeoLisp, follow these steps:

1. **Installation**: Clone the NeoLisp repository and build the project using Cargo.

   ```bash
   git clone https://github.com/yourusername/neolisp.git
   cd neolisp
   cargo build --release -- --help
   ```

1. **Run Examples**: Explore the examples provided in the `samples/` directory to understand NeoLisp's syntax and features.

   ```bash
   cargo run --release -- run samples/hello_world.nl
   ```

1. **_Docs_**: View the [documentation](Docs.md) for NeoLisp more in-depth info.

## Syntax

```lisp
;; Comments

(var value 10)

;; Functions

(fn add (x y) (+ x y))

;; Calling Functions

(add 10 20)

;; Conditionals

(if (= 10 10) "10 is equal to 10" "10 is not equal to 10")

;; Let bindings

(let (x 10) (+ x 10))

;; Quoting (worked in tree walker but still working on getting this in the new compiler)

'(10 20 30) ;;  -> (10 20 30)

(struct Point x 10 y 20)
(var point (Point:new :x 10 :y 20))
(var x (Point:get point :x))
(Point:set point :y 30)
```

## Builtin Testing

NeoLisp comes with a built-in testing framework that allows you to write and run tests for your code.

```lisp
;; Builtin testing run with `neolisp test`
(test add (assert-eq :expected 3 :actual (add 1 2) :description "1 + 2 = 3"))
```

By running the `neolisp test` command, NeoLisp will run all the tests in the `src/main.nl` file.
You can also provide a specific test file as an argument to the `neolisp test` command.

```bash
neolisp test
```

## Examples

- [Fibonacci](./samples/fib.nl)
- [Rule110](./samples/rule110.nl)

## Installation

To install NeoLisp, simply run `cargo install --path .` from your terminal while in the `neolisp` directory.

## Usage

To use NeoLisp, simply run the compiled binary with a NeoLisp script file as an argument.

```bash
neolisp run my_script.nl
```

You can also enter the NeoLisp REPL (Read-Eval-Print Loop) by running the binary with the `-r` flag.

```bash
neolisp
```

## Contributing

Contributions to NeoLisp are welcome! Whether you want to report a bug, suggest a feature, or contribute code, please feel free to open an issue or submit a pull request on the GitHub repository.

## License

NeoLisp is licensed under the Apache License. See the [LICENSE](LICENSE) file for details.
