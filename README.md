# NeoLisp

NeoLisp is a simple Lisp-inspired programming language designed for ease of use and extensibility. It is implemented in Rust, offering both performance and safety.

## Features

- **Simple Syntax**: NeoLisp follows a simple syntax inspired by Lisp, making it easy to learn and use.
- **Functional Programming**: Embrace functional programming paradigms with first-class functions, closures, and immutability.
- **Extensibility**: NeoLisp is designed to be easily extensible, allowing users to define their own functions.

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
   cargo run --release -- samples/hello_world.nl
   ```
1. ***Docs***: View the [documentation](Docs.md) for NeoLisp more in-depth info.

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

;; Quoting

'(10 20 30) ;;  -> (10 20 30)
```

## Installation

To install NeoLisp, simply run `cargo install --path .` from your terminal while in the `neolisp` directory.

## Usage

To use NeoLisp, simply run the compiled binary with a NeoLisp script file as an argument.

```bash
neolisp my_script.nl
```

You can also enter the NeoLisp REPL (Read-Eval-Print Loop) by running the binary with the `-r` flag.

```bash
neolisp -r
```

## Contributing

Contributions to NeoLisp are welcome! Whether you want to report a bug, suggest a feature, or contribute code, please feel free to open an issue or submit a pull request on the GitHub repository.

## License

NeoLisp is licensed under the Apache License. See the [LICENSE](LICENSE) file for details.
