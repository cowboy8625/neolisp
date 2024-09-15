mod ast;
mod builtins;
mod cli;
mod compiler;
mod environment;
mod error;
mod eval;
mod parser;
mod repl;
#[cfg(test)]
mod tests;
mod vm;

use clap::Parser as ClapParser;
// use compiler::compile;

fn main() {
    let args = cli::Cli::parse();

    if args.repl {
        repl::run(args).unwrap();
        return;
    }

    let Some(filename) = args.files.first() else {
        println!("no file specified");
        return;
    };

    let Ok(src) = std::fs::read_to_string(filename) else {
        panic!("failed to read file")
    };

    let lookup_table = match generate_lookup_table(&src) {
        Ok(table) => table,
        Err(errors) => {
            for e in errors.errors.iter() {
                println!("{:?}", e);
            }
            return;
        }
    };
    eprintln!("{:#?}", lookup_table);

    // let program = match compile(&src) {
    //     Ok(program) => program,
    //     Err(e) => {
    //         for e in e {
    //             println!("{:?}", e);
    //         }
    //         return Ok(());
    //     }
    // };

    // let binary_name = filename.split('.').collect::<Vec<&str>>()[0];
    // std::fs::write(binary_name, program.clone())?;

    // if args.decompile {
    //     let binary_name = filename.split('.').collect::<Vec<&str>>()[0];
    //     let (header, decompiled_program) = match compiler::decompile(&program) {
    //         Ok(result) => result,
    //         Err((msg, e)) => {
    //             eprintln!("{msg}");
    //             for e in e {
    //                 eprintln!("{:?}", e);
    //             }
    //             return Ok(());
    //         }
    //     };
    //     std::fs::write(
    //         format!("{binary_name}.xxd"),
    //         format!(
    //             "{:?}\n{}",
    //             header,
    //             decompiled_program
    //                 .into_iter()
    //                 .map(|i| format!("{i}\n"))
    //                 .collect::<String>()
    //         )
    //         .as_bytes(),
    //     )?;
    // }

    // let mut machine = vm::Machine::new(program, args.decompile);
    // for i in args.breakpoints {
    //     machine.add_breakpoint(i);
    // }
    // machine.run()?;
}

use ast::{Expr, Span, Spanned};
use chumsky::prelude::Parser;
use parser::parser;

type SymbolTable = std::collections::HashMap<String, Symbol>;

#[derive(Debug, Clone, PartialEq)]
enum ErrorKind {
    Parse(String),
    MissingFnName(Span),
    MissingFnParams(Span),
    ExpectedFnNameToBeSymbol(Spanned<Expr>),
    ExpectedFnParamsToBeList(Spanned<Expr>),
    ExpectedFnBodyToBeList(Spanned<Expr>),
}

#[derive(Debug)]
struct Errors {
    errors: Vec<ErrorKind>,
}

/// Represents the type of a symbol (could be extended to include more complex types)
#[derive(Debug, Clone, PartialEq)]
enum SymbolType {
    Int,
    Float,
    Function(Vec<SymbolType>, Box<SymbolType>), // (params, return type)
    String,
    Bool,
    Dynamic,
}

/// Represents the kind of a symbol (variable, function, etc.)
#[derive(Debug, Clone, PartialEq)]
enum SymbolKind {
    Variable,
    Function,
    Constant,
}

/// Represents the scope of a symbol
#[derive(Debug, Clone, Copy, PartialEq, Default)]
enum Scope {
    #[default]
    Global,
    Function,
    Block(u32),
}

/// Metadata for a symbol in the lookup table
#[derive(Debug, Clone)]
struct Symbol {
    name: String,
    symbol_type: SymbolType,
    kind: SymbolKind,
    scope: Scope,
    memory_location: Option<u32>,
}

#[derive(Debug, Default)]
struct SymbolWalker {
    table: SymbolTable,
    current_scope: Scope,
    errors: Vec<ErrorKind>,
}

impl SymbolWalker {
    fn walk(mut self, ast: &[Spanned<Expr>]) -> Result<SymbolTable, Errors> {
        for spanned in ast.iter() {
            self.walk_expr(spanned);
        }
        if self.errors.len() > 0 {
            return Err(Errors {
                errors: self.errors,
            });
        }
        Ok(self.table.clone())
    }

    fn walk_expr(&mut self, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(_) => todo!(),
            Expr::String(_) => todo!(),
            Expr::Symbol(_) => {}
            Expr::Number(_) => todo!(),
            Expr::List(elements) if starts_with(elements, "fn") => {
                self.walk_fn(elements, &spanned.span);
                self.current_scope = Scope::Global;
            }
            Expr::List(_) => todo!(),
            Expr::Builtin(_, _) => todo!(),
            Expr::Func(_) => todo!(),
            Expr::Lambda(_) => todo!(),
        }
    }
    fn walk_fn(&mut self, elements: &[Spanned<Expr>], span: &Span) {
        // NOTE: We know that the first element is the "fn" keyword

        let scope = self.current_scope;
        self.current_scope = Scope::Function;

        // Name of the function
        let Some(name_element) = elements.get(1) else {
            self.errors.push(ErrorKind::MissingFnName(span.clone()));
            return;
        };
        let Expr::Symbol(name) = &name_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnNameToBeSymbol(name_element.clone()));
            return;
        };

        // Parameters of the function
        let Some(params_element) = elements.get(2) else {
            self.errors.push(ErrorKind::MissingFnParams(span.clone()));
            return;
        };
        let Expr::List(params) = &params_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnParamsToBeList(params_element.clone()));
            return;
        };

        // Handle the params
        let mut params_symbol_types = Vec::new();
        for param in params.iter() {
            let expr = &param.expr;
            let Expr::Symbol(param_name) = expr else {
                self.errors
                    .push(ErrorKind::ExpectedFnParamsToBeList(params_element.clone()));
                return;
            };
            params_symbol_types.push(SymbolType::Dynamic);
            let symbol = Symbol {
                name: param_name.clone(),
                symbol_type: SymbolType::Dynamic,
                kind: SymbolKind::Variable,
                scope: self.current_scope,
                memory_location: None,
            };
            self.table.insert(param_name.clone(), symbol);
        }

        // Body of the function
        let Some(body_element) = elements.get(2) else {
            self.errors.push(ErrorKind::MissingFnName(span.clone()));
            return;
        };
        let Expr::List(body) = &body_element.expr else {
            self.errors
                .push(ErrorKind::ExpectedFnBodyToBeList(body_element.clone()));
            return;
        };

        // Handle the body
        for element in body.iter() {
            self.walk_expr(element);
        }
        let symbol = Symbol {
            name: name.clone(),
            symbol_type: SymbolType::Function(params_symbol_types, Box::new(SymbolType::Dynamic)),
            kind: SymbolKind::Function,
            scope,
            memory_location: None,
        };
        self.table.insert(name.clone(), symbol);
    }
    // fn process_fn(&mut self, elements: &[Spanned<Expr>]) {
    //     // Function signature looks like (fn name (params) body)
    //     if let Some(Spanned {
    //         expr: Expr::Symbol(fn_name),
    //         ..
    //     }) = elements.get(1)
    //     {
    //         if let Some(Spanned {
    //             expr: Expr::List(params),
    //             ..
    //         }) = elements.get(2)
    //         {
    //             // Collect the function symbol
    //             let param_names: Vec<String> = params
    //                 .iter()
    //                 .filter_map(|spanned| match &spanned.expr {
    //                     Expr::Symbol(param) => Some(param.clone()),
    //                     _ => None,
    //                 })
    //                 .collect();
    //
    //             let symbol = Symbol {
    //                 name: fn_name.clone(),
    //                 kind: SymbolKind::Function(param_names.clone()),
    //                 scope: self.current_scope.clone(),
    //             };
    //
    //             self.symbols.push(symbol);
    //
    //             // Enter a new function scope
    //             let old_scope = self.current_scope.clone();
    //             self.current_scope = Scope::Function(fn_name.clone());
    //
    //             // Add parameters to the scope
    //             for param_name in param_names {
    //                 let param_symbol = Symbol {
    //                     name: param_name.clone(),
    //                     kind: SymbolKind::Variable,
    //                     scope: self.current_scope.clone(),
    //                 };
    //                 self.symbols.push(param_symbol);
    //             }
    //
    //             // Process the function body
    //             if let Some(body) = elements.get(3) {
    //                 self.walk_ast(&body.expr);
    //             }
    //
    //             // Restore the previous scope
    //             self.current_scope = old_scope;
    //         }
    //     }
    // }
}

fn starts_with(elements: &[Spanned<Expr>], start: &str) -> bool {
    if let Some(Spanned {
        expr: Expr::Symbol(symbol),
        ..
    }) = elements.first()
    {
        symbol == start
    } else {
        false
    }
}

fn generate_lookup_table(src: &str) -> Result<SymbolTable, Errors> {
    let ast = get_ast(src)?;
    let symbol_table = SymbolWalker::default().walk(&ast)?;
    Ok(symbol_table)
}

fn get_ast(src: &str) -> Result<Vec<Spanned<Expr>>, Errors> {
    match parser().parse(src) {
        Ok(ast) => Ok(ast),
        Err(errors) => {
            let errors = errors
                .into_iter()
                .map(|e| ErrorKind::Parse(e.to_string()))
                .collect::<Vec<_>>();
            Err(Errors { errors })
        }
    }
}
