use crate::ast::{Expr, Spanned};
use crate::vm::OpCode;

pub trait WalkAst {
    /// Bool(bool),
    fn walk_bool(&mut self);
    /// String(String),
    fn walk_string(&mut self);
    /// Symbol(String),
    fn walk_symbol(&mut self, symbol: &str);
    /// Number(f64),
    fn walk_number(&mut self, number: &f64);
    /// List(Vec<Spanned<Expr>>),
    fn walk_list(&mut self, list: &[Spanned<Expr>]);
    /// Builtin(Builtin, String),
    fn walk_builtin(&mut self);
    /// Func(Func),
    fn walk_func(&mut self);
    /// Lambda(Lambda),
    fn walk_lambda(&mut self);
    /// Expr
    fn walk_expr(&mut self, expr: &Spanned<Expr>);
    fn walk_ast(&mut self, ast: &[Spanned<Expr>]);
}

#[derive(Debug, Clone, PartialEq)]
enum Instructions {
    F64(f64),
    AddF64,
}

impl Instructions {
    fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Instructions::F64(value) => {
                let mut bytes = vec![OpCode::PushF64 as u8];
                bytes.extend(value.to_le_bytes());
                bytes
            }
            Instructions::AddF64 => vec![OpCode::AddF64 as u8],
        }
    }
}

#[derive(Debug, Default)]
pub struct Compile {
    program: Vec<Instructions>,
}

impl Compile {
    pub fn compile(&mut self, ast: &[Spanned<Expr>]) -> Vec<u8> {
        self.walk_ast(ast);
        let mut bytes = Vec::new();
        for instruction in self.program.iter() {
            bytes.extend(instruction.to_bytecode());
        }
        bytes
    }
}

impl WalkAst for Compile {
    fn walk_bool(&mut self) {
        todo!()
    }

    fn walk_string(&mut self) {
        todo!()
    }

    fn walk_symbol(&mut self, symbol: &str) {
        match symbol {
            "+" => self.program.push(Instructions::AddF64),
            _ => unimplemented!("{} is not implemented", symbol),
        }
    }

    fn walk_number(&mut self, number: &f64) {
        // TODO: Push a f32
        self.program.push(Instructions::F64(*number));
    }

    fn walk_list(&mut self, list: &[Spanned<Expr>]) {
        for expr in list.iter().rev() {
            self.walk_expr(expr);
        }
    }

    fn walk_builtin(&mut self) {
        todo!()
    }

    fn walk_func(&mut self) {
        todo!()
    }

    fn walk_lambda(&mut self) {
        todo!()
    }

    fn walk_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.expr {
            Expr::Bool(_) => self.walk_bool(),
            Expr::String(_) => self.walk_string(),
            Expr::Symbol(symbol) => self.walk_symbol(symbol),
            Expr::Number(num) => self.walk_number(num),
            Expr::List(list) => self.walk_list(list),
            Expr::Builtin(_, _) => self.walk_builtin(),
            Expr::Func(_) => self.walk_func(),
            Expr::Lambda(_) => self.walk_lambda(),
        }
    }

    fn walk_ast(&mut self, ast: &[Spanned<Expr>]) {
        for expr in ast.iter() {
            self.walk_expr(expr);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parser;
    use chumsky::Parser;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_compile_s_expr() -> anyhow::Result<()> {
        let src = "(+ 1 2)";
        let ast = match parser().parse(src) {
            Ok(ast) => ast,
            Err(error) => {
                eprintln!("{:#?}", error);
                anyhow::bail!("failed to parse");
            }
        };

        let mut compiler = Compile::default();
        compiler.walk_ast(&ast);

        assert_eq!(
            compiler.program,
            vec![
                Instructions::F64(2.),
                Instructions::F64(1.),
                Instructions::AddF64,
            ]
        );

        // assert_eq!(
        //     compiler.compile(),
        //     vec![
        //         0x11, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x11, 0x00, 0x00, 0x00, 0x00,
        //         0x00, 0x00, 0xF0, 0x3F, 0x02,
        //     ]
        // );
        Ok(())
    }
}
