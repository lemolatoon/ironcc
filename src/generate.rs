use std::io::{BufWriter, Write};

use crate::{
    parse::{BinOpKind, Binary, Expr, ExprKind},
    tokenize::Position,
};

#[derive(Debug, Clone)]
pub struct Generater<'a> {
    input: &'a str,
}

impl<'a> Generater<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn gen_expr<W: Write>(
        &self,
        f: &mut BufWriter<W>,
        expr: Expr,
    ) -> Result<(), std::io::Error> {
        match expr.kind {
            ExprKind::Num(val) => writeln!(f, "  push {}", val),
            ExprKind::Binary(Binary { kind: op, lhs, rhs }) => {
                self.gen_expr(f, *lhs)?;
                self.gen_expr(f, *rhs)?;
                writeln!(f, "  pop rdi")?;
                writeln!(f, "  pop rax")?;
                match op {
                    BinOpKind::Add => writeln!(f, "  add rax, rdi")?,
                    BinOpKind::Sub => writeln!(f, "  sub rax, rdi")?,
                    BinOpKind::Mul => writeln!(f, "  imul rax, rdi")?,
                    BinOpKind::Div => {
                        // rdx-rax = rax
                        writeln!(f, "  cqo")?;
                        // rax = rdx-rax / rdi
                        // rdx = rdx-rax % rdi
                        writeln!(f, "  idiv rdi")?;
                    }
                };
                writeln!(f, "  push rax")?;
                Ok(())
            }
            ExprKind::Unary(_, _) => unimplemented!(),
        }
    }
    pub fn error_at(&self, pos: impl Into<Option<Position>>, msg: &str) -> ! {
        let pos: Option<Position> = pos.into();
        match pos {
            None => panic!("Passed pos info was None.\n{}", msg),
            Some(pos) => {
                let mut splited = self.input.split('\n');
                let line = splited.nth(pos.n_line).unwrap_or_else(|| {
                    panic!(
                        "Position is illegal, pos: {:?},\n input: {}",
                        pos, self.input
                    )
                });
                eprintln!("{}", line);
                let mut buffer = String::with_capacity(pos.n_char + 1);
                for _ in 0..pos.n_char {
                    buffer.push(' ');
                }
                buffer.push('^');
                eprintln!("{}", buffer);
                eprintln!("{}", msg);
                panic!();
            }
        }
    }
}
