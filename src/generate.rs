use std::io::{BufWriter, Write};

use crate::{
    analyze::{ConvBinOpKind, ConvBinary, ConvExpr, ConvExprKind},
    tokenize::Position,
};

#[derive(Debug, Clone)]
pub struct Generater<'a> {
    input: &'a str,
}

impl<'a> Generater<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// # Errors
    /// return errors when file IO failed
    pub fn gen_head<W: Write>(
        &self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), std::io::Error> {
        writeln!(f, ".intel_syntax noprefix\n")?;
        writeln!(f, ".global main")?;
        writeln!(f, "main:")?;
        self.gen_expr(f, expr)?;
        writeln!(f, "  pop rax")?;
        writeln!(f, "  ret")?;
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed
    pub fn gen_expr<W: Write>(
        &self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), std::io::Error> {
        match expr.kind {
            ConvExprKind::Num(val) => writeln!(f, "  push {}", val),
            ConvExprKind::Binary(ConvBinary { kind: op, lhs, rhs }) => {
                self.gen_expr(f, *lhs)?;
                self.gen_expr(f, *rhs)?;
                writeln!(f, "  pop rdi")?;
                writeln!(f, "  pop rax")?;
                match op {
                    ConvBinOpKind::Add => writeln!(f, "  add rax, rdi")?,
                    ConvBinOpKind::Sub => writeln!(f, "  sub rax, rdi")?,
                    ConvBinOpKind::Mul => writeln!(f, "  imul rax, rdi")?,
                    ConvBinOpKind::Div => {
                        // rdx-rax = rax
                        writeln!(f, "  cqo")?;
                        // rax = rdx-rax / rdi
                        // rdx = rdx-rax % rdi
                        writeln!(f, "  idiv rdi")?;
                    }
                    ConvBinOpKind::Eq => {
                        writeln!(f, "  cmp rax, rdi")?;
                        // al : lowwer 8bit of rax
                        // al = flag-reg(eq)
                        writeln!(f, "  sete al")?;
                        writeln!(f, "  movzx rax, al")?;
                    }
                    ConvBinOpKind::Le => {
                        writeln!(f, "  cmp rax, rdi")?;
                        // al = flag-reg(less than or equal to)
                        writeln!(f, "  setle al")?;
                        writeln!(f, "  movzx rax, al")?;
                    }
                    ConvBinOpKind::Lt => {
                        writeln!(f, "  cmp rax, rdi")?;
                        // al = flag-reg(less than)
                        writeln!(f, "  setl al")?;
                        writeln!(f, "  movzx rax, al")?;
                    }
                    ConvBinOpKind::Ne => {
                        writeln!(f, "  cmp rax, rdi")?;
                        // al = flag-reg(not equal to)
                        writeln!(f, "  setne al")?;
                        writeln!(f, "  movzx rax, al")?;
                    }
                };
                writeln!(f, "  push rax")?;
                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    /// # Panics
    /// always
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
