use std::io::{BufWriter, Write};

use crate::{
    analyze::{
        ConvBinOpKind, ConvBinary, ConvExpr, ConvExprKind, ConvProgram, ConvProgramKind, ConvStmt,
        ConvStmtKind, Lvar,
    },
    tokenize::Position,
};

#[derive(Debug, Clone)]
pub struct Generater<'a> {
    input: &'a str,
    label: usize,
}

impl<'a> Generater<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input, label: 0 }
    }

    /// # Errors
    /// return errors when file IO failed
    pub fn gen_head<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        program: ConvProgram,
    ) -> Result<(), std::io::Error> {
        writeln!(f, ".intel_syntax noprefix\n")?;
        writeln!(f, ".global main")?;
        writeln!(f, "main:")?;

        writeln!(f, "  push rbp")?;
        writeln!(f, "  mov rbp, rsp")?;
        // TODO: determine this dynamically by counting the number of local variables
        writeln!(f, "  sub rsp, {}", 26 * 8)?; // 64bit var * 8
        for component in program.into_iter() {
            match component {
                ConvProgramKind::Stmt(stmt) => self.gen_stmt(f, stmt)?,
            }
        }

        // TODO: change this label dynamically base on func name
        writeln!(f, ".Lmain_ret:")?;
        writeln!(f, "  mov rsp, rbp")?;
        writeln!(f, "  pop rbp")?;
        writeln!(f, "  ret")?;
        Ok(())
    }

    pub fn gen_stmt<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        stmt: ConvStmt,
    ) -> Result<(), std::io::Error> {
        match stmt.kind {
            ConvStmtKind::Expr(expr) => {
                self.gen_expr(f, expr)?;
                writeln!(f, "  pop rax")?;
            }
            ConvStmtKind::Return(expr) => {
                self.gen_expr(f, expr)?;
                writeln!(f, "  pop rax")?;
                writeln!(f, "  jmp .Lmain_ret")?;
            }
            ConvStmtKind::If(cond, then, Some(els)) => {
                let label_index = self.label();
                self.gen_expr(f, cond)?;
                writeln!(f, "  pop rax")?; // conditional expr
                writeln!(f, "  cmp rax, 0")?; // false
                writeln!(f, "  je .Lelse{}", label_index)?;
                self.gen_stmt(f, *then)?;
                writeln!(f, "  jmp .Lend{}", label_index)?;
                writeln!(f, ".Lelse{}:", label_index)?;
                self.gen_stmt(f, *els)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmtKind::If(cond, then, None) => {
                let label_index = self.label();
                self.gen_expr(f, cond)?;
                writeln!(f, "  pop rax")?;
                writeln!(f, "  cmp rax, 0")?;
                writeln!(f, "  je .Lend{}", label_index)?;
                self.gen_stmt(f, *then)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmtKind::While(cond, then) => {
                let label_index = self.label();
                writeln!(f, ".Lbegin{}:", label_index)?;
                self.gen_expr(f, cond)?;
                writeln!(f, "  pop rax")?;
                writeln!(f, "  cmp rax, 0")?;
                writeln!(f, "  je .Lend{}", label_index)?;
                self.gen_stmt(f, *then)?;
                writeln!(f, "  jmp .Lbegin{}", label_index)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmtKind::For(init, cond, inc, then) => {
                let label_index = self.label();
                if let Some(init) = init {
                    self.gen_expr(f, init)?;
                }
                writeln!(f, "  pop rax")?;
                writeln!(f, ".Lbegin{}:", label_index)?;
                if let Some(cond) = cond {
                    self.gen_expr(f, cond)?;
                    writeln!(f, "  pop rax")?;
                    writeln!(f, "  cmp rax, 0")?;
                    writeln!(f, "  je .Lend{}", label_index)?;
                }
                self.gen_stmt(f, *then)?;
                if let Some(inc) = inc {
                    self.gen_expr(f, inc)?;
                }
                writeln!(f, "  jmp .Lbegin{}", label_index)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(f, stmt)?;
                }
            }
        };
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
            ConvExprKind::Num(val) => writeln!(f, "  push {}", val)?,
            ConvExprKind::Binary(c_binary) => self.gen_binary(f, c_binary)?,
            ConvExprKind::Lvar(_) => {
                self.gen_lvalue(f, expr)?;
                writeln!(f, "  pop rax")?;
                writeln!(f, "  mov rax, [rax]")?; // curently only 64 bit
                writeln!(f, "  push rax")?;
            }
            ConvExprKind::Assign(lhs, rhs) => {
                self.gen_lvalue(f, *lhs)?;
                self.gen_expr(f, *rhs)?;
                writeln!(f, "  pop rdi")?; // rhs
                writeln!(f, "  pop rax")?; // lhs's addr
                writeln!(f, "  mov [rax], rdi")?; // curently only 64 bit
                writeln!(f, "  push rdi")?; // evaluated value of assign expr is rhs's value
            }
            ConvExprKind::Func(_) => todo!(),
        }
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed
    pub fn gen_lvalue<W: Write>(
        &self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), std::io::Error> {
        match expr.kind {
            ConvExprKind::Lvar(Lvar { offset }) => {
                writeln!(f, "  mov rax, rbp")?;
                writeln!(f, "  sub rax, {}", offset)?;
                writeln!(f, "  push rax")
            }
            _ => self.error_at(
                expr.pos,
                &format!(
                    "This {:?} Expr cannnot be used for generating left value",
                    expr.kind
                ),
            ),
        }
    }

    pub fn gen_binary<W: Write>(
        &self,
        f: &mut BufWriter<W>,
        c_binary: ConvBinary,
    ) -> Result<(), std::io::Error> {
        let ConvBinary { kind: op, lhs, rhs } = c_binary;
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
            ConvBinOpKind::Rem => {
                // rdx-rax = rax
                writeln!(f, "  cqo")?;
                // rax = rdx-rax / rdi
                // rdx = rdx-rax % rdi
                writeln!(f, "  idiv rdi")?;
                writeln!(f, "  mov rax, rdx")?;
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
        }
        writeln!(f, "  push rax")?;
        Ok(())
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

    pub fn label(&mut self) -> usize {
        self.label += 1;
        self.label
    }
}
