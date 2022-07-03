use std::io::{BufWriter, Write};

use crate::{
    analyze::{
        ConvBinOpKind, ConvBinary, ConvExpr, ConvExprKind, ConvFuncDef, ConvProgram,
        ConvProgramKind, ConvStmt, ConvStmtKind, Lvar,
    },
    tokenize::Position,
};

#[derive(Debug, Clone)]
pub struct Generater<'a> {
    input: &'a str,
    label: usize,
    depth: usize,
    current_func_name: Option<String>,
}

impl<'a> Generater<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self {
            input,
            label: 0,
            depth: 0,
            current_func_name: None,
        }
    }

    pub fn push<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        arg: std::fmt::Arguments,
    ) -> Result<(), std::io::Error> {
        writeln!(f, "  push {}", arg)?;
        self.depth += 1;
        Ok(())
    }

    pub fn pop<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        arg: std::fmt::Arguments,
    ) -> Result<(), std::io::Error> {
        writeln!(f, "  pop {}", arg)?;
        self.depth -= 1;
        Ok(())
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

        for component in program.into_iter() {
            match component {
                ConvProgramKind::Func(ConvFuncDef { name, args, body }) => {
                    writeln!(f, "{}:", name)?;
                    self.current_func_name = Some(name.clone());
                    self.push(f, format_args!("rbp"))?;
                    writeln!(f, "  mov rbp, rsp")?;
                    // TODO: determine this dynamically by counting the number of local variables
                    writeln!(f, "  sub rsp, {}", 26 * 8)?; // 64bit var * 8
                    self.gen_stmt(f, body)?;
                    // TODO: change this label dynamically base on func name
                    writeln!(
                        f,
                        ".L{}_ret:",
                        self.current_func_name
                            .as_ref()
                            .expect("Not generating func def")
                    )?;
                    writeln!(f, "  mov rsp, rbp")?;
                    self.pop(f, format_args!("rbp"))?;
                    writeln!(f, "  ret")?;
                    self.current_func_name = None;
                }
            }
        }

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
                self.pop(f, format_args!("rax"))?;
            }
            ConvStmtKind::Return(expr) => {
                self.gen_expr(f, expr)?;
                self.pop(f, format_args!("rax"))?;
                writeln!(
                    f,
                    "  jmp .L{}_ret",
                    self.current_func_name
                        .as_ref()
                        .expect("Not generating func def")
                )?;
            }
            ConvStmtKind::If(cond, then, Some(els)) => {
                let label_index = self.label();
                self.gen_expr(f, cond)?;
                self.pop(f, format_args!("rax"))?; // conditional expr
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
                self.pop(f, format_args!("rax"))?;
                writeln!(f, "  cmp rax, 0")?;
                writeln!(f, "  je .Lend{}", label_index)?;
                self.gen_stmt(f, *then)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmtKind::While(cond, then) => {
                let label_index = self.label();
                writeln!(f, ".Lbegin{}:", label_index)?;
                self.gen_expr(f, cond)?;
                self.pop(f, format_args!("rax"))?;
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
                self.pop(f, format_args!("rax"))?;
                writeln!(f, ".Lbegin{}:", label_index)?;
                if let Some(cond) = cond {
                    self.gen_expr(f, cond)?;
                    self.pop(f, format_args!("rax"))?;
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
        &mut self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), std::io::Error> {
        match expr.kind {
            ConvExprKind::Num(val) => self.push(f, format_args!("{}", val))?,
            ConvExprKind::Binary(c_binary) => self.gen_binary(f, c_binary)?,
            ConvExprKind::Lvar(_) => {
                self.gen_lvalue(f, expr)?;
                self.pop(f, format_args!("rax"))?;
                writeln!(f, "  mov rax, [rax]")?; // curently only 64 bit
                self.push(f, format_args!("rax"))?;
            }
            ConvExprKind::Assign(lhs, rhs) => {
                self.gen_lvalue(f, *lhs)?;
                self.gen_expr(f, *rhs)?;
                self.pop(f, format_args!("rdi"))?; // rhs
                self.pop(f, format_args!("rax"))?; // lhs's addr
                writeln!(f, "  mov [rax], rdi")?; // curently only 64 bit
                self.push(f, format_args!("rdi"))?; // evaluated value of assign expr is rhs's value
            }
            ConvExprKind::Func(name, args) => {
                let arg_reg = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                let len = args.len();
                if len > arg_reg.len() {
                    panic!("calling function args' len is greater than 6. Currently only support less than or equal to 6.");
                }
                for arg in args {
                    self.gen_expr(f, arg)?;
                } // push args
                for i in 0..len {
                    self.pop(f, format_args!("{}", arg_reg[i]))?;
                } // pop args

                // 16bit align
                if self.depth % 2 == 0 {
                    writeln!(f, "  call {}", name)?;
                } else {
                    writeln!(f, "  sub rsp, 8")?; // align
                    writeln!(f, "  call {}", name)?;
                    writeln!(f, "  add rsp, 8")?; // revert
                }
                self.push(f, format_args!("rax"))?;
            }
        }
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed
    pub fn gen_lvalue<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), std::io::Error> {
        match expr.kind {
            ConvExprKind::Lvar(Lvar { offset }) => {
                writeln!(f, "  mov rax, rbp")?;
                writeln!(f, "  sub rax, {}", offset)?;
                self.push(f, format_args!("rax"))?;
            }
            _ => self.error_at(
                expr.pos,
                &format!(
                    "This {:?} Expr cannnot be used for generating left value",
                    expr.kind
                ),
            ),
        }
        Ok(())
    }

    pub fn gen_binary<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        c_binary: ConvBinary,
    ) -> Result<(), std::io::Error> {
        let ConvBinary { kind: op, lhs, rhs } = c_binary;
        self.gen_expr(f, *lhs)?;
        self.gen_expr(f, *rhs)?;
        self.pop(f, format_args!("rdi"))?;
        self.pop(f, format_args!("rax"))?;
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
        self.push(f, format_args!("rax"))?;
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
