use std::io::{BufWriter, Write};

use crate::{
    analyze::{
        ConvBinOpKind, ConvBinary, ConvExpr, ConvExprKind, ConvFuncDef, ConvProgram,
        ConvProgramKind, ConvStmt, ConvStmtKind, Lvar, Type,
    },
    error::{CompileError, UnexpectedTypeSizeStatus},
};

#[derive(Debug, Clone)]
pub struct Generator<'a> {
    input: &'a str,
    label: usize,
    depth: usize,
}

impl<'a> Generator<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self {
            input,
            label: 0,
            depth: 0,
        }
    }

    pub fn push<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        arg: std::fmt::Arguments,
    ) -> Result<(), CompileError> {
        writeln!(f, "  push {}", arg)?;
        self.depth += 1;
        Ok(())
    }

    pub fn pop<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        arg: std::fmt::Arguments,
    ) -> Result<(), CompileError> {
        writeln!(f, "  pop {}", arg)?;
        self.depth -= 1;
        Ok(())
    }

    /// *lhs = rhs
    pub fn assign<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        lhs: RegKind,
        rhs: RegKind,
        ty: &Type,
        status: UnexpectedTypeSizeStatus,
    ) -> Result<(), CompileError> {
        let (prefix, lhs, rhs) = match ty.size_of() {
            4 => ("DWORD PTR", lhs.qword(), rhs.dword()),
            8 => ("QWORD PTR", lhs.qword(), rhs.qword()),
            _ => return Err(CompileError::new_type_size_error(self.input, status)),
        };
        writeln!(f, "  mov {} [{}], {}", prefix, lhs, rhs)?;
        Ok(())
    }

    /// lhs = *rhs
    pub fn deref<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        lhs: RegKind,
        rhs: RegKind,
        ty: &Type,
        status: UnexpectedTypeSizeStatus,
    ) -> Result<(), CompileError> {
        let (prefix, lhs, rhs) = match ty.size_of() {
            4 => ("DWORD PTR", lhs.dword(), rhs.qword()),
            8 => ("QWORD PTR", lhs.qword(), rhs.qword()),
            _ => return Err(CompileError::new_type_size_error(self.input, status)),
        };
        writeln!(f, "  mov {}, {} [{}]", lhs, prefix, rhs)?;
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed
    pub fn gen_head<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        program: ConvProgram,
    ) -> Result<(), CompileError> {
        writeln!(f, ".intel_syntax noprefix\n")?;
        writeln!(f, ".global main")?;

        for component in program {
            match component {
                ConvProgramKind::Func(ConvFuncDef {
                    ty: _, // TODO: determine how to use this
                    name,
                    args,
                    body,
                    lvars,
                }) => {
                    writeln!(f, "{}:", name)?;
                    self.push(f, format_args!("rbp"))?;
                    writeln!(f, "  mov rbp, rsp")?;
                    writeln!(f, "  sub rsp, {}", lvars.len() * 8)?; // 64bit var * 8

                    // assign args
                    let arg_reg: Vec<RegKind> = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
                        .into_iter()
                        .map(|reg| reg.try_into().unwrap())
                        .collect();
                    for (idx, Lvar { offset, ty }) in args.into_iter().enumerate() {
                        writeln!(f, "  mov rax, rbp")?;
                        writeln!(f, "  sub rax, {}", offset)?; // rax = &arg
                        self.assign(
                            f,
                            RegKind::Rax,
                            arg_reg[idx],
                            &ty,
                            UnexpectedTypeSizeStatus::FuncArgs(name.clone(), ty.clone()),
                        )?; // *rax = value
                    }
                    // gen body stmt
                    self.gen_stmt(f, body)?;
                    writeln!(f, ".L{}_ret:", name)?;
                    writeln!(f, "  mov rsp, rbp")?;
                    self.pop(f, format_args!("rbp"))?;
                    writeln!(f, "  ret")?;
                }
            }
        }

        Ok(())
    }

    pub fn gen_stmt<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        stmt: ConvStmt,
    ) -> Result<(), CompileError> {
        match stmt.kind {
            ConvStmtKind::Expr(expr) => {
                self.gen_expr(f, expr)?;
                self.pop(f, format_args!("rax"))?;
            }
            ConvStmtKind::Return(expr, name) => {
                self.gen_expr(f, expr)?;
                self.pop(f, format_args!("rax"))?;
                writeln!(f, "  jmp .L{}_ret", name)?;
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
    /// return errors when file IO failed or compilation falied
    pub fn gen_expr<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), CompileError> {
        match expr.kind {
            ConvExprKind::Num(val) => self.push(f, format_args!("{}", val))?,
            ConvExprKind::Binary(c_binary) => self.gen_binary(f, c_binary)?,
            ConvExprKind::Lvar(_) => {
                let ty = expr.ty.clone();
                self.gen_lvalue(f, expr.clone())?;
                self.pop(f, format_args!("rax"))?; // rax = &expr
                self.deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(expr),
                )?; // rax = *rax
                self.push(f, format_args!("rax"))?;
            }
            ConvExprKind::Assign(lhs, rhs) => {
                let ty = lhs.ty.clone();
                self.gen_lvalue(f, *lhs.clone())?;
                self.gen_expr(f, *rhs)?;
                self.pop(f, format_args!("rdi"))?; // rhs; rdi = rhs
                self.pop(f, format_args!("rax"))?; // lhs's addr; rax = &lhs
                self.assign(
                    f,
                    RegKind::Rax,
                    RegKind::Rdi,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*lhs),
                )?; // *rax = rdi
                self.push(f, format_args!("rdi"))?; // evaluated value of assign expr is rhs's value
            }
            ConvExprKind::Func(name, args) => {
                let arg_reg: Vec<RegKind> = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
                    .into_iter()
                    .map(|reg| reg.try_into().unwrap())
                    .collect();
                let arg_len = args.len();
                if arg_len > arg_reg.len() {
                    todo!("calling function args' len is greater than 6. Currently only support less than or equal to 6.");
                }
                for arg in args {
                    self.gen_expr(f, arg)?;
                } // push args
                for i in (0..arg_len).rev() {
                    self.pop(f, format_args!("{}", arg_reg[i].qword()))?;
                } // pop args
                  // e.g) if arg_len == 2, pop rsi, pop rdi

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
            ConvExprKind::Deref(expr) => {
                let ty = match Clone::clone(&expr.ty) {
                    Type::Base(_) | Type::Func(_, _) => {
                        return Err(CompileError::new_deref_error(self.input, *expr))
                    }
                    Type::Ptr(base) => *base,
                };
                self.gen_expr(f, *expr.clone())?;
                self.pop(f, format_args!("rax"))?; // rax = expr
                self.deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*expr),
                )?; // rax = *rax
                self.push(f, format_args!("rax"))?;
            }
            ConvExprKind::Addr(expr) => {
                self.gen_lvalue(f, *expr)?;
            }
        }
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed or compilation failed
    pub fn gen_lvalue<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), CompileError> {
        match expr.kind {
            ConvExprKind::Lvar(Lvar { offset, ty: _ }) => {
                writeln!(f, "  mov rax, rbp")?;
                writeln!(f, "  sub rax, {}", offset)?;
                self.push(f, format_args!("rax"))?;
            }
            ConvExprKind::Deref(expr) => {
                self.gen_expr(f, *expr)?;
            }
            _ => return Err(CompileError::new_lvalue_error(self.input, expr)),
        }
        Ok(())
    }

    pub fn gen_binary<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        c_binary: ConvBinary,
    ) -> Result<(), CompileError> {
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

    pub fn label(&mut self) -> usize {
        self.label += 1;
        self.label
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum RegKind {
    Rax,
    Rdi,
    Rsi,
    Rdx,
    Rcx,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl ToString for RegKind {
    fn to_string(&self) -> String {
        match self {
            RegKind::Rax => "rax",
            RegKind::Rdi => "rdi",
            RegKind::Rsi => "rsi",
            RegKind::Rdx => "rdx",
            RegKind::Rcx => "rcx",
            RegKind::Rbp => "rbp",
            RegKind::Rsp => "rsp",
            RegKind::R8 => "r8",
            RegKind::R9 => "r9",
            RegKind::R10 => "r10",
            RegKind::R11 => "r11",
            RegKind::R12 => "r12",
            RegKind::R13 => "r13",
            RegKind::R14 => "r14",
            RegKind::R15 => "r15",
        }
        .to_string()
    }
}

impl TryFrom<&str> for RegKind {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "rax" => RegKind::Rax,
            "rdi" => RegKind::Rdi,
            "rsi" => RegKind::Rsi,
            "rdx" => RegKind::Rdx,
            "rcx" => RegKind::Rcx,
            "r8" => RegKind::R8,
            "r9" => RegKind::R9,
            _ => return Err(()),
        })
    }
}

impl RegKind {
    pub const fn dword(&self) -> &str {
        match self {
            RegKind::Rax => "eax",
            RegKind::Rdi => "edi",
            RegKind::Rsi => "esi",
            RegKind::Rdx => "edx",
            RegKind::Rcx => "ecx",
            RegKind::Rbp => "ebp",
            RegKind::Rsp => "esp",
            RegKind::R8 => "r8d",
            RegKind::R9 => "r9d",
            RegKind::R10 => "r10d",
            RegKind::R11 => "r11d",
            RegKind::R12 => "r12d",
            RegKind::R13 => "r13d",
            RegKind::R14 => "r14d",
            RegKind::R15 => "r15d",
        }
    }

    pub const fn qword(&self) -> &str {
        match self {
            RegKind::Rax => "rax",
            RegKind::Rdi => "rdi",
            RegKind::Rsi => "rsi",
            RegKind::Rdx => "rdx",
            RegKind::Rcx => "rcx",
            RegKind::Rbp => "rbp",
            RegKind::Rsp => "rsp",
            RegKind::R8 => "r8",
            RegKind::R9 => "r9",
            RegKind::R10 => "r10",
            RegKind::R11 => "r11",
            RegKind::R12 => "r12",
            RegKind::R13 => "r13",
            RegKind::R14 => "r14",
            RegKind::R15 => "r15",
        }
    }
}
