use std::io::{BufWriter, Write};

use crate::{
    analyze::{
        BaseType, CastKind, ConstExpr, ConstExprKind, ConstInitializer, ConvBinOpKind, ConvBinary,
        ConvExpr, ConvExprKind, ConvFuncDef, ConvProgram, ConvProgramKind, ConvStmt, ConvUnaryOp,
        GVar, LVar, Type,
    },
    error::{CompileError, UnexpectedTypeSizeStatus},
    unimplemented_err,
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

    /// this function has the same function as `push`, but in the future this function will have the function, cast(with sign extension) and push
    pub fn push_with_sign_extension<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        kind: RegKind,
        _size: RegSize,
    ) -> Result<(), CompileError> {
        // todo!();
        // match size {
        //     RegSize::Byte => writeln!(f, "  movsx {}, {}", kind.qword(), kind.byte())?,
        //     RegSize::Dword => todo!(),
        //     RegSize::Qword => todo!(),
        // }
        // writeln!(
        //     f,
        //     "  movsx {}, {}",
        //     kind.qword(),
        //     kind.size_to_reg_name(size)
        // )?;
        writeln!(f, "  push {}", kind.qword())?;
        self.depth += 1;
        Ok(())
    }

    pub fn push<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        kind: RegKind,
    ) -> Result<(), CompileError> {
        writeln!(f, "  push {}", kind.qword())?;
        self.depth += 1;
        Ok(())
    }
    pub fn pop<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        kind: RegKind,
    ) -> Result<(), CompileError> {
        writeln!(f, "  pop {}", kind.qword())?;
        self.depth -= 1;
        Ok(())
    }

    pub fn push_lit<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        arg: isize,
    ) -> Result<(), CompileError> {
        writeln!(f, "  push {}", arg)?;
        self.depth += 1;
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
            1 => ("BYTE PTR", lhs.qword(), rhs.byte()),
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
            1 => ("BYTE PTR", lhs.byte(), rhs.qword()),
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
                    stack_size,
                }) => {
                    writeln!(f, ".text")?;
                    writeln!(f, ".global {}", name)?;
                    writeln!(f, "{}:", name)?;
                    self.push(f, RegKind::Rbp)?;
                    writeln!(f, "  mov rbp, rsp")?;
                    let align_to_16 = |rsp| {
                        if rsp % 16 == 0 {
                            rsp
                        } else {
                            rsp + 16 - rsp % 16
                        }
                    };
                    writeln!(f, "  sub rsp, {}", align_to_16(stack_size))?;

                    // assign args
                    let arg_reg: Vec<RegKind> = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]
                        .into_iter()
                        .map(|reg| reg.try_into().unwrap())
                        .collect();
                    for (idx, LVar { offset, ty }) in args.into_iter().enumerate() {
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
                    self.pop(f, RegKind::Rbp)?;
                    writeln!(f, "  ret")?;
                }
                ConvProgramKind::Global(GVar { name, ty, init }) => {
                    writeln!(f, ".data")?;
                    writeln!(f, "{}:", name)?;
                    let gvar = GVar {
                        name: name.clone(),
                        ty: ty.clone(),
                        init: init.clone(),
                    };
                    let size_hint = |ty: Type| match ty.base_type().size_of() {
                        // TODO: byte or string
                        1 => Ok("byte"),
                        4 => Ok("long"),
                        8 => Ok("quad"),
                        _ => Err(CompileError::new_type_size_error(
                            self.input,
                            UnexpectedTypeSizeStatus::Global(gvar),
                        )),
                    };
                    match init {
                        Some(init) => match ty {
                            Type::Base(BaseType::Int) => writeln!(f, ".long {}", init.get_num_lit().unwrap())?,
                            Type::Base(BaseType::Char) => writeln!(f, ".byte {}", init.get_num_lit().unwrap())?,
                            // TODO : ptr init
                            Type::Ptr(_) => writeln!(f, ".quad {}", init.display_content().ok_or_else(|| unimplemented_err!(self.input, init.get_pos(), "Ptr Initializer should not be array."))?)?,
                            Type::Func { ret_ty: _, args: _, is_flexible: _ } => panic!("Unreachable. `Type::Func` has to be analyzed as FuncDeclaration not global variable."),
                            Type::Array(ty, size) => {match init {
                                ConstInitializer::Array(vec) => {
                                    let size_of = ty.size_of();
                                    let size_explanation = size_hint(*ty)?;
                                    for i in 0..size {
                                        if let Some(ConstExpr { kind: ConstExprKind::Int(val), ty: _, pos: _ }) = vec.get(i) {
                                            writeln!(f, ".{} {}", size_explanation, val)?;
                                        } else {
                                            writeln!(f, ".zero {}", size_of)?;
                                        }
                                }},
                                ConstInitializer::Expr(_) => return Err(unimplemented_err!("Num literal initializer should not be used for array global variable.")),
                            }},
                            Type::Struct(_) => return Err(unimplemented_err!(self.input, init.get_pos(), "INTERNAL COMPILER ERROR: struct currently cannot be initialized at compile time.")),
                            Type::Void => unreachable!("void type initializer cannot be written."),
                            Type::InComplete(_) => todo!(),
                        },
                        None => writeln!(f, ".zero {}", ty.size_of())?,
                    }
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
        match stmt {
            ConvStmt::Expr(expr) => {
                self.gen_expr(f, expr)?;
                self.pop(f, RegKind::Rax)?;
            }
            ConvStmt::Return(expr, name) => {
                self.gen_expr(f, expr)?;
                self.pop(f, RegKind::Rax)?;
                writeln!(f, "  jmp .L{}_ret", name)?;
            }
            ConvStmt::If(cond, then, Some(els)) => {
                let label_index = self.label();
                let ty_size_of = cond.ty.size_of();
                self.gen_expr(f, cond)?;
                self.pop(f, RegKind::Rax)?; // conditional expr
                                            // writeln!(f, "  cmp rax, 0")?; // false
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    ty_size_of,
                    RegOrLit::Lit(0),
                    4,
                )?;
                writeln!(f, "  je .Lelse{}", label_index)?;
                self.gen_stmt(f, *then)?;
                writeln!(f, "  jmp .Lend{}", label_index)?;
                writeln!(f, ".Lelse{}:", label_index)?;
                self.gen_stmt(f, *els)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmt::If(cond, then, None) => {
                let label_index = self.label();
                let ty_size_of = cond.ty.size_of();
                self.gen_expr(f, cond)?;
                self.pop(f, RegKind::Rax)?;
                // writeln!(f, "  cmp rax, 0")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    ty_size_of,
                    RegOrLit::Lit(0),
                    4,
                )?;
                writeln!(f, "  je .Lend{}", label_index)?;
                self.gen_stmt(f, *then)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmt::While(cond, then) => {
                let label_index = self.label();
                let ty_size_of = cond.ty.size_of();
                writeln!(f, ".Lbegin{}:", label_index)?;
                self.gen_expr(f, cond)?;
                self.pop(f, RegKind::Rax)?;
                // writeln!(f, "  cmp rax, 0")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    ty_size_of,
                    RegOrLit::Lit(0),
                    4,
                )?;
                writeln!(f, "  je .Lend{}", label_index)?;
                self.gen_stmt(f, *then)?;
                writeln!(f, "  jmp .Lbegin{}", label_index)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmt::For(init, cond, inc, then) => {
                let label_index = self.label();
                if let Some(init) = init {
                    self.gen_expr(f, init)?;
                    self.pop(f, RegKind::Rax)?;
                }
                writeln!(f, ".Lbegin{}:", label_index)?;
                if let Some(cond) = cond {
                    let ty_size_of = cond.ty.size_of();
                    self.gen_expr(f, cond)?;
                    self.pop(f, RegKind::Rax)?;
                    // writeln!(f, "  cmp rax, 0")?;
                    Self::gen_cmp(
                        f,
                        RegOrLit::Reg(RegKind::Rax),
                        ty_size_of,
                        RegOrLit::Lit(0),
                        4,
                    )?;
                    writeln!(f, "  je .Lend{}", label_index)?;
                }
                self.gen_stmt(f, *then)?;
                if let Some(inc) = inc {
                    self.gen_expr(f, inc)?;
                    self.pop(f, RegKind::Rax)?;
                }
                writeln!(f, "  jmp .Lbegin{}", label_index)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvStmt::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(f, stmt)?;
                }
            }
        };
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed or compilation falied
    #[allow(clippy::too_many_lines)]
    pub fn gen_expr<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), CompileError> {
        let ty = expr.ty.clone();
        match expr.kind {
            ConvExprKind::Num(val) => self.push_lit(f, val)?,
            ConvExprKind::Binary(c_binary) => self.gen_binary(f, c_binary, &expr.ty)?,
            ConvExprKind::LVar(_) => {
                let ty = expr.ty.clone();
                self.gen_lvalue(f, expr.clone())?;
                self.pop(f, RegKind::Rax)?; // rax = &expr
                self.deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(expr.clone()),
                )?; // rax = *rax
                self.push_with_sign_extension(
                    f,
                    RegKind::Rax,
                    RegSize::try_new_with_error(expr.ty.size_of(), self.input, expr)?,
                )?;
            }
            ConvExprKind::Assign(lhs, rhs) => {
                let ty = lhs.ty.clone();
                self.gen_lvalue(f, *lhs.clone())?;
                self.gen_expr(f, *rhs.clone())?;
                self.pop(f, RegKind::Rdi)?; // rhs; rdi = rhs
                self.pop(f, RegKind::Rax)?; // lhs's addr; rax = &lhs
                self.assign(
                    f,
                    RegKind::Rax,
                    RegKind::Rdi,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*lhs),
                )?; // *rax = rdi

                // evaluated value of assign expr is rhs's value
                self.push_with_sign_extension(
                    f,
                    RegKind::Rdi,
                    RegSize::try_new_with_error(expr.ty.size_of(), self.input, *rhs)?,
                )?;
            }
            ConvExprKind::Func(name, args, n_floating) => {
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
                    self.pop(f, arg_reg[i])?;
                } // pop args
                  // e.g) if arg_len == 2, pop rsi, pop rdi

                // for flexible-length-arg function
                writeln!(f, "  mov al, {}", n_floating)?;

                // 16bit align
                if self.depth % 2 == 0 {
                    writeln!(f, "  sub rsp, 8")?; // align
                    writeln!(f, "  call {}", name)?;
                    writeln!(f, "  add rsp, 8")?; // revert
                } else {
                    writeln!(f, "  call {}", name)?;
                }
                self.push(f, RegKind::Rax)?;
            }
            ConvExprKind::Deref(expr) => {
                let ty = match Clone::clone(&expr.ty) {
                    Type::Base(_)
                    | Type::Func {
                        ret_ty: _,
                        args: _,
                        is_flexible: _,
                    }
                    | Type::Array(_, _)
                    | Type::Struct(_)
                    | Type::InComplete(_)
                    | Type::Void => return Err(CompileError::new_deref_error(self.input, *expr)),
                    Type::Ptr(base) => *base,
                };
                self.gen_expr(f, *expr.clone())?;
                self.pop(f, RegKind::Rax)?; // rax = expr
                self.deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*expr.clone()),
                )?; // rax = *rax
                self.push_with_sign_extension(
                    f,
                    RegKind::Rax,
                    RegSize::try_new_with_error(ty.size_of(), self.input, *expr)?,
                )?;
            }
            ConvExprKind::Addr(expr) => {
                self.gen_lvalue(f, *expr)?;
            }
            ConvExprKind::GVar(GVar { name, ty, init }) => {
                match ty.size_of() {
                    1 => writeln!(f, "  mov al, BYTE PTR {}[rip]", name)?,
                    4 => writeln!(f, "  mov eax, DWORD PTR {}[rip]", name)?,
                    8 => writeln!(f, "  mov rax, QWORD PTR {}[rip]", name)?,
                    _ => {
                        return Err(CompileError::new_type_size_error(
                            self.input,
                            UnexpectedTypeSizeStatus::Global(GVar { name, ty, init }),
                        ))
                    }
                };
                self.push_with_sign_extension(
                    f,
                    RegKind::Rax,
                    RegSize::try_new(ty.size_of()).unwrap(),
                )?;
            }
            ConvExprKind::Cast(expr, cast_kind) => self.gen_cast(f, *expr, &cast_kind)?,
            ConvExprKind::Unary(unary_op, operand) => self.gen_unary(f, *operand, &unary_op)?,
            ConvExprKind::Member {
                struct_expr,
                minus_offset,
            } => {
                self.gen_lvalue(f, *struct_expr.clone())?;
                self.pop(f, RegKind::Rax)?;
                writeln!(f, "  add rax, {}", minus_offset)?;
                self.deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*struct_expr.clone()),
                )?; // rax = *rax
                self.push_with_sign_extension(
                    f,
                    RegKind::Rax,
                    RegSize::try_new_with_error(expr.ty.size_of(), self.input, *struct_expr)?,
                )?;
            }
            ConvExprKind::Conditional { cond, then, els } => {
                let label_index = self.label();
                let ty_size_of = cond.ty.size_of();
                self.gen_expr(f, *cond)?;
                self.pop(f, RegKind::Rax)?; // conditional expr
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    ty_size_of,
                    RegOrLit::Lit(0),
                    4,
                )?;
                writeln!(f, "  je .Lelse{}", label_index)?;
                self.gen_expr(f, *then)?;
                writeln!(f, "  jmp .Lend{}", label_index)?;
                writeln!(f, ".Lelse{}:", label_index)?;
                self.gen_expr(f, *els)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
        }
        Ok(())
    }

    pub fn gen_unary<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        operand: ConvExpr,
        unary_op: &ConvUnaryOp,
    ) -> Result<(), CompileError> {
        let operand_ty = operand.ty.clone();
        self.gen_expr(f, operand)?;
        match unary_op {
            ConvUnaryOp::BitInvert => {
                assert!(operand_ty == Type::Base(BaseType::Int));
                self.pop(f, RegKind::Rax)?;
                writeln!(f, "  not eax")?;
                self.push(f, RegKind::Rax)?;
            }
        }
        Ok(())
    }

    pub fn gen_cast<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
        kind: &CastKind,
    ) -> Result<(), CompileError> {
        match kind {
            CastKind::Base2Base(from, to) => {
                // e.g) char -> int
                if from.bytes() < to.bytes() {
                    let from: RegSize = from.into();
                    let to: RegSize = to.into();
                    self.gen_expr(f, expr)?;
                    self.pop(f, RegKind::Rax)?;
                    // e.g) `movsx eax, al`
                    writeln!(
                        f,
                        "  movsx {}, {}",
                        RegKind::Rax.size_to_reg_name(to),
                        RegKind::Rax.size_to_reg_name(from)
                    )?;
                    self.push_with_sign_extension(f, RegKind::Rax, to)?;
                } else {
                    self.gen_expr(f, expr)?;
                }
            }
            CastKind::ToVoidPtr { ptr_to: _ }
            | CastKind::FromVoidPtr { ptr_to: _ }
            | CastKind::NoCast => {
                self.gen_expr(f, expr)?;
            }
            CastKind::Ptr2Ptr {
                from: _,
                to: _,
                cast_kind: _,
            } => todo!(),
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
            ConvExprKind::LVar(LVar { offset, ty: _ }) => {
                writeln!(f, "  mov rax, rbp")?;
                writeln!(f, "  sub rax, {}", offset)?;
                // push ptr
                self.push(f, RegKind::Rax)?;
            }
            ConvExprKind::Deref(expr) => {
                self.gen_expr(f, *expr)?;
            }
            ConvExprKind::GVar(GVar {
                name,
                ty: _,
                init: _,
            }) => {
                writeln!(f, "  mov rax, offset {}", name)?;
                self.push(f, RegKind::Rax)?;
            }
            ConvExprKind::Member {
                struct_expr,
                minus_offset,
            } => {
                self.gen_lvalue(f, *struct_expr)?;
                self.pop(f, RegKind::Rax)?;
                writeln!(f, "  add rax, {}", minus_offset)?;
                self.push(f, RegKind::Rax)?;
            }
            _ => return Err(CompileError::new_lvalue_error(self.input, expr)),
        }
        Ok(())
    }

    pub fn gen_binary<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        c_binary: ConvBinary,
        ty: &Type,
    ) -> Result<(), CompileError> {
        let ConvBinary { kind: op, lhs, rhs } = c_binary;
        let lhs_ty_sizeof = lhs.ty.size_of();
        let rhs_ty_sizeof = rhs.ty.size_of();
        self.gen_expr(f, *lhs)?; // -> rax
        self.gen_expr(f, *rhs)?; // -> rdi
        self.pop(f, RegKind::Rdi)?;
        self.pop(f, RegKind::Rax)?;
        match op {
            ConvBinOpKind::Add => writeln!(f, "  add rax, rdi")?,
            ConvBinOpKind::Sub => writeln!(f, "  sub rax, rdi")?,
            ConvBinOpKind::Mul => writeln!(f, "  imul rax, rdi")?,
            ConvBinOpKind::LShift => {
                writeln!(f, "  mov rcx, rdi")?;
                writeln!(f, "  sal rax, cl")?;
            }
            ConvBinOpKind::RShift => {
                writeln!(f, "  mov rcx, rdi")?;
                writeln!(f, "  sar rax, cl")?;
            }
            ConvBinOpKind::BitWiseAnd => {
                writeln!(f, "  and rax, rdi")?;
            }
            ConvBinOpKind::Div => {
                // edx-eax = eax
                writeln!(f, "  cdq")?;
                // 0x0000ffff -> // 0xffffffff

                // rdx-rax = rdx
                // cqo
                // 0x0000ffff -> // 0x0000ffff

                // rax = rdx-rax / rdi
                // rdx = rdx-rax % rdi
                writeln!(f, "  idiv edi")?;
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
                // writeln!(f, "  cmp rax, rdi")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    lhs_ty_sizeof,
                    RegOrLit::Reg(RegKind::Rdi),
                    rhs_ty_sizeof,
                )?;
                // al : lowwer 8bit of rax
                // al = flag-reg(eq)
                writeln!(f, "  sete al")?;
                writeln!(f, "  movzx rax, al")?;
            }
            ConvBinOpKind::Le => {
                // writeln!(f, "  cmp rax, rdi")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    lhs_ty_sizeof,
                    RegOrLit::Reg(RegKind::Rdi),
                    rhs_ty_sizeof,
                )?;
                // al = flag-reg(less than or equal to)
                writeln!(f, "  setle al")?;
                writeln!(f, "  movzx rax, al")?;
            }
            ConvBinOpKind::Lt => {
                // writeln!(f, "  cmp rax, rdi")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    lhs_ty_sizeof,
                    RegOrLit::Reg(RegKind::Rdi),
                    rhs_ty_sizeof,
                )?;
                // al = flag-reg(less than)
                writeln!(f, "  setl al")?;
                writeln!(f, "  movzx rax, al")?;
            }
            ConvBinOpKind::Ne => {
                // writeln!(f, "  cmp rax, rdi")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(RegKind::Rax),
                    lhs_ty_sizeof,
                    RegOrLit::Reg(RegKind::Rdi),
                    rhs_ty_sizeof,
                )?;
                // al = flag-reg(not equal to)
                writeln!(f, "  setne al")?;
                writeln!(f, "  movzx rax, al")?;
            }
        }
        self.push_with_sign_extension(f, RegKind::Rax, RegSize::try_new(ty.size_of()).unwrap())?;
        Ok(())
    }

    pub fn gen_cmp<W: Write>(
        f: &mut BufWriter<W>,
        lhs: RegOrLit,
        lhs_ty_size: usize,
        rhs: RegOrLit,
        rhs_ty_size: usize,
    ) -> Result<(), CompileError> {
        writeln!(
            f,
            "  cmp {}, {}",
            lhs.size_to_reg_name(RegSize::try_new(lhs_ty_size).unwrap()),
            rhs.size_to_reg_name(RegSize::try_new(rhs_ty_size).unwrap()),
        )?;
        Ok(())
    }

    pub fn label(&mut self) -> usize {
        self.label += 1;
        self.label
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum RegOrLit {
    Reg(RegKind),
    Lit(isize),
}

impl RegOrLit {
    pub fn size_to_reg_name(&self, size: RegSize) -> String {
        match size {
            RegSize::Byte => self.byte(),
            RegSize::Dword => self.dword(),
            RegSize::Qword => self.qword(),
        }
    }

    pub fn byte(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.byte().to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
    }

    pub fn dword(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.dword().to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
    }

    pub fn qword(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.qword().to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
    }
}

impl ToString for RegOrLit {
    fn to_string(&self) -> String {
        match self {
            RegOrLit::Reg(reg) => reg.to_string(),
            RegOrLit::Lit(num) => num.to_string(),
        }
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

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum RegSize {
    Byte,
    Dword,
    Qword,
}

impl RegSize {
    pub const fn try_new(size: usize) -> Option<Self> {
        Some(match size {
            1 => Self::Byte,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => return None,
        })
    }
    pub fn try_new_with_error(
        size: usize,
        src: &str,
        expr: ConvExpr,
    ) -> Result<Self, CompileError> {
        Ok(match size {
            1 => Self::Byte,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => {
                return Err(CompileError::new_type_size_error(
                    src,
                    UnexpectedTypeSizeStatus::Expr(expr),
                ));
            }
        })
    }
}

impl AsRef<str> for RegSize {
    fn as_ref(&self) -> &str {
        match self {
            RegSize::Byte => "BYTE",
            RegSize::Dword => "DWORD",
            RegSize::Qword => "QWORD",
        }
    }
}

impl RegSize {
    pub const fn size_to_name(size: usize) -> Option<RegSize> {
        match size {
            1 => Some(Self::Byte),
            4 => Some(Self::Dword),
            8 => Some(Self::Qword),
            _ => None,
        }
    }
}

impl RegKind {
    pub const fn size_to_reg_name(&self, size: RegSize) -> &str {
        match size {
            RegSize::Byte => self.byte(),
            RegSize::Dword => self.dword(),
            RegSize::Qword => self.qword(),
        }
    }

    pub const fn byte(&self) -> &str {
        match self {
            RegKind::Rax => "al",
            RegKind::Rdi => "dil",
            RegKind::Rsi => "sil",
            RegKind::Rdx => "dl",
            RegKind::Rcx => "cl",
            RegKind::Rbp => "bpl",
            RegKind::Rsp => "spl",
            RegKind::R8 => "r8b",
            RegKind::R9 => "r9b",
            RegKind::R10 => "r10b",
            RegKind::R11 => "r11b",
            RegKind::R12 => "r12b",
            RegKind::R13 => "r13b",
            RegKind::R14 => "r14b",
            RegKind::R15 => "r15b",
        }
    }

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
