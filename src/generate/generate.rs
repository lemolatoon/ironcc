use std::io::{BufWriter, Write};

use crate::{
    analyze::{
        analyze::{
            ConstExpr, ConstExprKind, ConstInitializer, ConvFuncDef, ConvProgram, ConvProgramKind,
            GVar, LVar,
        },
        expr::{
            CastKind, ConvBinOpKind, ConvBinary, ConvExpr, ConvExprKind, ConvUnaryOp,
            FuncCallTargetKind,
        },
        stmt::{ConvStmt, LoopControlKind},
        types::{BaseType, Type},
    },
    error::{CompileError, UnexpectedTypeSizeStatus},
    parse::expr::BinOpKind,
    unimplemented_err,
};

#[derive(Debug, Clone)]
pub struct Generator {
    label: usize,
    loop_label_stack: LoopLabelStack,
    depth: usize,
}

#[derive(Debug, Clone)]
pub struct LoopLabelStack(Vec<LoopLabel>);

impl LoopLabelStack {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, label: LoopLabel) {
        self.0.push(label);
    }

    pub fn pop(&mut self) -> Option<LoopLabel> {
        self.0.pop()
    }

    /// Returns the label of the innermost loop. `LoopLabel::For(_)` or `LoopLabel::While(_)`
    pub fn get_closest_continue_label_loop_and_pop(&mut self) -> Result<LoopLabel, CompileError> {
        let prev_loop_label;
        loop {
            if let Some(loop_label @ (LoopLabel::For(_) | LoopLabel::While(_))) = self.0.pop() {
                prev_loop_label = loop_label;
                break;
            } else if self.0.pop().is_none() {
                return Err(unimplemented_err!(
                    "INTERNAL COMPILER ERROR: continue stmt is not checked until analysis phase."
                ));
            }
        }
        self.0.push(prev_loop_label);
        Ok(prev_loop_label)
    }

    /// Returns the label of the innermost loop or switch. `LoopLabel::Switch(_)`, `LoopLabel::For(_)` or `LoopLabel::While(_)`
    pub fn get_closest_break_label_loop_or_switch_and_pop(
        &self,
    ) -> Result<LoopLabel, CompileError> {
        self.0.last().map_or_else(
            || {
                Err(unimplemented_err!(
                    "INTERNAL COMPILER ERROR: break stmt is not checked until analysis phase."
                ))
            },
            |loop_label| Ok(*loop_label),
        )
    }

    /// Returns the label of the innermost switch. `LoopLabel::Switch(_)`
    pub fn get_closest_switch_and_pop(&mut self) -> Result<usize, CompileError> {
        let prev_loop_label;
        loop {
            if let Some(loop_label @ LoopLabel::Switch(_)) = self.0.pop() {
                prev_loop_label = loop_label;
                break;
            } else if self.0.pop().is_none() {
                return Err(unimplemented_err!(
                    "INTERNAL COMPILER ERROR: case stmt is not checked until analysis phase."
                ));
            }
        }
        self.0.push(prev_loop_label);
        Ok(prev_loop_label.get_label())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum LoopLabel {
    While(usize),
    For(usize),
    Switch(usize),
}

impl LoopLabel {
    pub const fn get_label(&self) -> usize {
        match self {
            LoopLabel::For(label) | LoopLabel::Switch(label) | LoopLabel::While(label) => *label,
        }
    }
}

impl Generator {
    pub const fn new() -> Self {
        Self {
            label: 0,
            loop_label_stack: LoopLabelStack::new(),
            depth: 0,
        }
    }

    // pub fn gen_labeled_stmt(
    //     &mut self,
    //     f: &mut BufWriter<impl Write>,
    //     // TODO: make ConvLabelKind
    //     kind: LabelKind,
    //     stmt: ConvStmt,
    // ) -> Result<(), CompileError> {
    //     let label = self.loop_label_stack.last().unwrap();
    //     match label {
    //         LoopLabel::While(index) => todo!(),
    //         LoopLabel::For(index) => todo!(),
    //         LoopLabel::Switch(index) => match kind {
    //             LabelKind::Ident(label) => writeln!(f, "{}:", label)?,
    //             LabelKind::Case(case_index) => {
    //                 writeln!(f, ".Lcase{}_{}:", index, case_index)?;
    //                 case_indexes.push(label_index);
    //                 self.gen_stmt(&mut buf_writer_labels, stmt)?;
    //             }
    //             LabelKind::Default => todo!(),
    //         },
    //     }
    //     Ok(())
    // }

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
            _ => return Err(CompileError::new_type_size_error(status)),
        };
        writeln!(f, "  mov {} [{}], {}", prefix, lhs, rhs)?;
        Ok(())
    }

    /// lhs = *rhs
    pub fn deref<W: Write>(
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
            _ => return Err(CompileError::new_type_size_error(status)),
        };
        writeln!(f, "  mov {}, {} [{}]", lhs, prefix, rhs)?;
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed
    #[allow(clippy::too_many_lines)]
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
                    self.depth = 1;
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
                        Self::assign(
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
                ConvProgramKind::Global(GVar {
                    name,
                    ty,
                    init,
                    is_extern,
                }) => {
                    if is_extern {
                        continue;
                    }
                    writeln!(f, ".data")?;
                    if !name.starts_with('.') {
                        writeln!(f, ".global {}", name)?;
                    }
                    writeln!(f, "{}:", name)?;
                    match init {
                        Some(init) => match ty {
                            Type::Base(BaseType::Int) => writeln!(f, ".long {}", init.get_num_lit().unwrap())?,
                            Type::Base(BaseType::Char) => writeln!(f, ".byte {}", init.get_num_lit().unwrap())?,
                            Type::Ptr(_) => writeln!(f, ".quad {}", init.display_content().ok_or_else(|| unimplemented_err!(init.get_debug_info(), "Ptr Initializer should not be array."))?)?,
                            Type::Func { ret_ty: _, args: _, is_flexible: _ } => panic!("INTERNAL COMPILER ERROR. Unreachable. `Type::Func` has to be analyzed as FuncDeclaration not global variable."),
                            ty @ Type::Array(_, _) => {
                                self.gen_const_init_array(f,init, ty)?;
                            },
                            Type::Struct(_) => return Err(unimplemented_err!(init.get_debug_info(), "INTERNAL COMPILER ERROR: struct currently cannot be initialized at compile time.")),
                            Type::Void => unreachable!("void type initializer cannot be written."),
                            Type::InComplete(_) => todo!(),
                        },
                        None => writeln!(f, ".zero {}", ty.size_of())?,
                    }
                }
                ConvProgramKind::InlineAsm(asm) => {
                    writeln!(f, "{}", asm)?;
                }
            }
        }

        Ok(())
    }

    pub fn gen_const_init_array<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        initializer: ConstInitializer,
        ty: Type,
    ) -> Result<(), CompileError> {
        match initializer {
            ConstInitializer::Array(vec) => {
                if let Type::Array(ty, size) = ty {
                    for idx in 0..size {
                        if let Some(init) = vec.get(idx) {
                            self.gen_const_init_array(f, init.clone(), *ty.clone())?;
                        } else {
                            writeln!(f, ".zero {}", ty.size_of() * (size - idx))?;
                        }
                    }
                } else {
                    unreachable!(
                        "INTERNAL COMPILER ERROR: initializer is array but type is not array."
                    );
                }
            }
            ConstInitializer::Expr(ConstExpr {
                kind,
                ty: _,
                debug_info: _,
            }) => {
                match kind {
                    ConstExprKind::Int(val) => writeln!(f, ".long {}", val)?,
                    ConstExprKind::Char(val) => writeln!(f, ".byte {}", val)?,
                    ConstExprKind::Addr(gvar) => writeln!(f, ".quad {}", gvar.name)?,
                };
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
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
                if let Some(expr) = expr {
                    self.gen_expr(f, expr)?;
                    self.pop(f, RegKind::Rax)?;
                }
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
                self.loop_label_stack.push(LoopLabel::While(label_index));
                let ty_size_of = cond.ty.size_of();
                writeln!(f, ".Lbegin{}:", label_index)?;
                self.gen_expr(f, cond)?;
                self.pop(f, RegKind::Rax)?;
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
                self.loop_label_stack.pop();
            }
            ConvStmt::For(init, cond, inc, then) => {
                let label_index = self.label();
                self.loop_label_stack.push(LoopLabel::For(label_index));
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
                self.loop_label_stack.pop();
            }
            ConvStmt::Block(stmts) => {
                for stmt in stmts {
                    self.gen_stmt(f, stmt)?;
                }
            }
            ConvStmt::Switch {
                expr,
                cases,
                stmt,
                has_default,
            } => {
                let index = self.label();
                self.loop_label_stack.push(LoopLabel::Switch(index));
                self.gen_expr(f, expr)?;
                self.pop(f, RegKind::Rax)?;
                for label_index in cases {
                    writeln!(f, "  cmp rax, {}", label_index)?;
                    writeln!(f, "  je .Lcase{}_{}", index, label_index)?;
                }
                if has_default {
                    writeln!(f, "  jmp .Ldefault{}", index)?;
                }
                //for stmt in labels {
                //    match stmt {
                //        crate::analyze::SwitchBodyStmt::Stmt(stmt) => {
                //            self.gen_stmt(&mut buf_writer_labels, stmt)?;
                //        }
                //        crate::analyze::SwitchBodyStmt::Case(label_index, stmt) => {
                //            writeln!(&mut buf_writer_labels, ".Lcase{}_{}:", index, label_index)?;
                //            case_indexes.push(label_index);
                //            self.gen_stmt(&mut buf_writer_labels, stmt)?;
                //        }
                //        crate::analyze::SwitchBodyStmt::Default(stmt) => {
                //            writeln!(&mut buf_writer_labels, ".Ldefault{}:", index)?;
                //            self.gen_stmt(&mut buf_writer_labels, stmt)?;
                //        }
                //        crate::analyze::SwitchBodyStmt::Break => {
                //            writeln!(&mut buf_writer_labels, "  jmp .Lend{}", index)?;
                //        }
                //    }
                //}
                self.gen_stmt(f, *stmt)?;
                writeln!(f, ".Lend{}:", index)?;
                self.loop_label_stack.pop();
            }
            ConvStmt::LoopControl(LoopControlKind::Break) => {
                match self
                    .loop_label_stack
                    .get_closest_break_label_loop_or_switch_and_pop()?
                {
                    LoopLabel::While(index) => {
                        writeln!(f, "  jmp .Lend{}", index)?;
                    }
                    LoopLabel::For(index) => {
                        writeln!(f, "  jmp .Lend{}", index)?;
                    }
                    LoopLabel::Switch(index) => {
                        writeln!(f, "  jmp .Lend{}", index)?;
                    }
                }
            }
            ConvStmt::LoopControl(LoopControlKind::Continue) => {
                match self
                    .loop_label_stack
                    .get_closest_continue_label_loop_and_pop()?
                {
                    LoopLabel::While(index) => writeln!(f, "  jmp .Lbegin{}", index)?,
                    LoopLabel::For(index) => writeln!(f, "  jmp .Lbegin{}", index)?,
                    _ => unreachable!(),
                }
            }
            ConvStmt::LoopControl(LoopControlKind::Case(label_index, stmt)) => {
                let index = self.loop_label_stack.get_closest_switch_and_pop()?;
                writeln!(f, ".Lcase{}_{}:", index, label_index)?;
                self.gen_stmt(f, *stmt)?;
            }
            ConvStmt::LoopControl(LoopControlKind::Default(stmt)) => {
                let index = self.loop_label_stack.get_closest_switch_and_pop()?;
                writeln!(f, ".Ldefault{}:", index)?;
                self.gen_stmt(f, *stmt)?;
            }
            ConvStmt::VaStartInit { arg_n } => {
                let arg_regs = &["rdi", "rsi", "rdx", "rcx", "r8", "r9"][arg_n..6];
                let float_regs = [
                    "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
                ];
                let mut offset: usize = 176 - arg_n * 8;
                for arg_reg in arg_regs {
                    writeln!(f, "  mov QWORD PTR [rbp-{}], {}", offset, arg_reg)?;
                    offset -= 8;
                }
                for float_reg in float_regs {
                    writeln!(f, "  movaps XMMWORD PTR [rbp-{}], {}", offset, float_reg)?;
                    offset -= 16;
                }
                assert_eq!(offset, 0);
            }
        };
        Ok(())
    }

    /// # Errors
    /// return errors when file IO failed or compilation failed
    /// Caller saved registers: rax, rdi, rsi, rdx, rcx, r8, r9 .... (WIP)
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
            ConvExprKind::LVar(LVar { offset, ty: _ }) => {
                let ty = expr.ty.clone();
                writeln!(f, "  lea rax, [rbp-{}]", offset)?;
                Self::deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(expr.clone()),
                )?; // rax = *rax
                self.push_with_sign_extension(
                    f,
                    RegKind::Rax,
                    RegSize::try_new_with_error(expr.ty.size_of(), expr)?,
                )?;
            }
            ConvExprKind::Assign(lhs, rhs) => {
                let ty = lhs.ty.clone();
                self.gen_lvalue(f, *lhs.clone())?;
                self.gen_expr(f, *rhs.clone())?;
                self.pop(f, RegKind::Rdi)?; // rhs; rdi = rhs
                self.pop(f, RegKind::Rax)?; // lhs's addr; rax = &lhs
                Self::assign(
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
                    RegSize::try_new_with_error(expr.ty.size_of(), *rhs)?,
                )?;
            }
            ConvExprKind::Func(name, args, is_flexible_length, n_floating) => {
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

                // 16bit align
                // NOTE: local variable stack area is aligned to 16byte.
                // `calling after 1 push (including `push rbp`)` is OK.
                match name {
                    FuncCallTargetKind::Label(name) => {
                        if is_flexible_length {
                            // for flexible-length-arg function
                            writeln!(f, "  mov al, {}", n_floating)?;
                        }
                        if self.depth % 2 == 0 {
                            writeln!(f, "  call {}", name)?;
                        } else {
                            writeln!(f, "  sub rsp, 8")?; // align
                            writeln!(f, "  call {}", name)?;
                            writeln!(f, "  add rsp, 8")?; // revert
                        }
                    }
                    FuncCallTargetKind::Expr(expr) => {
                        self.gen_expr(f, *expr)?;
                        self.pop(f, RegKind::R10)?;
                        if is_flexible_length {
                            // for flexible-length-arg function
                            writeln!(f, "  mov al, {}", n_floating)?;
                        }
                        if self.depth % 2 == 0 {
                            writeln!(f, "  call {}", RegKind::R10.qword())?;
                        } else {
                            writeln!(f, "  sub rsp, 8")?; // align
                            writeln!(f, "  call {}", RegKind::R10.qword())?;
                            writeln!(f, "  add rsp, 8")?; // revert
                        }
                    }
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
                    | Type::Void => return Err(CompileError::new_deref_error(*expr)),
                    Type::Ptr(base) => *base,
                };
                self.gen_expr(f, *expr.clone())?;
                self.pop(f, RegKind::Rax)?; // rax = expr
                Self::deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*expr.clone()),
                )?; // rax = *rax
                self.push_with_sign_extension(
                    f,
                    RegKind::Rax,
                    RegSize::try_new_with_error(ty.size_of(), *expr)?,
                )?;
            }
            ConvExprKind::Addr(expr) => {
                self.gen_lvalue(f, *expr)?;
            }
            ConvExprKind::GVar(GVar {
                name,
                ty,
                init,
                is_extern: _,
            }) => {
                match ty.size_of() {
                    1 => writeln!(f, "  mov al, BYTE PTR {}[rip]", name)?,
                    4 => writeln!(f, "  mov eax, DWORD PTR {}[rip]", name)?,
                    8 => writeln!(f, "  mov rax, QWORD PTR {}[rip]", name)?,
                    _ => {
                        return Err(CompileError::new_type_size_error(
                            UnexpectedTypeSizeStatus::Global(GVar {
                                name,
                                ty,
                                init,
                                is_extern: false,
                            }),
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
                Self::deref(
                    f,
                    RegKind::Rax,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*struct_expr.clone()),
                )?; // rax = *rax
                self.push_with_sign_extension(
                    f,
                    RegKind::Rax,
                    RegSize::try_new_with_error(expr.ty.size_of(), *struct_expr)?,
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
                self.depth -= 1;
                writeln!(f, "  jmp .Lend{}", label_index)?;
                writeln!(f, ".Lelse{}:", label_index)?;
                self.gen_expr(f, *els)?;
                writeln!(f, ".Lend{}:", label_index)?;
            }
            ConvExprKind::PostfixIncrement(expr, value) => {
                let ty = expr.ty.clone();
                self.gen_lvalue(f, *expr.clone())?;
                self.pop(f, RegKind::Rax)?; // rax = &expr
                Self::deref(
                    f,
                    RegKind::Rdi,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*expr.clone()),
                )?; // rdi = *rax
                self.push(f, RegKind::Rdi)?; // push expr's value
                writeln!(f, "  add rdi, {}", value)?; // rdi = rdi + value
                Self::assign(
                    f,
                    RegKind::Rax,
                    RegKind::Rdi,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*expr),
                )?; // *rax = rdi

                // return expr's value
            }
            ConvExprKind::PostfixDecrement(expr, value) => {
                let ty = expr.ty.clone();
                self.gen_lvalue(f, *expr.clone())?;
                self.pop(f, RegKind::Rax)?; // rax = &expr
                Self::deref(
                    f,
                    RegKind::Rdi,
                    RegKind::Rax,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*expr.clone()),
                )?; // rdi = *rax
                self.push(f, RegKind::Rdi)?; // push expr's value
                writeln!(f, "  sub rdi, {}", value)?; // rdi = rdi + value
                Self::assign(
                    f,
                    RegKind::Rax,
                    RegKind::Rdi,
                    &ty,
                    UnexpectedTypeSizeStatus::Expr(*expr),
                )?; // *rax = rdi

                // return expr's value
            }
            ConvExprKind::Asm(asm) => {
                // This asm must have push the value as an evaluated value.
                writeln!(f, "  {}", asm)?;
            }
            ConvExprKind::OpAssign(lhs, rhs, kind) => {
                writeln!(f, "  # OpAssign")?;
                let bin_op: ConvBinOpKind =
                    ConvBinOpKind::new(&BinOpKind::try_from(kind).unwrap()).unwrap();
                let lhs_ty = lhs.ty.clone();
                self.gen_lvalue(f, *lhs.clone())?;
                self.pop(f, RegKind::R11)?; // r11 = &lhs
                Self::deref(
                    f,
                    RegKind::Rdi,
                    RegKind::R11,
                    &lhs_ty,
                    UnexpectedTypeSizeStatus::Expr(*lhs.clone()),
                )?; // rdi = *r11
                self.push(f, RegKind::Rdi)?;
                let rhs_ty_sizeof = rhs.ty.size_of();
                self.gen_expr(f, *rhs)?;
                self.pop(f, RegKind::R10)?; // r10 = rhs
                self.pop(f, RegKind::Rdi)?; // rdi = *r11 = lhs
                self.gen_binary_with_reg(
                    f,
                    bin_op,
                    RegKind::Rdi,
                    RegKind::R10,
                    lhs_ty.size_of(),
                    rhs_ty_sizeof,
                    &lhs_ty,
                )?;
                self.pop(f, RegKind::Rdi)?; // rdi = lhs op rhs
                Self::assign(
                    f,
                    RegKind::R11,
                    RegKind::Rdi,
                    &lhs_ty,
                    UnexpectedTypeSizeStatus::Expr(*lhs),
                )?; // *r11 = rdi
                self.push(f, RegKind::Rdi)?; // push calculated expr's value
                writeln!(f, "  # OpAssign end")?;
            }
            ConvExprKind::Block(stmt, expr) => {
                self.gen_stmt(f, *stmt)?;
                if let Some(expr) = expr {
                    self.gen_expr(f, *expr)?;
                } else {
                    self.push_lit(f, 0)?; // push tmp value
                }
            }
            ConvExprKind::FuncPtr(_, label) => {
                writeln!(f, "  lea rax, [rip+{}]", label)?;
                self.push(f, RegKind::Rax)?;
            }
            ConvExprKind::Comma(lhs, rhs) => {
                self.gen_expr(f, *lhs)?;
                self.pop(f, RegKind::Rax)?;
                self.gen_expr(f, *rhs)?;
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
        match unary_op {
            ConvUnaryOp::BitInvert => {
                self.gen_expr(f, operand)?;
                assert!(operand_ty == Type::Base(BaseType::Int));
                self.pop(f, RegKind::Rax)?;
                writeln!(f, "  not eax")?;
                self.push(f, RegKind::Rax)?;
            }
            ConvUnaryOp::Increment(value) => {
                self.gen_lvalue(f, operand.clone())?;
                self.pop(f, RegKind::Rax)?; // rax = &expr
                Self::deref(
                    f,
                    RegKind::Rdi,
                    RegKind::Rax,
                    &operand_ty,
                    UnexpectedTypeSizeStatus::Expr(operand.clone()),
                )?; // rdi = *rax
                writeln!(f, "  add rdi, {}", value)?; // rdi = rdi + value
                Self::assign(
                    f,
                    RegKind::Rax,
                    RegKind::Rdi,
                    &operand_ty,
                    UnexpectedTypeSizeStatus::Expr(operand),
                )?; // *rax = rdi
                self.push(f, RegKind::Rdi)?; // push incremented expr's value
            }
            ConvUnaryOp::Decrement(value) => {
                self.gen_lvalue(f, operand.clone())?;
                self.pop(f, RegKind::Rax)?; // rax = &expr
                Self::deref(
                    f,
                    RegKind::Rdi,
                    RegKind::Rax,
                    &operand_ty,
                    UnexpectedTypeSizeStatus::Expr(operand.clone()),
                )?; // rdi = *rax
                writeln!(f, "  sub rdi, {}", value)?; // rdi = rdi + value
                Self::assign(
                    f,
                    RegKind::Rax,
                    RegKind::Rdi,
                    &operand_ty,
                    UnexpectedTypeSizeStatus::Expr(operand),
                )?; // *rax = rdi
                self.push(f, RegKind::Rdi)?; // push incremented expr's value
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
            | CastKind::NoCast
            | CastKind::Base2FuncPtr(_, _) // TODO: check need movsx or not
            => {
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
    /// Caller saved registers: rax
    pub fn gen_lvalue<W: Write>(
        &mut self,
        f: &mut BufWriter<W>,
        expr: ConvExpr,
    ) -> Result<(), CompileError> {
        match expr.kind {
            ConvExprKind::LVar(LVar { offset, ty: _ }) => {
                writeln!(f, "  lea rax, [rbp-{}]", offset)?;
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
                is_extern: _,
            }) => {
                writeln!(f, "  lea rax, [rip+{}]", name)?;
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
            _ => return Err(CompileError::new_lvalue_error(expr)),
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
        self.gen_binary_with_reg(
            f,
            op,
            RegKind::Rax,
            RegKind::Rdi,
            lhs_ty_sizeof,
            rhs_ty_sizeof,
            ty,
        )?;
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub fn gen_binary_with_reg(
        &mut self,
        f: &mut BufWriter<impl Write>,
        bin_op: ConvBinOpKind,
        lhs: RegKind,
        rhs: RegKind,
        lhs_ty_size: usize,
        rhs_ty_size: usize,
        ty: &Type,
    ) -> Result<(), CompileError> {
        // rax :lhs , rdi : rhs
        match bin_op {
            ConvBinOpKind::Add => {
                writeln!(f, "  add {}, {}", lhs.qword(), rhs.qword())?;
                return self.push_with_sign_extension(
                    f,
                    lhs,
                    RegSize::try_new(ty.size_of()).unwrap(),
                );
            }
            ConvBinOpKind::Sub => {
                writeln!(f, "  sub {}, {}", lhs.qword(), rhs.qword())?;

                return self.push_with_sign_extension(
                    f,
                    lhs,
                    RegSize::try_new(ty.size_of()).unwrap(),
                );
            }
            ConvBinOpKind::Mul => {
                writeln!(f, "  imul {}, {}", lhs.qword(), rhs.qword())?;
                return self.push_with_sign_extension(
                    f,
                    lhs,
                    RegSize::try_new(ty.size_of()).unwrap(),
                );
            }
            ConvBinOpKind::LShift => {
                writeln!(f, "  mov rcx, {}", rhs.qword())?;
                writeln!(f, "  sal {}, cl", lhs.qword())?;
                return self.push_with_sign_extension(
                    f,
                    lhs,
                    RegSize::try_new(ty.size_of()).unwrap(),
                );
            }
            ConvBinOpKind::RShift => {
                writeln!(f, "  mov rcx, {}", rhs.qword())?;
                writeln!(f, "  sar {}, cl", lhs.qword())?;
                return self.push_with_sign_extension(
                    f,
                    lhs,
                    RegSize::try_new(ty.size_of()).unwrap(),
                );
            }
            ConvBinOpKind::BitWiseAnd => {
                writeln!(f, "  and {}, {}", lhs.qword(), rhs.qword())?;
                return self.push_with_sign_extension(
                    f,
                    lhs,
                    RegSize::try_new(ty.size_of()).unwrap(),
                );
            }
            ConvBinOpKind::Div => {
                if lhs != RegKind::Rax {
                    writeln!(f, "  mov rax, {}", lhs.qword())?;
                }
                if rhs != RegKind::Rdi {
                    writeln!(f, "  mov rdi, {}", rhs.qword())?;
                }
                if lhs_ty_size == 8 {
                    // rdx-rax = rdx
                    writeln!(f, "  cqo")?;
                    // 0x0000ffff -> // 0x0000ffff
                } else if lhs_ty_size == 4 {
                    // edx-eax = eax
                    writeln!(f, "  cdq")?;
                    // 0x0000ffff -> // 0xffffffff
                }

                // rax = rdx-rax / rdi
                // rdx = rdx-rax % rdi
                writeln!(f, "  idiv edi")?;
            }
            ConvBinOpKind::Rem => {
                if lhs != RegKind::Rax {
                    writeln!(f, "  mov rax, {}", lhs.qword())?;
                }
                if rhs != RegKind::Rdi {
                    writeln!(f, "  mov rdi, {}", rhs.qword())?;
                }
                if lhs_ty_size == 8 {
                    // rdx-rax = rdx
                    writeln!(f, "  cqo")?;
                    // 0x0000ffff -> // 0x0000ffff
                } else if lhs_ty_size == 4 {
                    // edx-eax = eax
                    writeln!(f, "  cdq")?;
                    // 0x0000ffff -> // 0xffffffff
                }
                // rax = rdx-rax / rdi
                // rdx = rdx-rax % rdi
                writeln!(f, "  idiv rdi")?;
                writeln!(f, "  mov rax, rdx")?;
            }
            ConvBinOpKind::Eq => {
                // writeln!(f, "  cmp rax, rdi")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(lhs),
                    lhs_ty_size,
                    RegOrLit::Reg(rhs),
                    rhs_ty_size,
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
                    RegOrLit::Reg(lhs),
                    lhs_ty_size,
                    RegOrLit::Reg(rhs),
                    rhs_ty_size,
                )?;
                // al = flag-reg(less than or equal to)
                writeln!(f, "  setle al")?;
                writeln!(f, "  movzx rax, al")?;
            }
            ConvBinOpKind::Lt => {
                // writeln!(f, "  cmp rax, rdi")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(lhs),
                    lhs_ty_size,
                    RegOrLit::Reg(rhs),
                    rhs_ty_size,
                )?;
                // al = flag-reg(less than)
                writeln!(f, "  setl al")?;
                writeln!(f, "  movzx rax, al")?;
            }
            ConvBinOpKind::Ne => {
                // writeln!(f, "  cmp rax, rdi")?;
                Self::gen_cmp(
                    f,
                    RegOrLit::Reg(lhs),
                    lhs_ty_size,
                    RegOrLit::Reg(rhs),
                    rhs_ty_size,
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

    // clippy complains about this, but it's wrong
    #[allow(clippy::missing_const_for_fn)]
    pub fn try_new_with_error(size: usize, expr: ConvExpr) -> Result<Self, CompileError> {
        Ok(match size {
            1 => Self::Byte,
            4 => Self::Dword,
            8 => Self::Qword,
            _ => {
                return Err(CompileError::new_type_size_error(
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
