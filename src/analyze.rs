use std::{
    collections::{BTreeMap, BTreeSet},
    vec,
};

use crate::{
    error::{AnalyzeErrorKind, CompileError, CompileErrorKind, VariableKind},
    parse::{
        BinOpKind, Binary, Declarator, DirectDeclarator, Expr, ExprKind, ForInitKind, Initializer,
        Program, ProgramComponent, ProgramKind, SizeOfOperandKind, Stmt, StmtKind, TypeSpec, UnOp,
    },
    tokenize::Position,
    unimplemented_err,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Analyzer<'a> {
    input: &'a str,
    offset: usize,
    func_map: BTreeMap<String, Func>,
    pub scope: Scope,
}

impl<'a> Analyzer<'a> {
    pub fn new(input: &'a str) -> Self {
        let func_map: BTreeMap<String, Func> = BTreeMap::new();
        Self {
            input,
            offset: 0,
            func_map,
            scope: Scope::new(),
        }
    }

    pub fn down_program(&mut self, program: Program) -> Result<ConvProgram, CompileError> {
        let mut conv_program = ConvProgram::new();
        for component in program {
            match component {
                ProgramComponent {
                    kind: ProgramKind::FuncDef(type_spec, declrtr, body),

                    pos,
                } => {
                    assert_eq!(self.offset, 0);
                    conv_program.push(self.down_func_def(&type_spec, &declrtr, body, pos)?);
                    self.scope.reset_stack();
                }
                ProgramComponent {
                    kind: ProgramKind::Declaration(declaration),
                    pos,
                } => {
                    let name = declaration.ident_name();
                    if self.func_map.get(&name.to_string()).is_some() {
                        return Err(CompileError::new_redefined_variable(
                            self.input,
                            name.to_string(),
                            pos,
                            VariableKind::Func,
                        ));
                    }
                    match declaration.ty(self)? {
                        Type::Base(_) | Type::Ptr(_) | Type::Array(_, _) => {
                            return Err(unimplemented_err!(
                                self.input,
                                pos,
                                "Global variable is not yet unimplemented."
                            ))
                        }
                        Type::Func(ret_ty, args) => {
                            self.func_map.insert(
                                name.to_string(),
                                Func::new_raw(name.to_string(), args, *ret_ty, pos),
                            );
                        }
                    }
                }
            }
        }
        Ok(conv_program)
    }

    pub fn down_func_def(
        &mut self,
        ty_spec: &TypeSpec,
        declrtr: &Declarator,
        body: Stmt,
        pos: Position,
    ) -> Result<ConvProgramKind, CompileError> {
        let mut lvars = Vec::new();
        let ident = declrtr.d_declrtr.ident_name();
        let ty = self.get_type(ty_spec, declrtr)?;

        let (ret_ty, args_ty);
        if let Type::Func(this_ret_ty, this_args) = ty.clone() {
            (ret_ty, args_ty) = (this_ret_ty, this_args);
        } else {
            return Err(CompileError::new_type_expect_failed(
                self.input,
                pos,
                Type::Func(
                    Box::new(Type::Base(BaseType::Int)),
                    vec![Type::Base(BaseType::Int)],
                ),
                ty,
            ));
        }
        let args = if let Some(args) = declrtr.d_declrtr.args() {
            args
        } else {
            return Err(unimplemented_err!(
                self.input,
                pos,
                "function declarator should not be `Ident(_)`"
            ));
        };
        self.scope.push_scope();
        let body = if let StmtKind::Block(stmts) = body.kind {
            for arg in args {
                // register func args as lvar
                let ty = self.get_type(&arg.ty_spec, &arg.declrtr)?;
                let name = arg.ident_name();
                let lvar =
                    self.scope
                        .register_var(self.input, arg.pos, &mut self.offset, name, ty)?;
                lvars.push(lvar.clone());
            }
            self.func_map.insert(
                ident.to_string(),
                Func::new_raw(ident.to_string(), args_ty, *ret_ty, pos),
            );
            ConvStmt::new_block(
                stmts
                    .into_iter()
                    .map(|stmt| self.down_stmt(stmt, ident.to_string()))
                    .collect::<Result<Vec<_>, CompileError>>()?,
            )
        } else {
            return Err(unimplemented_err!(
                self.input,
                pos,
                "Function body must be block stmt."
            ));
        };
        self.scope.pop_scope(&mut self.offset);
        let stack_size = self.scope.get_stack_size(); // this_func_map.into_values().collect::<BTreeSet<_>>(),
        Ok(ConvProgramKind::Func(ConvFuncDef::new(
            ty,
            ident.to_owned(),
            lvars,
            body,
            stack_size,
        )))
    }

    #[allow(clippy::pedantic)]
    pub fn down_stmt(&mut self, stmt: Stmt, fn_name: String) -> Result<ConvStmt, CompileError> {
        Ok(match stmt.kind {
            StmtKind::Expr(expr) => ConvStmt::new_expr(self.down_expr(expr, BTreeSet::new())?),
            StmtKind::Return(expr) => {
                ConvStmt::new_ret(self.down_expr(expr, BTreeSet::new())?, fn_name)
            }
            StmtKind::If(cond, then, els) => ConvStmt::new_if(
                self.down_expr(cond, BTreeSet::new())?,
                self.down_stmt(*then, fn_name.clone())?,
                els.map(|stmt| self.down_stmt(*stmt, fn_name.clone()))
                    .transpose()?,
            ),
            StmtKind::While(cond, then) => ConvStmt::new_while(
                self.down_expr(cond, BTreeSet::new())?,
                self.down_stmt(*then, fn_name)?,
            ),
            StmtKind::For(init, cond, inc, then) => {
                self.scope.push_scope();
                let for_stmt = ConvStmt::new_for(
                    init.map_or(Ok(None), |expr| match expr {
                        ForInitKind::Expr(expr) => self.down_expr(expr, BTreeSet::new()).map(Some),
                        ForInitKind::Declaration(declaration) => {
                            let ty = declaration.ty(self)?;
                            let lvar = self.scope.register_var(
                                self.input,
                                declaration.pos,
                                &mut self.offset,
                                declaration.ident_name(),
                                ty.clone(),
                            )?;
                            // lvar_map.insert(declaration.ident_name().to_string(), lvar.clone());
                            if let Some(init) = declaration.initializer {
                                Ok(Some(self.new_init_expr(
                                    init,
                                    lvar,
                                    ty,
                                    BTreeSet::new(),
                                    declaration.pos,
                                )?))
                            } else {
                                Ok(None)
                            }
                        }
                    })?,
                    cond.map(|expr| self.down_expr(expr, BTreeSet::new()))
                        .transpose()?,
                    inc.map(|expr| self.down_expr(expr, BTreeSet::new()))
                        .transpose()?,
                    self.down_stmt(*then, fn_name)?,
                );
                self.scope.pop_scope(&mut self.offset);
                for_stmt
            }
            StmtKind::Block(stmts) => {
                self.scope.push_scope();
                let block = ConvStmt::new_block(
                    stmts
                        .into_iter()
                        .map(|stmt| self.down_stmt(stmt, fn_name.clone()))
                        .collect::<Result<Vec<_>, CompileError>>()?,
                );
                self.scope.pop_scope(&mut self.offset);
                block
            }
            StmtKind::Declare(declare) => {
                let ty = declare.ty(self)?;
                // TODO check: Function pointer declaration is allowed here (or not)?
                let name = declare.ident_name();
                let lvar = self.scope.register_var(
                    self.input,
                    declare.pos,
                    &mut self.offset,
                    name,
                    ty.clone(),
                )?;
                match declare.initializer {
                    Some(Initializer::Expr(init)) => {
                        let rhs = self.down_expr(init, BTreeSet::new())?;
                        ConvStmt::new_expr(self.new_assign_expr_with_type_check(
                            // Safety:
                            // the lvar is generated by `Self::new_lvar` which initializes lvar_map
                            ConvExpr::new_lvar_raw(lvar, ty, declare.pos),
                            rhs,
                            declare.pos,
                        )?)
                    }
                    Some(Initializer::Array(_)) => {
                        return Err(unimplemented_err!(
                            self.input,
                            declare.pos,
                            "Array initializer is not currently supported."
                        ))
                    }
                    // just declaration does nothing
                    None => ConvStmt::new_block(vec![]),
                }
            }
        })
    }

    // this `&self` will be used when type checking is activated
    #[allow(clippy::unused_self)]
    pub fn new_assign_expr_with_type_check(
        &self,
        lhs: ConvExpr,
        rhs: ConvExpr,
        pos: Position,
    ) -> Result<ConvExpr, CompileError> {
        if lhs.ty != rhs.ty {
            return Err(CompileError::new_type_error(
                self.input,
                lhs,
                rhs,
                Some("Assign expression's lhs and rhs has to have compatible types"),
            ));
        }
        Ok(ConvExpr::new_assign(lhs, rhs, pos))
    }

    pub fn down_expr(
        &mut self,
        expr: Expr,
        // lvar_map: &mut BTreeMap<String, Lvar>,
        mut attrs: BTreeSet<DownExprAttribute>,
    ) -> Result<ConvExpr, CompileError> {
        let pos = expr.pos;
        let expr = match expr.kind {
            // `a >= b` := `b <= a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Ge,
                lhs,
                rhs,
            }) => self.down_binary(Binary::new(BinOpKind::Le, rhs, lhs), pos, attrs.clone()),
            // `a > b` := `b < a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Gt,
                lhs,
                rhs,
            }) => self.down_binary(Binary::new(BinOpKind::Lt, rhs, lhs), pos, attrs.clone()),
            // do nothing
            ExprKind::Binary(binary) => self.down_binary(binary, pos, attrs.clone()), // do nothing
            ExprKind::Num(n) => Ok(ConvExpr::new_num(n, pos)),
            // substitute `-x` into `0-x`
            ExprKind::Unary(UnOp::Minus, operand) => self.down_binary(
                Binary::new(BinOpKind::Sub, Box::new(Expr::new_num(0, pos)), operand),
                pos,
                attrs.clone(),
            ),
            // do nothing
            ExprKind::Unary(UnOp::Plus, operand) => self.down_expr(*operand, attrs.clone()),
            // do nothing
            ExprKind::Assign(lhs, rhs) => {
                let lhs = self.down_expr(*lhs, attrs.clone())?;
                let rhs = self.down_expr(*rhs, attrs.clone())?;
                self.new_assign_expr_with_type_check(lhs, rhs, pos)
            }
            ExprKind::LVar(name) => self.fetch_lvar(&name, pos),
            ExprKind::Func(name, args) => {
                self.new_call_func_with_type_check(name, args, pos, &attrs)
            }
            ExprKind::Deref(expr) => {
                // type check
                let conv_expr = self.down_expr(*expr, attrs.clone())?;
                match conv_expr.ty.clone() {
                    ty @ (Type::Base(_) | Type::Func(_, _)) => {
                        Err(CompileError::new_type_expect_failed(
                            self.input,
                            conv_expr.pos,
                            // TODO: base type is not a problem of this error
                            Type::Ptr(Box::new(Type::Base(BaseType::Int))),
                            ty,
                        ))
                    }
                    // `*array` := `*(&array[0])`
                    Type::Array(array_base, _) => Ok(ConvExpr::new_deref(
                        // TODO: define appropriate error type
                        conv_expr.convert_array_to_ptr(),
                        *array_base,
                        pos,
                    )),
                    Type::Ptr(ptr_base) => Ok(ConvExpr::new_deref(conv_expr, *ptr_base, pos)),
                }
            }
            ExprKind::Addr(expr) => Ok(ConvExpr::new_addr(
                self.down_expr(*expr, attrs.clone())?,
                pos,
            )),
            ExprKind::SizeOf(SizeOfOperandKind::Expr(expr)) => {
                attrs.insert(DownExprAttribute::NoArrayPtrConversion);
                let size = self.down_expr(*expr, attrs.clone())?.ty.size_of() as isize;
                Ok(ConvExpr::new_num(size, pos))
            }
            ExprKind::SizeOf(SizeOfOperandKind::Type(type_name)) => {
                let size = type_name.ty().size_of() as isize;
                Ok(ConvExpr::new_num(size, pos))
            }
            ExprKind::Array(expr, index) => {
                let pos = expr.pos;
                // `a[i]` := `*(a + i)`
                let desugered =
                    Expr::new_deref(Expr::new_binary(BinOpKind::Add, *expr, *index, pos), pos);
                self.down_expr(desugered, attrs.clone())
            }
        };
        if attrs.contains(&DownExprAttribute::NoArrayPtrConversion) {
            expr
        } else {
            expr.map(ConvExpr::convert_array_to_ptr)
        }
    }

    pub fn new_call_func_with_type_check(
        &mut self,
        name: String,
        args: Vec<Expr>,
        pos: Position,
        attrs: &BTreeSet<DownExprAttribute>,
    ) -> Result<ConvExpr, CompileError> {
        // args type check
        let args = args
            .into_iter()
            .map(|expr| self.down_expr(expr, attrs.clone()))
            .collect::<Result<Vec<_>, CompileError>>()?;
        let Func {
            name: _,
            args: args_ty,
            ret: ret_ty,
            pos: declared_pos,
        } = match self.func_map.get(&name) {
            Some(func) => func,
            None => {
                return Err(CompileError::new_undeclared_error(
                    self.input,
                    name,
                    pos,
                    VariableKind::Func,
                ))
            }
        };
        if args_ty.len() != args.len() {
            return Err(CompileError::new_args_error(
                self.input,
                name,
                pos,
                args_ty.len(),
                args.len(),
                *declared_pos,
            ));
        }
        for (expected_ty, got_expr) in args_ty.iter().zip(args.iter()) {
            if *expected_ty != got_expr.ty {
                return Err(CompileError::new_type_expect_failed(
                    self.input,
                    got_expr.pos,
                    expected_ty.clone(),
                    got_expr.ty.clone(),
                ));
            }
        }
        Ok(ConvExpr::new_func(name, args, ret_ty.clone(), pos))
    }

    #[allow(clippy::too_many_lines)]
    pub fn down_binary(
        &mut self,
        Binary { kind, lhs, rhs }: Binary,
        pos: Position,
        // lvar_map: &mut BTreeMap<String, Lvar>,
        attrs: BTreeSet<DownExprAttribute>,
    ) -> Result<ConvExpr, CompileError> {
        let mut rhs = self.down_expr(*rhs, attrs.clone())?;
        let mut lhs = self.down_expr(*lhs, attrs)?;
        let kind = ConvBinOpKind::new(&kind).unwrap();
        match kind {
            ConvBinOpKind::Add | ConvBinOpKind::Sub => match (&lhs.ty, &rhs.ty) {
                (Type::Base(lhs_ty), Type::Base(rhs_ty)) => {
                    if lhs_ty != rhs_ty {
                        return Err(CompileError::new_type_error(
                            self.input,
                            lhs,
                            rhs,
                            Some(
                                "incompatible type addition or subtraction is not allowed"
                                    .to_string(),
                            ),
                        ));
                    }
                    let base_ty = *lhs_ty;
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Base(base_ty),
                        pos,
                    ))
                }
                (Type::Base(base), Type::Ptr(ptr_base)) => {
                    if *base != BaseType::Int {
                        return Err(CompileError::new_type_expect_failed(
                            self.input,
                            lhs.pos,
                            Type::Base(BaseType::Int),
                            Type::Base(*base),
                        ));
                    }
                    lhs = ConvExpr::new_binary(
                        ConvBinOpKind::Mul,
                        ConvExpr::new_num(ptr_base.size_of() as isize, pos),
                        lhs.clone(),
                        Type::Base(BaseType::Int),
                        pos,
                    ); // i + p -> sizeof(*p) * i + p
                    let ptr_base = ptr_base.clone();
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Ptr(ptr_base),
                        pos,
                    ))
                }
                (Type::Ptr(ptr_base), Type::Base(base)) => {
                    if *base != BaseType::Int {
                        return Err(CompileError::new_type_expect_failed(
                            self.input,
                            lhs.pos,
                            Type::Base(*base),
                            Type::Base(BaseType::Int),
                        ));
                    }

                    rhs = ConvExpr::new_binary(
                        ConvBinOpKind::Mul,
                        rhs.clone(),
                        ConvExpr::new_num(ptr_base.size_of() as isize, pos),
                        // Type::Ptr(ptr_base.clone()),
                        Type::Base(BaseType::Int),
                        pos,
                    ); // p + i ->  p + i * sizeof(*p)
                    let ptr_base = ptr_base.clone();
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Ptr(ptr_base),
                        pos,
                    ))
                }
                (Type::Ptr(lhs_base), Type::Ptr(rhs_base))
                    if matches!(kind, ConvBinOpKind::Sub) =>
                {
                    if *lhs_base != *rhs_base {
                        return Err(CompileError::new_type_error(
                            self.input,
                            lhs,
                            rhs,
                            Some("incompatible type on ptr subtraction is not allowed".to_string()),
                        ));
                    }
                    let base_ty = lhs_base.clone();
                    let size_of = ConvExpr::new_num(base_ty.size_of() as isize, pos);
                    let subtracted = ConvExpr::new_binary(
                        ConvBinOpKind::Sub,
                        lhs,
                        rhs,
                        Type::Ptr(base_ty.clone()),
                        pos,
                    );
                    Ok(ConvExpr::new_binary(
                        ConvBinOpKind::Div,
                        subtracted,
                        size_of,
                        *base_ty,
                        pos,
                    ))
                }
                (Type::Ptr(_), Type::Ptr(_)) => Err(CompileError::new_type_error(
                    self.input,
                    lhs,
                    rhs,
                    Some("ptr + ptr or ptr - ptr is not allowed. ".to_string()),
                )),
                (Type::Base(_), Type::Func(_, _)) => todo!(),
                (Type::Ptr(_), Type::Func(_, _)) => todo!(),
                (Type::Func(_, _), Type::Base(_)) => todo!(),
                (Type::Func(_, _), Type::Ptr(_)) => todo!(),
                (Type::Func(_, _), Type::Func(_, _)) => todo!(),
                // TODO: convert array to ptr before this match expr
                (Type::Base(_), Type::Array(_, _)) => todo!(),
                (Type::Ptr(_), Type::Array(_, _)) => todo!(),
                (Type::Func(_, _), Type::Array(_, _)) => todo!(),
                (Type::Array(_, _), Type::Base(_)) => todo!(),
                (Type::Array(_, _), Type::Ptr(_)) => todo!(),
                (Type::Array(_, _), Type::Func(_, _)) => todo!(),
                (Type::Array(_, _), Type::Array(_, _)) => todo!(),
            },
            ConvBinOpKind::Mul | ConvBinOpKind::Div => {
                if lhs.ty != rhs.ty {
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some(
                            "incompatible type multiplication or division is not allowed"
                                .to_string(),
                        ),
                    ));
                }
                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(kind, lhs, rhs, lhs_ty, pos))
            }
            ConvBinOpKind::Rem => {
                if lhs.ty != rhs.ty {
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some("incompatible type take remaint is not allowed".to_string()),
                    ));
                }

                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(kind, lhs, rhs, lhs_ty, pos))
            }
            ConvBinOpKind::Eq | ConvBinOpKind::Le | ConvBinOpKind::Lt | ConvBinOpKind::Ne => {
                if lhs.ty != rhs.ty {
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some("incompatible type binary expr is not allowed".to_string()),
                    ));
                }

                Ok(ConvExpr::new_binary(
                    kind,
                    lhs,
                    rhs,
                    Type::Base(BaseType::Int),
                    pos,
                ))
            }
        }
    }

    pub fn new_init_expr(
        &mut self,
        init: Initializer,
        lvar: Lvar,
        ty: Type,
        // lvar_map: &mut BTreeMap<String, Lvar>,
        attrs: BTreeSet<DownExprAttribute>,
        pos: Position,
    ) -> Result<ConvExpr, CompileError> {
        match init {
            Initializer::Expr(init) => {
                let rhs = self.down_expr(init, attrs)?;
                self.new_assign_expr_with_type_check(
                    // Safety:
                    // the lvar is generated by `Self::new_lvar` which initializes lvar_map
                    ConvExpr::new_lvar_raw(lvar, ty, pos),
                    rhs,
                    pos,
                )
            }
            Initializer::Array(_) => Err(unimplemented_err!(
                self.input,
                pos,
                "Array initializer is not currently supported."
            )),
        }
    }

    pub fn get_type<T: Into<Type> + Clone>(
        &self,
        ty_spec: T,
        declrtr: &Declarator,
    ) -> Result<Type, CompileError> {
        let mut ty = ty_spec.into();
        for _ in 0..declrtr.n_star {
            ty = Type::Ptr(Box::new(ty));
        }
        let mut watching_d_declrtr = &declrtr.d_declrtr;
        loop {
            match watching_d_declrtr {
                DirectDeclarator::Func(d_declrtr, args) => {
                    // TODO: support function type for declaration
                    // e.g) int a(arg1: int, arg2: int*)(arg0: int) -> Func(Func(Int, vec![arg0]), vec![arg1, arg2])
                    ty = Type::Func(
                        Box::new(ty),
                        args.iter()
                            .map(|declaration| {
                                self.get_type(&declaration.ty_spec, &declaration.declrtr)
                            })
                            .collect::<Result<Vec<_>, CompileError>>()?,
                    );
                    watching_d_declrtr = d_declrtr;
                }
                DirectDeclarator::Array(d_declrtr, expr) => {
                    #[allow(clippy::cast_sign_loss)]
                    let size = *if let Expr {
                        kind: ExprKind::Num(size),
                        pos: _,
                    } = expr
                    {
                        size
                    } else {
                        return Err(unimplemented_err!(format!(
                            "Other than num expr is not allowed as lenghth of array {:?}",
                            expr
                        )));
                    } as usize;
                    ty = Type::Array(Box::new(ty), size);
                    watching_d_declrtr = d_declrtr;
                }
                DirectDeclarator::Declarator(declarator) => {
                    ty = self.get_type(ty, declarator)?;
                    break;
                }
                DirectDeclarator::Ident(_) => break,
            }
        }

        Ok(ty)
    }

    pub fn fetch_lvar(
        &self,
        name: &str,
        pos: Position,
        // lvar_map: &mut BTreeMap<String, Lvar>,
    ) -> Result<ConvExpr, CompileError> {
        let lvar = match self.scope.look_up(&name.to_string()) {
            Some(lvar) => lvar.clone(),
            None => {
                return Err(CompileError::new(
                    self.input,
                    CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                        name.to_string(),
                        pos,
                        VariableKind::Local,
                    )),
                ))
            }
        };
        let ty = lvar.ty.clone();
        Ok(ConvExpr::new_lvar_raw(lvar, ty, pos))
    }

    /// create first defined variable
    pub fn new_lvar(
        src: &str,
        name: String,
        pos: Position,
        new_offset: &mut usize,
        ty: Type,
        lvar_map: &mut BTreeMap<String, Lvar>,
    ) -> Result<Lvar, CompileError> {
        let offset = if lvar_map.get(&name).is_some() {
            return Err(CompileError::new_redefined_variable(
                src,
                name,
                pos,
                VariableKind::Local,
            ));
        } else {
            *new_offset += ty.size_of();
            lvar_map.insert(name, Lvar::new_raw(*new_offset, ty.clone()));
            *new_offset
        };
        Ok(Lvar::new_raw(offset, ty))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvProgram {
    components: Vec<ConvProgramKind>,
}

impl ConvProgram {
    pub const fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn with_vec(vec: Vec<ConvProgramKind>) -> Self {
        Self { components: vec }
    }

    pub fn push(&mut self, kind: ConvProgramKind) {
        self.components.push(kind);
    }
}

impl IntoIterator for ConvProgram {
    type Item = ConvProgramKind;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvProgramKind {
    Func(ConvFuncDef),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvFuncDef {
    pub ty: Type,
    pub name: String,
    pub args: Vec<Lvar>,
    pub body: ConvStmt,
    pub stack_size: usize,
}

impl ConvFuncDef {
    pub fn new(ty: Type, name: String, args: Vec<Lvar>, body: ConvStmt, stack_size: usize) -> Self {
        Self {
            ty,
            name,
            args,
            body,
            stack_size,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvStmt {
    pub kind: ConvStmtKind,
}

impl ConvStmt {
    pub const fn new_expr(expr: ConvExpr) -> Self {
        Self {
            kind: ConvStmtKind::Expr(expr),
        }
    }

    pub const fn new_ret(expr: ConvExpr, name: String) -> Self {
        Self {
            kind: ConvStmtKind::Return(expr, name),
        }
    }

    pub fn new_block(stmts: Vec<ConvStmt>) -> Self {
        Self {
            kind: ConvStmtKind::Block(stmts),
        }
    }

    pub fn new_if(cond: ConvExpr, then: ConvStmt, els: Option<ConvStmt>) -> Self {
        Self {
            kind: ConvStmtKind::If(cond, Box::new(then), els.map(Box::new)),
        }
    }

    pub fn new_while(cond: ConvExpr, then: ConvStmt) -> Self {
        Self {
            kind: ConvStmtKind::While(cond, Box::new(then)),
        }
    }

    pub fn new_for(
        init: Option<ConvExpr>,
        cond: Option<ConvExpr>,
        inc: Option<ConvExpr>,
        then: ConvStmt,
    ) -> Self {
        Self {
            kind: ConvStmtKind::For(init, cond, inc, Box::new(then)),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvStmtKind {
    Expr(ConvExpr),
    Return(ConvExpr, String),
    Block(Vec<ConvStmt>),
    If(ConvExpr, Box<ConvStmt>, Option<Box<ConvStmt>>),
    While(ConvExpr, Box<ConvStmt>),
    For(
        Option<ConvExpr>,
        Option<ConvExpr>,
        Option<ConvExpr>,
        Box<ConvStmt>,
    ),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug, Copy)]
pub enum DownExprAttribute {
    NoArrayPtrConversion,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvExpr {
    pub kind: ConvExprKind,
    pub ty: Type,
    pub pos: Position,
}
impl ConvExpr {
    /// if `self.kind == Type::Array(_)` return ptr-converted expr, otherwise return `self` itself
    pub fn convert_array_to_ptr(mut self) -> Self {
        if let Type::Array(base_ty, _) = self.ty.clone() {
            self.ty = *base_ty;
        } else {
            return self;
        };
        let pos = self.pos;
        ConvExpr::new_addr(self, pos)
    }

    #[must_use]
    pub fn map_ty(mut self, f: impl FnOnce(Type) -> Type) -> Self {
        self.ty = f(self.ty);
        self
    }

    pub fn new_binary(
        kind: ConvBinOpKind,
        lhs: ConvExpr,
        rhs: ConvExpr,
        ty: Type,
        pos: Position,
    ) -> Self {
        Self {
            kind: ConvExprKind::Binary(ConvBinary::new(kind, Box::new(lhs), Box::new(rhs))),
            ty,
            pos,
        }
    }

    pub fn new_func(name: String, args: Vec<ConvExpr>, ret_ty: Type, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Func(name, args),
            ty: ret_ty,
            pos,
        }
    }

    pub const fn new_num(num: isize, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Num(num),
            ty: Type::Base(BaseType::Int),
            pos,
        }
    }

    pub fn new_assign(lhs: ConvExpr, rhs: ConvExpr, pos: Position) -> Self {
        let ty = lhs.ty.clone();
        ConvExpr {
            kind: ConvExprKind::Assign(Box::new(lhs), Box::new(rhs)),
            ty,
            pos,
        }
    }

    pub const fn new_lvar_raw(lvar: Lvar, ty: Type, pos: Position) -> Self {
        ConvExpr {
            kind: ConvExprKind::Lvar(lvar),
            ty,
            pos,
        }
    }

    pub fn new_deref(expr: ConvExpr, base_ty: Type, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Deref(Box::new(expr)),
            ty: base_ty,
            pos,
        }
    }

    pub fn new_addr(expr: ConvExpr, pos: Position) -> Self {
        let ty = expr.ty.clone();
        Self {
            kind: ConvExprKind::Addr(Box::new(expr)),
            ty: Type::Ptr(Box::new(ty)),
            pos,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvExprKind {
    Binary(ConvBinary),
    Num(isize),
    Lvar(Lvar),
    Assign(Box<ConvExpr>, Box<ConvExpr>),
    Func(String, Vec<ConvExpr>),
    Deref(Box<ConvExpr>),
    Addr(Box<ConvExpr>),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Lvar {
    /// Used like `mov rax, [rbp - offset]`
    pub offset: usize,
    pub ty: Type,
}

impl Lvar {
    pub const fn new_raw(offset: usize, ty: Type) -> Self {
        Self { offset, ty }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    global: BTreeMap<String, Lvar>,
    pub scopes: Vec<BTreeMap<String, Lvar>>,
    max_stack_size: usize,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            global: BTreeMap::new(),
            scopes: Vec::new(),
            max_stack_size: 0,
        }
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(BTreeMap::new());
    }

    pub fn pop_scope(&mut self, offset: &mut usize) {
        assert!(!self.scopes.is_empty());
        let poped = self.scopes.pop();
        *offset -= poped.map_or(0, |scope_map| {
            scope_map
                .values()
                .fold(0, |acc, lvar| acc + lvar.ty.size_of())
        });
    }

    pub const fn get_stack_size(&self) -> usize {
        self.max_stack_size
    }

    pub fn reset_stack(&mut self) {
        assert!(self.scopes.is_empty());
        self.max_stack_size = 0;
    }

    pub fn look_up(&self, name: &String) -> Option<&Lvar> {
        for map in self.scopes.iter().rev() {
            if let Some(lvar) = map.get(name) {
                return Some(lvar);
            }
        }
        self.global.get(name)
    }

    fn get_current_scope_mut(&mut self) -> &mut BTreeMap<String, Lvar> {
        if let Some(map) = self.scopes.last_mut() {
            map
        } else {
            &mut self.global
        }
    }

    pub fn register_var(
        &mut self,
        src: &str,
        pos: Position,
        new_offset: &mut usize,
        name: &str,
        ty: Type,
    ) -> Result<Lvar, CompileError> {
        for scope in &self.scopes {
            if scope.contains_key(name) {
                return Err(CompileError::new_redefined_variable(
                    src,
                    name.to_string(),
                    pos,
                    VariableKind::Local,
                ));
            }
        }
        if self.global.contains_key(name) {
            return Err(CompileError::new_redefined_variable(
                src,
                name.to_string(),
                pos,
                VariableKind::Global,
            ));
        }
        let wathing_scope = self.get_current_scope_mut();
        *new_offset += ty.size_of();
        let lvar = Lvar::new_raw(*new_offset, ty);
        wathing_scope.insert(name.to_string(), lvar.clone());
        self.max_stack_size = usize::max(self.max_stack_size, *new_offset);
        Ok(lvar)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct GlobalVar {
    name: String,
    ty: Type,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Func {
    pub name: String,
    pub args: Vec<Type>,
    pub ret: Type,
    pub pos: Position,
}

impl Func {
    pub const fn new_raw(name: String, args: Vec<Type>, ret: Type, pos: Position) -> Self {
        Self {
            name,
            args,
            ret,
            pos,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Base(BaseType),
    Ptr(Box<Type>),
    Func(Box<Type>, Vec<Type>),
    Array(Box<Type>, usize),
}

impl From<&TypeSpec> for Type {
    fn from(ty_spec: &TypeSpec) -> Self {
        match ty_spec {
            TypeSpec::Int => Type::Base(BaseType::Int),
        }
    }
}

impl Type {
    pub const fn size_of(&self) -> usize {
        match self {
            Type::Base(BaseType::Int) => 4,
            Type::Ptr(_) | Type::Func(_, _) => 8,
            Type::Array(ty, size) => ty.size_of() * *size,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum BaseType {
    Int,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvBinary {
    pub kind: ConvBinOpKind,
    pub lhs: Box<ConvExpr>,
    pub rhs: Box<ConvExpr>,
}

impl ConvBinary {
    pub fn new(kind: ConvBinOpKind, lhs: Box<ConvExpr>, rhs: Box<ConvExpr>) -> Self {
        Self { kind, lhs, rhs }
    }
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvBinOpKind {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (remains)
    Rem,
    /// The `==` operator (equality)
    Eq,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `<` operator (less than)
    Lt,
    /// The `!=` operator (Not equal to)
    Ne,
}

impl ConvBinOpKind {
    pub const fn new(kind: &BinOpKind) -> Option<Self> {
        match kind {
            BinOpKind::Add => Some(ConvBinOpKind::Add),
            BinOpKind::Sub => Some(ConvBinOpKind::Sub),
            BinOpKind::Mul => Some(ConvBinOpKind::Mul),
            BinOpKind::Div => Some(ConvBinOpKind::Div),
            BinOpKind::Rem => Some(ConvBinOpKind::Rem),
            BinOpKind::Eq => Some(ConvBinOpKind::Eq),
            BinOpKind::Le => Some(ConvBinOpKind::Le),
            BinOpKind::Lt => Some(ConvBinOpKind::Lt),
            BinOpKind::Ge | BinOpKind::Gt => None,
            BinOpKind::Ne => Some(ConvBinOpKind::Ne),
        }
    }
}
