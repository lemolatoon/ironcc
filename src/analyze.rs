use std::{
    collections::{BTreeMap, BTreeSet},
    vec,
};

use crate::{
    error::{AnalyzeErrorKind, CompileError, CompileErrorKind, VariableKind},
    generate::RegSize,
    parse::{
        BinOpKind, Binary, Declarator, DirectDeclarator, Expr, ExprKind, ForInitKind, Initializer,
        Program, ProgramComponent, ProgramKind, SizeOfOperandKind, Stmt, StmtKind,
        StructDeclaration, StructOrUnionSpec, TypeSpec, UnaryOp,
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
    pub tag_scope: Vec<BTreeMap<String, Taged>>,
    pub conv_program: ConvProgram,
    lc_label: usize,
}

impl<'a> Analyzer<'a> {
    pub fn new(input: &'a str) -> Self {
        let func_map: BTreeMap<String, Func> = BTreeMap::new();
        Self {
            input,
            offset: 0,
            func_map,
            tag_scope: Vec::new(),
            scope: Scope::new(),
            conv_program: ConvProgram::new(),
            lc_label: 0,
        }
    }

    pub fn get_lc_label(&mut self) -> usize {
        let lc_label = self.lc_label;
        self.lc_label += 1;
        lc_label
    }

    pub fn look_up_struct_tag(&self, name: &str) -> Option<&Taged> {
        for map in self.tag_scope.iter().rev() {
            if let Some(taged) = map.get(name) {
                return Some(taged);
            }
        }
        None
    }

    pub fn traverse_program(&mut self, program: Program) -> Result<ConvProgram, CompileError> {
        self.tag_scope.push(BTreeMap::new());
        for component in program {
            match component {
                ProgramComponent {
                    kind: ProgramKind::FuncDef(type_spec, declarator, body),

                    pos,
                } => {
                    assert_eq!(self.offset, 0);
                    let func_def = self.traverse_func_def(&type_spec, &declarator, body, pos)?;
                    self.conv_program.push(func_def);
                    self.scope.reset_stack();
                }
                ProgramComponent {
                    kind: ProgramKind::Declaration(declaration),
                    pos,
                } => {
                    if let Some(init_declarator) = declaration.init_declarator {
                        let name = init_declarator.ident_name();
                        let init = &init_declarator.initializer.as_ref();
                        let pos = declaration.pos;
                        let converted_type =
                            self.resolve_name_and_convert_to_type(&declaration.ty_spec, pos)?;
                        match self.get_type(converted_type, &init_declarator.declarator)? {
                            ty @ (Type::Base(_) | Type::Ptr(_) | Type::Array(_, _)) => {
                                let gvar = self.new_global_variable(init, name, ty, pos)?;
                                self.conv_program.push(ConvProgramKind::Global(gvar));
                            }
                            Type::Func(ret_ty, args) => {
                                if self.func_map.get(&name.to_string()).is_some() {
                                    return Err(CompileError::new_redefined_variable(
                                        self.input,
                                        name.to_string(),
                                        pos,
                                        VariableKind::Func,
                                    ));
                                }
                                self.func_map.insert(
                                    name.to_string(),
                                    Func::new_raw(name.to_string(), args, *ret_ty, pos),
                                );
                            }
                            Type::Struct(_) => todo!(),
                        }
                    } else {
                        // struct declaration
                        match declaration.ty_spec {
                            TypeSpec::StructOrUnion(StructOrUnionSpec::WithList(
                                Some(name),
                                vec,
                            )) => {
                                let types = vec
                                    .iter()
                                    .map(|struct_declaration| {
                                        struct_declaration.get_type(self, struct_declaration.pos)
                                    })
                                    .collect::<Result<_, CompileError>>()?;
                                let names = vec
                                    .iter()
                                    .map(|struct_declaration| {
                                        struct_declaration.ident_name().to_string()
                                    })
                                    .collect::<Vec<String>>();
                                self.tag_scope
                                    .last_mut()
                                    .expect("INTERNAL COMPILER ERROR.")
                                    .insert(name, Taged::new_struct_tag(names, types));
                            }
                            _ => {
                                return Err(unimplemented_err!(
                                    self.input,
                                    pos,
                                    "Expected struct declaration with name and list."
                                ))
                            }
                        }
                    }
                }
            }
        }
        self.tag_scope.pop();
        assert!(self.tag_scope.is_empty());
        Ok(self.conv_program.clone())
    }

    /// Construct Global Variable and register it to `self.scope`
    pub fn new_global_variable(
        &mut self,
        init: &Option<&Initializer>,
        name: &str,
        // global variable's type
        ty: Type,
        pos: Position,
    ) -> Result<GVar, CompileError> {
        let init = init
            // .as_ref()
            .map(|expr| {
                expr.clone().map(|expr| {
                    ConstExpr::try_eval_as_const(
                        self.input,
                        self.traverse_expr(expr, BTreeSet::new())?,
                    )
                })
            })
            .transpose()?;
        let init_pos_ty = match &init {
            Some(ConstInitializer::Array(exprs)) => {
                // array initializer's items have the same types each or not.
                let mut unequal_type_index = 0;
                if !exprs.get(0).map_or(true, |first| {
                    exprs
                        .iter()
                        .map(|expr| &expr.ty)
                        .enumerate()
                        .all(|(idx, ty)| {
                            unequal_type_index = idx;
                            *ty == first.ty
                        })
                }) {
                    return Err(CompileError::new_type_error_const(
                        self.input,
                        exprs[0].clone(),
                        exprs[unequal_type_index].clone(),
                        Some("Array Initializer has incompatible types"),
                    ));
                }
                Some((exprs[0].pos, exprs[0].ty.clone()))
            }
            Some(ConstInitializer::Expr(expr)) => Some((expr.pos, expr.ty.clone())),
            None => None,
        };
        if let Some((init_pos, init_ty)) = init_pos_ty {
            // type check
            match ty.clone() {
                Type::Base(_) | Type::Ptr(_) => {
                    if ty != init_ty {
                        if ty.is_base() && init_ty.is_base() {
                        } else {
                            return Err(CompileError::new_type_error_types(
                                self.input,
                                pos,
                                init_pos,
                                ty,
                                init_ty,
                                Some("Incompatible type at expr initializer"),
                            ));
                        }
                    }
                }
                Type::Func(_, _) => unreachable!(),
                Type::Struct(_) => {
                    return Err(unimplemented_err!(
                        self.input,
                        pos,
                        "Global Variable Initialization with struct is not currently supported."
                    ))
                }
                Type::Array(base, _) => {
                    if *base != init_ty {
                        if base.is_base() && init_ty.is_base() {
                        } else {
                            return Err(CompileError::new_type_error_types(
                                self.input,
                                pos,
                                init_pos,
                                *base,
                                init_ty,
                                Some("Incompatible type at array initializer"),
                            ));
                        }
                    }
                }
            };
        }

        self.scope.register_gvar(self.input, pos, name, ty, init)
    }

    pub fn traverse_func_def(
        &mut self,
        ty_spec: &TypeSpec,
        declarator: &Declarator,
        body: Stmt,
        pos: Position,
    ) -> Result<ConvProgramKind, CompileError> {
        let mut lvars = Vec::new();
        let ident = declarator.direct_declarator.ident_name();
        let converted_type = self.resolve_name_and_convert_to_type(ty_spec, pos)?;
        let ty = self.get_type(converted_type, declarator)?;

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
        let args = if let Some(args) = declarator.direct_declarator.args() {
            args
        } else {
            return Err(unimplemented_err!(
                self.input,
                pos,
                "function declarator should not be `Ident(_)`"
            ));
        };
        self.scope.push_scope();
        self.tag_scope.push(BTreeMap::new());
        let body = if let StmtKind::Block(stmts) = body.kind {
            for arg in args {
                // register func args as lvar
                let converted_type =
                    self.resolve_name_and_convert_to_type(&arg.ty_spec, arg.pos)?;
                let ty = self.get_type(
                    converted_type,
                    &arg.init_declarator
                        .as_ref()
                        .unwrap_or_else(|| todo!("struct"))
                        .declarator,
                )?;
                let name = arg.ident_name();
                let name = name.as_ref().expect("struct");
                let lvar =
                    self.scope
                        .register_lvar(self.input, arg.pos, &mut self.offset, name, ty)?;
                lvars.push(lvar.clone());
            }
            self.func_map.insert(
                ident.to_string(),
                Func::new_raw(ident.to_string(), args_ty, *ret_ty, pos),
            );
            ConvStmt::new_block(
                stmts
                    .into_iter()
                    .map(|stmt| self.traverse_stmt(stmt, ident.to_string()))
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
        self.tag_scope.pop();
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
    pub fn traverse_stmt(&mut self, stmt: Stmt, fn_name: String) -> Result<ConvStmt, CompileError> {
        Ok(match stmt.kind {
            StmtKind::Expr(expr) => ConvStmt::new_expr(self.traverse_expr(expr, BTreeSet::new())?),
            StmtKind::Return(expr) => {
                ConvStmt::new_ret(self.traverse_expr(expr, BTreeSet::new())?, fn_name)
            }
            StmtKind::If(cond, then, els) => ConvStmt::new_if(
                self.traverse_expr(cond, BTreeSet::new())?,
                self.traverse_stmt(*then, fn_name.clone())?,
                els.map(|stmt| self.traverse_stmt(*stmt, fn_name.clone()))
                    .transpose()?,
            ),
            StmtKind::While(cond, then) => ConvStmt::new_while(
                self.traverse_expr(cond, BTreeSet::new())?,
                self.traverse_stmt(*then, fn_name)?,
            ),
            StmtKind::For(init, cond, inc, then) => {
                self.scope.push_scope();
                let for_stmt = ConvStmt::new_for(
                    init.map_or(Ok(None), |expr| match expr {
                        ForInitKind::Expr(expr) => {
                            self.traverse_expr(expr, BTreeSet::new()).map(Some)
                        }
                        ForInitKind::Declaration(declaration) => {
                            let ty = declaration.ty(self, declaration.pos)?;
                            let lvar = self.scope.register_lvar(
                                self.input,
                                declaration.pos,
                                &mut self.offset,
                                declaration.ident_name().unwrap_or_else(|| todo!("struct")),
                                ty.clone(),
                            )?;
                            // lvar_map.insert(declaration.ident_name().to_string(), lvar.clone());
                            if let Some(init) = declaration
                                .init_declarator
                                .map_or(None, |init_declarator| init_declarator.initializer)
                            {
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
                    cond.map(|expr| self.traverse_expr(expr, BTreeSet::new()))
                        .transpose()?,
                    inc.map(|expr| self.traverse_expr(expr, BTreeSet::new()))
                        .transpose()?,
                    self.traverse_stmt(*then, fn_name)?,
                );
                self.scope.pop_scope(&mut self.offset);
                for_stmt
            }
            StmtKind::Block(stmts) => {
                self.scope.push_scope();
                let block = ConvStmt::new_block(
                    stmts
                        .into_iter()
                        .map(|stmt| self.traverse_stmt(stmt, fn_name.clone()))
                        .collect::<Result<Vec<_>, CompileError>>()?,
                );
                self.scope.pop_scope(&mut self.offset);
                block
            }
            StmtKind::Declare(declaration) => {
                let ty = declaration.ty(self, declaration.pos)?;
                // TODO check: Function pointer declaration is allowed here (or not)?
                let name = declaration.ident_name().unwrap_or_else(|| todo!("struct"));
                let lvar = self.scope.register_lvar(
                    self.input,
                    declaration.pos,
                    &mut self.offset,
                    name,
                    ty.clone(),
                )?;
                match declaration
                    .init_declarator
                    .map_or(None, |init_declarator| init_declarator.initializer)
                {
                    Some(Initializer::Expr(init)) => {
                        let rhs = self.traverse_expr(init, BTreeSet::new())?;
                        ConvStmt::new_expr(self.new_assign_expr_with_type_check(
                            // Safety:
                            // the lvar is generated by `register_lvar` which initializes lvar_map
                            ConvExpr::new_lvar_raw(lvar, ty, declaration.pos),
                            rhs,
                            declaration.pos,
                        )?)
                    }
                    Some(Initializer::Array(vec)) => {
                        ConvStmt::new_block(
                            vec.into_iter()
                                .enumerate()
                                .map(|(idx, expr)| {
                                    let expr_pos = expr.pos;
                                    let rhs = self.traverse_expr(expr, BTreeSet::new())?;
                                    Ok(ConvStmt::new_expr(self.new_assign_expr_with_type_check(
                                        ConvExpr::new_deref(
                                            ConvExpr::new_binary(
                                                ConvBinOpKind::Add,
                                                // Safety:
                                                // the lvar is generated by `register_lvar` which initializes lvar_map
                                                // this lvar is ptr converted array
                                                ConvExpr::new_addr(
                                                    ConvExpr::new_lvar_raw(
                                                        lvar.clone(),
                                                        ty.base_type().clone(),
                                                        expr_pos,
                                                    ),
                                                    expr_pos,
                                                ),
                                                ConvExpr::new_num(
                                                    (ty.base_type().size_of() * idx) as isize,
                                                    declaration.pos,
                                                ),
                                                ty.base_type().clone(),
                                                expr_pos,
                                            ),
                                            ty.base_type().clone(),
                                            declaration.pos,
                                        ),
                                        rhs,
                                        declaration.pos,
                                    )?))
                                })
                                .collect::<Result<Vec<_>, CompileError>>()?,
                        )
                        // return Err(unimplemented_err!(
                        //     self.input,
                        //     declare.pos,
                        //     "Array initializer is not currently supported."
                        // ))
                    }
                    // just declaration does nothing
                    None => ConvStmt::new_block(vec![]),
                }
            }
        })
    }

    pub fn new_assign_expr_with_type_check(
        &self,
        lhs: ConvExpr,
        rhs: ConvExpr,
        pos: Position,
    ) -> Result<ConvExpr, CompileError> {
        let mut rhs = rhs;
        if lhs.ty != rhs.ty {
            match (lhs.ty.get_base(), rhs.ty.get_base()) {
                (Some(lhs_base), Some(rhs_base)) => {
                    let rhs_base = *rhs_base;
                    rhs = ConvExpr::new_cast(
                        rhs,
                        lhs.ty.clone(),
                        CastKind::Base2Base(rhs_base, *lhs_base),
                    );
                }
                _ => {
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some("Assign expression's lhs and rhs has to have compatible types"),
                    ))
                }
            }
        }
        Ok(ConvExpr::new_assign(lhs, rhs, pos))
    }

    pub fn traverse_expr(
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
            }) => self.traverse_binary(Binary::new(BinOpKind::Le, rhs, lhs), pos),
            // `a > b` := `b < a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Gt,
                lhs,
                rhs,
            }) => self.traverse_binary(Binary::new(BinOpKind::Lt, rhs, lhs), pos),
            // do nothing
            ExprKind::Binary(binary) => self.traverse_binary(binary, pos), // do nothing
            ExprKind::Num(n) => Ok(ConvExpr::new_num(n, pos)),
            ExprKind::StrLit(mut letters) => {
                let name = format!(".LC{}", self.get_lc_label());
                let len = letters.len();
                letters.push('\0');
                let init = Some(Initializer::Array(
                    letters
                        .chars()
                        .map(|c| Expr::new_num(u8::try_from(c).unwrap() as isize, pos))
                        .collect(),
                ));
                let gvar = self.new_global_variable(
                    &init.as_ref(),
                    &name,
                    Type::Array(Box::new(Type::Base(BaseType::Char)), len + 1),
                    pos,
                )?;
                self.conv_program
                    .push(ConvProgramKind::Global(gvar.clone()));
                // This gvar is initialized with `self.new_global_variable`, that's why this is ok.
                Ok(ConvExpr::new_gvar(gvar, pos))
            }
            // substitute `-x` into `0-x`
            ExprKind::Unary(unary_op, operand) => self.traverse_unary(&unary_op, operand, pos),
            ExprKind::Assign(lhs, rhs) => {
                let lhs = self.traverse_expr(*lhs, BTreeSet::new())?;
                let rhs = self.traverse_expr(*rhs, BTreeSet::new())?;
                self.new_assign_expr_with_type_check(lhs, rhs, pos)
            }
            ExprKind::LVar(name) => self.fetch_lvar(&name, pos),
            ExprKind::Func(name, args) => {
                self.new_call_func_with_type_check(name, args, pos, &attrs)
            }
            ExprKind::Deref(expr) => {
                // type check
                let conv_expr = self.traverse_expr(*expr, BTreeSet::new())?;
                match conv_expr.ty.clone() {
                    ty @ (Type::Base(_) | Type::Func(_, _) | Type::Struct(_)) => {
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
                        conv_expr.convert_array_to_ptr(),
                        *array_base,
                        pos,
                    )),
                    Type::Ptr(ptr_base) => Ok(ConvExpr::new_deref(conv_expr, *ptr_base, pos)),
                }
            }
            ExprKind::Addr(expr) => Ok(ConvExpr::new_addr(
                self.traverse_expr(*expr, BTreeSet::new())?,
                pos,
            )),
            ExprKind::SizeOf(SizeOfOperandKind::Expr(expr)) => {
                attrs.insert(DownExprAttribute::NoArrayPtrConversion);
                let size = self.traverse_expr(*expr, attrs.clone())?.ty.size_of() as isize;
                Ok(ConvExpr::new_num(size, pos))
            }
            ExprKind::SizeOf(SizeOfOperandKind::Type(type_name)) => {
                let size = type_name.ty().size_of() as isize;
                Ok(ConvExpr::new_num(size, pos))
            }
            ExprKind::Array(expr, index) => {
                let pos = expr.pos;
                // `a[i]` := `*(a + i)`
                let desugared =
                    Expr::new_deref(Expr::new_binary(BinOpKind::Add, *expr, *index, pos), pos);
                self.traverse_expr(desugared, BTreeSet::new())
            }
            ExprKind::Member(_, _) => todo!(),
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
        _attrs: &BTreeSet<DownExprAttribute>,
    ) -> Result<ConvExpr, CompileError> {
        // args type check
        let mut args = args
            .into_iter()
            .map(|expr| self.traverse_expr(expr, BTreeSet::new()))
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
        for (expected_ty, got_expr) in args_ty.iter().zip(args.iter_mut()) {
            if *expected_ty != got_expr.ty {
                if expected_ty.is_base() && got_expr.ty.is_base() {
                    let to = *expected_ty.get_base().unwrap();
                    let from = *got_expr.ty.get_base().unwrap();
                    *got_expr = ConvExpr::new_cast(
                        got_expr.clone(),
                        expected_ty.clone(),
                        CastKind::Base2Base(from, to),
                    );
                } else {
                    return Err(CompileError::new_type_expect_failed(
                        self.input,
                        got_expr.pos,
                        expected_ty.clone(),
                        got_expr.ty.clone(),
                    ));
                }
            }
        }
        Ok(ConvExpr::new_func(name, args, ret_ty.clone(), pos))
    }

    #[allow(clippy::too_many_lines)]
    pub fn traverse_binary(
        &mut self,
        Binary { kind, lhs, rhs }: Binary,
        pos: Position,
    ) -> Result<ConvExpr, CompileError> {
        let mut rhs = self.traverse_expr(*rhs, BTreeSet::new())?;
        let mut lhs = self.traverse_expr(*lhs, BTreeSet::new())?;
        let kind = ConvBinOpKind::new(&kind).unwrap();
        match kind {
            ConvBinOpKind::Add | ConvBinOpKind::Sub => match (&lhs.ty, &rhs.ty) {
                (Type::Base(lhs_ty), Type::Base(rhs_ty)) => {
                    let cast_needed = lhs_ty.bytes() != rhs_ty.bytes();
                    let base_ty = if lhs_ty.bytes() >= rhs_ty.bytes() {
                        *lhs_ty
                    } else {
                        *rhs_ty
                    };
                    if cast_needed {
                        let lhs_ty = *lhs_ty;
                        let rhs_ty = *rhs_ty;
                        if lhs_ty.bytes() > rhs_ty.bytes() {
                            // TODO: use fn `implicit_cast`
                            rhs = ConvExpr::new_cast(
                                rhs,
                                Type::Base(lhs_ty),
                                CastKind::Base2Base(rhs_ty, lhs_ty),
                            );
                        } else {
                            lhs = ConvExpr::new_cast(
                                lhs,
                                Type::Base(rhs_ty),
                                CastKind::Base2Base(lhs_ty, rhs_ty),
                            );
                        }
                    }
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
                (_, Type::Func(_, _)) | (Type::Func(_, _), _) => Err(unimplemented_err!(
                    self.input,
                    pos,
                    "binary expr of function is not currently supported."
                )),
                (_, Type::Struct(_)) | (Type::Struct(_), _) => Err(unimplemented_err!(
                    self.input,
                    pos,
                    "binary expr of function is illegal operation."
                )),
                (_, Type::Array(_, _)) | (Type::Array(_, _), _) => {
                    unreachable!(
                        "binary expr's operands(one of which is array) must be casted to ptr."
                    )
                }
            },
            op @ (ConvBinOpKind::LShift | ConvBinOpKind::RShift) => {
                if lhs.ty != rhs.ty {
                    // TODO: char -> int
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some("incompatible type on lshift or rshift is not allowed".to_string()),
                    ));
                }
                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(op, lhs, rhs, lhs_ty, pos))
            }
            op @ ConvBinOpKind::BitWiseAnd => {
                if lhs.ty != rhs.ty {
                    // TODO: char -> int
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some("incompatible type on bit wise and is not allowed".to_string()),
                    ));
                }
                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(op, lhs, rhs, lhs_ty, pos))
            }
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
                        Some("incompatible type take remainder is not allowed".to_string()),
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

    pub fn traverse_unary(
        &mut self,
        unary_op: &UnaryOp,
        operand: Box<Expr>,
        pos: Position,
    ) -> Result<ConvExpr, CompileError> {
        match unary_op {
            UnaryOp::Plus => self.traverse_expr(*operand, BTreeSet::new()),
            UnaryOp::Minus => self.traverse_binary(
                Binary::new(BinOpKind::Sub, Box::new(Expr::new_num(0, pos)), operand),
                pos,
            ),
            UnaryOp::BitInvert => {
                let mut operand = self.traverse_expr(*operand, BTreeSet::new())?;
                if operand.ty != Type::Base(BaseType::Int) {
                    if let Some(base_ty) = operand.ty.clone().get_base() {
                        operand = ConvExpr::new_cast(
                            operand,
                            Type::Base(BaseType::Int),
                            CastKind::Base2Base(*base_ty, BaseType::Int),
                        );
                    } else {
                        return Err(CompileError::new_type_error_types(
                            self.input,
                            operand.pos,
                            operand.pos,
                            operand.ty,
                            Type::Base(BaseType::Int),
                            Some("Type Conversion failed at BitInvert `~`."),
                        ));
                    }
                }
                Ok(ConvExpr::new_unary(ConvUnaryOp::BitInvert, operand))
            }
            UnaryOp::LogicalNot => {
                let eq_with_0 =
                    Binary::new(BinOpKind::Eq, operand, Box::new(Expr::new_num(0, pos)));
                Ok(self.traverse_binary(eq_with_0, pos)?)
            }
        }
    }

    pub fn new_init_expr(
        &mut self,
        init: Initializer,
        lvar: LVar,
        ty: Type,
        attrs: BTreeSet<DownExprAttribute>,
        pos: Position,
    ) -> Result<ConvExpr, CompileError> {
        match init {
            Initializer::Expr(init) => {
                let rhs = self.traverse_expr(init, attrs)?;
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

    pub fn get_type(
        &mut self,
        mut ty: Type,
        declarator: &Declarator,
    ) -> Result<Type, CompileError> {
        // let mut ty = ty_spec.try_into_with_analyzer(self)?;
        for _ in 0..declarator.n_star {
            ty = Type::Ptr(Box::new(ty));
        }
        let mut watching_direct_declarator = &declarator.direct_declarator;
        loop {
            match watching_direct_declarator {
                DirectDeclarator::Func(direct_declarator, args) => {
                    // e.g) int a(arg1: int, arg2: int*)(arg0: int) -> Func(Func(Int, vec![arg0]), vec![arg1, arg2])
                    ty = Type::Func(
                        Box::new(ty),
                        args.iter()
                            .map(|declaration| {
                                let converted_ty_spec = self.resolve_name_and_convert_to_type(
                                    &declaration.ty_spec,
                                    declaration.pos,
                                )?;
                                self.get_type(
                                    converted_ty_spec,
                                    &declaration
                                        .init_declarator
                                        .as_ref()
                                        .expect("get type for struct is not yet implemented.")
                                        .declarator,
                                )
                            })
                            .collect::<Result<Vec<_>, CompileError>>()?,
                    );
                    watching_direct_declarator = direct_declarator;
                }
                DirectDeclarator::Array(direct_declarator, expr) => {
                    #[allow(clippy::cast_sign_loss)]
                    let size = ConstExpr::try_eval_as_const(
                        self.input,
                        // TODO: not unwrap, but check has init or not.
                        self.traverse_expr(expr.clone().unwrap(), BTreeSet::new())?,
                    )?
                    .get_num_lit()? as usize;
                    ty = Type::Array(Box::new(ty), size);
                    watching_direct_declarator = direct_declarator;
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

    pub fn fetch_lvar(&self, name: &str, pos: Position) -> Result<ConvExpr, CompileError> {
        let var = match self.scope.look_up(&name.to_string()) {
            Some(lvar) => lvar,
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
        Ok(match var {
            Var::GVar(global) => ConvExpr::new_gvar(global, pos),
            Var::LVar(local) => {
                let ty = local.ty.clone();
                ConvExpr::new_lvar_raw(local, ty, pos)
            }
        })
    }

    /// create first defined variable
    pub fn new_lvar(
        src: &str,
        name: String,
        pos: Position,
        new_offset: &mut usize,
        ty: Type,
        lvar_map: &mut BTreeMap<String, LVar>,
    ) -> Result<LVar, CompileError> {
        let offset = if lvar_map.get(&name).is_some() {
            return Err(CompileError::new_redefined_variable(
                src,
                name,
                pos,
                VariableKind::Local,
            ));
        } else {
            *new_offset += ty.size_of();
            lvar_map.insert(name, LVar::new_raw(*new_offset, ty.clone()));
            *new_offset
        };
        Ok(LVar::new_raw(offset, ty))
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
    Global(GVar),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvFuncDef {
    pub ty: Type,
    pub name: String,
    pub args: Vec<LVar>,
    pub body: ConvStmt,
    pub stack_size: usize,
}

impl ConvFuncDef {
    pub fn new(ty: Type, name: String, args: Vec<LVar>, body: ConvStmt, stack_size: usize) -> Self {
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
pub enum ConvStmt {
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

impl ConvStmt {
    pub const fn new_expr(expr: ConvExpr) -> Self {
        ConvStmt::Expr(expr)
    }

    pub const fn new_ret(expr: ConvExpr, name: String) -> Self {
        ConvStmt::Return(expr, name)
    }

    pub fn new_block(stmts: Vec<ConvStmt>) -> Self {
        ConvStmt::Block(stmts)
    }

    pub fn new_if(cond: ConvExpr, then: ConvStmt, els: Option<ConvStmt>) -> Self {
        ConvStmt::If(cond, Box::new(then), els.map(Box::new))
    }

    pub fn new_while(cond: ConvExpr, then: ConvStmt) -> Self {
        ConvStmt::While(cond, Box::new(then))
    }

    pub fn new_for(
        init: Option<ConvExpr>,
        cond: Option<ConvExpr>,
        inc: Option<ConvExpr>,
        then: ConvStmt,
    ) -> Self {
        ConvStmt::For(init, cond, inc, Box::new(then))
    }
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
    #[must_use]
    pub fn convert_array_to_ptr(mut self) -> Self {
        if let Type::Array(base_ty, _) = self.ty.clone() {
            self.ty = *base_ty;
        } else {
            return self;
        };
        let pos = self.pos;
        ConvExpr::new_addr(self, pos)
    }

    pub fn implicit_cast(
        self,
        src: &str,
        ty: &Type,
        ctx: CastContext,
    ) -> Result<Self, CompileError> {
        match (ctx, self.ty.get_base(), ty.get_base()) {
            (CastContext::Assign, Some(from_base), Some(to_base)) if from_base == to_base => {
                // No Cast
                Ok(self)
            }
            (CastContext::Assign, Some(from_base), Some(to_base)) if from_base > to_base => {
                // Castable
                let (from_base, to_base) = (*from_base, *to_base);
                Ok(ConvExpr::new_cast(
                    self,
                    Type::Base(to_base),
                    CastKind::Base2Base(from_base, to_base),
                ))
            }
            _ => Err(unimplemented_err!(
                src,
                self.pos,
                format!("Implicit Cast Failed.: {:?} -> {:?}", self.ty, ty)
            )),
        }
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

    pub fn new_unary(unary_op: ConvUnaryOp, expr: ConvExpr) -> Self {
        let ty = expr.ty.clone();
        let pos = expr.pos;
        Self {
            kind: ConvExprKind::Unary(unary_op, Box::new(expr)),
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

    pub const fn new_lvar_raw(lvar: LVar, ty: Type, pos: Position) -> Self {
        ConvExpr {
            kind: ConvExprKind::LVar(lvar),
            ty,
            pos,
        }
    }

    pub fn new_gvar(gvar: GVar, pos: Position) -> Self {
        let ty = gvar.ty.clone();
        ConvExpr {
            kind: ConvExprKind::GVar(gvar),
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

    pub fn new_cast(expr: ConvExpr, to_ty: Type, kind: CastKind) -> Self {
        let pos = expr.pos;
        Self {
            kind: ConvExprKind::Cast(Box::new(expr), kind),
            ty: to_ty,
            pos,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum CastContext {
    Assign,
    // Binary,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvExprKind {
    Binary(ConvBinary),
    Unary(ConvUnaryOp, Box<ConvExpr>),
    Num(isize),
    LVar(LVar),
    GVar(GVar),
    Assign(Box<ConvExpr>, Box<ConvExpr>),
    Func(String, Vec<ConvExpr>),
    Deref(Box<ConvExpr>),
    Addr(Box<ConvExpr>),
    Cast(Box<ConvExpr>, CastKind),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ConvUnaryOp {
    BitInvert,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum CastKind {
    Base2Base(BaseType, BaseType),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct LVar {
    /// Used like `mov rax, [rbp - offset]`
    pub offset: usize,
    pub ty: Type,
}

impl LVar {
    pub const fn new_raw(offset: usize, ty: Type) -> Self {
        Self { offset, ty }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Scope {
    global: BTreeMap<String, GVar>,
    pub scopes: Vec<BTreeMap<String, LVar>>,
    max_stack_size: usize,
    diff: Vec<usize>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            global: BTreeMap::new(),
            scopes: Vec::new(),
            max_stack_size: 0,
            diff: Vec::new(),
        }
    }
    pub fn push_scope(&mut self) {
        self.diff.push(0);
        self.scopes.push(BTreeMap::new());
    }

    pub fn pop_scope(&mut self, offset: &mut usize) {
        assert!(!self.scopes.is_empty());
        self.scopes.pop();
        *offset -= self.diff.pop().expect("diff has to exist.");
    }

    pub const fn get_stack_size(&self) -> usize {
        self.max_stack_size
    }

    pub fn reset_stack(&mut self) {
        assert!(self.scopes.is_empty());
        self.max_stack_size = 0;
    }

    pub fn look_up(&self, name: &String) -> Option<Var> {
        for map in self.scopes.iter().rev() {
            if let Some(lvar) = map.get(name) {
                return Some(Var::LVar(lvar.clone()));
            }
        }
        self.global.get(name).map(|gvar| Var::GVar(gvar.clone()))
    }

    fn insert_lvar_to_current_scope(&mut self, name: String, lvar: LVar) {
        self.scopes.last_mut().map_or_else(
            || {
                unreachable!(
                    "Unreachable. scope stacks have to have local scope when you register lvar."
                );
            },
            |map| {
                map.insert(name, lvar);
            },
        );
    }

    fn insert_global_var_to_global_scope(&mut self, gvar: GVar) {
        self.global.insert(gvar.name.clone(), gvar);
    }

    pub fn register_gvar(
        &mut self,
        src: &str,
        pos: Position,
        name: &str,
        ty: Type,
        init: Option<ConstInitializer>,
    ) -> Result<GVar, CompileError> {
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
        let gvar = GVar {
            name: name.to_string(),
            ty,
            init,
        };
        self.insert_global_var_to_global_scope(gvar.clone());
        Ok(gvar)
    }

    pub fn register_lvar(
        &mut self,
        src: &str,
        pos: Position,
        new_offset: &mut usize,
        name: &str,
        ty: Type,
    ) -> Result<LVar, CompileError> {
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
        *self.diff.last_mut().expect("this has to exist.") +=
            aligned_offset(*new_offset, &ty) - *new_offset;
        *new_offset = aligned_offset(*new_offset, &ty);
        let lvar = LVar::new_raw(*new_offset, ty);
        self.insert_lvar_to_current_scope(name.to_string(), lvar.clone());
        self.max_stack_size = usize::max(self.max_stack_size, *new_offset);
        Ok(lvar)
    }
}

/// calculate aligned next offset
pub fn aligned_offset(current_offset: usize, ty: &Type) -> usize {
    if (current_offset + ty.size_of()) % ty.align_of() == 0 {
        return current_offset + ty.size_of();
    }
    current_offset + ty.size_of() + ty.align_of() - (current_offset + ty.size_of()) % ty.align_of()
}

pub const fn align_to(current_offset: usize, alignment: usize) -> usize {
    if current_offset % alignment == 0 {
        current_offset
    } else {
        current_offset + alignment - current_offset % alignment
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Taged {
    Struct(Struct),
}

impl Taged {
    pub fn new_struct_tag(names: Vec<String>, types: Vec<Type>) -> Self {
        Self::Struct(Struct::new(names, types))
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Struct {
    members: Vec<StructMember>,
}

impl Struct {
    pub fn new(names: Vec<String>, types: Vec<Type>) -> Self {
        let mut struct_members = Vec::with_capacity(names.len());
        let mut offset = 0;
        for (name, ty) in names.into_iter().zip(types.into_iter()) {
            dbg!(offset, &ty);
            offset = aligned_offset(offset, &ty);
            struct_members.push(StructMember { name, offset, ty });
        }
        dbg!(offset);
        Self {
            members: struct_members,
        }
    }

    pub fn size_of(&self) -> usize {
        align_to(
            self.members
                .last()
                .expect("struct should have at least one member.")
                .offset,
            self.align_of(),
        )
    }

    pub fn align_of(&self) -> usize {
        self.members
            .iter()
            .map(|struct_member| struct_member.ty.align_of())
            .max()
            .expect("struct should have at least one member.")
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct StructMember {
    name: String,
    offset: usize,
    ty: Type,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct GVar {
    pub name: String,
    pub ty: Type,
    pub init: Option<ConstInitializer>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Var {
    GVar(GVar),
    LVar(LVar),
}

impl Var {
    pub fn ty(&self) -> Type {
        match self {
            Var::GVar(gvar) => gvar.ty.clone(),
            Var::LVar(lvar) => lvar.ty.clone(),
        }
    }
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
pub enum ConstInitializer {
    Expr(ConstExpr),
    Array(Vec<ConstExpr>),
}

impl ConstInitializer {
    #[must_use]
    pub fn map_ty(self, f: impl Fn(Type) -> Type) -> Self {
        match self {
            ConstInitializer::Expr(expr) => ConstInitializer::Expr(expr.map_ty(f)),
            ConstInitializer::Array(exprs) => {
                ConstInitializer::Array(exprs.into_iter().map(|expr| expr.map_ty(&f)).collect())
            }
        }
    }

    pub fn get_num_lit(&self) -> Option<isize> {
        match self {
            ConstInitializer::Expr(expr) => expr.get_num_lit().ok(),
            ConstInitializer::Array(_) => None,
        }
    }

    /// When Initializer is Array return None, when Expr return displayed literal (or variable name of global variable)
    pub fn display_content(&self) -> Option<String> {
        match self {
            ConstInitializer::Expr(expr) => Some(expr.display_literal()),
            ConstInitializer::Array(_) => None,
        }
    }

    pub fn get_pos(&self) -> Position {
        match self {
            ConstInitializer::Expr(expr) => expr.pos,
            ConstInitializer::Array(vec) => vec.first().map(|expr| expr.pos).unwrap_or_default(),
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct ConstExpr {
    pub kind: ConstExprKind,
    pub ty: Type,
    pub pos: Position,
}

impl ConstExpr {
    pub const fn new_int(n: i32, pos: Position) -> Self {
        Self {
            kind: ConstExprKind::Int(n),
            ty: Type::Base(BaseType::Int),
            pos,
        }
    }

    pub const fn new_char(n: i8, pos: Position) -> Self {
        Self {
            kind: ConstExprKind::Char(n),
            ty: Type::Base(BaseType::Int),
            pos,
        }
    }

    pub fn display_literal(&self) -> String {
        match &self.kind {
            ConstExprKind::Int(n) => n.to_string(),
            ConstExprKind::Char(n) => n.to_string(),
            ConstExprKind::Addr(gvar) => gvar.name.clone(),
        }
    }

    pub fn get_num_lit(&self) -> Result<isize, CompileError> {
        Ok(match &self.kind {
            ConstExprKind::Int(n) => *n as isize,
            ConstExprKind::Char(n) => *n as isize,
            ConstExprKind::Addr(_) => {
                return Err(unimplemented_err!(format!(
                    "This const Expr {:?} should be Literal.",
                    self
                )))
            }
        })
    }

    pub fn apply_unary_op(&self, unary_op: &ConvUnaryOp) -> Result<ConstExpr, CompileError> {
        let pos = self.pos;
        Ok(match unary_op {
            ConvUnaryOp::BitInvert => match &self.kind {
                ConstExprKind::Int(n) => ConstExpr::new_int(!*n, pos),
                ConstExprKind::Char(n) => ConstExpr::new_char(!*n, pos),
                ConstExprKind::Addr(_) => {
                    return Err(unimplemented_err!(format!(
                        "Const Expr which will be applied `~` unary op ({:?}) should be Literal.",
                        self
                    )))
                }
            },
        })
    }

    #[must_use]
    pub fn map_ty(mut self, f: impl FnOnce(Type) -> Type) -> Self {
        self.ty = f(self.ty);
        self
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ConstExprKind {
    Int(i32),
    Char(i8),
    Addr(Box<GVar>),
}

impl ConstExpr {
    #[allow(clippy::too_many_lines)]
    fn try_eval_as_const(src: &str, expr: ConvExpr) -> Result<Self, CompileError> {
        let pos = expr.pos;
        let ty = expr.ty.clone();
        let kind = expr.kind;
        let bool_to_isize = |b| if b { 1 } else { 0 };
        let num_expr = |num: isize| ConstExpr {
            kind: ConstExprKind::Int(num as i32),
            ty,
            pos,
        };
        Ok(match kind {
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Add,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    + Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Sub,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    - Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Mul,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    * Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Div,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    / Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Rem,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    % Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Eq,
                lhs,
                rhs,
            }) => num_expr(bool_to_isize(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    == Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            )),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Ne,
                lhs,
                rhs,
            }) => num_expr(bool_to_isize(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    != Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            )),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Lt,
                lhs,
                rhs,
            }) => num_expr(bool_to_isize(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    < Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            )),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Le,
                lhs,
                rhs,
            }) => num_expr(bool_to_isize(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    <= Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            )),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::LShift,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    << Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::RShift,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    >> Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::BitWiseAnd,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(src, *lhs)?.get_num_lit()?
                    & Self::try_eval_as_const(src, *rhs)?.get_num_lit()?,
            ),
            ConvExprKind::Num(num) => num_expr(num),
            ConvExprKind::LVar(_)
            | ConvExprKind::GVar(_)
            | ConvExprKind::Assign(_, _)
            | ConvExprKind::Func(_, _)
            | ConvExprKind::Deref(_)
            | ConvExprKind::Cast(_, _) => {
                return Err(CompileError::new_const_expr_error(src, pos, kind))
            }
            ConvExprKind::Addr(expr) => {
                let expr_pos = expr.pos;
                let expr_ty = expr.ty.clone();
                let gvar = if let ConvExprKind::GVar(gvar) = expr.kind {
                    gvar
                } else {
                    return Err(CompileError::new_const_expr_error(src, pos, expr.kind));
                };
                ConstExpr {
                    kind: ConstExprKind::Addr(Box::new(gvar)),
                    ty: Type::Ptr(Box::new(expr_ty)),
                    pos: expr_pos,
                }
            }
            ConvExprKind::Unary(unary_op, expr) => {
                Self::try_eval_as_const(src, *expr)?.apply_unary_op(&unary_op)?
            }
        })
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Base(BaseType),
    Ptr(Box<Type>),
    Func(Box<Type>, Vec<Type>),
    Array(Box<Type>, usize),
    Struct(Struct),
}

impl Type {
    pub fn ptr(ty: Type) -> Self {
        Type::Ptr(Box::new(ty))
    }
}

impl<'a> Analyzer<'a> {
    pub fn resolve_name_and_convert_to_type(
        &mut self,
        ty_spec: &TypeSpec,
        pos: Position,
    ) -> Result<Type, CompileError> {
        Ok(match ty_spec {
            TypeSpec::Int => Type::Base(BaseType::Int),
            TypeSpec::Char => Type::Base(BaseType::Char),
            TypeSpec::StructOrUnion(StructOrUnionSpec::WithTag(tag)) => {
                let members = if let Some(Taged::Struct(Struct { members })) =
                    self.look_up_struct_tag(tag.as_str())
                {
                    members
                } else {
                    return Err(unimplemented_err!(
                        self.input,
                        pos,
                        "tag of struct declaration with should be declared before."
                    ));
                };
                Type::Struct(Struct {
                    members: members.clone(),
                })
            }
            TypeSpec::StructOrUnion(StructOrUnionSpec::WithList(name, vec)) => {
                let types = vec
                    .iter()
                    .map(|struct_declaration| {
                        struct_declaration.get_type(self, struct_declaration.pos)
                    })
                    .collect::<Result<_, CompileError>>()?;
                let names = vec
                    .iter()
                    .map(|struct_declaration| struct_declaration.ident_name().to_string())
                    .collect::<Vec<String>>();
                let struct_struct = Struct::new(names, types);
                // tag compatibility check
                if let Some(name) = name {
                    if let Some(Taged::Struct(Struct { members })) =
                        self.look_up_struct_tag(name.as_str())
                    {
                        if *members != struct_struct.members {
                            return Err(unimplemented_err!(self.input, pos, "this declaration's tag is incompatible with another tag whose tag-name is same."));
                        }
                    } else {
                        self.tag_scope.last_mut().expect(
                            "INTERNAL COMPILER ERROR. tag_scope should have at least one scope.",
                        ).insert(name.clone(), Taged::Struct(struct_struct.clone()));
                    }
                }

                Type::Struct(struct_struct)
            }
            _ => todo!(),
        })
    }
}

impl Type {
    pub fn size_of(&self) -> usize {
        match self {
            Type::Base(BaseType::Int) => 4,
            Type::Base(BaseType::Char) => 1,
            Type::Ptr(_) => 8,
            Type::Func(_, _) => 1,
            Type::Array(ty, size) => ty.size_of() * *size,
            Type::Struct(struct_struct) => struct_struct.size_of(),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Base(BaseType::Int) => 4,
            Type::Base(BaseType::Char) => 1,
            Type::Ptr(_) => 8,
            Type::Func(_, _) => todo!(),
            Type::Array(base_ty, _) => base_ty.align_of(),
            Type::Struct(struct_struct) => struct_struct.align_of(),
        }
    }

    pub const fn base_type(&self) -> &Type {
        match self {
            ty @ Type::Base(_) => ty,
            Type::Ptr(base) | Type::Func(base, _) | Type::Array(base, _) => base,
            Type::Struct(_) => todo!(),
        }
    }

    pub const fn get_base(&self) -> Option<&BaseType> {
        match self {
            Type::Base(base) => Some(base),
            Type::Ptr(_) | Type::Func(_, _) | Type::Array(_, _) => None,
            Type::Struct(_) => todo!(),
        }
    }

    pub const fn is_base(&self) -> bool {
        match self {
            Type::Base(_) => true,
            Type::Ptr(_) | Type::Func(_, _) | Type::Array(_, _) | Type::Struct(_) => false,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum BaseType {
    Int,
    Char,
}

impl BaseType {
    pub const fn bytes(&self) -> usize {
        match self {
            BaseType::Int => 4,
            BaseType::Char => 1,
        }
    }
}

impl From<&BaseType> for RegSize {
    fn from(base: &BaseType) -> Self {
        match base {
            BaseType::Char => RegSize::Byte,
            BaseType::Int => RegSize::Dword,
        }
    }
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
    /// The `<<` operator
    LShift,
    /// The `>>` operator
    RShift,
    /// The `&` operator
    BitWiseAnd,
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
            BinOpKind::LShift => Some(ConvBinOpKind::LShift),
            BinOpKind::RShift => Some(ConvBinOpKind::RShift),
            BinOpKind::BitWiseAnd => Some(ConvBinOpKind::BitWiseAnd),
        }
    }
}
