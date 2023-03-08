use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    fmt::Debug,
    vec,
};

use crate::{
    error::{AnalyzeErrorKind, CompileError, CompileErrorKind, VariableKind},
    generate::generate::RegSize,
    parse::parse::{
        BinOpKind, Binary, DeclarationSpecifier, Declarator, DirectDeclarator, EnumConstant,
        EnumSpec, Expr, ExprKind, ForInitKind, Initializer, LabelKind, Program, ProgramComponent,
        ProgramKind, SizeOfOperandKind, Stmt, StmtKind, StorageClassSpecifier, StructOrUnionSpec,
        TypeSpecifier, UnaryOp,
    },
    tokenize::{debug_infos::DebugInfo, tokenize::AssignBinOpToken},
    unimplemented_err,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Analyzer {
    offset: usize,
    func_map: BTreeMap<String, Func>,
    pub scope: Scope,
    pub conv_program: ConvProgram,
    lc_label: usize,
    loop_stack: LoopStack,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct TraverseStmtContext {
    fn_name: String,
}

impl TraverseStmtContext {
    pub fn new(fn_name: String) -> Self {
        Self { fn_name }
    }
    pub fn fn_name(&self) -> &str {
        &self.fn_name
    }
}

const VA_AREA_LEN: usize = 136;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LoopStack(Vec<LoopKind>);

impl LoopStack {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, kind: LoopKind) {
        self.0.push(kind);
    }

    pub fn pop(&mut self) -> Option<LoopKind> {
        self.0.pop()
    }

    pub fn register_case_label(&mut self, expr: &ConstExpr) -> Result<(), CompileError> {
        if let Some(LoopKind::Switch {
            ref mut cases,
            has_default: _,
        }) = self.0.last_mut()
        {
            cases.push(expr.get_num_lit()?);
            Ok(())
        } else {
            Err(CompileError::new_not_allowed_stmt_error(
                expr.debug_info.clone(),
                crate::error::NotAllowedStmtKind::Case,
            ))
        }
    }

    pub fn register_default(&mut self, debug_info: DebugInfo) -> Result<(), CompileError> {
        if let Some(LoopKind::Switch {
            cases: _,
            has_default: ref mut has_break,
        }) = self.0.last_mut()
        {
            *has_break = true;
            Ok(())
        } else {
            Err(CompileError::new_not_allowed_stmt_error(
                debug_info,
                crate::error::NotAllowedStmtKind::Default,
            ))
        }
    }

    pub fn allows_break(&self) -> bool {
        !self.0.is_empty()
    }

    pub fn allows_continue(&self) -> bool {
        self.0
            .iter()
            .any(|kind| matches!(kind, LoopKind::Switch { .. } | LoopKind::For))
    }

    pub fn allows_case(&self) -> bool {
        self.0
            .iter()
            .any(|kind| matches!(kind, LoopKind::Switch { .. }))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LoopKind {
    While,
    For,
    Switch {
        cases: Vec<isize>,
        has_default: bool,
    },
}

impl LoopKind {
    pub const fn new_switch() -> Self {
        Self::Switch {
            cases: vec![],
            has_default: false,
        }
    }
}

impl Analyzer {
    pub fn new() -> Self {
        let func_map: BTreeMap<String, Func> = BTreeMap::new();
        Self {
            offset: 0,
            func_map,
            scope: Scope::new(),
            conv_program: ConvProgram::new(),
            lc_label: 0,
            loop_stack: LoopStack::new(),
        }
    }

    pub fn new_for_parser() -> Self {
        let func_map: BTreeMap<String, Func> = BTreeMap::new();
        let mut analyzer = Self {
            offset: 0,
            func_map,
            scope: Scope::new(),
            conv_program: ConvProgram::new(),
            lc_label: 0,
            loop_stack: LoopStack::new(),
        };
        analyzer.scope.push_tag_scope();
        analyzer
    }

    pub fn get_lc_label(&mut self) -> usize {
        let lc_label = self.lc_label;
        self.lc_label += 1;
        lc_label
    }

    pub fn insert_declaration_as_gvar(&mut self, gvar: GVar) {
        self.conv_program.push_front(ConvProgramKind::Global(gvar));
    }

    #[allow(clippy::too_many_lines)]
    pub fn traverse_program(&mut self, program: Program) -> Result<ConvProgram, CompileError> {
        self.scope.push_tag_scope();
        for component in program {
            match component {
                ProgramComponent {
                    kind: ProgramKind::FuncDef(type_spec, declarator, body),

                    debug_info,
                } => {
                    assert_eq!(self.offset, 0);
                    let func_def =
                        self.traverse_func_def(&type_spec, &declarator, body, debug_info)?;
                    self.conv_program.push(func_def);
                    self.scope.reset_stack();
                }
                ProgramComponent {
                    kind: ProgramKind::InlineAsm(asm),
                    debug_info: _,
                } => {
                    self.conv_program.push(ConvProgramKind::InlineAsm(asm));
                }
                ProgramComponent {
                    kind: ProgramKind::Declaration(declaration),
                    debug_info,
                } => {
                    let mut is_typedef = false;
                    let mut is_extern = false;
                    let mut declaration_specifiers = Vec::new();
                    for specifier in &declaration.declaration_specifiers {
                        match specifier {
                            DeclarationSpecifier::StorageClass(StorageClassSpecifier::Extern) => {
                                is_extern = true;
                            }
                            DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef) => {
                                is_typedef = true;
                            }
                            specifier => declaration_specifiers.push(specifier.clone()),
                        }
                    }
                    if let Some(init_declarator) = declaration.init_declarator {
                        let name = init_declarator.ident_name();
                        let init = &init_declarator.initializer.as_ref();
                        let debug_info = declaration.debug_info.clone();
                        if !init_declarator.declarator.direct_declarator.is_func() {
                            self.register_struct_tag_from_type_specifier(
                                &declaration.declaration_specifiers,
                            )?;
                        }
                        let converted_type = self.resolve_name_and_convert_to_type(
                            &declaration.declaration_specifiers,
                            debug_info.clone(),
                        )?;
                        // register enum with init declarator
                        match &declaration_specifiers[0] {
                            DeclarationSpecifier::Type(TypeSpecifier::Enum(
                                EnumSpec::WithList(Some(name), vec),
                            )) => {
                                let enum_ident_map: BTreeMap<String, usize> = vec
                                    .into_iter()
                                    .map(
                                        |EnumConstant {
                                             ident,
                                             debug_info: _,
                                         }| ident,
                                    )
                                    .enumerate()
                                    .map(|(idx, ident)| (ident.clone(), idx))
                                    .collect();
                                self.scope.register_tag(
                                    name.to_string(),
                                    Taged::new_enum_tag(name.clone(), enum_ident_map),
                                );
                                // fall thorough here, because with init declarator it must be global int-typed variable
                            }
                            DeclarationSpecifier::Type(TypeSpecifier::Enum(
                                EnumSpec::WithList(None, vec),
                            )) => {
                                let enum_ident_map: BTreeMap<String, usize> = vec
                                    .into_iter()
                                    .map(
                                        |EnumConstant {
                                             ident,
                                             debug_info: _,
                                         }| ident,
                                    )
                                    .enumerate()
                                    .map(|(idx, ident)| (ident.clone(), idx))
                                    .collect();
                                self.scope.register_anonymous_enum_tag(enum_ident_map);
                            }
                            _ => {}
                        }
                        match self.get_type(converted_type, &init_declarator.declarator, None)? {
                            _ if is_typedef => {}
                            ty @ (Type::Base(_)
                            | Type::Ptr(_)
                            | Type::Array(_, _)
                            | Type::Struct(_)) => {
                                let gvar = self
                                    .new_global_variable(*init, name, ty, is_extern, debug_info)?;
                                self.conv_program.push(ConvProgramKind::Global(gvar));
                            }
                            Type::Func {
                                ret_ty,
                                args,
                                is_flexible,
                            } => {
                                if self.func_map.get(&name.to_string()).is_some() {
                                    // TODO: check if this definition has compatible signature.

                                    // redefined prototype is allowed.
                                    continue;
                                }
                                self.func_map.insert(
                                    name.to_string(),
                                    Func::new_raw(
                                        name.to_string(),
                                        BTreeMap::new(),
                                        args,
                                        is_flexible,
                                        *ret_ty,
                                        debug_info,
                                    ),
                                );
                            }
                            Type::Void => {
                                return Err(CompileError::new_unexpected_void(
                                    debug_info,
                                    "void type declaration found.".to_string(),
                                ));
                            }
                            Type::InComplete(InCompleteKind::Struct(tag_name)) => {
                                let ty = if let Some(Taged::Struct(StructTagKind::Struct(
                                    struct_struct,
                                ))) = self.scope.look_up_struct_tag(&tag_name)
                                {
                                    Type::Struct(struct_struct.clone())
                                } else {
                                    return Err(unimplemented_err!(
                                        debug_info,
                                        format!("struct tag {} is not defined.", tag_name)
                                    ));
                                };
                                let gvar = self
                                    .new_global_variable(*init, name, ty, is_extern, debug_info)?;
                                self.conv_program.push(ConvProgramKind::Global(gvar));
                            }
                            Type::InComplete(InCompleteKind::Enum(_)) => todo!(),
                        }
                    } else {
                        // struct or enum declaration

                        self.register_struct_tag_from_type_specifier(
                            &declaration.declaration_specifiers,
                        )?;
                        // TODO: check more than one type specifier
                        assert!(declaration.declaration_specifiers.len() == 1);
                        match declaration
                            .declaration_specifiers
                            .last()
                            .unwrap()
                            .get_type_specifier()
                            .unwrap()
                        {
                            TypeSpecifier::StructOrUnion(StructOrUnionSpec::WithList(
                                Some(name),
                                vec,
                            )) => {
                                let types = vec
                                    .iter()
                                    .map(|struct_declaration| {
                                        let mut ty = struct_declaration.get_type(
                                            self,
                                            struct_declaration.debug_info.clone(),
                                        )?;
                                        if let Type::InComplete(InCompleteKind::Struct(tag)) = &ty {
                                            let taged = self.scope.look_up_struct_tag(tag);
                                            if let Some(Taged::Struct(StructTagKind::Struct(
                                                struct_struct,
                                            ))) = taged
                                            {
                                                ty = Type::Struct(struct_struct.clone());
                                            }
                                        };
                                        Ok(ty)
                                    })
                                    .collect::<Result<_, CompileError>>()?;
                                let names = vec
                                    .iter()
                                    .map(|struct_declaration| {
                                        struct_declaration.ident_name().to_string()
                                    })
                                    .collect::<Vec<String>>();
                                self.scope.register_tag(
                                    name.clone(),
                                    Taged::new_struct_tag(name, names, types),
                                );
                            }
                            TypeSpecifier::Enum(EnumSpec::WithList(Some(name), vec)) => {
                                let enum_ident_map: BTreeMap<String, usize> = vec
                                    .into_iter()
                                    .map(
                                        |EnumConstant {
                                             ident,
                                             debug_info: _,
                                         }| ident,
                                    )
                                    .enumerate()
                                    .map(|(idx, ident)| (ident, idx))
                                    .collect();
                                self.scope.register_tag(
                                    name.to_string(),
                                    Taged::new_enum_tag(name, enum_ident_map),
                                );
                            }
                            _ => {
                                eprintln!("{:?}", &declaration.declaration_specifiers);
                                return Err(unimplemented_err!(
                                    debug_info,
                                    "Expected struct declaration with name and list."
                                ));
                            }
                        }
                    }
                }
            }
        }
        self.scope.pop_tag_scope();
        Ok(self.conv_program.clone())
    }

    /// Construct Global Variable and register it to `self.scope`
    pub fn new_global_variable(
        &mut self,
        init: Option<&Initializer>,
        name: &str,
        // global variable's type
        ty: Type,
        is_extern: bool,
        debug_info: DebugInfo,
    ) -> Result<GVar, CompileError> {
        let init = init
            .map(|expr| {
                let cast_to = match expr {
                    Initializer::Expr(_) => ty.clone(),
                    Initializer::Array(_) => ty.get_array_base_recursively().clone(),
                };
                expr.clone().map(&mut |expr| {
                    ConstExpr::try_eval_as_const(Self::new_cast_expr_with_type_check(
                        self.traverse_expr(expr, None, BTreeSet::new())?,
                        cast_to.clone(),
                    )?)
                })
            })
            .transpose()?;

        self.scope
            .register_gvar(debug_info, name, ty, is_extern, init)
    }

    #[allow(clippy::too_many_lines)]
    pub fn traverse_func_def(
        &mut self,
        ty_spec: &TypeSpecifier,
        declarator: &Declarator,
        body: Stmt,
        debug_info: DebugInfo,
    ) -> Result<ConvProgramKind, CompileError> {
        let mut lvars = Vec::new();
        let ident = declarator.direct_declarator.ident_name();
        let ty_spec = vec![DeclarationSpecifier::Type(ty_spec.clone())];
        let converted_type = self.resolve_name_and_convert_to_type(&ty_spec, debug_info.clone())?;
        let ty = self.get_type(converted_type, declarator, None)?;

        let (ret_ty, args_ty, is_flexible) = if let Type::Func {
            ret_ty: this_ret_ty,
            args: this_args,
            is_flexible,
        } = ty.clone()
        {
            (this_ret_ty, this_args, is_flexible)
        } else {
            return Err(CompileError::new_type_expect_failed_with_str(
                debug_info,
                "Type::Func(_)".to_string(),
                ty,
            ));
        };
        let args = if let Some(args) = declarator.direct_declarator.args() {
            args
        } else {
            return Err(unimplemented_err!(
                debug_info,
                "function declarator should not be `Ident(_)`"
            ));
        };
        self.scope.push_scope();
        let mut traverse_stmt_context = TraverseStmtContext::new(ident.to_string());
        let body = if let StmtKind::Block(stmts) = body.kind {
            let arg_n = args.len();
            let mut has_va_start = false;
            for stmt in &stmts {
                if matches!(
                    stmt.kind,
                    StmtKind::Expr(Expr {
                        kind: ExprKind::BuiltinVaStart(_, _),
                        debug_info: _
                    })
                ) {
                    has_va_start = true;
                    break;
                };
            }
            if is_flexible && has_va_start {
                self.scope.register_lvar(
                    debug_info.clone(),
                    &mut self.offset,
                    "__va_area__",
                    Type::Array(Box::new(Type::Base(BaseType::Char)), VA_AREA_LEN),
                )?;
                eprintln!("register `__va_area__` to {}", ident);
            };
            for arg in args {
                // register func args as lvar
                let converted_type = self.resolve_name_and_convert_to_type(
                    &arg.declaration_specifiers,
                    arg.debug_info.clone(),
                )?;
                let ty = self.get_type(
                    converted_type,
                    &arg.init_declarator
                        .as_ref()
                        .unwrap_or_else(|| todo!("struct"))
                        .declarator,
                    None,
                )?;
                let name = arg.ident_name();
                let name = name.as_ref().expect("struct");
                let lvar =
                    self.scope
                        .register_lvar(arg.debug_info.clone(), &mut self.offset, name, ty)?;
                lvars.push(lvar.clone());
            }
            self.func_map.insert(
                ident.to_string(),
                Func::new_raw(
                    ident.to_string(),
                    BTreeMap::new(),
                    args_ty,
                    is_flexible,
                    *ret_ty,
                    debug_info,
                ),
            );
            // Add VaStartInit
            let mut conv_stmts = if is_flexible && has_va_start {
                eprintln!("register `__va_area__` to {}", ident);
                vec![ConvStmt::VaStartInit { arg_n }]
            } else {
                Vec::new()
            };
            let conv_stmts_iter = stmts
                .into_iter()
                .map(|stmt| self.traverse_stmt(stmt, &mut traverse_stmt_context));
            for conv_stmt in conv_stmts_iter {
                conv_stmts.push(conv_stmt?);
            }
            ConvStmt::new_block(conv_stmts)
        } else {
            return Err(unimplemented_err!(
                debug_info,
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
    pub fn traverse_stmt(
        &mut self,
        stmt: Stmt,
        ctx: &mut TraverseStmtContext,
    ) -> Result<ConvStmt, CompileError> {
        Ok(match stmt.kind {
            StmtKind::Expr(expr) => ConvStmt::new_expr(self.traverse_expr(
                expr,
                Some(&ctx.fn_name().to_string()),
                BTreeSet::new(),
            )?),
            StmtKind::Return(expr) => ConvStmt::new_ret(
                expr.map(|expr| {
                    self.traverse_expr(expr, Some(&ctx.fn_name().to_string()), BTreeSet::new())
                })
                .transpose()?,
                ctx.fn_name().to_string(),
            ),
            StmtKind::If(cond, then, els) => ConvStmt::new_if(
                self.traverse_expr(cond, Some(&ctx.fn_name().to_string()), BTreeSet::new())?,
                self.traverse_stmt(*then, ctx)?,
                els.map(|stmt| self.traverse_stmt(*stmt, ctx)).transpose()?,
            ),
            StmtKind::While(cond, then) => ConvStmt::new_while(
                self.traverse_expr(cond, Some(&ctx.fn_name().to_string()), BTreeSet::new())?,
                self.traverse_stmt(*then, ctx)?,
            ),
            StmtKind::For(init, cond, inc, then) => {
                self.scope.push_scope();
                let for_stmt = ConvStmt::new_for(
                    init.map_or(Ok(None), |expr| match expr {
                        ForInitKind::Expr(expr) => self
                            .traverse_expr(expr, Some(&ctx.fn_name().to_string()), BTreeSet::new())
                            .map(Some),
                        ForInitKind::Declaration(declaration) => {
                            let ty = declaration.ty(self, declaration.debug_info.clone())?;
                            let lvar = self.scope.register_lvar(
                                declaration.debug_info.clone(),
                                &mut self.offset,
                                declaration.ident_name().unwrap_or_else(|| todo!("struct")),
                                ty.clone(),
                            )?;
                            // lvar_map.insert(declaration.ident_name().to_string(), lvar.clone());
                            if let Some(init) = declaration
                                .init_declarator
                                .and_then(|init_declarator| init_declarator.initializer)
                            {
                                Ok(Some(self.new_init_expr(
                                    init,
                                    lvar,
                                    ty,
                                    Some(&ctx.fn_name().to_string()),
                                    BTreeSet::new(),
                                    declaration.debug_info,
                                )?))
                            } else {
                                Ok(None)
                            }
                        }
                    })?,
                    cond.map(|expr| {
                        self.traverse_expr(expr, Some(&ctx.fn_name().to_string()), BTreeSet::new())
                    })
                    .transpose()?,
                    inc.map(|expr| {
                        self.traverse_expr(expr, Some(&ctx.fn_name().to_string()), BTreeSet::new())
                    })
                    .transpose()?,
                    self.traverse_stmt(*then, ctx)?,
                );
                self.scope.pop_scope(&mut self.offset);
                for_stmt
            }
            StmtKind::Block(stmts) => {
                self.scope.push_scope();
                let block = ConvStmt::new_block(
                    stmts
                        .into_iter()
                        .map(|stmt| self.traverse_stmt(stmt, ctx))
                        .collect::<Result<Vec<_>, CompileError>>()?,
                );
                self.scope.pop_scope(&mut self.offset);
                block
            }
            StmtKind::Declare(declaration) => {
                let mut is_typedef = false;
                let mut is_static = false;
                for declaration_specifier in &declaration.declaration_specifiers {
                    match declaration_specifier {
                        DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef) => {
                            is_typedef = true
                        }
                        DeclarationSpecifier::StorageClass(StorageClassSpecifier::Static) => {
                            is_static = true
                        }
                        DeclarationSpecifier::StorageClass(specifier) => {
                            return Err(unimplemented_err!(
                                declaration.debug_info.clone(),
                                format!("processing {:?} is not yet implemented.", specifier)
                            ))
                        }
                        DeclarationSpecifier::Type(_) => {}
                    }
                }
                // — void
                // — char
                // — signed char
                // — unsigned char
                // — short, signed short, short int, or signed short int
                // — unsigned short, or unsigned short int
                // — int, signed, or signed int
                // — unsigned, or unsigned int
                // — long, signed long, long int, or signed long int
                // — unsigned long, or unsigned long int
                // — long long, signed long long, long long int, or signed long long int
                // — unsigned long long, or unsigned long long int
                // — float
                // — double
                // — long double
                // — _Bool
                // — float _Complex
                // — double _Complex
                // — long double _Complex
                // — atomic type specifier
                // — struct or union specifier
                // — enum specifier
                // — typedef name
                if declaration.init_declarator.is_none() || is_typedef {
                    // TODO: check more than one type-specifier possibility
                    // let mut declaration_specifiers =
                    //     Vec::with_capacity(declaration.declaration_specifiers.len());
                    // for declaration_specifier in &declaration.declaration_specifiers {
                    //     if DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef)
                    //         != *declaration_specifier
                    //     {
                    //         declaration_specifiers.push(declaration_specifier.clone());
                    //     }
                    // }
                    let declaration_specifiers = declaration
                        .declaration_specifiers
                        .iter()
                        .filter(|specifier| {
                            !matches!(
                                specifier,
                                DeclarationSpecifier::StorageClass(
                                    StorageClassSpecifier::Typedef | StorageClassSpecifier::Static
                                )
                            )
                        })
                        .collect::<Vec<_>>();
                    if declaration_specifiers.len() == 1 {
                        match declaration_specifiers
                            .last()
                            .unwrap()
                            .get_type_specifier()
                            .unwrap()
                        {
                            _ if is_static => return Err(unimplemented_err!(declaration.debug_info, "struct or enum declaration in function with static is not currently implemented.")),
                            TypeSpecifier::StructOrUnion(StructOrUnionSpec::WithList(
                                Some(name),
                                vec,
                            )) => {
                                self.scope.register_tag(
                                    name.to_string(),
                                    Taged::Struct(StructTagKind::OnlyTag(name.to_string())),
                                );
                                let types = vec
                                    .iter()
                                    .map(|struct_declaration| {
                                        struct_declaration
                                            .get_type(self, struct_declaration.debug_info.clone())
                                    })
                                    .collect::<Result<_, CompileError>>()?;
                                let names = vec
                                    .iter()
                                    .map(|struct_declaration| {
                                        struct_declaration.ident_name().to_string()
                                    })
                                    .collect::<Vec<String>>();
                                self.scope.register_tag(
                                    name.clone(),
                                    Taged::new_struct_tag(name, names, types),
                                );
                                return Ok(ConvStmt::new_block(vec![]));
                            }
                            TypeSpecifier::Enum(EnumSpec::WithList(
                                Some(ref tag),
                                ref variants,
                            )) => {
                                let enum_ident_map: BTreeMap<String, usize> = variants
                                    .clone()
                                    .into_iter()
                                    .map(
                                        |EnumConstant {
                                             ident,
                                             debug_info: _,
                                         }| ident,
                                    )
                                    .enumerate()
                                    .map(|(idx, ident)| (ident, idx))
                                    .collect();
                                self.scope.register_tag(
                                    tag.to_string(),
                                    Taged::new_enum_tag(tag.clone(), enum_ident_map),
                                );
                                return Ok(ConvStmt::new_block(vec![]));
                            }
                            TypeSpecifier::Enum(EnumSpec::WithList(None, ref variants))
                                if is_typedef =>
                            {
                                let enum_ident_map: BTreeMap<String, usize> = variants
                                    .clone()
                                    .into_iter()
                                    .map(
                                        |EnumConstant {
                                             ident,
                                             debug_info: _,
                                         }| ident,
                                    )
                                    .enumerate()
                                    .map(|(idx, ident)| (ident, idx))
                                    .collect();
                                self.scope.register_anonymous_enum_tag(enum_ident_map);
                                return Ok(ConvStmt::new_block(vec![]));
                            }
                            TypeSpecifier::StructOrUnion(_) if !is_typedef => {
                                return Err(unimplemented_err!(
                                    declaration.debug_info,
                                    "Expected struct or enum declaration with name and list."
                                ))
                            }
                            TypeSpecifier::Enum(EnumSpec::WithTag(_)) if !is_typedef => {
                                return Err(unimplemented_err!(
                                    declaration.debug_info,
                                    "Expected enum declaration with name and list."
                                ))
                            }
                            _ if is_typedef => {
                                return Ok(ConvStmt::new_block(vec![]));
                            }
                            _ => {
                                return Err(unimplemented_err!(
                                    declaration.debug_info,
                                    "Not struct declaration has to have init declarator."
                                ))
                            }
                        };
                    } else {
                        return Err(unimplemented_err!(
                            declaration.debug_info,
                            format!(
                                "more than one type-specifier is not currently supported.\n{:?}",
                                declaration_specifiers
                            )
                        ));
                    }
                }
                if is_typedef {
                    return Ok(ConvStmt::new_block(Vec::new()));
                }

                let ty = declaration.ty(self, declaration.debug_info.clone())?;
                // TODO check: Function pointer declaration is allowed here (or not)?
                let name = declaration.ident_name().unwrap_or_else(|| todo!("struct"));
                let ty =
                    self.resolve_incomplete_type_second_depth(ty, declaration.debug_info.clone())?;

                if is_static {
                    let debug_info = declaration.debug_info.clone();
                    let default_initializer = || {
                        Ok(match &ty {
                            Type::Base(base_ty) => ConstInitializer::Expr(
                                ConstExpr::new_literal_with_type(0, *base_ty, debug_info.clone()),
                            ),
                            ty @ Type::Array(..) => ConstInitializer::Array(vec![
                                    ConstInitializer::Expr(ConstExpr::new_char(
                                        0,
                                        debug_info.clone(),
                                    ));
                                    ty.size_of()
                                ]),
                            Type::Ptr(_) => {
                                ConstInitializer::Expr(ConstExpr::null_ptr(debug_info.clone()))
                            }
                            ty => {
                                return Err(unimplemented_err!(
                                    debug_info.clone(),
                                    format!(
                                    "{:?} type static variable in function is not yet implemented.",
                                    ty
                                )
                                ))
                            }
                        })
                    };
                    let initializer: ConstInitializer = declaration
                        .init_declarator
                        .clone()
                        .and_then(|init_initializer| init_initializer.initializer)
                        .map_or_else(default_initializer, |initializer| {
                            ConstInitializer::try_new(self, initializer)
                        })?;

                    let gvar = self.insert_static_func_variable(
                        name.to_string(),
                        ty,
                        Some(initializer),
                        &ctx.fn_name().to_string(),
                        &debug_info,
                    )?;
                    self.insert_declaration_as_gvar(gvar);
                    ConvStmt::new_block(Vec::new())
                } else {
                    let lvar = self.scope.register_lvar(
                        declaration.debug_info.clone(),
                        &mut self.offset,
                        name,
                        ty,
                    )?;

                    match declaration
                        .init_declarator
                        .and_then(|init_declarator| init_declarator.initializer)
                    {
                        Some(init @ (Initializer::Array(_) | Initializer::Expr(_))) => {
                            let debug_info = declaration.debug_info.clone();
                            let left_ty = lvar.ty.clone();
                            init.gen_assign_exprs_stmt(self, lvar, &left_ty, debug_info)?
                        }
                        // just declaration does nothing
                        None => ConvStmt::new_block(vec![]),
                    }
                }
            }
            StmtKind::Switch(expr, stmt) => {
                let debug_info = expr.debug_info.clone();
                self.loop_stack.push(LoopKind::new_switch());
                let conv_expr =
                    self.traverse_expr(expr, Some(&ctx.fn_name().to_string()), BTreeSet::new())?;
                let conv_stmt = self.traverse_stmt(*stmt, ctx)?;
                let switch = self.loop_stack.pop();
                if let Some(LoopKind::Switch { cases, has_default }) = switch {
                    ConvStmt::new_switch(conv_expr, cases, conv_stmt, has_default)
                } else {
                    return Err(unimplemented_err!(
                        debug_info,
                        "for or while statement over switch statement."
                    ));
                }
            }
            StmtKind::Labeled(LabelKind::Case(expr), stmt) => {
                let const_expr = ConstExpr::try_eval_as_const(self.traverse_expr(
                    expr,
                    Some(&ctx.fn_name().to_string()),
                    BTreeSet::new(),
                )?)?;
                let stmt = self.traverse_stmt(*stmt, ctx)?;
                self.loop_stack.register_case_label(&const_expr)?;
                ConvStmt::LoopControl(LoopControlKind::Case(
                    const_expr.get_num_lit()?,
                    Box::new(stmt),
                ))
            }
            StmtKind::Labeled(LabelKind::Ident(_), _) => todo!(),
            StmtKind::Labeled(LabelKind::Default(debug_info), stmt) => {
                self.loop_stack.register_default(debug_info)?;
                ConvStmt::LoopControl(LoopControlKind::Default(Box::new(
                    self.traverse_stmt(*stmt, ctx)?,
                )))
            }
            StmtKind::Break => ConvStmt::LoopControl(LoopControlKind::Break),
            StmtKind::Continue => ConvStmt::LoopControl(LoopControlKind::Continue),
        })
    }

    pub fn new_cast_expr_with_type_check(
        expr: ConvExpr,
        cast_to: Type,
    ) -> Result<ConvExpr, CompileError> {
        if expr.ty.ty_eq(&cast_to) {
            return Ok(expr);
        }
        let expr_ty = expr.ty.clone();
        let expr_ptr_to = expr_ty.get_ptr_to();
        let cast_to_ptr_to = cast_to.get_ptr_to();
        if expr_ty.is_void_ptr() && cast_to_ptr_to.is_some() {
            // Safety: `rhs_ptr_to.is_some()` is true on this branch
            let ptr_to = unsafe { cast_to_ptr_to.unwrap_unchecked() }.clone();
            let expr = ConvExpr::new_cast(
                expr,
                Type::Ptr(Box::new(Type::Void)),
                CastKind::ToVoidPtr { ptr_to },
            );
            return Ok(expr);
        } else if expr_ptr_to.is_some() && cast_to.is_void_ptr() {
            let expr = ConvExpr::new_cast(
                expr,
                Type::Ptr(Box::new(Type::Void)),
                CastKind::FromVoidPtr {
                    ptr_to: expr_ptr_to.unwrap().clone(),
                },
            );
            return Ok(expr);
        }
        if let (Some(lhs_base), Some(rhs_base)) = (expr_ty.clone().get_base(), cast_to.get_base()) {
            let rhs_base = *rhs_base;
            let expr = ConvExpr::new_cast(expr, expr_ty, CastKind::Base2Base(rhs_base, *lhs_base));
            Ok(expr)
        } else {
            let debug_info0 = expr.debug_info.clone();
            let debug_info1 = debug_info0.clone();
            Err(CompileError::new_type_error_types(
                debug_info0,
                debug_info1,
                expr.ty,
                cast_to,
                Some("Trying to cast, but have incompatible types"),
            ))
        }
    }

    pub fn new_assign_expr_with_type_check(
        lhs: ConvExpr,
        rhs: ConvExpr,
        assign_bin_op: AssignBinOpToken,
        debug_info: DebugInfo,
    ) -> Result<ConvExpr, CompileError> {
        let mut rhs = rhs;
        match assign_bin_op {
            AssignBinOpToken::Eq => {
                match (lhs.ty.clone(), rhs.ty.clone()) {
                    (
                        Type::Struct(Struct { tag: _, members }),
                        Type::Struct(Struct { tag: _, members: _ }),
                    ) if lhs.ty.ty_eq(&rhs.ty) => {
                        let mut stmts = Vec::with_capacity(members.len());
                        for member in members {
                            let minus_offset = member.offset - member.ty.size_of();
                            stmts.push(ConvStmt::new_expr(ConvExpr::new_assign(
                                ConvExpr::new_member(
                                    lhs.clone(),
                                    member.ty.clone(),
                                    minus_offset,
                                    debug_info.clone(),
                                ),
                                ConvExpr::new_member(
                                    rhs.clone(),
                                    member.ty,
                                    minus_offset,
                                    debug_info.clone(),
                                ),
                                debug_info.clone(),
                            )));
                        }
                        Ok(ConvExpr {
                            kind: ConvExprKind::Block(Box::new(ConvStmt::Block(stmts)), None),
                            ty: Type::Void,
                            debug_info,
                        })
                    }
                    (lhs_ty, rhs_ty)
                        if lhs_ty.ty_eq(&rhs_ty) || matches!(&rhs.kind, ConvExprKind::Asm(_)) =>
                    {
                        Ok(ConvExpr::new_assign(lhs, rhs, debug_info))
                    }
                    (Type::Ptr(void_ptr_to), Type::Ptr(ptr_to)) if *void_ptr_to == Type::Void => {
                        // Safety: `rhs_ptr_to.is_some()` is true on this branch
                        rhs = ConvExpr::new_cast(
                            rhs,
                            Type::Ptr(Box::new(Type::Void)),
                            CastKind::ToVoidPtr { ptr_to: *ptr_to },
                        );
                        Ok(ConvExpr::new_assign(lhs, rhs, debug_info))
                    }
                    (Type::Ptr(ptr_to), Type::Ptr(void_ptr_to)) if *void_ptr_to == Type::Void => {
                        rhs = ConvExpr::new_cast(
                            rhs,
                            Type::Ptr(Box::new(Type::Void)),
                            CastKind::FromVoidPtr { ptr_to: *ptr_to },
                        );
                        Ok(ConvExpr::new_assign(lhs, rhs, debug_info))
                    }
                    (Type::Base(lhs_base), Type::Base(rhs_base)) => {
                        rhs = ConvExpr::new_cast(
                            rhs,
                            Type::Base(lhs_base),
                            CastKind::Base2Base(rhs_base, lhs_base),
                        );
                        Ok(ConvExpr::new_assign(lhs, rhs, debug_info))
                    }
                    _ => Err(CompileError::new_type_error(
                        lhs,
                        rhs,
                        Some("Assign expression's lhs and rhs has to have compatible types"),
                    )),
                }
            }
            kind => Ok(ConvExpr::new_op_assign(lhs, rhs, debug_info, kind)),
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn traverse_expr(
        &mut self,
        expr: Expr,
        fn_name: Option<&String>,
        mut attrs: BTreeSet<DownExprAttribute>,
    ) -> Result<ConvExpr, CompileError> {
        let debug_info = expr.debug_info;
        let expr = match expr.kind {
            // `a >= b` := `b <= a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Ge,
                lhs,
                rhs,
            }) => self.traverse_binary(Binary::new(BinOpKind::Le, rhs, lhs), fn_name, debug_info),
            // `a > b` := `b < a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Gt,
                lhs,
                rhs,
            }) => self.traverse_binary(Binary::new(BinOpKind::Lt, rhs, lhs), fn_name, debug_info),
            ExprKind::Binary(Binary {
                kind: BinOpKind::LogicalOr,
                lhs,
                rhs,
            }) => {
                let debug_info = lhs.debug_info.clone();
                let conditional = Expr::new_conditional(
                    *lhs,
                    Expr::new_num(1, debug_info.clone()),
                    *rhs,
                    debug_info,
                );
                self.traverse_expr(conditional, fn_name, attrs.clone())
            }
            ExprKind::Binary(Binary {
                kind: BinOpKind::LogicalAnd,
                lhs,
                rhs,
            }) => {
                let debug_info = lhs.debug_info.clone();
                let conditional = Expr::new_conditional(
                    *lhs,
                    *rhs,
                    Expr::new_num(0, debug_info.clone()),
                    debug_info,
                );
                self.traverse_expr(conditional, fn_name, attrs.clone())
            }
            // do nothing
            ExprKind::Binary(binary) => self.traverse_binary(binary, fn_name, debug_info), // do nothing
            ExprKind::Num(n) => Ok(ConvExpr::new_num(n, debug_info)),
            ExprKind::StrLit(mut letters) => {
                let name = format!(".LC{}", self.get_lc_label());
                let len = letters.len();
                letters.push('\0');
                let init = Some(Initializer::Array(
                    letters
                        .bytes()
                        .map(|byte| {
                            Initializer::Expr(Expr::new_num(byte as isize, debug_info.clone()))
                        })
                        .collect(),
                ));
                let gvar = self.new_global_variable(
                    init.as_ref(),
                    &name,
                    Type::Array(Box::new(Type::Base(BaseType::Char)), len + 1),
                    false,
                    debug_info.clone(),
                )?;
                self.conv_program
                    .push(ConvProgramKind::Global(gvar.clone()));
                // This gvar is initialized with `self.new_global_variable`, that's why this is ok.
                Ok(ConvExpr::new_gvar(gvar, debug_info))
            }
            // substitute `-x` into `0-x`
            ExprKind::Unary(unary_op, operand) => {
                self.traverse_unary(&unary_op, operand, fn_name, debug_info)
            }
            ExprKind::Assign(lhs, rhs, assign_bin_op) => {
                let rhs = if assign_bin_op != AssignBinOpToken::Eq {
                    let tmp_binary = self.traverse_binary(
                        Binary {
                            kind: assign_bin_op.try_into().unwrap(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        fn_name,
                        debug_info.clone(),
                    )?; // type check and necessary cast and ptr calc conversion
                    if let ConvExprKind::Binary(ConvBinary {
                        kind: _,
                        lhs: _,
                        rhs,
                    }) = tmp_binary.kind
                    {
                        *rhs
                    } else {
                        unreachable!()
                    }
                } else {
                    self.traverse_expr(*rhs, fn_name, BTreeSet::new())?
                };
                let lhs = self.traverse_expr(*lhs, fn_name, BTreeSet::new())?;
                Self::new_assign_expr_with_type_check(lhs, rhs, assign_bin_op, debug_info)
            }
            ExprKind::Ident(name) => {
                let err = |name: &str, debug_info: DebugInfo| {
                    Err(CompileError::new(CompileErrorKind::AnalyzeError(
                        AnalyzeErrorKind::UndeclaredError(
                            name.to_string(),
                            debug_info,
                            VariableKind::LocalOrGlobalOrFunc,
                        ),
                    )))
                };
                self.fetch_var(&name, fn_name, debug_info.clone())?
                    .map_or_else(
                        move || {
                            self.fetch_func_ptr(&name, debug_info.clone())
                                .map_or_else(|| err(&name, debug_info), |expr| Ok(expr))
                        },
                        |expr| Ok(expr),
                    )
            }
            ExprKind::Func(name, args) => {
                self.new_call_func_with_type_check(name, args, debug_info, fn_name, &attrs)
            }
            ExprKind::Deref(expr) => {
                // type check
                let conv_expr = self.traverse_expr(*expr, fn_name, BTreeSet::new())?;
                match conv_expr.ty.clone() {
                    ty @ (Type::Base(_)
                    | Type::Func {
                        ret_ty: _,
                        args: _,
                        is_flexible: _,
                    }
                    | Type::Struct(_)
                    // | Type::Enum(_)
                    | Type::Void) => {
                        Err(CompileError::new_type_expect_failed_with_str(
                            conv_expr.debug_info,
                            // TODO: base type is not a problem of this error
                            "Type::Ptr(_))".to_string(),
                            ty,
                        ))
                    }
                    // `*array` := `*(&array[0])`
                    Type::Array(array_base, _) => Ok(ConvExpr::new_deref(
                        conv_expr.convert_array_to_ptr(),
                        *array_base,
                        debug_info,
                    )),
                    Type::Ptr(ptr_base) => {
                        Ok(ConvExpr::new_deref(conv_expr, *ptr_base, debug_info))
                    }
                    Type::InComplete(_) => todo!(),
                }
            }
            ExprKind::Addr(expr) => Ok(ConvExpr::new_addr(
                self.traverse_expr(*expr, fn_name, BTreeSet::new())?,
                debug_info,
            )),
            ExprKind::SizeOf(SizeOfOperandKind::Expr(expr)) => {
                attrs.insert(DownExprAttribute::NoArrayPtrConversion);
                let size = self
                    .traverse_expr(*expr, fn_name, attrs.clone())?
                    .ty
                    .size_of() as isize;
                Ok(ConvExpr::new_num(size, debug_info))
            }
            ExprKind::SizeOf(SizeOfOperandKind::Type(type_name)) => {
                let mut ty = type_name.ty();
                ty = self.resolve_incomplete_type(ty, debug_info.clone())?;
                Ok(ConvExpr::new_num(ty.size_of() as isize, debug_info))
            }
            ExprKind::Array(expr, index) => {
                let debug_info = expr.debug_info.clone();
                // `a[i]` := `*(a + i)`
                let desugared = Expr::new_deref(
                    Expr::new_binary(BinOpKind::Add, *expr, *index, debug_info.clone()),
                    debug_info,
                );
                self.traverse_expr(desugared, fn_name, BTreeSet::new())
            }
            ExprKind::Member(expr, ident_name) => {
                let expr = self.traverse_expr(*expr, fn_name, BTreeSet::new())?;
                let ty = self.resolve_incomplete_type(expr.ty.clone(), expr.debug_info.clone())?;
                let member = if let Type::Struct(Struct { tag, members }) = ty {
                    members
                        .into_iter()
                        .find(|struct_member| struct_member.name == ident_name)
                        .ok_or_else(|| {
                            CompileError::new_no_such_member(tag, debug_info.clone(), ident_name)
                        })?
                } else {
                    return Err(CompileError::new_type_expect_failed_with_str(
                        debug_info,
                        "Type::Struct(_)".to_string(),
                        expr.ty,
                    ));
                };

                let minus_offset = member.offset - member.ty.size_of();
                Ok(ConvExpr::new_member(
                    expr,
                    member.ty,
                    minus_offset,
                    debug_info,
                ))
            }
            ExprKind::Arrow(expr, ident_name) => {
                let debug_info = expr.debug_info.clone();
                let converted_expr = Expr::new_member(
                    Expr::new_deref(*expr, debug_info.clone()),
                    ident_name,
                    debug_info,
                );
                self.traverse_expr(converted_expr, fn_name, BTreeSet::new())
            }
            ExprKind::Conditional { cond, then, els } => {
                let cond = self.traverse_expr(*cond, fn_name, BTreeSet::new())?;
                let then = self.traverse_expr(*then, fn_name, BTreeSet::new())?;
                let els = self.traverse_expr(*els, fn_name, BTreeSet::new())?;
                Self::new_conditional_with_type_checking(cond, then, els)
            }
            ExprKind::PostfixIncrement(expr) => {
                let expr = self.traverse_expr(*expr, fn_name, BTreeSet::new())?;
                let expr_ty = expr.ty.clone();
                let debug_info = expr.debug_info.clone();
                let value = match &expr_ty {
                    Type::Base(_) => 1,
                    Type::Ptr(ptr_to) => ptr_to.size_of(),
                    Type::InComplete(_)
                    | Type::Func {
                        ret_ty: _,
                        args: _,
                        is_flexible: _,
                    }
                    | Type::Struct(_)
                    // | Type::Enum(_) => {
                        => {
                        return Err(CompileError::new_type_expect_failed_with_str(
                            debug_info,
                            "Type::Base(_) | Type::Ptr(_)".to_string(),
                            expr_ty,
                        ))
                    }
                    Type::Void | Type::Array(_, _) => unreachable!(),
                };
                Ok(ConvExpr::new_postfix_increment(
                    expr, value, expr_ty, debug_info,
                ))
            }
            ExprKind::PostfixDecrement(expr) => {
                let expr = self.traverse_expr(*expr, fn_name, BTreeSet::new())?;
                let expr_ty = expr.ty.clone();
                let debug_info = expr.debug_info.clone();
                let value = match &expr_ty {
                    Type::Base(_) => 1,
                    Type::Ptr(ptr_to) => ptr_to.size_of(),
                    Type::InComplete(_)
                    | Type::Func {
                        ret_ty: _,
                        args: _,
                        is_flexible: _,
                    }
                    | Type::Struct(_)
                    // | Type::Enum(_) => {
                       => {
                        return Err(CompileError::new_type_expect_failed_with_str(
                            debug_info,
                            "Type::Base(_) | Type::Ptr(_)".to_string(),
                            expr_ty,
                        ))
                    }
                    Type::Void | Type::Array(_, _) => unreachable!(),
                };
                Ok(ConvExpr::new_postfix_decrement(
                    expr, value, expr_ty, debug_info,
                ))
            }
            ExprKind::UnaryIncrement(_) | ExprKind::UnaryDecrement(_) => todo!(),
            ExprKind::Asm(asm) => Ok(ConvExpr::new_inline_asm(asm, debug_info)),
            ExprKind::NullPtr => {
                let ty = Type::Ptr(Box::new(Type::Void));
                Ok(ConvExpr {
                    ty,
                    kind: ConvExprKind::Num(0),
                    debug_info,
                })
            }
            ExprKind::BuiltinVaStart(ap, last) => {
                // `__builtin_va_start(ap, last)` shall be re-read as below
                // `*ap = *(*struct __builtin_va_list)__va_area__`
                let va_area = self
                    .scope
                    .look_up_lvar(&"__va_area__".to_string())
                    .ok_or_else(|| {
                        unimplemented_err!(
                            debug_info.clone(),
                            "Cannot find `__va_area__` in this scope."
                        )
                    })?;
                let va_area = if let Var::LVar(va_area) = va_area {
                    va_area
                } else {
                    return Err(unimplemented_err!(
                        debug_info,
                        "`__va_area__` shall not be global variable."
                    ));
                };
                let va_area_ty = va_area.ty.clone();
                let ptr_converted_va_area =
                    ConvExpr::new_lvar_raw(va_area, va_area_ty, debug_info.clone())
                        .convert_array_to_ptr();
                let tag_name = String::from("__builtin_va_list");
                let ty_builtin_va_list = self
                    .scope
                    .resolve_tag_name_and_get_ty(&tag_name.to_string())
                    .ok_or_else(|| {
                        CompileError::new_undeclared_error(
                            tag_name,
                            debug_info.clone(),
                            VariableKind::Struct,
                        )
                    })?;
                let casted_va_area = ConvExpr::new_cast(
                    ptr_converted_va_area,
                    Type::Ptr(Box::new(ty_builtin_va_list.clone())),
                    CastKind::FromVoidPtr {
                        ptr_to: ty_builtin_va_list.clone(),
                    },
                );
                let derefed_va_area =
                    ConvExpr::new_deref(casted_va_area, ty_builtin_va_list, debug_info.clone());
                let ap = self.traverse_expr(*ap, fn_name, BTreeSet::new())?;
                let ap_base_ty = ap.ty.get_ptr_to().unwrap().clone();
                let derefed_ap = ConvExpr::new_deref(ap, ap_base_ty, debug_info.clone());
                Ok(Self::new_assign_expr_with_type_check(
                    derefed_ap,
                    derefed_va_area,
                    AssignBinOpToken::Eq,
                    debug_info,
                )?)
            }
            ExprKind::Comma(lhs, rhs) => {
                let rhs = self.traverse_expr(*rhs, fn_name, BTreeSet::new())?;
                let ty = rhs.ty.clone();
                Ok(ConvExpr::new_comma(
                    self.traverse_expr(*lhs, fn_name, BTreeSet::new())?,
                    rhs,
                    ty,
                    debug_info,
                ))
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
        debug_info: DebugInfo,
        fn_name: Option<&String>,
        _attrs: &BTreeSet<DownExprAttribute>,
    ) -> Result<ConvExpr, CompileError> {
        // TODO: check if `name` is a variable which is function ptr
        let (target, arg_types, ret_ty, declared_debug_info, is_flexible_length): (
            FuncCallTargetKind,
            Vec<Type>,
            Type,
            DebugInfo,
            bool,
        ) = (|name: &str, debug_info: &DebugInfo| {
            if let Some(var) = self.fetch_var(&name, fn_name, debug_info.clone())? {
                if let Some(Type::Func {
                    ret_ty,
                    args,
                    is_flexible,
                }) = var.ty.get_ptr_to_recursively()
                {
                    let (args_types, ret_ty, declared_debug_info, is_flexible) = (
                        args.clone(),
                        *ret_ty.clone(),
                        var.debug_info.clone(),
                        *is_flexible,
                    );
                    return Ok((
                        FuncCallTargetKind::Expr(Box::new(var)),
                        args_types,
                        ret_ty,
                        declared_debug_info,
                        is_flexible,
                    ));
                }
            }
            let func = match self.func_map.get(name) {
                Some(func) => func,
                None => {
                    return Err(CompileError::new_undeclared_error(
                        name.to_string(),
                        debug_info.clone(),
                        VariableKind::Func,
                    ));
                }
            };
            let is_flexible_length = func.args.is_flexible_length();
            let Func {
                name,
                static_variables: _static_variables, // TODO: deal with static variables even if it is a function pointer.
                args,
                ret,
                debug_info,
            } = func.clone();
            Ok((
                FuncCallTargetKind::Label(name),
                args.args,
                ret,
                debug_info,
                is_flexible_length,
            ))
        })(&name, &debug_info)?;
        // args type check
        let mut args = args
            .into_iter()
            .map(|expr| self.traverse_expr(expr, fn_name, BTreeSet::new()))
            .collect::<Result<Vec<_>, CompileError>>()?;

        if !is_flexible_length && arg_types.len() != args.len() {
            return Err(CompileError::new_args_error(
                target,
                debug_info,
                arg_types.len(),
                args.len(),
                declared_debug_info.clone(),
            ));
        }

        // implicit cast
        for (expected_ty, got_expr) in arg_types.iter().zip(args.iter_mut()) {
            let expected_ty = expected_ty.clone().into_ptr();
            if !expected_ty.ty_eq(&got_expr.ty) {
                if expected_ty.is_base() && got_expr.ty.is_base() {
                    let to = *expected_ty.get_base().unwrap();
                    let from = *got_expr.ty.get_base().unwrap();
                    *got_expr = ConvExpr::new_cast(
                        got_expr.clone(),
                        expected_ty.clone(),
                        CastKind::Base2Base(from, to),
                    );
                } else if expected_ty.is_void_ptr() && got_expr.ty.get_ptr_to().is_some() {
                    *got_expr = ConvExpr::new_cast(
                        got_expr.clone(),
                        expected_ty.clone(),
                        CastKind::ToVoidPtr {
                            ptr_to: got_expr.ty.get_ptr_to().unwrap().clone(),
                        },
                    );
                } else if got_expr.ty.is_void_ptr() && expected_ty.get_ptr_to().is_some() {
                    *got_expr = ConvExpr::new_cast(
                        got_expr.clone(),
                        expected_ty.clone(),
                        CastKind::FromVoidPtr {
                            ptr_to: got_expr.ty.get_ptr_to().unwrap().clone(),
                        },
                    );
                } else if got_expr.ty.is_base()
                    && expected_ty.get_ptr_to().map_or(false, |ty| ty.is_func())
                {
                    *got_expr = ConvExpr::new_cast(
                        got_expr.clone(),
                        expected_ty.clone(),
                        CastKind::Base2FuncPtr(
                            got_expr.ty.get_base().unwrap().clone(),
                            expected_ty,
                        ),
                    );
                } else {
                    return Err(CompileError::new_type_expect_failed(
                        got_expr.debug_info.clone(),
                        expected_ty,
                        got_expr.ty.clone(),
                    ));
                }
            }
        }
        Ok(ConvExpr::new_func(
            target,
            args,
            ret_ty.clone(),
            is_flexible_length,
            debug_info,
        ))
    }

    pub fn new_conditional_with_type_checking(
        cond: ConvExpr,
        mut then: ConvExpr,
        mut els: ConvExpr,
    ) -> Result<ConvExpr, CompileError> {
        if cond.ty != Type::Base(BaseType::Int) {
            return Err(CompileError::new_type_expect_failed(
                cond.debug_info,
                Type::Base(BaseType::Int),
                cond.ty,
            ));
        }
        if then.ty.ty_eq(&els.ty) {
            let ty = then.ty.clone();
            Ok(ConvExpr::new_conditional_raw(cond, then, els, ty))
        } else {
            // implicit cast
            ConvExpr::binary_implicit_cast(&mut then, &mut els)?;
            let new_ty = then.ty.clone();
            Ok(ConvExpr::new_conditional_raw(cond, then, els, new_ty))
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn traverse_binary(
        &mut self,
        Binary { kind, lhs, rhs }: Binary,
        fn_name: Option<&String>,
        debug_info: DebugInfo,
    ) -> Result<ConvExpr, CompileError> {
        let mut rhs = self.traverse_expr(*rhs, fn_name, BTreeSet::new())?;
        let mut lhs = self.traverse_expr(*lhs, fn_name, BTreeSet::new())?;
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
                        debug_info,
                    ))
                }
                (Type::Base(base), Type::Ptr(ptr_base)) => {
                    if *base != BaseType::Int {
                        return Err(CompileError::new_type_expect_failed(
                            lhs.debug_info,
                            Type::Base(BaseType::Int),
                            Type::Base(*base),
                        ));
                    }
                    lhs = ConvExpr::new_binary(
                        ConvBinOpKind::Mul,
                        ConvExpr::new_num(ptr_base.size_of() as isize, debug_info.clone()),
                        lhs.clone(),
                        Type::Base(BaseType::Int),
                        debug_info.clone(),
                    ); // i + p -> sizeof(*p) * i + p
                    let ptr_base = ptr_base.clone();
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Ptr(ptr_base),
                        debug_info,
                    ))
                }
                (Type::Ptr(ptr_base), Type::Base(base)) => {
                    if *base != BaseType::Int {
                        return Err(CompileError::new_type_expect_failed(
                            lhs.debug_info,
                            Type::Base(*base),
                            Type::Base(BaseType::Int),
                        ));
                    }

                    rhs = ConvExpr::new_binary(
                        ConvBinOpKind::Mul,
                        rhs.clone(),
                        ConvExpr::new_num(ptr_base.size_of() as isize, debug_info.clone()),
                        // Type::Ptr(ptr_base.clone()),
                        Type::Base(BaseType::Int),
                        debug_info.clone(),
                    ); // p + i ->  p + i * sizeof(*p)
                    let ptr_base = ptr_base.clone();
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Ptr(ptr_base),
                        debug_info,
                    ))
                }
                (Type::Ptr(lhs_base), Type::Ptr(rhs_base))
                    if matches!(kind, ConvBinOpKind::Sub) =>
                {
                    if *lhs_base != *rhs_base {
                        return Err(CompileError::new_type_error(
                            lhs,
                            rhs,
                            Some("incompatible type on ptr subtraction is not allowed".to_string()),
                        ));
                    }
                    let base_ty = lhs_base.clone();
                    let size_of = ConvExpr::new_num(base_ty.size_of() as isize, debug_info.clone());
                    let subtracted = ConvExpr::new_binary(
                        ConvBinOpKind::Sub,
                        lhs,
                        rhs,
                        Type::Ptr(base_ty.clone()),
                        debug_info.clone(),
                    );
                    Ok(ConvExpr::new_binary(
                        ConvBinOpKind::Div,
                        subtracted,
                        size_of,
                        *base_ty,
                        debug_info,
                    ))
                }
                (Type::Ptr(_), Type::Ptr(_)) => Err(CompileError::new_type_error(
                    lhs,
                    rhs,
                    Some("ptr + ptr or ptr - ptr is not allowed. ".to_string()),
                )),
                (_, Type::Func { ret_ty: _, args:_, is_flexible :_}) | (Type::Func { ret_ty: _, args: _, is_flexible :_}, _) => Err(unimplemented_err!(
                    debug_info,
                    "binary expr of function is not currently supported."
                )),
                (_, Type::Struct(_)) | (Type::Struct(_), _) => Err(unimplemented_err!(
                    debug_info,
                    "binary expr of struct is illegal operation."
                )),
                // (_, Type::Enum(_)) | (Type::Enum(_), _) => Err(unimplemented_err!(
                //     debug_info,
                //     "binary expr of enum is illegal operation."
                // )),
                (_, Type::Array(_, _)) | (Type::Array(_, _), _) => {
                    unreachable!(
                        "binary expr's operands(one of which is array) must be casted to ptr."
                    )
                }
                (_, Type::Void) | (Type::Void, _) => Err(unimplemented_err!(
                    debug_info,
                    "binary expr of void is illegal operation."
                )),
                (_, Type::InComplete(_)) | (Type::InComplete(_), _) => Err(unimplemented_err!(
                    debug_info,
                    "INTERNAL COMPILER ERROR, binary expr of incomplete type shouldnt be calculated."
                )),
            },
            op @ (ConvBinOpKind::LShift | ConvBinOpKind::RShift) => {
                if !lhs.ty.ty_eq(&rhs.ty) {
                    // TODO: char -> int
                    return Err(CompileError::new_type_error(
                        lhs,
                        rhs,
                        Some("incompatible type on lshift or rshift is not allowed".to_string()),
                    ));
                }
                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(op, lhs, rhs, lhs_ty, debug_info))
            }
            op @ ConvBinOpKind::BitWiseAnd => {
                if !lhs.ty.ty_eq(&rhs.ty) {
                    // TODO: char -> int
                    return Err(CompileError::new_type_error(
                        lhs,
                        rhs,
                        Some("incompatible type on bit wise and is not allowed".to_string()),
                    ));
                }
                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(op, lhs, rhs, lhs_ty, debug_info))
            }
            ConvBinOpKind::Mul | ConvBinOpKind::Div => {
                if !lhs.ty.ty_eq(&rhs.ty) {
                    return Err(CompileError::new_type_error(
                        lhs,
                        rhs,
                        Some(
                            "incompatible type multiplication or division is not allowed"
                                .to_string(),
                        ),
                    ));
                }
                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(kind, lhs, rhs, lhs_ty, debug_info))
            }
            ConvBinOpKind::Rem => {
                if !lhs.ty.ty_eq(&rhs.ty) {
                    return Err(CompileError::new_type_error(
                        lhs,
                        rhs,
                        Some("incompatible type take remainder is not allowed".to_string()),
                    ));
                }

                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(kind, lhs, rhs, lhs_ty, debug_info))
            },
            ConvBinOpKind::Eq | ConvBinOpKind::Ne => {
                if lhs.ty.ty_eq(&rhs.ty) || (lhs.ty.is_ptr() && rhs.ty.is_ptr()) {
                     Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Base(BaseType::Int),
                        debug_info,
                    ))
                } else if let (Some(lhs_base), Some(rhs_base)) =(lhs.ty.get_base(), rhs.ty.get_base()) {
                    let cast_needed = lhs_base.bytes() != rhs_base.bytes();
                    if cast_needed {
                        let lhs_base = *lhs_base;
                        let rhs_base = *rhs_base;
                        if lhs_base.bytes() > rhs_base.bytes() {
                            // TODO: use fn `implicit_cast`
                            rhs = ConvExpr::new_cast(
                                rhs,
                                Type::Base(lhs_base),
                                CastKind::Base2Base(rhs_base, lhs_base),
                            );
                        } else {
                            lhs = ConvExpr::new_cast(
                                lhs,
                                Type::Base(rhs_base),
                                CastKind::Base2Base(lhs_base, rhs_base),
                            );
                        }
                    }
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Base(BaseType::Int),
                        debug_info,
                    ))
                } else {
                     Err(CompileError::new_type_error(
                        lhs,
                        rhs,
                        Some("incompatible type binary expr is not allowed".to_string()),
                    ))
                }},
            ConvBinOpKind::Le | ConvBinOpKind::Lt  => {
                if lhs.ty.ty_eq(&rhs.ty) {
                Ok(ConvExpr::new_binary(
                    kind,
                    lhs,
                    rhs,
                    Type::Base(BaseType::Int),
                    debug_info,
                ))
                } else if let (Some(lhs_base), Some(rhs_base)) =(lhs.ty.get_base(), rhs.ty.get_base()) {
                    let cast_needed = lhs_base.bytes() != rhs_base.bytes();
                    if cast_needed {
                        let lhs_base = *lhs_base;
                        let rhs_base = *rhs_base;
                        if lhs_base.bytes() > rhs_base.bytes() {
                            // TODO: use fn `implicit_cast`
                            rhs = ConvExpr::new_cast(
                                rhs,
                                Type::Base(lhs_base),
                                CastKind::Base2Base(rhs_base, lhs_base),
                            );
                        } else {
                            lhs = ConvExpr::new_cast(
                                lhs,
                                Type::Base(rhs_base),
                                CastKind::Base2Base(lhs_base, rhs_base),
                            );
                        }
                    }
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Base(BaseType::Int),
                        debug_info,
                    ))
                } else {
                     Err(CompileError::new_type_error(
                        lhs,
                        rhs,
                        Some("incompatible type binary expr is not allowed".to_string()),
                    ))
                }

            }
        }
    }

    pub fn traverse_unary(
        &mut self,
        unary_op: &UnaryOp,
        operand: Box<Expr>,
        fn_name: Option<&String>,
        debug_info: DebugInfo,
    ) -> Result<ConvExpr, CompileError> {
        match unary_op {
            UnaryOp::Plus => self.traverse_expr(*operand, fn_name, BTreeSet::new()),
            UnaryOp::Minus => self.traverse_binary(
                Binary::new(
                    BinOpKind::Sub,
                    Box::new(Expr::new_num(0, debug_info.clone())),
                    operand,
                ),
                fn_name,
                debug_info,
            ),
            UnaryOp::BitInvert => {
                let mut operand = self.traverse_expr(*operand, fn_name, BTreeSet::new())?;
                if !operand.ty.ty_eq(&Type::Base(BaseType::Int)) {
                    if let Some(base_ty) = operand.ty.clone().get_base() {
                        operand = ConvExpr::new_cast(
                            operand,
                            Type::Base(BaseType::Int),
                            CastKind::Base2Base(*base_ty, BaseType::Int),
                        );
                    } else {
                        return Err(CompileError::new_type_error_types(
                            operand.debug_info.clone(),
                            operand.debug_info,
                            operand.ty,
                            Type::Base(BaseType::Int),
                            Some("Type Conversion failed at BitInvert `~`."),
                        ));
                    }
                }
                Ok(ConvExpr::new_unary(ConvUnaryOp::BitInvert, operand))
            }
            UnaryOp::LogicalNot => {
                let eq_with_0 = Binary::new(
                    BinOpKind::Eq,
                    operand,
                    Box::new(Expr::new_num(0, debug_info.clone())),
                );
                Ok(self.traverse_binary(eq_with_0, fn_name, debug_info)?)
            }
            UnaryOp::Increment => {
                let operand = self.traverse_expr(*operand, fn_name, BTreeSet::new())?;
                let expr_ty = operand.ty.clone();
                let debug_info = operand.debug_info.clone();
                let value = match &expr_ty {
                    Type::Base(_) => 1,
                    Type::Ptr(ptr_to) => ptr_to.size_of(),
                    Type::InComplete(_)
                    | Type::Func {
                        ret_ty: _,
                        args: _,
                        is_flexible: _,
                    }
                    | Type::Struct(_) => {
                        return Err(CompileError::new_type_expect_failed_with_str(
                            debug_info,
                            "Type::Base(_) | Type::Ptr(_)".to_string(),
                            expr_ty,
                        ))
                    }
                    Type::Void | Type::Array(_, _) => unreachable!(),
                };
                Ok(ConvExpr::new_unary(ConvUnaryOp::Increment(value), operand))
            }
            UnaryOp::Decrement => {
                let operand = self.traverse_expr(*operand, fn_name, BTreeSet::new())?;
                let expr_ty = operand.ty.clone();
                let debug_info = operand.debug_info.clone();
                let value = match &expr_ty {
                    Type::Base(_) => 1,
                    Type::Ptr(ptr_to) => ptr_to.size_of(),
                    Type::InComplete(_)
                    | Type::Func {
                        ret_ty: _,
                        args: _,
                        is_flexible: _,
                    }
                    | Type::Struct(_) => {
                        return Err(CompileError::new_type_expect_failed_with_str(
                            debug_info,
                            "Type::Base(_) | Type::Ptr(_)".to_string(),
                            expr_ty,
                        ))
                    }
                    Type::Void | Type::Array(_, _) => unreachable!(),
                };
                Ok(ConvExpr::new_unary(ConvUnaryOp::Decrement(value), operand))
            }
        }
    }

    pub fn new_init_expr(
        &mut self,
        init: Initializer,
        lvar: LVar,
        ty: Type,
        fn_name: Option<&String>,
        attrs: BTreeSet<DownExprAttribute>,
        debug_info: DebugInfo,
    ) -> Result<ConvExpr, CompileError> {
        match init {
            Initializer::Expr(init) => {
                let rhs = self.traverse_expr(init, fn_name, attrs)?;
                Self::new_assign_expr_with_type_check(
                    // Safety:
                    // the lvar is generated by `Self::register_lvar` which initializes lvar_map
                    ConvExpr::new_lvar_raw(lvar, ty, debug_info.clone()),
                    rhs,
                    AssignBinOpToken::Eq,
                    debug_info,
                )
            }
            Initializer::Array(_) => Err(unimplemented_err!(
                debug_info,
                "Array initializer is not currently supported."
            )),
        }
    }

    pub fn get_type(
        &mut self,
        mut ty: Type,
        declarator: &Declarator,
        fn_name: Option<&String>,
    ) -> Result<Type, CompileError> {
        for _ in 0..declarator.n_star {
            ty = Type::Ptr(Box::new(ty));
        }
        let mut watching_direct_declarator = &declarator.direct_declarator;
        loop {
            match watching_direct_declarator {
                DirectDeclarator::Func(direct_declarator, args, is_flexible) => {
                    // e.g) int a(arg1: int, arg2: int*)(arg0: int) -> Func(Func(Int, vec![arg0]), vec![arg1, arg2])
                    ty = Type::Func {
                        ret_ty: Box::new(ty),
                        args: args
                            .iter()
                            .map(|declaration| {
                                let converted_ty_spec = self.resolve_name_and_convert_to_type(
                                    &declaration.declaration_specifiers,
                                    declaration.debug_info.clone(),
                                )?;
                                self.get_type(
                                    converted_ty_spec,
                                    &declaration
                                        .init_declarator
                                        .as_ref()
                                        .expect("get type for struct is not yet implemented.")
                                        .declarator,
                                    fn_name,
                                )
                            })
                            .collect::<Result<Vec<_>, CompileError>>()?,
                        is_flexible: *is_flexible,
                    };
                    watching_direct_declarator = direct_declarator;
                }
                DirectDeclarator::Array(direct_declarator, expr) => {
                    #[allow(clippy::cast_sign_loss)]
                    let size = ConstExpr::try_eval_as_const(
                        // TODO: not unwrap, but check has init or not.
                        self.traverse_expr(
                            expr.clone()
                                .ok_or_else(|| unimplemented_err!("No Array size expr"))?,
                            fn_name,
                            BTreeSet::new(),
                        )?,
                    )?
                    .get_num_lit()? as usize;
                    ty = Type::Array(Box::new(ty), size);
                    watching_direct_declarator = direct_declarator;
                }
                DirectDeclarator::Declarator(declarator) => {
                    ty = self.get_type(ty, declarator, fn_name)?;
                    break;
                }
                DirectDeclarator::Ident(_) => break,
            }
        }

        Ok(ty)
    }

    pub fn fetch_var(
        &self,
        name: &str,
        fn_name: Option<&String>,
        debug_info: DebugInfo,
    ) -> Result<Option<ConvExpr>, CompileError> {
        let var: Option<Var> = (|| {
            if let Some(var) = self.scope.look_up_lvar(&name.to_string()) {
                return Result::<_, CompileError>::Ok(Some(var));
            }

            if let Some(fn_name) = fn_name {
                let func = self.func_map.get(fn_name).ok_or_else(|| unimplemented_err!(debug_info.clone(), format!("INTERNAL COMPILER ERROR: tried to define static in function({}) variable, but the function {} was not defined.", fn_name, fn_name)))?;
                if let Some(gvar) = func.static_variables.get(name) {
                    return Ok(Some(Var::GVar(gvar.clone())));
                }
            }

            if let Some(var) = self.scope.look_up_gvar(&name.to_string()) {
                return Ok(Some(var));
            }

            if let Some(var) = self.scope.look_up_enum_variant(&name.to_string()) {
                return Ok(Some(var));
            }

            Ok(None)
        })()?;

        if let Some(var) = var {
            Ok(Some(match var {
                Var::GVar(global) => ConvExpr::new_gvar(global, debug_info),
                Var::LVar(local) => {
                    let ty = local.ty.clone();
                    ConvExpr::new_lvar_raw(local, ty, debug_info)
                }
                Var::EnumVariant(EnumVariant { name: _, value }) => {
                    ConvExpr::new_num(value as isize, debug_info)
                }
            }))
        } else {
            Ok(None)
        }
    }

    pub fn fetch_func_ptr(&self, name: &str, debug_info: DebugInfo) -> Option<ConvExpr> {
        let func = self.func_map.get(name)?;
        let func_ty = Type::from(func.clone());
        Some(ConvExpr::new_func_ptr(
            func_ty,
            name.to_string(),
            debug_info,
        ))
    }

    pub fn insert_static_func_variable(
        &mut self,
        variable_name: String,
        ty: Type,
        init: Option<ConstInitializer>,
        fn_name: &String,
        debug_info: &DebugInfo,
    ) -> Result<GVar, CompileError> {
        let func = self
            .func_map
            .get_mut(fn_name)
            .ok_or_else(|| unimplemented_err!(debug_info.clone(), format!("INTERNAL COMPILER ERROR: tried to define static in function({}) variable, but the function {} was not defined.", fn_name, fn_name)))?;
        let name = format!(".L{}_{}", fn_name, variable_name);
        let gvar = GVar {
            name,
            ty,
            init,
            is_extern: false,
        };
        if func
            .static_variables
            .insert(variable_name.clone(), gvar.clone())
            .is_some()
        {
            Err(CompileError::new_redefined_variable(
                variable_name,
                debug_info.clone(),
                VariableKind::StaticInFunction,
            ))
        } else {
            Ok(gvar)
        }
    }

    pub fn look_up_static_variable(
        &self,
        fn_name: &String,
        name: &String,
        debug_info: &DebugInfo,
    ) -> Result<Option<GVar>, CompileError> {
        let func = self
            .func_map
            .get(fn_name)
            .ok_or_else(|| unimplemented_err!(debug_info.clone(), format!("INTERNAL COMPILER ERROR: tried to define static in function({}) variable, but the function {} was not defined.", fn_name, fn_name)))?;
        Ok(func.static_variables.get(name).map(|gvar| gvar.clone()))
    }
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvProgram {
    components: VecDeque<ConvProgramKind>,
}

impl ConvProgram {
    pub fn new() -> Self {
        Self {
            components: VecDeque::new(),
        }
    }

    pub fn with_vec(vec: VecDeque<ConvProgramKind>) -> Self {
        Self { components: vec }
    }

    pub fn push(&mut self, kind: ConvProgramKind) {
        self.components.push_back(kind);
    }

    pub fn push_front(&mut self, kind: ConvProgramKind) {
        self.components.push_front(kind);
    }
}

impl IntoIterator for ConvProgram {
    type Item = ConvProgramKind;

    type IntoIter = std::collections::vec_deque::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvProgramKind {
    Func(ConvFuncDef),
    Global(GVar),
    InlineAsm(String),
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
    Return(Option<ConvExpr>, String),
    Block(Vec<ConvStmt>),
    If(ConvExpr, Box<ConvStmt>, Option<Box<ConvStmt>>),
    While(ConvExpr, Box<ConvStmt>),
    For(
        Option<ConvExpr>,
        Option<ConvExpr>,
        Option<ConvExpr>,
        Box<ConvStmt>,
    ),
    Switch {
        expr: ConvExpr,
        cases: Vec<isize>,
        stmt: Box<ConvStmt>,
        has_default: bool,
    },
    LoopControl(LoopControlKind),
    VaStartInit {
        arg_n: usize,
    },
    // TODO: in `analyze.rs` check labels are in appropriate position on each stmt.
    // Labeled(ConvLabeled),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SwitchBodyStmt {
    Stmt(ConvStmt),
    Case(isize, ConvStmt),
    Default(ConvStmt),
    Break,
}

impl ConvStmt {
    pub const fn new_expr(expr: ConvExpr) -> Self {
        ConvStmt::Expr(expr)
    }

    pub const fn new_ret(expr: Option<ConvExpr>, name: String) -> Self {
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

    pub fn new_switch(
        expr: ConvExpr,
        cases: Vec<isize>,
        stmt: ConvStmt,
        has_default: bool,
    ) -> Self {
        ConvStmt::Switch {
            expr,
            cases,
            stmt: Box::new(stmt),
            has_default,
        }
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
    pub debug_info: DebugInfo,
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
        let debug_info = self.debug_info.clone();
        ConvExpr::new_addr(self, debug_info)
    }

    pub fn type_cast(
        from_ty: Type,
        to_ty: Type,
        debug_info: DebugInfo,
    ) -> Result<(Type, CastKind), CompileError> {
        match (from_ty, to_ty) {
            (Type::Base(from_base), Type::Base(to_base)) if from_base == to_base => {
                // No Cast
                Ok((Type::Base(to_base), CastKind::NoCast))
            }
            (Type::Base(from_base), Type::Base(to_base)) /*if *from_base > *to_base */ => {
                // Castable
                Ok((Type::Base(to_base), CastKind::Base2Base(from_base, to_base)))
            }
            (Type::Ptr(void_base), Type::Ptr(ptr_to)) if *void_base == Type::Void => {
                // Void -> Ptr
                Ok((
                    Type::Ptr(ptr_to.clone()),
                    CastKind::FromVoidPtr {
                        ptr_to: *ptr_to,
                    },
                ))
            }
            (Type::Ptr(ptr_to), Type::Ptr(void_base)) if *void_base == Type::Void => {
                // Ptr -> Void
                Ok((
                    Type::Ptr(Box::new(Type::Void)),
                    CastKind::ToVoidPtr {
                        ptr_to: *ptr_to,
                    },
                ))
            }
            (Type::Ptr(ptr_to_from), Type::Ptr(ptr_to_to)) => {
                let (new_ptr_to, cast_kind) = Self::type_cast( *ptr_to_from.clone(), *ptr_to_to.clone(), debug_info)?;
                Ok((
                    Type::Ptr(Box::new(new_ptr_to)),
                    CastKind::Ptr2Ptr {
                        from: Type::Ptr(ptr_to_from),
                        to: Type::Ptr(ptr_to_to),
                        cast_kind: Box::new(cast_kind),
                    },
                ))
            }
            (from_ty, to_ty) => Err(unimplemented_err!(
                debug_info,
                format!("Implicit Cast Failed.: {:?} -> {:?}", from_ty, to_ty)
            )),
        }
    }

    pub fn implicit_cast(self, ty: &Type) -> Result<Self, CompileError> {
        // self.ty -> ty
        let (new_ty, cast_kind) =
            ConvExpr::type_cast(self.ty.clone(), ty.clone(), self.debug_info.clone())?;
        Ok(ConvExpr::new_cast(self, new_ty, cast_kind))
    }

    pub fn binary_implicit_cast(
        lhs: &mut ConvExpr,
        rhs: &mut ConvExpr,
    ) -> Result<(), CompileError> {
        if lhs.ty.ty_eq(&rhs.ty) {
            return Ok(());
        }
        let lhs_ty = lhs.ty.clone();
        let rhs_ty = rhs.ty.clone();
        let debug_info = lhs.debug_info.clone();
        match (lhs_ty, rhs_ty) {
            (Type::Base(base_lhs), Type::Base(base_rhs))
                if base_lhs.bytes() == base_rhs.bytes() =>
            {
                // No Cast
                return Ok(());
            }
            (Type::Base(base_lhs), Type::Base(base_rhs)) if base_lhs.bytes() > base_rhs.bytes() => {
                // rhs_ty -> lhs_ty
                let (new_ty, cast_kind) =
                    Self::type_cast(Type::Base(base_rhs), Type::Base(base_lhs), debug_info)?;
                let rhs_cloned = rhs.clone();
                *rhs = ConvExpr::new_cast(rhs_cloned, new_ty, cast_kind);
            }
            (Type::Base(base_lhs), Type::Base(base_rhs)) if base_lhs.bytes() < base_rhs.bytes() => {
                // lhs_ty -> rhs_ty
                let (new_ty, cast_kind) =
                    Self::type_cast(Type::Base(base_lhs), Type::Base(base_rhs), debug_info)?;
                let lhs_cloned = lhs.clone();
                *lhs = ConvExpr::new_cast(lhs_cloned, new_ty, cast_kind);
            }
            (Type::Ptr(void_base), Type::Ptr(ptr_to)) if *void_base == Type::Void => {
                // Ptr -> Void
                let rhs_cloned = rhs.clone();
                *rhs = ConvExpr::new_cast(
                    rhs_cloned,
                    Type::Ptr(Box::new(Type::Void)),
                    CastKind::ToVoidPtr { ptr_to: *ptr_to },
                );
            }
            (Type::Ptr(ptr_to), Type::Ptr(void_base)) if *void_base == Type::Void => {
                // Ptr -> Void
                let lhs_cloned = lhs.clone();
                *lhs = ConvExpr::new_cast(
                    lhs_cloned,
                    Type::Ptr(Box::new(Type::Void)),
                    CastKind::ToVoidPtr { ptr_to: *ptr_to },
                );
            }
            _ => todo!(),
        }
        Ok(())
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
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            kind: ConvExprKind::Binary(ConvBinary::new(kind, Box::new(lhs), Box::new(rhs))),
            ty,
            debug_info,
        }
    }
    pub fn new_comma(lhs: ConvExpr, rhs: ConvExpr, ty: Type, debug_info: DebugInfo) -> Self {
        Self {
            kind: ConvExprKind::Comma(Box::new(lhs), Box::new(rhs)),
            ty,
            debug_info,
        }
    }

    pub fn new_postfix_increment(
        lhs: ConvExpr,
        value: usize,
        ty: Type,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            kind: ConvExprKind::PostfixIncrement(Box::new(lhs), value),
            ty,
            debug_info,
        }
    }

    pub fn new_postfix_decrement(
        lhs: ConvExpr,
        value: usize,
        ty: Type,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            kind: ConvExprKind::PostfixDecrement(Box::new(lhs), value),
            ty,
            debug_info,
        }
    }

    pub fn new_conditional_raw(cond: ConvExpr, then: ConvExpr, els: ConvExpr, ty: Type) -> Self {
        let debug_info = cond.debug_info.clone();
        Self {
            kind: ConvExprKind::Conditional {
                cond: Box::new(cond),
                then: Box::new(then),
                els: Box::new(els),
            },
            ty,
            debug_info,
        }
    }

    pub fn new_unary(unary_op: ConvUnaryOp, expr: ConvExpr) -> Self {
        let ty = expr.ty.clone();
        let debug_info = expr.debug_info.clone();
        Self {
            kind: ConvExprKind::Unary(unary_op, Box::new(expr)),
            ty,
            debug_info,
        }
    }

    pub fn new_func(
        target: FuncCallTargetKind,
        args: Vec<ConvExpr>,
        ret_ty: Type,
        is_flexible: bool,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            kind: ConvExprKind::Func(target, args, is_flexible, 0),
            ty: ret_ty,
            debug_info,
        }
    }

    pub const fn new_num(num: isize, debug_info: DebugInfo) -> Self {
        Self {
            kind: ConvExprKind::Num(num),
            ty: Type::Base(BaseType::Int),
            debug_info,
        }
    }

    pub fn new_assign(lhs: ConvExpr, rhs: ConvExpr, debug_info: DebugInfo) -> Self {
        let ty = lhs.ty.clone();
        ConvExpr {
            kind: ConvExprKind::Assign(Box::new(lhs), Box::new(rhs)),
            ty,
            debug_info,
        }
    }

    pub fn new_op_assign(
        lhs: ConvExpr,
        rhs: ConvExpr,
        debug_info: DebugInfo,
        kind: AssignBinOpToken,
    ) -> Self {
        let ty = lhs.ty.clone();
        ConvExpr {
            kind: ConvExprKind::OpAssign(Box::new(lhs), Box::new(rhs), kind),
            ty,
            debug_info,
        }
    }

    pub fn new_member(
        expr: ConvExpr,
        member_ty: Type,
        minus_offset: usize,
        debug_info: DebugInfo,
    ) -> Self {
        ConvExpr {
            kind: ConvExprKind::Member {
                struct_expr: Box::new(expr),
                minus_offset,
            },
            ty: member_ty,
            debug_info,
        }
    }

    pub const fn new_lvar_raw(lvar: LVar, ty: Type, debug_info: DebugInfo) -> Self {
        ConvExpr {
            kind: ConvExprKind::LVar(lvar),
            ty,
            debug_info,
        }
    }

    pub fn new_gvar(gvar: GVar, debug_info: DebugInfo) -> Self {
        let ty = gvar.ty.clone();
        ConvExpr {
            kind: ConvExprKind::GVar(gvar),
            ty,
            debug_info,
        }
    }

    pub fn new_func_ptr(func_ty: Type, func_name: String, debug_info: DebugInfo) -> Self {
        let ty = Type::ptr(func_ty.clone());
        ConvExpr {
            kind: ConvExprKind::FuncPtr(func_ty, func_name),
            ty,
            debug_info,
        }
    }

    pub fn new_deref(expr: ConvExpr, base_ty: Type, debug_info: DebugInfo) -> Self {
        Self {
            kind: ConvExprKind::Deref(Box::new(expr)),
            ty: base_ty,
            debug_info,
        }
    }

    pub fn new_addr(expr: ConvExpr, debug_info: DebugInfo) -> Self {
        let ty = expr.ty.clone();
        Self {
            kind: ConvExprKind::Addr(Box::new(expr)),
            ty: Type::Ptr(Box::new(ty)),
            debug_info,
        }
    }

    pub fn new_cast(expr: ConvExpr, to_ty: Type, kind: CastKind) -> Self {
        let debug_info = expr.debug_info.clone();
        Self {
            kind: ConvExprKind::Cast(Box::new(expr), kind),
            ty: to_ty,
            debug_info,
        }
    }

    pub fn new_inline_asm(asm: String, debug_info: DebugInfo) -> Self {
        Self {
            kind: ConvExprKind::Asm(asm),
            ty: Type::Ptr(Box::new(Type::Void)), // TODO: which type is appropriate here.
            debug_info,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum CastContext {
    Assign,
    // Binary,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum FuncCallTargetKind {
    Label(String),
    Expr(Box<ConvExpr>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvExprKind {
    Member {
        struct_expr: Box<ConvExpr>,
        minus_offset: usize,
    },
    Conditional {
        cond: Box<ConvExpr>,
        then: Box<ConvExpr>,
        els: Box<ConvExpr>,
    },
    Binary(ConvBinary),
    Comma(Box<ConvExpr>, Box<ConvExpr>),
    Unary(ConvUnaryOp, Box<ConvExpr>),
    Num(isize),
    LVar(LVar),
    GVar(GVar),
    FuncPtr(Type, String),
    Assign(Box<ConvExpr>, Box<ConvExpr>),
    OpAssign(Box<ConvExpr>, Box<ConvExpr>, AssignBinOpToken),
    Func(
        FuncCallTargetKind,
        Vec<ConvExpr>,
        /* is_flexible_length */ bool,
        usize,
    ),
    Deref(Box<ConvExpr>),
    Addr(Box<ConvExpr>),
    Cast(Box<ConvExpr>, CastKind),
    PostfixIncrement(Box<ConvExpr>, /* + n */ usize),
    PostfixDecrement(Box<ConvExpr>, /* - n */ usize),
    Asm(String),
    Block(Box<ConvStmt>, Option<Box<ConvExpr>>), /* create one scope for this expr. Return evaluated last expr. */
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ConvUnaryOp {
    BitInvert,
    Increment(usize),
    Decrement(usize),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum CastKind {
    Base2Base(BaseType /* to */, BaseType /* from */),
    NoCast,
    Ptr2Ptr {
        from: Type,
        to: Type,
        cast_kind: Box<CastKind>,
    },
    ToVoidPtr {
        ptr_to: Type,
    },
    FromVoidPtr {
        ptr_to: Type,
    },
    Base2FuncPtr(BaseType, Type),
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
    pub tag_scope: Vec<BTreeMap<String, Taged>>,
    max_stack_size: usize,
    diff: Vec<usize>,
    index_for_anonymous_enum_variants: usize,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            global: BTreeMap::new(),
            scopes: Vec::new(),
            tag_scope: Vec::new(),
            max_stack_size: 0,
            diff: Vec::new(),
            index_for_anonymous_enum_variants: 0,
        }
    }

    pub fn register_tag(&mut self, name: String, taged: Taged) {
        self.tag_scope
            .last_mut()
            .expect("INTERNAL COMPILER ERROR.")
            .insert(name, taged);
    }

    pub fn register_anonymous_enum_tag(&mut self, enum_ident_map: BTreeMap<String, usize>) {
        let name = format!(
            ".__anonymous_enum_{}",
            self.index_for_anonymous_enum_variants
        );
        self.index_for_anonymous_enum_variants += 1;
        self.tag_scope
            .last_mut()
            .expect("INTERNAL COMPILER ERROR.")
            .insert(name.clone(), Taged::new_enum_tag(name, enum_ident_map));
    }

    pub fn push_tag_scope(&mut self) {
        self.tag_scope.push(BTreeMap::new());
    }

    pub fn pop_tag_scope(&mut self) {
        self.tag_scope.pop();
    }

    pub fn push_scope(&mut self) {
        self.diff.push(0);
        self.scopes.push(BTreeMap::new());
        self.tag_scope.push(BTreeMap::new());
    }

    pub fn pop_scope(&mut self, offset: &mut usize) {
        assert!(!self.scopes.is_empty());
        assert!(!self.tag_scope.is_empty());
        self.scopes.pop();
        self.tag_scope.pop();
        *offset -= self.diff.pop().expect("diff has to exist.");
    }

    pub fn look_up_struct_tag(&self, name: &str) -> Option<&Taged> {
        for map in self.tag_scope.iter().rev() {
            if let Some(taged) = map.get(name) {
                return Some(taged);
            }
        }
        None
    }

    pub fn resolve_tag_name(&self, tag_name: &String) -> Option<&Taged> {
        for map in self.tag_scope.iter().rev() {
            if let Some(map) = map.get(tag_name) {
                return Some(map);
            }
        }
        None
    }

    pub fn resolve_tag_name_and_get_ty(&self, tag_name: &String) -> Option<Type> {
        if let Some(Taged::Struct(StructTagKind::Struct(struct_struct))) =
            self.resolve_tag_name(tag_name)
        {
            Some(Type::Struct(struct_struct.clone()))
        } else {
            None
        }
    }

    pub const fn get_stack_size(&self) -> usize {
        self.max_stack_size
    }

    pub fn reset_stack(&mut self) {
        assert!(self.scopes.is_empty());
        self.max_stack_size = 0;
    }

    pub fn look_up_lvar(&self, name: &String) -> Option<Var> {
        for map in self.scopes.iter().rev() {
            if let Some(lvar) = map.get(name) {
                return Some(Var::LVar(lvar.clone()));
            }
        }
        None
    }

    pub fn look_up_gvar(&self, name: &String) -> Option<Var> {
        if let Some(gvar) = self.global.get(name).map(|gvar| Var::GVar(gvar.clone())) {
            return Some(gvar);
        }
        None
    }

    pub fn look_up_enum_variant(&self, name: &String) -> Option<Var> {
        if let Some(enum_variant) = self.resolve_enum_variant_name(name) {
            return Some(Var::EnumVariant(enum_variant));
        }

        None
    }

    pub fn look_up(&self, name: &String) -> Option<Var> {
        for map in self.scopes.iter().rev() {
            if let Some(lvar) = map.get(name) {
                return Some(Var::LVar(lvar.clone()));
            }
        }
        if let Some(gvar) = self.global.get(name).map(|gvar| Var::GVar(gvar.clone())) {
            return Some(gvar);
        }

        if let Some(enum_variant) = self.resolve_enum_variant_name(name) {
            return Some(Var::EnumVariant(enum_variant));
        }

        None
    }

    pub fn resolve_enum_variant_name(&self, name: &String) -> Option<EnumVariant> {
        for map in self.tag_scope.iter().rev() {
            let map_iter = map.values();
            for taged in map_iter {
                if let Taged::Enum(EnumTagKind::Enum(Enum {
                    tag: _,
                    members: map,
                })) = taged
                {
                    if let Some(value) = map.get(name) {
                        return Some(EnumVariant {
                            name: name.clone(),
                            value: *value,
                        });
                    }
                }
            }
        }
        None
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
        debug_info: DebugInfo,
        name: &str,
        ty: Type,
        is_extern: bool,
        init: Option<ConstInitializer>,
    ) -> Result<GVar, CompileError> {
        for scope in &self.scopes {
            if scope.contains_key(name) {
                return Err(CompileError::new_redefined_variable(
                    name.to_string(),
                    debug_info,
                    VariableKind::Local,
                ));
            }
        }
        if let Some(gvar) = self.global.get(name) {
            if !(gvar.is_extern || is_extern) {
                return Err(CompileError::new_redefined_variable(
                    name.to_string(),
                    debug_info,
                    VariableKind::Global,
                ));
            }
        }
        let gvar = GVar {
            name: name.to_string(),
            ty,
            init,
            is_extern,
        };
        self.insert_global_var_to_global_scope(gvar.clone());
        Ok(gvar)
    }

    pub fn register_lvar(
        &mut self,
        debug_info: DebugInfo,
        new_offset: &mut usize,
        name: &str,
        ty: Type,
    ) -> Result<LVar, CompileError> {
        for scope in &self.scopes {
            if scope.contains_key(name) {
                return Err(CompileError::new_redefined_variable(
                    name.to_string(),
                    debug_info,
                    VariableKind::Local,
                ));
            }
        }
        // if self.global.contains_key(name) {
        //     return Err(CompileError::new_redefined_variable(
        //         name.to_string(),
        //         debug_info,
        //         VariableKind::Global,
        //     ));
        // }
        let mut ty = ty;
        if let Type::InComplete(InCompleteKind::Struct(tag)) = &ty {
            let taged = self.look_up_struct_tag(tag);
            if let Some(Taged::Struct(StructTagKind::Struct(struct_struct))) = taged {
                ty = Type::Struct(struct_struct.clone());
            }
        };
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
    Struct(StructTagKind),
    Enum(EnumTagKind),
}

impl Taged {
    pub fn new_struct_tag(tag: String, names: Vec<String>, types: Vec<Type>) -> Self {
        Self::Struct(StructTagKind::Struct(Struct::new(tag, names, types)))
    }

    pub fn new_enum_tag(tag: String, enum_ident_map: BTreeMap<String, usize>) -> Self {
        Self::Enum(EnumTagKind::Enum(Enum {
            tag: Some(tag),
            members: enum_ident_map,
        }))
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum StructTagKind {
    Struct(Struct),
    OnlyTag(String),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum EnumTagKind {
    Enum(Enum),
    OnlyTag(String),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Struct {
    tag: Option<String>,
    members: Vec<StructMember>,
}

impl Struct {
    pub fn construct_members(names: Vec<String>, types: Vec<Type>) -> Vec<StructMember> {
        let mut struct_members = Vec::with_capacity(names.len());
        let mut offset = 0;
        for (name, ty) in names.into_iter().zip(types.into_iter()) {
            offset = aligned_offset(offset, &ty);
            struct_members.push(StructMember { name, offset, ty });
        }
        struct_members
    }

    pub fn new(tag: String, names: Vec<String>, types: Vec<Type>) -> Self {
        Self {
            tag: Some(tag),
            members: Self::construct_members(names, types),
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
pub struct Enum {
    tag: Option<String>,
    // Map of enum identifier to value
    members: BTreeMap<String, usize>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct GVar {
    pub name: String,
    pub ty: Type,
    pub init: Option<ConstInitializer>,
    pub is_extern: bool,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct EnumVariant {
    pub name: String,
    pub value: usize,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Var {
    GVar(GVar),
    LVar(LVar),
    EnumVariant(EnumVariant),
}

impl Var {
    pub fn ty(&self) -> Type {
        match self {
            Var::GVar(gvar) => gvar.ty.clone(),
            Var::LVar(lvar) => lvar.ty.clone(),
            Var::EnumVariant(_) => Type::Base(BaseType::Int),
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Func {
    pub name: String,
    pub static_variables: BTreeMap<String, GVar>,
    pub args: FuncArgs,
    pub ret: Type,
    pub debug_info: DebugInfo,
}

impl Func {
    pub const fn new_raw(
        name: String,
        static_variables: BTreeMap<String, GVar>,
        args: Vec<Type>,
        is_flexible_length_arg: bool,
        ret: Type,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            name,
            static_variables,
            args: FuncArgs::new(args, is_flexible_length_arg),
            ret,
            debug_info,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct FuncArgs {
    pub args: Vec<Type>,
    is_flexible_length: bool,
}

impl FuncArgs {
    pub const fn new(args: Vec<Type>, is_flexible_length: bool) -> Self {
        Self {
            args,
            is_flexible_length,
        }
    }

    pub const fn is_flexible_length(&self) -> bool {
        self.is_flexible_length
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ConstInitializer {
    Expr(ConstExpr),
    Array(Vec<ConstInitializer>),
}

impl ConstInitializer {
    pub fn try_new(
        analyzer: &mut Analyzer,
        initializer: Initializer,
    ) -> Result<Self, CompileError> {
        Ok(match initializer {
            Initializer::Expr(expr) => ConstInitializer::Expr(ConstExpr::try_eval_as_const(
                analyzer.traverse_expr(expr, None, BTreeSet::new())?,
            )?),
            Initializer::Array(array) => ConstInitializer::Array(
                array
                    .into_iter()
                    .map(|initializer| Self::try_new(analyzer, initializer))
                    .collect::<Result<Vec<ConstInitializer>, CompileError>>()?,
            ),
        })
    }

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

    pub fn get_debug_info(&self) -> DebugInfo {
        match self {
            ConstInitializer::Expr(expr) => expr.debug_info.clone(),
            ConstInitializer::Array(vec) => vec
                .first()
                .map(ConstInitializer::get_debug_info)
                .unwrap_or_default(),
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct ConstExpr {
    pub kind: ConstExprKind,
    pub ty: Type,
    pub debug_info: DebugInfo,
}

impl ConstExpr {
    pub const fn new_literal_with_type(n: isize, base_ty: BaseType, debug_info: DebugInfo) -> Self {
        match base_ty {
            BaseType::Int => Self::new_int(n as i32, debug_info),
            BaseType::Char => Self::new_char(n as i8, debug_info),
        }
    }

    pub fn null_ptr(debug_info: DebugInfo) -> Self {
        Self {
            kind: ConstExprKind::Int(0),
            ty: Type::ptr(Type::Void),
            debug_info,
        }
    }

    pub const fn new_int(n: i32, debug_info: DebugInfo) -> Self {
        Self {
            kind: ConstExprKind::Int(n),
            ty: Type::Base(BaseType::Int),
            debug_info,
        }
    }

    pub const fn new_char(n: i8, debug_info: DebugInfo) -> Self {
        Self {
            kind: ConstExprKind::Char(n),
            ty: Type::Base(BaseType::Int),
            debug_info,
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
        let debug_info = self.debug_info.clone();
        Ok(match unary_op {
            ConvUnaryOp::BitInvert => match &self.kind {
                ConstExprKind::Int(n) => ConstExpr::new_int(!*n, debug_info),
                ConstExprKind::Char(n) => ConstExpr::new_char(!*n, debug_info),
                ConstExprKind::Addr(_) => {
                    return Err(unimplemented_err!(format!(
                        "Const Expr which will be applied `~` unary op ({:?}) should be Literal.",
                        self
                    )))
                }
            },
            ConvUnaryOp::Increment(value) => {
                let n = self.get_num_lit()?;
                // This const expr has type `int`, so truncations is ok.
                #[allow(clippy::cast_possible_truncation)]
                ConstExpr::new_int(n as i32 + *value as i32, debug_info)
            }
            ConvUnaryOp::Decrement(value) => {
                let n = self.get_num_lit()?;
                // This const expr has type `int`, so truncations is ok.
                #[allow(clippy::cast_possible_truncation)]
                ConstExpr::new_int(n as i32 - *value as i32, debug_info)
            }
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
    fn try_eval_as_const(expr: ConvExpr) -> Result<Self, CompileError> {
        let debug_info = expr.debug_info.clone();
        let ty = expr.ty.clone();
        let kind = expr.kind;
        let bool_to_isize = |b| if b { 1 } else { 0 };
        let num_expr = |num: isize, debug_info, ty: BaseType| {
            let kind = match ty {
                // Truncations are ok because each const expr's type has its bit size.
                #[allow(clippy::cast_possible_truncation)]
                BaseType::Int => ConstExprKind::Int(num as i32),
                #[allow(clippy::cast_possible_truncation)]
                BaseType::Char => ConstExprKind::Char(num as i8),
            };
            ConstExpr {
                kind,
                ty: Type::Base(ty),
                debug_info,
            }
        };
        Ok(match kind {
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Add,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(*lhs)?.get_num_lit()?
                    + Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Sub,
                lhs,
                rhs,
            }) => {
                num_expr(
                    Self::try_eval_as_const(*lhs)?.get_num_lit()?
                        - Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                    debug_info,
                    *ty.get_base().unwrap(), // This Option is always `Some`, because literal or not is checked in `get_num_lit`.
                )
            }
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Mul,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(*lhs)?.get_num_lit()?
                    * Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Div,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(*lhs)?.get_num_lit()?
                    / Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Rem,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(*lhs)?.get_num_lit()?
                    % Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Eq,
                lhs,
                rhs,
            }) => num_expr(
                bool_to_isize(
                    Self::try_eval_as_const(*lhs)?.get_num_lit()?
                        == Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                ),
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Ne,
                lhs,
                rhs,
            }) => num_expr(
                bool_to_isize(
                    Self::try_eval_as_const(*lhs)?.get_num_lit()?
                        != Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                ),
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Lt,
                lhs,
                rhs,
            }) => num_expr(
                bool_to_isize(
                    Self::try_eval_as_const(*lhs)?.get_num_lit()?
                        < Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                ),
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::Le,
                lhs,
                rhs,
            }) => num_expr(
                bool_to_isize(
                    Self::try_eval_as_const(*lhs)?.get_num_lit()?
                        <= Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                ),
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::LShift,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(*lhs)?.get_num_lit()?
                    << Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::RShift,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(*lhs)?.get_num_lit()?
                    >> Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::Binary(ConvBinary {
                kind: ConvBinOpKind::BitWiseAnd,
                lhs,
                rhs,
            }) => num_expr(
                Self::try_eval_as_const(*lhs)?.get_num_lit()?
                    & Self::try_eval_as_const(*rhs)?.get_num_lit()?,
                debug_info,
                *ty.get_base().unwrap(),
            ),
            ConvExprKind::GVar(ref gvar) => {
                // TODO: check if gvar is const or not
                if let Some(val) = gvar.init.as_ref().and_then(ConstInitializer::get_num_lit) {
                    eprintln!("Have to check {:?} is const or not.", &gvar);
                    return Ok(num_expr(val, debug_info, BaseType::Int));
                }
                return Err(CompileError::new_const_expr_error(debug_info, kind));
            }
            ConvExprKind::Num(num) => num_expr(num, debug_info, *ty.get_base().unwrap()),
            ConvExprKind::LVar(_)
            | ConvExprKind::Assign(_, _)
            | ConvExprKind::Func(..)
            | ConvExprKind::Deref(_)
            | ConvExprKind::Asm(..)
            | ConvExprKind::Member {
                struct_expr: _,
                minus_offset: _,
            }
            | ConvExprKind::OpAssign(_, _, _)
            | ConvExprKind::FuncPtr(_, _)
            | ConvExprKind::Cast(_, CastKind::Base2FuncPtr(_, _)) => {
                return Err(CompileError::new_const_expr_error(debug_info, kind))
            }
            ConvExprKind::Cast(expr, CastKind::Base2Base(to, _)) => num_expr(
                Self::try_eval_as_const(*expr)?.get_num_lit()?,
                debug_info,
                to,
            ),
            ConvExprKind::Cast(expr, CastKind::FromVoidPtr { ptr_to }) => {
                let mut const_expr = Self::try_eval_as_const(*expr)?;
                const_expr.ty = Type::Ptr(Box::new(ptr_to));
                const_expr
            }
            ConvExprKind::Cast(expr, CastKind::ToVoidPtr { ptr_to: _ }) => {
                let mut const_expr = Self::try_eval_as_const(*expr)?;
                const_expr.ty = Type::Ptr(Box::new(Type::Void));
                const_expr
            }
            ConvExprKind::Cast(
                expr,
                CastKind::Ptr2Ptr {
                    from: _,
                    to,
                    cast_kind: _,
                },
            ) => {
                let mut const_expr = Self::try_eval_as_const(*expr)?;
                const_expr.ty = Type::Ptr(Box::new(to));
                const_expr
            }
            ConvExprKind::Cast(expr, CastKind::NoCast) => Self::try_eval_as_const(*expr)?,
            ConvExprKind::Addr(expr) => {
                let expr_debug_info = expr.debug_info;
                let expr_ty = expr.ty.clone();
                let gvar = if let ConvExprKind::GVar(gvar) = expr.kind {
                    gvar
                } else {
                    return Err(CompileError::new_const_expr_error(debug_info, expr.kind));
                };
                ConstExpr {
                    kind: ConstExprKind::Addr(Box::new(gvar)),
                    ty: Type::Ptr(Box::new(expr_ty)),
                    debug_info: expr_debug_info,
                }
            }
            ConvExprKind::Unary(unary_op, expr) => {
                Self::try_eval_as_const(*expr)?.apply_unary_op(&unary_op)?
            }
            ConvExprKind::Conditional { cond, then, els } => {
                let cond_val = Self::try_eval_as_const(*cond)?.get_num_lit()?;
                if cond_val == 0 {
                    Self::try_eval_as_const(*els)?
                } else {
                    Self::try_eval_as_const(*then)?
                }
            }
            ConvExprKind::PostfixIncrement(expr, _) | ConvExprKind::PostfixDecrement(expr, _) => {
                let expr_debug_info = expr.debug_info.clone();
                let value = Self::try_eval_as_const(*expr)?.get_num_lit()?;
                // This truncation is ok because this const expr has the type `int`
                #[allow(clippy::cast_possible_truncation)]
                ConstExpr {
                    kind: ConstExprKind::Int(value as i32),
                    ty: Type::Base(BaseType::Int),
                    debug_info: expr_debug_info,
                }
            }
            ConvExprKind::Comma(_, rhs) => Self::try_eval_as_const(*rhs)?,
            ConvExprKind::Block(_, _) => todo!(),
        })
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Void,
    /// Tag unresolved struct or union
    InComplete(InCompleteKind),
    Base(BaseType),
    Ptr(Box<Type>),
    Func {
        ret_ty: Box<Type>,
        args: Vec<Type>,
        is_flexible: bool,
    },
    Array(Box<Type>, usize),
    // Tag resolved struct
    Struct(Struct),
    // Enum(Enum),
}

impl Type {
    pub fn ty_eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Type::Void, Type::Void) => true,
            (Type::InComplete(tag_name_lhs), Type::InComplete(tag_name_rhs)) => {
                tag_name_lhs == tag_name_rhs
            }
            (Type::Base(base_ty_lhs), Type::Base(base_ty_rhs)) => base_ty_lhs == base_ty_rhs,
            (Type::Ptr(ptr_to_lhs), Type::Ptr(ptr_to_rhs)) => ptr_to_lhs.ty_eq(ptr_to_rhs),
            (
                Type::Func {
                    ret_ty: return_ty_lhs,
                    args: arg_ty_lhs,
                    is_flexible: is_flexible_lhs,
                },
                Type::Func {
                    ret_ty: return_ty_rhs,
                    args: arg_ty_rhs,
                    is_flexible: is_flexible_rhs,
                },
            ) => {
                return_ty_lhs.ty_eq(return_ty_rhs)
                    && is_flexible_lhs == is_flexible_rhs
                    && arg_ty_lhs
                        .iter()
                        .zip(arg_ty_rhs.iter())
                        .all(|(lhs_ty, rhs_ty)| lhs_ty.ty_eq(rhs_ty))
            }
            (
                Type::Array(array_base_ty_lhs, length_lhs),
                Type::Array(array_base_ty_rhs, length_rhs),
            ) => array_base_ty_lhs == array_base_ty_rhs && length_lhs == length_rhs,
            (Type::Struct(struct_lhs), Type::Struct(struct_rhs)) => struct_lhs == struct_rhs,
            (Type::Struct(struct_lhs), Type::InComplete(InCompleteKind::Struct(tag_name_rhs))) => {
                struct_lhs.tag.as_ref() == Some(tag_name_rhs)
            }
            (Type::InComplete(InCompleteKind::Struct(tag_name_lhs)), Type::Struct(struct_rhs)) => {
                Some(tag_name_lhs) == struct_rhs.tag.as_ref()
            }
            _ => false,
        }
    }

    pub const fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }

    /// if `self` is array, return ptr-converted self
    pub fn into_ptr(self) -> Self {
        if let Type::Array(base_ty, _) = self {
            Type::Ptr(base_ty)
        } else {
            self
        }
    }

    pub fn get_ptr_to_recursively(&self) -> Option<&Self> {
        let mut ptr_to = self.get_ptr_to()?;
        while let Some(next_ptr_to) = ptr_to.get_ptr_to() {
            ptr_to = next_ptr_to;
        }
        return Some(ptr_to);
    }

    pub const fn is_func(&self) -> bool {
        matches!(self, Type::Func { .. })
    }
}

impl From<Func> for Type {
    fn from(func: Func) -> Self {
        Type::Func {
            ret_ty: Box::new(func.ret),
            args: func.args.args,
            is_flexible: func.args.is_flexible_length,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum InCompleteKind {
    Struct(String),
    Enum(String),
}

impl Type {
    pub fn ptr(ty: Type) -> Self {
        Type::Ptr(Box::new(ty))
    }

    pub const fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub const fn is_void_ptr(&self) -> bool {
        if let Type::Ptr(ptr_to) = self {
            ptr_to.is_void()
        } else {
            false
        }
    }

    pub const fn get_ptr_to(&self) -> Option<&Type> {
        match self {
            Type::Base(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Array(_, _)
            | Type::Struct(_)
            | Type::Void => None,
            Type::Ptr(ptr_to) => Some(ptr_to),
            Type::InComplete(_) => todo!(),
        }
    }

    /// Get the base type of array recursively, if the type is not an array just return its reference.
    pub const fn get_array_base_recursively(&self) -> &Type {
        match self {
            Type::Array(base_ty, _) => base_ty.get_array_base_recursively(),
            _ => self,
        }
    }

    pub const fn get_array_base(&self) -> Option<&Type> {
        match self {
            Type::Base(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Struct(_)
            | Type::Void
            | Type::Ptr(_) => None,
            Type::Array(base, _) => Some(base),
            Type::InComplete(_) => todo!(),
        }
    }
}

impl Analyzer {
    #[allow(clippy::too_many_lines)]
    pub fn resolve_name_and_convert_to_type(
        &mut self,
        ty_spec: &Vec<DeclarationSpecifier>,
        debug_info: DebugInfo,
    ) -> Result<Type, CompileError> {
        let mut is_typedef = false;
        let mut _is_extern = false;
        let mut is_static = false;
        let mut declaration_specifiers = Vec::with_capacity(ty_spec.len());
        for declaration_specifier in ty_spec {
            match *declaration_specifier {
                DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef) => {
                    is_typedef = true
                }
                DeclarationSpecifier::StorageClass(StorageClassSpecifier::Extern) => {
                    _is_extern = true
                }
                DeclarationSpecifier::StorageClass(StorageClassSpecifier::Static) => {
                    is_static = true
                }
                _ => declaration_specifiers.push(declaration_specifier.clone()),
            }
        }
        // In this case, the enum specifier and int specifier have no relationships.
        #[allow(clippy::match_same_arms)]
        if declaration_specifiers.len() == 1 {
            Ok(
                // TODO: Add support for more than one type specifier such as `long int`, `long long`...
                match declaration_specifiers
                    .first()
                    .map_or_else(|| None, DeclarationSpecifier::get_type_specifier)
                    .unwrap()
                {
                    TypeSpecifier::Int => Type::Base(BaseType::Int),
                    TypeSpecifier::Char => Type::Base(BaseType::Char),
                    TypeSpecifier::Void => Type::Void,
                    TypeSpecifier::StructOrUnion(StructOrUnionSpec::WithTag(tag)) => {
                        match self.scope.look_up_struct_tag(tag.as_str()) {
                            Some(Taged::Struct(StructTagKind::Struct(Struct {
                                tag: _,
                                members: _,
                            }))) => Type::InComplete(InCompleteKind::Struct(tag.clone())),
                            Some(Taged::Struct(StructTagKind::OnlyTag(tag))) => {
                                Type::InComplete(InCompleteKind::Struct(tag.clone()))
                            }
                            Some(Taged::Enum(_)) => {
                                // All enum variant has type of int
                                Type::Base(BaseType::Int)
                            }
                            None => {
                                if is_typedef {
                                    return Ok(Type::InComplete(InCompleteKind::Struct(
                                        tag.clone(),
                                    )));
                                }
                                // TODO: support struct which has the self-type member
                                return Err(unimplemented_err!(
                                    debug_info,
                                    "tag of struct declaration with should be declared before."
                                ));
                            }
                        }
                    }
                    TypeSpecifier::StructOrUnion(StructOrUnionSpec::WithList(name, vec)) => {
                        // register tag for now
                        if let Some(ref name) = name {
                            self.scope.register_tag(
                                name.to_string(),
                                Taged::Struct(StructTagKind::OnlyTag(name.to_string())),
                            );
                        }
                        let types = vec
                            .iter()
                            .map(|struct_declaration| {
                                let mut ty = struct_declaration
                                    .get_type(self, struct_declaration.debug_info.clone())?;
                                if let Type::InComplete(InCompleteKind::Struct(tag)) = &ty {
                                    let taged = self.scope.look_up_struct_tag(tag);
                                    if let Some(Taged::Struct(StructTagKind::Struct(
                                        struct_struct,
                                    ))) = taged
                                    {
                                        ty = Type::Struct(struct_struct.clone());
                                    }
                                };
                                Ok(ty)
                            })
                            .collect::<Result<_, CompileError>>()?;
                        let names = vec
                            .iter()
                            .map(|struct_declaration| struct_declaration.ident_name().to_string())
                            .collect::<Vec<String>>();
                        let constructed_members = Struct::construct_members(names, types);
                        // tag compatibility check
                        if let Some(ref name) = name {
                            if let Some(Taged::Struct(StructTagKind::Struct(Struct {
                                tag: _,
                                members: looked_up_members,
                            }))) = self.scope.look_up_struct_tag(name.as_str())
                            {
                                if *looked_up_members != constructed_members {
                                    return Err(unimplemented_err!( debug_info, "this declaration's tag is incompatible with another tag whose tag-name is same."));
                                }
                            } else {
                                self.scope.register_tag(
                                    name.clone(),
                                    Taged::Struct(StructTagKind::Struct(Struct {
                                        tag: Some(name.clone()),
                                        members: constructed_members.clone(),
                                    })),
                                );
                            }
                        }

                        name.as_ref().map_or_else(
                            || {
                                Type::Struct(Struct {
                                    tag: None,
                                    members: constructed_members,
                                })
                            },
                            |name| Type::InComplete(InCompleteKind::Struct(name.clone())),
                        )
                    }
                    TypeSpecifier::Enum(
                        EnumSpec::WithList(Some(name), _) | EnumSpec::WithTag(name),
                    ) => {
                        if self.scope.resolve_tag_name(&name).is_none() {
                            if is_typedef {
                                return Ok(Type::InComplete(InCompleteKind::Enum(name)));
                            }
                            return Err(unimplemented_err!(
                                debug_info,
                                "While resolving enum tag, tag is not found in the scope."
                            ));
                        }
                        Type::Base(BaseType::Int)
                    }
                    TypeSpecifier::Enum(EnumSpec::WithList(None, _)) => Type::Base(BaseType::Int),
                    TypeSpecifier::TypeDefName(ty) => ty,
                },
            )
        } else {
            unimplemented!(
                "Convert `declaration specifier with more than one specifier` into analyzed enum `Type` is not supported yet."
            )
        }
    }

    pub fn resolve_incomplete_type_second_depth(
        &mut self,
        ty: Type,
        debug_info: DebugInfo,
    ) -> Result<Type, CompileError> {
        match ty {
            ty @ (Type::Struct(_)
            | Type::Void
            | Type::Base(_)
            | Type::Ptr(_)
            | Type::Func { .. }) => Ok(ty),
            Type::InComplete(_) => self.resolve_incomplete_type(ty, debug_info),
            Type::Array(base, size) => {
                let base = self.resolve_incomplete_type(*base, debug_info)?;
                Ok(Type::Array(Box::new(base), size))
            }
        }
    }

    pub fn resolve_incomplete_type(
        &mut self,
        ty: Type,
        debug_info: DebugInfo,
    ) -> Result<Type, CompileError> {
        if let Type::InComplete(InCompleteKind::Struct(name)) = ty {
            let got = self.scope.resolve_tag_name(&name);
            match got {
                Some(Taged::Struct(StructTagKind::Struct(structure))) => {
                    Ok(Type::Struct(structure.clone()))
                }
                Some(Taged::Struct(StructTagKind::OnlyTag(name))) => {
                    Err(CompileError::new_undeclared_error(
                        name.clone(),
                        debug_info,
                        VariableKind::Struct,
                    ))
                }
                Some(Taged::Enum(EnumTagKind::OnlyTag(name))) => {
                    Err(CompileError::new_undeclared_error(
                        name.clone(),
                        debug_info,
                        VariableKind::Enum,
                    ))
                }
                Some(Taged::Enum(EnumTagKind::Enum(_))) => Ok(Type::Base(BaseType::Int)),
                None => todo!(),
            }
        } else {
            Ok(ty)
        }
    }

    /// Register struct tag as just `StructTagKind::OnlyTag(name)`
    pub fn register_struct_tag_from_type_specifier(
        &mut self,
        declaration_specifiers: &Vec<DeclarationSpecifier>,
    ) -> Result<(), CompileError> {
        for declaration_specifer in declaration_specifiers {
            if let DeclarationSpecifier::Type(TypeSpecifier::StructOrUnion(
                StructOrUnionSpec::WithTag(name) | StructOrUnionSpec::WithList(Some(name), _),
            )) = declaration_specifer
            {
                self.scope.register_tag(
                    name.clone(),
                    Taged::Struct(StructTagKind::OnlyTag(name.clone())),
                );
            }
            match declaration_specifer {
                DeclarationSpecifier::Type(TypeSpecifier::StructOrUnion(
                    StructOrUnionSpec::WithTag(name),
                )) => {
                    self.scope.register_tag(
                        name.clone(),
                        Taged::Struct(StructTagKind::OnlyTag(name.clone())),
                    );
                }
                DeclarationSpecifier::Type(TypeSpecifier::StructOrUnion(
                    StructOrUnionSpec::WithList(Some(name), list),
                )) => {
                    let names = list
                        .iter()
                        .map(|struct_declaration| struct_declaration.ident_name().to_string())
                        .collect();
                    let types = list
                        .iter()
                        .map(|struct_declaration| {
                            struct_declaration.get_type(self, struct_declaration.debug_info.clone())
                        })
                        .collect::<Result<_, CompileError>>()?;
                    self.scope.register_tag(
                        name.clone(),
                        Taged::Struct(StructTagKind::Struct(Struct::new(
                            name.clone(),
                            names,
                            types,
                        ))),
                    );
                }
                _ => {}
            }
            // Note: Enum has no necessary to register tag before analyze its variants.
        }
        Ok(())
    }
}

impl Type {
    pub fn size_of(&self) -> usize {
        match self {
            Type::Base(BaseType::Int) => 4,
            Type::Base(BaseType::Char)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Void => 1,
            Type::Ptr(_) => 8,
            Type::Array(ty, size) => ty.size_of() * *size,
            Type::Struct(struct_struct) => struct_struct.size_of(),
            Type::InComplete(InCompleteKind::Enum(_)) => Type::Base(BaseType::Int).size_of(),
            Type::InComplete(_) => todo!("{:?}", self),
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            Type::Void => todo!("return appropriate error"),
            Type::Base(BaseType::Int) => 4,
            Type::Base(BaseType::Char) => 1,
            Type::Ptr(_) => 8,
            Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::InComplete(_) => todo!(),
            Type::Array(base_ty, _) => base_ty.align_of(),
            Type::Struct(struct_struct) => struct_struct.align_of(),
        }
    }

    pub const fn base_type(&self) -> &Type {
        match self {
            ty @ Type::Base(_) => ty,
            Type::Ptr(base)
            | Type::Func {
                ret_ty: base,
                args: _,
                is_flexible: _,
            }
            | Type::Array(base, _) => base,
            Type::Struct(_) | Type::Void | Type::InComplete(_) => todo!(),
        }
    }

    pub const fn get_base(&self) -> Option<&BaseType> {
        match self {
            Type::Base(base) => Some(base),
            Type::Ptr(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Array(_, _) => None,
            Type::Struct(_) | Type::Void | Type::InComplete(_) => todo!(),
        }
    }

    pub const fn is_base(&self) -> bool {
        match self {
            Type::Base(_) => true,
            Type::Void
            | Type::Ptr(_)
            | Type::Func {
                ret_ty: _,
                args: _,
                is_flexible: _,
            }
            | Type::Array(_, _)
            | Type::Struct(_) => false,
            Type::InComplete(_) => todo!(),
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LoopControlKind {
    Case(isize, Box<ConvStmt>),
    Default(Box<ConvStmt>),
    Break,
    Continue,
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
            BinOpKind::Ne => Some(ConvBinOpKind::Ne),
            BinOpKind::LShift => Some(ConvBinOpKind::LShift),
            BinOpKind::RShift => Some(ConvBinOpKind::RShift),
            BinOpKind::BitWiseAnd => Some(ConvBinOpKind::BitWiseAnd),
            BinOpKind::LogicalOr | BinOpKind::LogicalAnd | BinOpKind::Ge | BinOpKind::Gt => None,
        }
    }
}
