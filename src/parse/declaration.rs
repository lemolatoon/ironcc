use std::collections::BTreeSet;

use crate::{
    analyze::{
        analyze::{Analyzer, ConstExpr, ConstInitializer, ConvExpr, ConvStmt, LVar},
        types::Type,
    },
    error::CompileError,
    tokenize::{debug_infos::DebugInfo, tokenize::AssignBinOpToken},
    unimplemented_err,
};

use super::{expr::Expr, parse::EnumSpec};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
/// Declaration
pub struct Declaration {
    // declaration-specifier
    pub declaration_specifiers: Vec<DeclarationSpecifier>,
    // init-declarator
    pub init_declarator: Option<InitDeclarator>,
    pub debug_info: DebugInfo,
}

impl Declaration {
    // TODO: need or not
    pub const fn new(
        declaration_specifiers: Vec<DeclarationSpecifier>,
        n_star: usize,
        direct_declarator: DirectDeclarator,
        initializer: Option<Initializer>,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            declaration_specifiers,
            init_declarator: Some(InitDeclarator {
                declarator: Declarator::new(n_star, direct_declarator),
                initializer,
            }),
            debug_info,
        }
    }

    pub fn ident_name(&self) -> Option<&str> {
        self.init_declarator
            .as_ref()
            .map(|init_declarator| init_declarator.declarator.direct_declarator.ident_name())
    }

    pub fn ty(&self, analyzer: &mut Analyzer, debug_info: DebugInfo) -> Result<Type, CompileError> {
        let converted_type = analyzer
            .resolve_name_and_convert_to_type(&self.declaration_specifiers, debug_info.clone())?;
        analyzer.get_type(
            converted_type,
            &self
                .init_declarator
                .as_ref()
                .ok_or_else(|| {
                    unimplemented_err!(debug_info, "get_type for struct is not yet implemented.")
                })?
                .declarator,
            None,
        )
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
}

impl InitDeclarator {
    pub fn ident_name(&self) -> &str {
        self.declarator.direct_declarator.ident_name()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Declarator {
    pub n_star: usize,
    pub direct_declarator: DirectDeclarator,
}

impl Declarator {
    pub const fn new(n_star: usize, direct_declarator: DirectDeclarator) -> Self {
        Self {
            n_star,
            direct_declarator,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum DirectDeclarator {
    Ident(String),
    Array(Box<DirectDeclarator>, Option<Expr>),
    Func(
        Box<DirectDeclarator>,
        Vec<Declaration>,
        /*is_flexible arg*/ bool,
    ),
    Declarator(Box<Declarator>),
}

impl DirectDeclarator {
    pub fn ident_name(&self) -> &str {
        match self {
            DirectDeclarator::Ident(name) => name,
            DirectDeclarator::Func(direct_declarator, _, _)
            | DirectDeclarator::Array(direct_declarator, _) => direct_declarator.ident_name(),
            DirectDeclarator::Declarator(declarator) => declarator.direct_declarator.ident_name(),
        }
    }

    pub fn args(&self) -> Option<Vec<Declaration>> {
        match self {
            DirectDeclarator::Ident(_) => None,
            DirectDeclarator::Array(direct_declarator, _) => direct_declarator.args(),
            DirectDeclarator::Declarator(declarator) => declarator.direct_declarator.args(),
            DirectDeclarator::Func(_, args, _) => Some(args.clone()),
        }
    }

    pub fn is_func(&self) -> bool {
        matches!(self, DirectDeclarator::Func(_, _, _))
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Initializer {
    // <initializer>    :=
    Expr(Expr),              // <assign>
    Array(Vec<Initializer>), // "{" <initializer-list> "," "}"
}

impl Initializer {
    pub fn map(
        self,
        f: &mut impl FnMut(Expr) -> Result<ConstExpr, CompileError>,
    ) -> Result<ConstInitializer, CompileError> {
        Ok(match self {
            Initializer::Expr(expr) => ConstInitializer::Expr(f(expr)?),
            Initializer::Array(init_list) => ConstInitializer::Array(
                init_list
                    .into_iter()
                    .map(|initializer| initializer.map(f))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
        })
    }

    pub fn get_debug_info(&self) -> Result<DebugInfo, CompileError> {
        match self {
            Initializer::Expr(expr) => Ok(expr.debug_info.clone()),
            Initializer::Array(vec) => vec.first().map_or_else(
                || {
                    Err(unimplemented_err!(
                        "Initializer has to have at least one element."
                    ))
                },
                |initializer| Ok(initializer.get_debug_info()),
            )?,
        }
    }

    pub fn gen_assign_exprs_stmt(
        self,
        analyzer: &mut Analyzer,
        lvar: LVar, /* initialized local variable */
        left_ty: &Type,
        debug_info: DebugInfo,
    ) -> Result<ConvStmt, CompileError> {
        match self {
            Initializer::Expr(expr) => {
                let rhs = analyzer.traverse_expr(expr, None, BTreeSet::new())?;
                Ok(ConvStmt::new_expr(
                    Analyzer::new_assign_expr_with_type_check(
                        ConvExpr::new_lvar_raw(
                            lvar,
                            left_ty.get_array_base_recursively().clone(),
                            debug_info.clone(),
                        ),
                        rhs,
                        AssignBinOpToken::Eq,
                        debug_info,
                    )?,
                ))
            }
            Initializer::Array(init_list) => {
                Ok(ConvStmt::new_block(
                    init_list
                        .into_iter()
                        .enumerate()
                        .map(|(idx, init)| {
                            let init_debug_info = init.get_debug_info()?;
                            let array_base_ty = lvar.ty.get_array_base().ok_or_else(|| {
                                unimplemented_err!(
                                    init_debug_info.clone(),
                                    "Initializer should have compatible array dimension"
                                )
                            })?;
                            let stmts = init.gen_assign_exprs_stmt(
                                analyzer,
                                /* lvar[idx] */
                                LVar::new_raw(
                                    lvar.offset - idx * array_base_ty.size_of(),
                                    array_base_ty.clone(),
                                ),
                                left_ty,
                                init_debug_info,
                            )?;
                            Ok(stmts)
                        })
                        .collect::<Result<Vec<ConvStmt>, CompileError>>()?,
                ))
            }
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum DeclarationSpecifier {
    Type(TypeSpecifier),
    StorageClass(StorageClassSpecifier),
}

impl DeclarationSpecifier {
    pub fn get_type_specifier(&self) -> Option<TypeSpecifier> {
        match self {
            DeclarationSpecifier::Type(type_specifier) => Some(type_specifier.clone()),
            _ => None,
        }
    }

    pub fn get_storage_class_specifier(&self) -> Option<StorageClassSpecifier> {
        match self {
            DeclarationSpecifier::StorageClass(storage_class_specifier) => {
                Some(storage_class_specifier.clone())
            }
            _ => None,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum StorageClassSpecifier {
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum TypeSpecifier {
    Int,
    Char,
    Void,
    StructOrUnion(StructOrUnionSpec),
    Enum(EnumSpec),
    TypeDefName(Type),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum StructOrUnionSpec {
    WithList(Option<String>, Vec<StructDeclaration>),
    WithTag(String),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct StructDeclaration {
    pub debug_info: DebugInfo,
    // TODO: Add type-qualifier
    pub(crate) ty_spec: Vec<TypeSpecifier>,
    pub(crate) declarator: Declarator,
}

impl StructDeclaration {
    pub fn ident_name(&self) -> &str {
        self.declarator.direct_declarator.ident_name()
    }

    pub fn get_type(
        &self,
        analyzer: &mut Analyzer,
        debug_info: DebugInfo,
    ) -> Result<Type, CompileError> {
        let conveted_type = analyzer.resolve_name_and_convert_to_type(
            &self
                .ty_spec
                .iter()
                .map(|ty_spec| DeclarationSpecifier::Type(ty_spec.clone()))
                .collect(),
            debug_info,
        )?;
        analyzer.get_type(conveted_type, &self.declarator, None)
    }
}
