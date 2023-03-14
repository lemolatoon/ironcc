use crate::{
    error::CompileError,
    parse::expr::BinOpKind,
    tokenize::{debug_infos::DebugInfo, tokenize::AssignBinOpToken},
    unimplemented_err,
};

use super::{
    analyze::{GVar, LVar},
    stmt::ConvStmt,
    types::{BaseType, Type},
};

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

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum FuncCallTargetKind {
    Label(String),
    Expr(Box<ConvExpr>),
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
            BinOpKind::Ne => Some(ConvBinOpKind::Ne),
            BinOpKind::LShift => Some(ConvBinOpKind::LShift),
            BinOpKind::RShift => Some(ConvBinOpKind::RShift),
            BinOpKind::BitWiseAnd => Some(ConvBinOpKind::BitWiseAnd),
            BinOpKind::LogicalOr | BinOpKind::LogicalAnd | BinOpKind::Ge | BinOpKind::Gt => None,
        }
    }
}
