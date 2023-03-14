use crate::tokenize::{debug_infos::DebugInfo, tokenize::AssignBinOpToken};

use super::parse::TypeName;

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub debug_info: DebugInfo,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ExprKind {
    Binary(Binary),
    Comma(Box<Expr>, Box<Expr>),
    Num(isize),
    StrLit(String),
    Unary(UnaryOp, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>, AssignBinOpToken),
    Ident(String),
    Func(String, Vec<Expr>),
    Deref(Box<Expr>),
    Addr(Box<Expr>),
    SizeOf(SizeOfOperandKind),
    Array(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, String),
    Arrow(Box<Expr>, String),
    Conditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    },
    PostfixIncrement(Box<Expr>),
    PostfixDecrement(Box<Expr>),
    UnaryIncrement(Box<Expr>),
    UnaryDecrement(Box<Expr>),
    Asm(String),
    NullPtr,
    BuiltinVaStart(Box<Expr>, Box<Expr>),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum SizeOfOperandKind {
    Expr(Box<Expr>),
    Type(TypeName),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum UnaryOp {
    Plus,
    Increment,
    Minus,
    Decrement,
    BitInvert,
    LogicalNot,
}

impl Expr {
    pub fn new_comma(lhs: Expr, rhs: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Comma(Box::new(lhs), Box::new(rhs)),
            debug_info,
        }
    }
    pub fn new_inline_asm(asm: String, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Asm(asm),
            debug_info,
        }
    }

    pub fn new_null_ptr(debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::NullPtr,
            debug_info,
        }
    }

    pub fn new_conditional(cond: Expr, then: Expr, els: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Conditional {
                cond: Box::new(cond),
                then: Box::new(then),
                els: Box::new(els),
            },
            debug_info,
        }
    }
    pub fn new_member(expr: Expr, ident: String, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Member(Box::new(expr), ident),
            debug_info,
        }
    }
    pub fn new_arrow(expr: Expr, ident: String, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Arrow(Box::new(expr), ident),
            debug_info,
        }
    }

    pub fn new_postfix_increment(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::PostfixIncrement(Box::new(expr)),
            debug_info,
        }
    }

    pub fn new_postfix_decrement(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::PostfixDecrement(Box::new(expr)),
            debug_info,
        }
    }

    pub fn new_unary_increment(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::UnaryIncrement(Box::new(expr)),
            debug_info,
        }
    }

    pub fn new_unary_decrement(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::UnaryDecrement(Box::new(expr)),
            debug_info,
        }
    }
    pub fn new_array(expr: Expr, index: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Array(Box::new(expr), Box::new(index)),
            debug_info,
        }
    }

    pub fn new_binary(kind: BinOpKind, lhs: Expr, rhs: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Binary(Binary::new(kind, Box::new(lhs), Box::new(rhs))),
            debug_info,
        }
    }

    pub const fn new_num(num: isize, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Num(num),
            debug_info,
        }
    }

    pub const fn new_str_lit(letters: String, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::StrLit(letters),
            debug_info,
        }
    }

    pub const fn new_lvar(name: String, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Ident(name),
            debug_info,
        }
    }

    pub fn new_unary(kind: UnaryOp, expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Unary(kind, Box::new(expr)),
            debug_info,
        }
    }

    pub fn new_assign(
        lhs: Expr,
        rhs: Expr,
        assign_bin_op_token: AssignBinOpToken,
        debug_info: DebugInfo,
    ) -> Self {
        // pos is Position of TokenKind::Eq (i.e. `=`)
        Self {
            kind: ExprKind::Assign(Box::new(lhs), Box::new(rhs), assign_bin_op_token),
            debug_info,
        }
    }

    pub fn new_func(name: String, args: Vec<Expr>, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Func(name, args),
            debug_info,
        }
    }

    pub fn new_built_in_va_start(ap: Expr, last: Expr) -> Self {
        let debug_info = ap.debug_info.clone();
        Self {
            kind: ExprKind::BuiltinVaStart(Box::new(ap), Box::new(last)),
            debug_info,
        }
    }

    pub fn new_deref(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Deref(Box::new(expr)),
            debug_info,
        }
    }

    pub fn new_addr(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::Addr(Box::new(expr)),
            debug_info,
        }
    }

    pub fn new_expr_sizeof(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::SizeOf(SizeOfOperandKind::Expr(Box::new(expr))),
            debug_info,
        }
    }

    pub fn new_type_sizeof(type_name: TypeName, debug_info: DebugInfo) -> Self {
        Self {
            kind: ExprKind::SizeOf(SizeOfOperandKind::Type(type_name)),
            debug_info,
        }
    }

    #[cfg(test)]
    pub fn kind_eq(&self, lhs: &Expr) -> bool {
        lhs == self
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Binary {
    pub kind: BinOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn new(kind: BinOpKind, lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Self { kind, lhs, rhs }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum BinOpKind {
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
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
    /// The `!=` operator (Not equal to)
    Ne,
    /// The `<<` operator
    LShift,
    /// The `>>` operator
    RShift,
    /// The `&` operator (bit wise and)
    BitWiseAnd,
    /// The `||` operator (logical or)
    LogicalOr,
    /// The `&&` operator (logical and)
    LogicalAnd,
}
