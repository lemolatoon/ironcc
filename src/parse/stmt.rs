use crate::tokenize::debug_infos::DebugInfo;

use super::{
    declaration::Declaration,
    expr::Expr,
    parse::{ForInitKind, LabelKind, StmtKind},
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub debug_info: DebugInfo,
}

impl Stmt {
    pub const fn expr(expr: Expr, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::Expr(expr),
            debug_info,
        }
    }

    pub const fn ret(expr: Option<Expr>, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::Return(expr),
            debug_info,
        }
    }

    pub fn new_block(stmts: Vec<Stmt>, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::Block(stmts),
            debug_info,
        }
    }

    pub fn new_if(cond: Expr, then: Stmt, els: Option<Stmt>, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::If(cond, Box::new(then), els.map(Box::new)),
            debug_info,
        }
    }

    pub fn new_while(cond: Expr, then: Stmt, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::While(cond, Box::new(then)),
            debug_info,
        }
    }

    pub fn new_for(
        init: Option<ForInitKind>,
        cond: Option<Expr>,
        inc: Option<Expr>,
        then: Stmt,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            kind: StmtKind::For(init, cond, inc, Box::new(then)),
            debug_info,
        }
    }

    pub fn new_switch(expr: Expr, stmt: Stmt, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::Switch(expr, Box::new(stmt)),
            debug_info,
        }
    }

    pub fn new_labeled_stmt(label_kind: LabelKind, stmt: Stmt, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::Labeled(label_kind, Box::new(stmt)),
            debug_info,
        }
    }

    pub const fn new_declare(declaration: Declaration, debug_info: DebugInfo) -> Self {
        Self {
            kind: StmtKind::Declare(declaration),
            debug_info,
        }
    }
}
