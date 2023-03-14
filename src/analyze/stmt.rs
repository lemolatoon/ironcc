use super::expr::ConvExpr;

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

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum LoopControlKind {
    Case(isize, Box<ConvStmt>),
    Default(Box<ConvStmt>),
    Break,
    Continue,
}
