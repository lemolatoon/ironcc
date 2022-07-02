use std::collections::BTreeMap;

use crate::{
    parse::{BinOpKind, Binary, Expr, ExprKind, Program, ProgramKind, Stmt, StmtKind, UnOp},
    tokenize::Position,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Analyzer<'a> {
    input: &'a str,
    offset: usize,
}

impl<'a> Analyzer<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    pub fn down_program(&mut self, program: Program) -> ConvProgram {
        let mut conv_program = ConvProgram::new();
        let mut lvar_map = BTreeMap::new();
        for component in program.into_iter() {
            match component {
                ProgramKind::Stmt(stmt) => {
                    conv_program.push_stmt(self.down_stmt(stmt, &mut lvar_map))
                }
            }
        }
        conv_program
    }

    pub fn down_stmt(&mut self, stmt: Stmt, lvar_map: &mut BTreeMap<String, usize>) -> ConvStmt {
        match stmt.kind {
            // do nothing
            StmtKind::Expr(expr) => ConvStmt::new_expr(self.down_expr(expr, lvar_map)),
            // do nothing
            StmtKind::Return(expr) => ConvStmt::new_ret(self.down_expr(expr, lvar_map)),
            // do nothing
            StmtKind::If(cond, then, els) => ConvStmt::new_if(
                self.down_expr(cond, lvar_map),
                self.down_stmt(*then, lvar_map),
                els.map(|stmt| self.down_stmt(*stmt, lvar_map)),
            ),
            // do nothing
            StmtKind::While(cond, then) => ConvStmt::new_while(
                self.down_expr(cond, lvar_map),
                self.down_stmt(*then, lvar_map),
            ),
            // do nothing
            StmtKind::For(init, cond, inc, then) => ConvStmt::new_for(
                init.map(|expr| self.down_expr(expr, lvar_map)),
                cond.map(|expr| self.down_expr(expr, lvar_map)),
                inc.map(|expr| self.down_expr(expr, lvar_map)),
                self.down_stmt(*then, lvar_map),
            ),
        }
    }

    pub fn down_expr(&mut self, expr: Expr, lvar_map: &mut BTreeMap<String, usize>) -> ConvExpr {
        let pos = expr.pos.clone();
        match expr.kind {
            // `a >= b` := `b <= a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Ge,
                lhs,
                rhs,
            }) => ConvExpr::new_binary(
                ConvBinOpKind::Le,
                self.down_expr(*rhs, lvar_map),
                self.down_expr(*lhs, lvar_map),
                pos,
            ),
            // `a > b` := `b < a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Gt,
                lhs,
                rhs,
            }) => ConvExpr::new_binary(
                ConvBinOpKind::Lt,
                self.down_expr(*rhs, lvar_map),
                self.down_expr(*lhs, lvar_map),
                pos,
            ),
            // do nothing
            ExprKind::Binary(Binary { kind, lhs, rhs }) => ConvExpr::new_binary(
                ConvBinOpKind::new(kind).unwrap(),
                self.down_expr(*lhs, lvar_map),
                self.down_expr(*rhs, lvar_map),
                pos,
            ),
            // do nothing
            ExprKind::Num(n) => ConvExpr::new_num(n, pos),
            // substitute `-x` into `0-x`
            ExprKind::Unary(UnOp::Minus, operand) => ConvExpr::new_binary(
                ConvBinOpKind::Sub,
                ConvExpr::new_num(0, pos.clone()),
                self.down_expr(*operand, lvar_map),
                pos,
            ),

            // do nothing
            ExprKind::Unary(UnOp::Plus, operand) => self.down_expr(*operand, lvar_map),
            // do nothing
            ExprKind::Assign(lhs, rhs) => ConvExpr::new_assign(
                self.down_expr(*lhs, lvar_map),
                self.down_expr(*rhs, lvar_map),
                pos,
            ),
            // currently all ident is local variable
            ExprKind::Ident(name) => ConvExpr::new_lvar(name, pos, &mut self.offset, lvar_map),
        }
    }

    /// # Panics
    /// always
    pub fn error_at(&self, pos: impl Into<Option<Position>>, msg: &str) -> ! {
        let pos: Option<Position> = pos.into();
        match pos {
            None => panic!("Passed pos info was None.\n{}", msg),
            Some(pos) => {
                let mut splited = self.input.split('\n');
                let line = splited.nth(pos.n_line).unwrap_or_else(|| {
                    panic!(
                        "Position is illegal, pos: {:?},\n input: {}",
                        pos, self.input
                    )
                });
                eprintln!("{}", line);
                let mut buffer = String::with_capacity(pos.n_char + 1);
                for _ in 0..pos.n_char {
                    buffer.push(' ');
                }
                buffer.push('^');
                eprintln!("{}", buffer);
                eprintln!("{}", msg);
                panic!();
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvProgram {
    components: Vec<ConvProgramKind>,
}

impl ConvProgram {
    pub fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn with_vec(vec: Vec<ConvProgramKind>) -> Self {
        Self { components: vec }
    }

    pub fn push_stmt(&mut self, stmt: ConvStmt) {
        self.components.push(ConvProgramKind::Stmt(stmt));
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
    Stmt(ConvStmt),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvStmt {
    pub kind: ConvStmtKind,
}

impl ConvStmt {
    pub fn new_expr(expr: ConvExpr) -> Self {
        Self {
            kind: ConvStmtKind::Expr(expr),
        }
    }

    pub fn new_ret(expr: ConvExpr) -> Self {
        Self {
            kind: ConvStmtKind::Return(expr),
        }
    }

    pub fn new_if(cond: ConvExpr, then: ConvStmt, els: Option<ConvStmt>) -> Self {
        Self {
            kind: ConvStmtKind::If(cond, Box::new(then), els.map(|stmt| Box::new(stmt))),
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
    Return(ConvExpr),
    If(ConvExpr, Box<ConvStmt>, Option<Box<ConvStmt>>),
    While(ConvExpr, Box<ConvStmt>),
    For(
        Option<ConvExpr>,
        Option<ConvExpr>,
        Option<ConvExpr>,
        Box<ConvStmt>,
    ),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvExpr {
    pub kind: ConvExprKind,
    pub pos: Position,
}
impl ConvExpr {
    pub fn new_binary(kind: ConvBinOpKind, lhs: ConvExpr, rhs: ConvExpr, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Binary(ConvBinary::new(kind, Box::new(lhs), Box::new(rhs))),
            pos,
        }
    }

    pub const fn new_num(num: isize, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Num(num),
            pos,
        }
    }

    pub fn new_assign(lhs: ConvExpr, rhs: ConvExpr, pos: Position) -> Self {
        ConvExpr {
            kind: ConvExprKind::Assign(Box::new(lhs), Box::new(rhs)),
            pos,
        }
    }

    pub fn new_lvar(
        name: String,
        pos: Position,
        new_offset: &mut usize,
        lvar_map: &mut BTreeMap<String, usize>,
    ) -> Self {
        let offset = match lvar_map.get(&name) {
            Some(offset) => *offset,
            None => {
                *new_offset += 8;
                lvar_map.insert(name, *new_offset);
                *new_offset
            }
        };
        ConvExpr {
            kind: ConvExprKind::Lvar(Lvar { offset: offset }),
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
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Lvar {
    /// Used like `mov rax, [rbp - offset]`
    pub offset: usize,
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
    Add,
    Sub,
    Mul,
    Div,
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
    pub const fn new(kind: BinOpKind) -> Option<Self> {
        match kind {
            BinOpKind::Add => Some(ConvBinOpKind::Add),
            BinOpKind::Sub => Some(ConvBinOpKind::Sub),
            BinOpKind::Mul => Some(ConvBinOpKind::Mul),
            BinOpKind::Div => Some(ConvBinOpKind::Div),
            BinOpKind::Eq => Some(ConvBinOpKind::Eq),
            BinOpKind::Le => Some(ConvBinOpKind::Le),
            BinOpKind::Lt => Some(ConvBinOpKind::Le),
            BinOpKind::Ge | BinOpKind::Gt => None,
            BinOpKind::Ne => Some(ConvBinOpKind::Ne),
        }
    }
}
