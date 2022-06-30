use crate::{
    parse::{BinOpKind, Binary, Expr, ExprKind, Program, ProgramKind, Stmt, StmtKind, UnOp},
    tokenize::Position,
};

pub struct Analyzer<'a> {
    input: &'a str,
}

impl<'a> Analyzer<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn down_program(&self, program: Program) -> ConvProgram {
        let mut conv_program = ConvProgram::new();
        for component in program.into_iter() {
            match component {
                ProgramKind::Stmt(stmt) => conv_program.push_stmt(self.down_stmt(stmt)),
            }
        }
        conv_program
    }

    pub fn down_stmt(&self, stmt: Stmt) -> ConvStmt {
        match stmt.kind {
            StmtKind::Expr(expr) => ConvStmt::new_expr(self.down_expr(expr)),
        }
    }

    pub fn down_expr(&self, expr: Expr) -> ConvExpr {
        let pos = expr.pos.clone();
        match expr.kind {
            // `a >= b` := `b <= a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Ge,
                lhs,
                rhs,
            }) => ConvExpr::new_binary(
                ConvBinOpKind::Le,
                self.down_expr(*rhs),
                self.down_expr(*lhs),
                pos,
            ),
            // `a > b` := `b < a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Gt,
                lhs,
                rhs,
            }) => ConvExpr::new_binary(
                ConvBinOpKind::Lt,
                self.down_expr(*rhs),
                self.down_expr(*lhs),
                pos,
            ),
            // do nothing
            ExprKind::Binary(Binary { kind, lhs, rhs }) => ConvExpr::new_binary(
                ConvBinOpKind::new(kind).unwrap(),
                self.down_expr(*lhs),
                self.down_expr(*rhs),
                pos,
            ),
            // do nothing
            ExprKind::Num(n) => ConvExpr::new_num(n, pos),
            // substitute `-x` into `0-x`
            ExprKind::Unary(UnOp::Minus, operand) => ConvExpr::new_binary(
                ConvBinOpKind::Sub,
                ConvExpr::new_num(0, pos.clone()),
                self.down_expr(*operand),
                pos,
            ),

            // do nothing
            ExprKind::Unary(UnOp::Plus, operand) => self.down_expr(*operand),
            // do nothing
            ExprKind::Assign(lhs, rhs) => {
                ConvExpr::new_assign(self.down_expr(*lhs), self.down_expr(*rhs), pos)
            }
            // currently all ident is local variable
            ExprKind::Ident(name) => ConvExpr::new_lvar(name, pos),
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

    pub fn into_iter(self) -> impl Iterator<Item = ConvProgramKind> {
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
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvStmtKind {
    Expr(ConvExpr),
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
            pos: pos,
        }
    }

    pub fn new_lvar(name: String, pos: Position) -> Self {
        // assume name is one character currently
        assert!(name.len() == 1);
        let index = ('a'..='z')
            .position(|c| c == name.chars().next().unwrap())
            .expect("Expected alphabet here");
        let offset = index * 8;
        ConvExpr {
            kind: ConvExprKind::Lvar(Lvar { offset }),
            pos: pos,
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
    offset: usize,
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
