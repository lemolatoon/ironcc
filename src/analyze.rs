use crate::{
    parse::{BinOpKind, Binary, Expr, ExprKind, UnOp},
    tokenize::Position,
};

pub struct Analyzer<'a> {
    input: &'a str,
}

impl<'a> Analyzer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn down_expr(&self, expr: Expr) -> ConvExpr {
        let pos = expr.pos.clone();
        match expr.kind {
            // do nothing
            ExprKind::Binary(Binary { kind, lhs, rhs }) => {
                ConvExpr::new_binary(kind, self.down_expr(*lhs), self.down_expr(*rhs), pos)
            }
            // do nothing
            ExprKind::Num(n) => ConvExpr::new_num(n, pos),
            // substitute `-x` into `0-x`
            ExprKind::Unary(UnOp::Minus, operand) => ConvExpr::new_binary(
                BinOpKind::Sub,
                ConvExpr::new_num(0, pos.clone()),
                self.down_expr(*operand),
                pos,
            ),

            // do nothing
            ExprKind::Unary(UnOp::Plus, operand) => self.down_expr(*operand),
        }
    }

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
pub struct ConvExpr {
    pub kind: ConvExprKind,
    pub pos: Position,
}
impl ConvExpr {
    pub fn new_binary(kind: BinOpKind, lhs: ConvExpr, rhs: ConvExpr, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Binary(ConvBinary::new(kind, Box::new(lhs), Box::new(rhs))),
            pos,
        }
    }

    pub fn new_num(num: isize, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Num(num),
            pos,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvExprKind {
    Binary(ConvBinary),
    Num(isize),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvBinary {
    pub kind: BinOpKind,
    pub lhs: Box<ConvExpr>,
    pub rhs: Box<ConvExpr>,
}

impl ConvBinary {
    pub fn new(kind: BinOpKind, lhs: Box<ConvExpr>, rhs: Box<ConvExpr>) -> Self {
        Self { kind, lhs, rhs }
    }
}
