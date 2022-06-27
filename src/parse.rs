use crate::tokenize::{BinOpToken, Position, Token, TokenKind, TokenStream};

pub struct Parser<'a> {
    input: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn parse_expr<'b, I: Iterator<Item = Token>>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone,
    {
        let mut lhs = self.parse_mul(tokens);
        while let Some(Token { kind, pos }) = tokens.peek() {
            lhs = match &**kind {
                TokenKind::BinOp(op @ (BinOpToken::Plus | BinOpToken::Minus)) => {
                    let op = match op {
                        BinOpToken::Plus => BinOpKind::Add,
                        BinOpToken::Minus => BinOpKind::Sub,
                        _ => panic!(),
                    };
                    let pos = pos.clone();
                    tokens.next();
                    Expr::new_binary(op, lhs, self.parse_mul(tokens), pos)
                }
                _ => break,
            };
        }
        lhs
    }

    pub fn parse_mul<'b, I: Iterator<Item = Token>>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone,
    {
        let mut lhs = self.parse_primary(tokens);
        while let Some(Token { kind, pos }) = tokens.peek() {
            lhs = match &**kind {
                TokenKind::BinOp(op @ (BinOpToken::Mul | BinOpToken::Div)) => {
                    let op = match op {
                        BinOpToken::Mul => BinOpKind::Mul,
                        BinOpToken::Div => BinOpKind::Div,
                        _ => panic!(),
                    };
                    let pos = pos.clone();
                    tokens.next();
                    Expr::new_binary(op, lhs, self.parse_primary(tokens), pos)
                }
                _ => break,
            };
        }
        lhs
    }

    pub fn parse_primary<'b, I: Iterator<Item = Token>>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Expr
    where
        I: Clone,
    {
        match tokens.next() {
            Some(Token { kind, pos }) => match *kind {
                TokenKind::Num(num) => Expr::new_num(num, pos.clone()),
                _ => self.error_at(
                    Some(pos.clone()),
                    &format!("Expected number, but got {:?}", kind),
                ),
            },
            None => self.error_at(None, "Next token is None. in `parse_primary`"),
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
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Position,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ExprKind {
    Binary(Binary),
    Num(isize),
}

impl Expr {
    pub fn new_binary(kind: BinOpKind, lhs: Expr, rhs: Expr, pos: Position) -> Self {
        Self {
            kind: ExprKind::Binary(Binary::new(kind, Box::new(lhs), Box::new(rhs))),
            pos,
        }
    }

    pub fn new_num(num: isize, pos: Position) -> Self {
        Self {
            kind: ExprKind::Num(num),
            pos,
        }
    }

    #[cfg(test)]
    pub fn kind_eq(&self, lhs: &Expr) -> bool {
        lhs == self
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Binary {
    kind: BinOpKind,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

impl Binary {
    pub fn new(kind: BinOpKind, lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Self { kind, lhs, rhs }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}
