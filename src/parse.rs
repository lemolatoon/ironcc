use crate::tokenize::{BinOpToken, DelimToken, Position, Token, TokenKind, TokenStream};

pub struct Parser<'a> {
    input: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn parse_expr<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        self.parse_equality(tokens)
    }

    pub fn parse_equality<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_relational(tokens);
        while let Some(Token { kind, pos }) = tokens.peek() {
            let op = match &**kind {
                TokenKind::EqEq => BinOpKind::Eq,
                TokenKind::Ne => BinOpKind::Ne,
                _ => break,
            };
            let pos = pos.clone();
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_relational(tokens), pos);
        }
        lhs
    }

    pub fn parse_relational<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_add(tokens);
        while let Some(Token { kind, pos }) = tokens.peek() {
            let op = match &**kind {
                TokenKind::Lt => BinOpKind::Lt,
                TokenKind::Le => BinOpKind::Le,
                TokenKind::Gt => BinOpKind::Gt,
                TokenKind::Ge => BinOpKind::Ge,
                _ => break,
            };
            let pos = pos.clone();
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_add(tokens), pos);
        }
        lhs
    }

    pub fn parse_add<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_mul(tokens);
        while let Some(Token { kind, pos }) = tokens.peek() {
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::Plus) => BinOpKind::Add,
                TokenKind::BinOp(BinOpToken::Minus) => BinOpKind::Sub,
                _ => break,
            };
            let pos = pos.clone();
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_mul(tokens), pos);
        }
        lhs
    }

    pub fn parse_mul<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_unary(tokens);
        while let Some(Token { kind, pos }) = tokens.peek() {
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::Mul) => BinOpKind::Mul,
                TokenKind::BinOp(BinOpToken::Div) => BinOpKind::Div,
                _ => break,
            };
            let pos = pos.clone();
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_unary(tokens), pos);
        }
        lhs
    }

    pub fn parse_unary<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        let (kind, pos) = match tokens.peek() {
            Some(Token { kind, pos }) => (kind, pos.clone()),
            None => self.error_at(None, "Expected token, but got None"),
        };
        match **kind {
            TokenKind::BinOp(BinOpToken::Plus) => {
                tokens.next();
                Expr::new_unary(UnOp::Plus, self.parse_primary(tokens), pos)
            }
            TokenKind::BinOp(BinOpToken::Minus) => {
                tokens.next();
                Expr::new_unary(UnOp::Minus, self.parse_primary(tokens), pos)
            }
            _ => self.parse_primary(tokens),
        }
    }

    pub fn parse_primary<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        match tokens.next() {
            Some(Token { kind, pos }) => match *kind {
                TokenKind::Num(num) => Expr::new_num(num, pos),
                TokenKind::OpenDelim(DelimToken::Paran) => {
                    let expr = self.parse_expr(tokens);
                    tokens.expect(TokenKind::CloseDelim(DelimToken::Paran));
                    expr
                }
                _ => self.error_at(
                    Some(pos),
                    &format!("In `parse_primary`, got unexpected token: {:?}", kind),
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
    Unary(UnOp, Box<Expr>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum UnOp {
    Plus,
    /// before analysis only
    Minus,
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

    pub fn new_unary(kind: UnOp, expr: Expr, pos: Position) -> Self {
        Self {
            kind: ExprKind::Unary(kind, Box::new(expr)),
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
    pub kind: BinOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
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
}
