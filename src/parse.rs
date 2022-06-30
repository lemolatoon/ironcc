use crate::tokenize::{BinOpToken, DelimToken, Position, Token, TokenKind, TokenStream};

pub struct Parser<'a> {
    input: &'a str,
}

impl<'a> Parser<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input }
    }
    pub fn parse_program<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Program
    where
        I: Clone + Iterator<Item = Token>,
    {
        let mut program = Program::new();
        while !tokens.at_eof() {
            program.push_stmt(self.parse_stmt(tokens));
            tokens.expect(TokenKind::Semi);
        }
        tokens.expect(TokenKind::Eof);
        assert!(tokens.next().is_none());
        program
    }

    pub fn parse_stmt<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Stmt
    where
        I: Clone + Iterator<Item = Token>,
    {
        let expr = self.parse_expr(tokens);
        Stmt::expr(expr)
    }

    pub fn parse_expr<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        self.parse_assign(tokens)
    }

    pub fn parse_assign<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Expr
    where
        I: Clone + Iterator<Item = Token>,
    {
        let lhs = self.parse_equality(tokens);
        let (kind, pos) = match tokens.peek() {
            Some(Token { kind, pos }) => (kind, pos.clone()),
            None => self.error_at(None, "Expected token, but got None"),
        };
        match **kind {
            TokenKind::Eq => {
                tokens.next();
                Expr::new_assign(lhs, self.parse_assign(tokens), pos)
            }
            _ => lhs,
        }
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
                TokenKind::Ident(name) => Expr::new_ident(name, pos),
                _ => self.error_at(
                    Some(pos),
                    &format!("In `parse_primary`, got unexpected token: {:?}", kind),
                ),
            },
            None => self.error_at(None, "Next token is None. in `parse_primary`"),
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
pub struct Program {
    components: Vec<ProgramKind>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn with_vec(vec: Vec<ProgramKind>) -> Self {
        Self { components: vec }
    }

    pub fn push_stmt(&mut self, stmt: Stmt) {
        self.components.push(ProgramKind::Stmt(stmt));
    }

    pub fn into_iter(self) -> impl Iterator<Item = ProgramKind> {
        self.components.into_iter()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ProgramKind {
    Stmt(Stmt),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    pub fn expr(expr: Expr) -> Self {
        Self {
            kind: StmtKind::Expr(expr),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum StmtKind {
    Expr(Expr),
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
    Assign(Box<Expr>, Box<Expr>),
    Ident(String),
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

    pub fn new_ident(name: String, pos: Position) -> Self {
        Self {
            kind: ExprKind::Ident(name),
            pos,
        }
    }

    pub fn new_unary(kind: UnOp, expr: Expr, pos: Position) -> Self {
        Self {
            kind: ExprKind::Unary(kind, Box::new(expr)),
            pos,
        }
    }

    pub fn new_assign(lhs: Expr, rhs: Expr, pos: Position) -> Self {
        // pos is Position of TokenKind::Eq (i.e. `=`)
        Self {
            kind: ExprKind::Assign(Box::new(lhs), Box::new(rhs)),
            pos: pos,
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

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
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
