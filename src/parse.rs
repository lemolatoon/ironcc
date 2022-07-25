use crate::{
    analyze::{BaseType, Type},
    error::CompileError,
    tokenize::{BinOpToken, DelimToken, Position, Token, TokenKind, TokenStream, TypeToken},
};
use std::fmt::Debug;

pub struct Parser<'a> {
    /// input source code
    /// this member is only used when macro `unimplemented_err` is called
    #[allow(dead_code)]
    input: &'a str,
}

impl<'a> Parser<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input }
    }
    pub fn parse_program<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Program, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut program = Program::new();
        while !tokens.at_eof() {
            program.push(self.parse_func(tokens)?);
        }
        tokens.expect(TokenKind::Eof)?;
        assert!(tokens.next().is_none());
        Ok(program)
    }

    pub fn parse_func<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<ProgramKind, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let declaration = self.parse_declaration(tokens)?;
        // have to be block stmt
        tokens.expect(TokenKind::OpenDelim(DelimToken::Brace))?;
        let mut stmts = Vec::new();
        while !tokens.consume(&TokenKind::CloseDelim(DelimToken::Brace)) {
            stmts.push(self.parse_stmt(tokens)?);
        }
        let body = Stmt::new_block(stmts);
        Ok(ProgramKind::Func(declaration, body))
    }

    pub fn parse_declaration<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Declaration, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        // <declaration-specifiers> := <type-specifiers>
        let (type_spec, pos) = Self::parse_type_specifier(tokens)?;
        // <pointer>*
        let n_star = Self::parse_pointer(tokens)?;
        // first element of direct diclarator is Ident
        let mut direct_diclarator = DirectDeclarator::Ident(tokens.consume_ident()?);
        if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paran)) {
            let mut args = Vec::new();
            // function declaration
            if !tokens.consume(&TokenKind::CloseDelim(DelimToken::Paran)) {
                args.push(self.parse_declaration(tokens)?);
                while tokens.consume(&TokenKind::Comma) {
                    args.push(self.parse_declaration(tokens)?);
                }
                tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
            }
            direct_diclarator = DirectDeclarator::Func(Box::new(direct_diclarator), args);
        }
        let init = if tokens.consume(&TokenKind::Eq) {
            Some(self.parse_initializer(tokens)?)
        } else {
            None
        };
        Ok(Declaration::new(
            type_spec,
            n_star,
            direct_diclarator,
            init,
            pos,
        ))
    }
    pub fn parse_initializer<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Initializer, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
            let mut exprs = vec![self.parse_assign(tokens)?];
            while tokens.consume(&TokenKind::Comma) {
                if tokens.peek_expect(&TokenKind::CloseDelim(DelimToken::Brace)) {
                    break;
                };

                exprs.push(self.parse_assign(tokens)?);
            }
            tokens.expect(TokenKind::CloseDelim(DelimToken::Brace))?;
            Ok(Initializer::Array(exprs))
        } else {
            Ok(Initializer::Expr(self.parse_assign(tokens)?))
        }
    }

    pub fn parse_type_specifier<I>(
        tokens: &mut TokenStream<I>,
    ) -> Result<(TypeSpec, Position), CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        match tokens.next() {
            Some(Token { kind, pos }) => match *kind {
                TokenKind::Type(TypeToken::Int) => Ok((TypeSpec::Int, pos)),
                _ => Err(tokens
                    .new_expected_failed(Box::new("TokenKind::Type(_)"), Token::new(*kind, pos))),
            },
            None => Err(tokens.new_unexpected_eof(Box::new("ToKenKind::Type(_)"))),
        }
    }

    pub fn parse_pointer<I>(tokens: &mut TokenStream<I>) -> Result<usize, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut n_star = 0;
        while tokens.consume(&TokenKind::BinOp(BinOpToken::Star)) {
            n_star += 1;
        }
        Ok(n_star)
    }

    pub fn parse_stmt<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Result<Stmt, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        if tokens.consume(&TokenKind::Return) {
            // return stmt
            let returning_expr = self.parse_expr(tokens)?;
            tokens.expect(TokenKind::Semi)?;
            Ok(Stmt::ret(returning_expr))
        } else if tokens.consume(&TokenKind::If) {
            tokens.expect(TokenKind::OpenDelim(DelimToken::Paran))?;
            let conditional_expr = self.parse_expr(tokens)?;
            tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
            let then_stmt = self.parse_stmt(tokens)?;
            let else_stmt = if tokens.consume(&TokenKind::Else) {
                Some(self.parse_stmt(tokens)?)
            } else {
                None
            };
            Ok(Stmt::new_if(conditional_expr, then_stmt, else_stmt))
        } else if tokens.consume(&TokenKind::While) {
            tokens.expect(TokenKind::OpenDelim(DelimToken::Paran))?;
            let conditional_expr = self.parse_expr(tokens)?;
            tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
            let then_stmt = self.parse_stmt(tokens)?;
            Ok(Stmt::new_while(conditional_expr, then_stmt))
        } else if tokens.consume(&TokenKind::For) {
            tokens.expect(TokenKind::OpenDelim(DelimToken::Paran))?;
            let init_expr = if tokens.consume(&TokenKind::Semi) {
                None
            } else {
                let expr = self.parse_expr(tokens)?;
                tokens.expect(TokenKind::Semi)?;
                Some(expr)
            };
            let cond_expr = if tokens.consume(&TokenKind::Semi) {
                None
            } else {
                let expr = self.parse_expr(tokens)?;
                tokens.expect(TokenKind::Semi)?;
                Some(expr)
            };
            let inc_expr = if tokens.consume(&TokenKind::Semi) {
                None
            } else {
                let expr = self.parse_expr(tokens)?;
                Some(expr)
            };
            tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
            let then_stmt = self.parse_stmt(tokens)?;
            Ok(Stmt::new_for(init_expr, cond_expr, inc_expr, then_stmt))
        } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
            let mut stmts = Vec::new();
            while !tokens.consume(&TokenKind::CloseDelim(DelimToken::Brace)) {
                stmts.push(self.parse_stmt(tokens)?);
            }
            Ok(Stmt::new_block(stmts))
        } else if tokens.is_type() {
            let stmt = Stmt::new_declare(self.parse_declaration(tokens)?);
            tokens.expect(TokenKind::Semi)?;
            Ok(stmt)
        } else {
            let expr = self.parse_expr(tokens)?;
            tokens.expect(TokenKind::Semi)?;
            Ok(Stmt::expr(expr))
        }
    }

    pub fn parse_expr<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        self.parse_assign(tokens)
    }

    pub fn parse_assign<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let lhs = self.parse_equality(tokens)?;
        let (kind, &pos) = match tokens.peek() {
            Some(Token { kind, pos }) => (kind, pos),
            None => return Err(tokens.new_unexpected_eof(Box::new("Token"))),
        };
        match **kind {
            TokenKind::Eq => {
                tokens.next();
                Ok(Expr::new_assign(lhs, self.parse_assign(tokens)?, pos))
            }
            _ => Ok(lhs),
        }
    }

    pub fn parse_equality<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_relational(tokens)?;
        while let Some(Token { kind, pos }) = tokens.peek() {
            let pos = *pos;
            let op = match &**kind {
                TokenKind::EqEq => BinOpKind::Eq,
                TokenKind::Ne => BinOpKind::Ne,
                _ => break,
            };
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_relational(tokens)?, pos);
        }
        Ok(lhs)
    }

    pub fn parse_relational<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_add(tokens)?;
        while let Some(Token { kind, pos }) = tokens.peek() {
            let pos = *pos;
            let op = match &**kind {
                TokenKind::Lt => BinOpKind::Lt,
                TokenKind::Le => BinOpKind::Le,
                TokenKind::Gt => BinOpKind::Gt,
                TokenKind::Ge => BinOpKind::Ge,
                _ => break,
            };
            let pos = pos;
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_add(tokens)?, pos);
        }
        Ok(lhs)
    }

    pub fn parse_add<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_mul(tokens)?;
        while let Some(Token { kind, pos }) = tokens.peek() {
            let pos = *pos;
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::Plus) => BinOpKind::Add,
                TokenKind::BinOp(BinOpToken::Minus) => BinOpKind::Sub,
                _ => break,
            };
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_mul(tokens)?, pos);
        }
        Ok(lhs)
    }

    pub fn parse_mul<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut lhs = self.parse_unary(tokens)?;
        while let Some(Token { kind, pos }) = tokens.peek() {
            let pos = *pos;
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::Star) => BinOpKind::Mul,
                TokenKind::BinOp(BinOpToken::Slash) => BinOpKind::Div,
                TokenKind::BinOp(BinOpToken::Percent) => BinOpKind::Rem,
                _ => break,
            };
            let pos = pos;
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_unary(tokens)?, pos);
        }
        Ok(lhs)
    }

    pub fn parse_unary<'b, I>(&self, tokens: &mut TokenStream<'b, I>) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let (kind, pos) = match tokens.peek() {
            Some(Token { kind, pos }) => (kind, pos),
            None => return Err(tokens.new_unexpected_eof(Box::new("Token"))),
        };
        let pos = *pos;
        Ok(match **kind {
            TokenKind::BinOp(BinOpToken::Plus) => {
                tokens.next();
                Expr::new_unary(UnOp::Plus, self.parse_primary(tokens)?, pos)
            }
            TokenKind::BinOp(BinOpToken::Minus) => {
                tokens.next();
                Expr::new_unary(UnOp::Minus, self.parse_primary(tokens)?, pos)
            }
            TokenKind::BinOp(BinOpToken::Star) => {
                tokens.next();
                Expr::new_deref(self.parse_unary(tokens)?, pos)
            }
            TokenKind::BinOp(BinOpToken::And) => {
                tokens.next();
                Expr::new_addr(self.parse_unary(tokens)?, pos)
            }
            TokenKind::SizeOf => {
                tokens.next();
                let mut tmp_tokens = tokens.clone();
                if let (Some(TokenKind::OpenDelim(DelimToken::Paran)), Some(TokenKind::Type(_))) = (
                    tmp_tokens.next().map(|token| *token.kind()),
                    tmp_tokens.next().map(|token| *token.kind()),
                ) {
                    // e.g) sizeof(int)
                    tokens.next(); // -> TokenKind::OpenDelim(DelimToken::Paran))
                    let expr = Expr::new_type_sizeof(Self::parse_type_name(tokens)?, pos);
                    tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
                    expr
                } else {
                    // e.g) sizeof (5)
                    Expr::new_expr_sizeof(self.parse_unary(tokens)?, pos)
                }
            }
            _ => self.parse_primary(tokens)?,
        })
    }

    pub fn parse_primary<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        match tokens.next() {
            Some(Token { kind, pos }) => Ok(match *kind {
                TokenKind::Num(num) => Expr::new_num(num, pos),
                TokenKind::OpenDelim(DelimToken::Paran) => {
                    let expr = self.parse_expr(tokens)?;
                    tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
                    expr
                }
                TokenKind::Ident(name) => {
                    if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paran)) {
                        // func call
                        let mut args = Vec::new();
                        if tokens.consume(&TokenKind::CloseDelim(DelimToken::Paran)) {
                            return Ok(Expr::new_func(name, args, pos));
                        }
                        args.push(self.parse_expr(tokens)?);
                        while !tokens.consume(&TokenKind::CloseDelim(DelimToken::Paran)) {
                            tokens.expect(TokenKind::Comma)?;
                            args.push(self.parse_expr(tokens)?);
                        }
                        return Ok(Expr::new_func(name, args, pos));
                    }
                    // local variable
                    Expr::new_lvar(name, pos)
                }
                TokenKind::Eof => return Err(tokens.new_unexpected_eof(Box::new("TokenKind::Num(_) | TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Paran)"))),
                _ => return Err(tokens.new_expected_failed(Box::new("TokenKind::Num(_) | TokenKind::OpenDelim(DelimToken::Paran) | TokenKind::Ident"), Token::new(*kind, pos))),
            }),
            None => Err(tokens.new_unexpected_eof(Box::new(
                "TokenKind::Num(_) | TokenKind::OpenDelim(DelimToken::Paran) | TokenKind::Ident",
            ))),
        }
    }

    pub fn parse_type_name<I>(tokens: &mut TokenStream<I>) -> Result<TypeName, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let (ty_spec, pos) = Self::parse_type_specifier(tokens)?;
        let n_star = Self::parse_pointer(tokens)?;
        // TODO: support DirectAbstractDeclarator
        let abs_declrtr = if n_star == 0 {
            None
        } else {
            Some(AbstractDeclarator::new(n_star, None))
        };
        Ok(TypeName::new(SpecQual(ty_spec), abs_declrtr, pos))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Program {
    components: Vec<ProgramKind>,
}

impl Program {
    pub const fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn with_vec(vec: Vec<ProgramKind>) -> Self {
        Self { components: vec }
    }

    pub fn push(&mut self, func: ProgramKind) {
        self.components.push(func);
    }
}

impl IntoIterator for Program {
    type Item = ProgramKind;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ProgramKind {
    Func(Declaration, Stmt),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// Declaration
pub struct Declaration {
    // type-specifier
    pub ty_spec: TypeSpec,
    // init-declarator
    // declarator
    pub n_star: usize,
    pub declrtr: DirectDeclarator,
    // initializer
    pub initializer: Option<Initializer>,
    pub pos: Position,
}

impl Declaration {
    // TODO: need or not
    pub const fn new(
        ty_spec: TypeSpec,
        n_star: usize,
        declrtr: DirectDeclarator,
        initializer: Option<Initializer>,
        pos: Position,
    ) -> Self {
        Self {
            ty_spec,
            n_star,
            declrtr,
            initializer,
            pos,
        }
    }

    pub fn ident_name(&self) -> &str {
        self.declrtr.ident_name()
    }

    pub fn ty(&self) -> Type {
        let base = match self.ty_spec {
            TypeSpec::Int => BaseType::Int,
        };
        let mut ty = Type::Base(base);
        for _ in 0..self.n_star {
            ty = Type::Ptr(Box::new(ty));
        }
        ty
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum DirectDeclarator {
    Ident(String),
    Func(Box<DirectDeclarator>, Vec<Declaration>),
}

impl DirectDeclarator {
    pub fn ident_name(&self) -> &str {
        match self {
            DirectDeclarator::Ident(name) => name,
            DirectDeclarator::Func(direct_declarator, _) => direct_declarator.ident_name(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Initializer {
    Expr(Expr),
    Array(Vec<Expr>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeSpec {
    Int,
}

impl Stmt {
    pub const fn expr(expr: Expr) -> Self {
        Self {
            kind: StmtKind::Expr(expr),
        }
    }

    pub const fn ret(expr: Expr) -> Self {
        Self {
            kind: StmtKind::Return(expr),
        }
    }

    pub fn new_block(stmts: Vec<Stmt>) -> Self {
        Self {
            kind: StmtKind::Block(stmts),
        }
    }

    pub fn new_if(cond: Expr, then: Stmt, els: Option<Stmt>) -> Self {
        Self {
            kind: StmtKind::If(cond, Box::new(then), els.map(Box::new)),
        }
    }

    pub fn new_while(cond: Expr, then: Stmt) -> Self {
        Self {
            kind: StmtKind::While(cond, Box::new(then)),
        }
    }

    pub fn new_for(init: Option<Expr>, cond: Option<Expr>, inc: Option<Expr>, then: Stmt) -> Self {
        Self {
            kind: StmtKind::For(init, cond, inc, Box::new(then)),
        }
    }

    pub const fn new_declare(declaration: Declaration) -> Self {
        Self {
            kind: StmtKind::Declare(declaration),
        }
    }
}

/// \<type-name\> := \<specifier-qualifier-list\> \<abstract-declarator\>?
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TypeName {
    // TODO: actually Vec
    spec_quals: SpecQual,
    abstract_declarator: Option<AbstractDeclarator>,
    pos: Position,
}

impl TypeName {
    pub const fn new(
        spec_quals: SpecQual,
        abs_declrtr: Option<AbstractDeclarator>,
        pos: Position,
    ) -> Self {
        Self {
            spec_quals,
            abstract_declarator: abs_declrtr,
            pos,
        }
    }

    pub fn ty(&self) -> Type {
        // TODO: also consider array
        let base = match self.spec_quals.0 {
            TypeSpec::Int => BaseType::Int,
        };
        let mut ty = Type::Base(base);
        if let Some(ref abs_declrtr) = self.abstract_declarator {
            for _ in 0..abs_declrtr.n_star {
                ty = Type::Ptr(Box::new(ty));
            }
        }
        ty
    }
}

/// \<specifier-sualifier-list\> := \<type-specifiers\>
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct SpecQual(TypeSpec);

/// \<abstract-declarator\> := "\*"\* \<direct-abstract-declarator\>?
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct AbstractDeclarator {
    /// "\*"\*
    n_star: usize,
    /// \<direct-abstruct-declarator\>?
    direct_abstruct_declarator: Option<DirectAbstructDeclarator>,
}

impl AbstractDeclarator {
    pub const fn new(n_star: usize, d_abs_declrtr: Option<DirectAbstructDeclarator>) -> Self {
        Self {
            n_star,
            direct_abstruct_declarator: d_abs_declrtr,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum DirectAbstructDeclarator {
    /// <direct-abstruct-declarator>? "[]"
    Array(Option<Box<DirectAbstructDeclarator>>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum StmtKind {
    Expr(Expr),
    Return(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Declare(Declaration),
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
    LVar(String),
    Func(String, Vec<Expr>),
    Deref(Box<Expr>),
    Addr(Box<Expr>),
    SizeOf(SizeOfOperandKind),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SizeOfOperandKind {
    Expr(Box<Expr>),
    Type(TypeName),
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

    pub const fn new_num(num: isize, pos: Position) -> Self {
        Self {
            kind: ExprKind::Num(num),
            pos,
        }
    }

    pub const fn new_lvar(name: String, pos: Position) -> Self {
        Self {
            kind: ExprKind::LVar(name),
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
            pos,
        }
    }

    pub fn new_func(name: String, args: Vec<Expr>, pos: Position) -> Self {
        Self {
            kind: ExprKind::Func(name, args),
            pos,
        }
    }

    pub fn new_deref(expr: Expr, pos: Position) -> Self {
        Self {
            kind: ExprKind::Deref(Box::new(expr)),
            pos,
        }
    }

    pub fn new_addr(expr: Expr, pos: Position) -> Self {
        Self {
            kind: ExprKind::Addr(Box::new(expr)),
            pos,
        }
    }

    pub fn new_expr_sizeof(expr: Expr, pos: Position) -> Self {
        Self {
            kind: ExprKind::SizeOf(SizeOfOperandKind::Expr(Box::new(expr))),
            pos,
        }
    }

    pub const fn new_type_sizeof(type_name: TypeName, pos: Position) -> Self {
        Self {
            kind: ExprKind::SizeOf(SizeOfOperandKind::Type(type_name)),
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

#[derive(PartialEq, Eq, Clone, Debug, Copy)]
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
}
