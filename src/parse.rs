use crate::{
    analyze::{Analyzer, BaseType, ConstExpr, ConstInitializer, Type},
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
            let mut tmp_tokens = tokens.clone();
            let component = {
                let declaration = self.parse_declaration(&mut tmp_tokens)?;
                if Some(TokenKind::Semi) == tmp_tokens.peek_kind() {
                    tmp_tokens.expect(TokenKind::Semi).unwrap();
                    *tokens = tmp_tokens;
                    let pos = declaration.pos;
                    ProgramComponent::new(ProgramKind::Declaration(declaration), pos)
                } else {
                    self.parse_func_def(tokens)?
                }
            };
            program.push(component);
        }
        tokens.expect(TokenKind::Eof)?;
        assert!(tokens.next().is_none());
        Ok(program)
    }

    pub fn parse_func_def<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<ProgramComponent, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let (type_spec, pos) = self.parse_type_specifier(tokens)?;
        let n_star = Self::parse_pointer(tokens)?;
        let direct_declarator = self.parse_direct_declarator(tokens)?;
        // have to be block stmt
        tokens.expect(TokenKind::OpenDelim(DelimToken::Brace))?;
        let mut stmts = Vec::new();
        while !tokens.consume(&TokenKind::CloseDelim(DelimToken::Brace)) {
            stmts.push(self.parse_stmt(tokens)?);
        }
        let body = Stmt::new_block(stmts);
        let kind = ProgramKind::new_funcdef(type_spec, n_star, direct_declarator, body);
        Ok(ProgramComponent::new(kind, pos))
    }

    pub fn parse_declaration<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Declaration, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        // <declaration-specifiers> := <type-specifiers>
        // first element of direct diclarator is Ident
        let (ty_spec, pos) = self.parse_type_specifier(tokens)?;
        // <pointer>*
        let n_star = Self::parse_pointer(tokens)?;
        // dbg
        println!("before parse_direct_declarator");
        dbg!(tokens.peek());
        if tokens.peek_expect(&TokenKind::Semi) {
            // InitDeclarator is None
            return Ok(Declaration {
                ty_spec,
                init_declarator: None,
                pos,
            });
        }
        let direct_declarator = self.parse_direct_declarator(tokens)?;
        let init = if tokens.consume(&TokenKind::Eq) {
            Some(self.parse_initializer(tokens)?)
        } else {
            None
        };
        Ok(Declaration::new(
            ty_spec,
            n_star,
            direct_declarator,
            init,
            pos,
        ))
    }

    pub fn parse_declarator<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Declarator, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        Ok(Declarator::new(
            Self::parse_pointer(tokens)?,
            self.parse_direct_declarator(tokens)?,
        ))
    }

    pub fn parse_direct_declarator<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<DirectDeclarator, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut direct_declarator = if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paran)) {
            // "(" <declaration> ")"
            let direct_declarator =
                DirectDeclarator::Declarator(Box::new(self.parse_declarator(tokens)?));
            tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
            direct_declarator
        } else {
            // <ident>
            DirectDeclarator::Ident(tokens.consume_ident()?)
        };
        loop {
            if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paran)) {
                // function declaration
                if tokens.consume(&TokenKind::CloseDelim(DelimToken::Paran)) {
                    // e.g) `main()`
                    direct_declarator =
                        DirectDeclarator::Func(Box::new(direct_declarator), Vec::new());
                } else {
                    let mut args = Vec::new();
                    args.push(self.parse_declaration(tokens)?);
                    while tokens.consume(&TokenKind::Comma) {
                        args.push(self.parse_declaration(tokens)?);
                    }
                    direct_declarator = DirectDeclarator::Func(Box::new(direct_declarator), args);
                    tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
                }
            } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Bracket)) {
                if tokens.consume(&TokenKind::CloseDelim(DelimToken::Bracket)) {
                    direct_declarator = DirectDeclarator::Array(Box::new(direct_declarator), None);
                } else {
                    let expr = self.parse_assign(tokens)?;
                    direct_declarator =
                        DirectDeclarator::Array(Box::new(direct_declarator), Some(expr));
                    tokens.expect(TokenKind::CloseDelim(DelimToken::Bracket))?;
                }
            } else {
                break;
            }
        }
        Ok(direct_declarator)
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
        &self,
        tokens: &mut TokenStream<I>,
    ) -> Result<(TypeSpec, Position), CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let peeked = tokens.peek().cloned();
        match peeked {
            Some(Token { kind, pos }) => match *kind {
                TokenKind::Type(TypeToken::Int) => {
                    tokens.next();
                    Ok((TypeSpec::Int, pos))
                }
                TokenKind::Type(TypeToken::Char) => {
                    tokens.next();
                    Ok((TypeSpec::Char, pos))
                }
                TokenKind::Struct => {
                    tokens.next();
                    Ok((
                        TypeSpec::StructOrUnion(self.parse_struct_or_union_specifier(tokens)?),
                        pos,
                    ))
                }
                _ => Err(CompileError::new_expected_failed(
                    self.input,
                    Box::new("TokenKind::Type(_)"),
                    Token::new(*kind, pos),
                )),
            },
            None => Err(CompileError::new_unexpected_eof(
                self.input,
                Box::new("ToKenKind::Type(_)"),
            )),
        }
    }

    pub fn parse_struct_or_union_specifier<I>(
        &self,
        tokens: &mut TokenStream<I>,
    ) -> Result<StructOrUnionSpec, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        if let Ok(name) = tokens.consume_ident() {
            if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
                // <struct-declaration-list>
                dbg!(tokens.clone().collect::<Vec<_>>());
                let list = self.parse_struct_declaration_list(tokens)?;
                dbg!(&list);
                dbg!(tokens.clone().collect::<Vec<_>>());
                tokens.expect(TokenKind::CloseDelim(DelimToken::Brace))?;
                dbg!(tokens.peek());
                return Ok(StructOrUnionSpec::WithList(Some(name), list));
            }
            return Ok(StructOrUnionSpec::WithTag(name));
        } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
            // <struct-declaration-list>
            dbg!(tokens.clone().collect::<Vec<_>>());
            let list = self.parse_struct_declaration_list(tokens)?;
            dbg!(&list);
            dbg!(tokens.clone().collect::<Vec<_>>());
            tokens.expect(TokenKind::CloseDelim(DelimToken::Brace))?;
            return Ok(StructOrUnionSpec::WithList(None, list));
        }

        match tokens.peek() {
            Some(token) => Err(CompileError::new_expected_failed(
                self.input,
                Box::new("TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Brace)"),
                token.clone(),
            )),
            None => Err(CompileError::new_unexpected_eof(
                self.input,
                Box::new("TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Brace)"),
            )),
        }
    }
    pub fn parse_struct_declaration_list<I>(
        &self,
        tokens: &mut TokenStream<I>,
    ) -> Result<Vec<StructDeclaration>, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let (ty_spec, pos) = self.parse_type_specifier(tokens)?;
        let declarator = self.parse_declarator(tokens)?;
        let mut list = vec![StructDeclaration {
            ty_spec,
            declarator,
        }];
        tokens.expect(TokenKind::Semi)?;
        while let Ok((ty_spec, _)) = self.parse_type_specifier(tokens) {
            let declarator = self.parse_declarator(tokens)?;
            list.push(StructDeclaration {
                ty_spec,
                declarator,
            });
            tokens.expect(TokenKind::Semi)?;
        }
        Ok(list)
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
                Some(if tokens.is_type() {
                    let declaration = self.parse_declaration(tokens)?;
                    tokens.expect(TokenKind::Semi)?;
                    ForInitKind::Declaration(declaration)
                } else {
                    let expr = self.parse_expr(tokens)?;
                    tokens.expect(TokenKind::Semi)?;
                    ForInitKind::Expr(expr)
                })
            };
            let cond_expr = if tokens.consume(&TokenKind::Semi) {
                None
            } else {
                let expr = self.parse_expr(tokens)?;
                tokens.expect(TokenKind::Semi)?;
                Some(expr)
            };
            let inc_expr = if tokens.consume(&TokenKind::CloseDelim(DelimToken::Paran)) {
                None
            } else {
                let expr = self.parse_expr(tokens)?;
                tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
                Some(expr)
            };
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
            None => {
                return Err(CompileError::new_unexpected_eof(
                    self.input,
                    Box::new("Token"),
                ))
            }
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
                TokenKind::BinOp(BinOpToken::EqEq) => BinOpKind::Eq,
                TokenKind::BinOp(BinOpToken::Ne) => BinOpKind::Ne,
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
                TokenKind::BinOp(BinOpToken::Lt) => BinOpKind::Lt,
                TokenKind::BinOp(BinOpToken::Le) => BinOpKind::Le,
                TokenKind::BinOp(BinOpToken::Gt) => BinOpKind::Gt,
                TokenKind::BinOp(BinOpToken::Ge) => BinOpKind::Ge,
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
            None => {
                return Err(CompileError::new_unexpected_eof(
                    self.input,
                    Box::new("Token"),
                ))
            }
        };
        let pos = *pos;
        Ok(match **kind {
            TokenKind::BinOp(BinOpToken::Plus) => {
                tokens.next();
                Expr::new_unary(UnaryOp::Plus, self.parse_mul(tokens)?, pos)
            }
            TokenKind::BinOp(BinOpToken::Minus) => {
                tokens.next();
                Expr::new_unary(UnaryOp::Minus, self.parse_mul(tokens)?, pos)
            }
            TokenKind::Tilde => {
                tokens.next();
                Expr::new_unary(UnaryOp::BitInvert, self.parse_mul(tokens)?, pos)
            }
            TokenKind::Exclamation => {
                tokens.next();
                Expr::new_unary(UnaryOp::LogicalNot, self.parse_mul(tokens)?, pos)
            }
            TokenKind::BinOp(BinOpToken::Star) => {
                tokens.next();
                Expr::new_deref(self.parse_mul(tokens)?, pos)
            }
            TokenKind::BinOp(BinOpToken::And) => {
                tokens.next();
                Expr::new_addr(self.parse_mul(tokens)?, pos)
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
                    let expr = Expr::new_type_sizeof(self.parse_type_name(tokens)?, pos);
                    tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
                    expr
                } else {
                    // e.g) sizeof (5)
                    Expr::new_expr_sizeof(self.parse_unary(tokens)?, pos)
                }
            }
            _ => self.parse_postfix(tokens)?,
        })
    }
    pub fn parse_postfix<'b, I>(
        &self,
        tokens: &mut TokenStream<'b, I>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let mut expr = self.parse_primary(tokens)?;
        let mut pos = expr.pos;

        while tokens.consume(&TokenKind::OpenDelim(DelimToken::Bracket)) {
            expr = Expr::new_array(expr, self.parse_primary(tokens)?, pos);
            pos = expr.pos;
            tokens.expect(TokenKind::CloseDelim(DelimToken::Bracket))?;
        }
        Ok(expr)
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
                TokenKind::Str(letters) => Expr::new_str_lit(letters, pos),
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
                TokenKind::Eof => return Err(CompileError::new_unexpected_eof(self.input, Box::new("TokenKind::Num(_) | TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Paran)"))),
                _ => return Err(CompileError::new_expected_failed(self.input, Box::new("TokenKind::Num(_) | TokenKind::OpenDelim(DelimToken::Paran) | TokenKind::Ident"), Token::new(*kind, pos))),
            }),
            None => Err(CompileError::new_unexpected_eof(self.input, Box::new(
                "TokenKind::Num(_) | TokenKind::OpenDelim(DelimToken::Paran) | TokenKind::Ident",
            ))),
        }
    }

    pub fn parse_type_name<I>(&self, tokens: &mut TokenStream<I>) -> Result<TypeName, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token>,
    {
        let (ty_spec, pos) = self.parse_type_specifier(tokens)?;
        let n_star = Self::parse_pointer(tokens)?;
        // TODO: support DirectAbstractDeclarator
        let abs_declarator = if n_star == 0 {
            None
        } else {
            Some(AbstractDeclarator::new(n_star, None))
        };
        Ok(TypeName::new(SpecQual(ty_spec), abs_declarator, pos))
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Program {
    components: Vec<ProgramComponent>,
}

impl Program {
    pub const fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn with_vec(vec: Vec<ProgramComponent>) -> Self {
        Self { components: vec }
    }

    pub fn push(&mut self, component: ProgramComponent) {
        self.components.push(component);
    }
}

impl IntoIterator for Program {
    type Item = ProgramComponent;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct ProgramComponent {
    pub kind: ProgramKind,
    pub pos: Position,
}

impl ProgramComponent {
    pub const fn new(kind: ProgramKind, pos: Position) -> Self {
        Self { kind, pos }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ProgramKind {
    FuncDef(TypeSpec, Declarator, Stmt),
    Declaration(Declaration),
}

impl ProgramKind {
    pub const fn new_funcdef(
        type_spec: TypeSpec,
        n_star: usize,
        direct_declarator: DirectDeclarator,
        body: Stmt,
    ) -> Self {
        ProgramKind::FuncDef(type_spec, Declarator::new(n_star, direct_declarator), body)
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
/// Declaration
pub struct Declaration {
    // type-specifier
    pub ty_spec: TypeSpec,
    // init-declarator
    pub init_declarator: Option<InitDeclarator>,
    pub pos: Position,
}

impl Declaration {
    // TODO: need or not
    pub const fn new(
        ty_spec: TypeSpec,
        n_star: usize,
        direct_declarator: DirectDeclarator,
        initializer: Option<Initializer>,
        pos: Position,
    ) -> Self {
        Self {
            ty_spec,
            init_declarator: Some(InitDeclarator {
                declarator: Declarator::new(n_star, direct_declarator),
                initializer,
            }),
            pos,
        }
    }

    pub fn ident_name(&self) -> Option<&str> {
        self.init_declarator
            .as_ref()
            .map(|init_declarator| init_declarator.declarator.direct_declarator.ident_name())
    }

    pub fn ty(&self, analyzer: &mut Analyzer) -> Result<Type, CompileError> {
        analyzer.get_type(
            &self.ty_spec,
            &self
                .init_declarator
                .as_ref()
                .expect("get_type for struct is not yet implemented.")
                .declarator,
        )
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
}

impl InitDeclarator {
    pub fn ident_name(&self) -> &str {
        self.declarator.direct_declarator.ident_name()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Declarator {
    pub n_star: usize,
    pub direct_declarator: DirectDeclarator,
}

impl Declarator {
    pub const fn new(n_star: usize, direct_declarator: DirectDeclarator) -> Self {
        Self {
            n_star,
            direct_declarator,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum DirectDeclarator {
    Ident(String),
    Array(Box<DirectDeclarator>, Option<Expr>),
    Func(Box<DirectDeclarator>, Vec<Declaration>),
    Declarator(Box<Declarator>),
}

impl DirectDeclarator {
    pub fn ident_name(&self) -> &str {
        match self {
            DirectDeclarator::Ident(name) => name,
            DirectDeclarator::Func(direct_declarator, _)
            | DirectDeclarator::Array(direct_declarator, _) => direct_declarator.ident_name(),
            DirectDeclarator::Declarator(declarator) => declarator.direct_declarator.ident_name(),
        }
    }

    pub fn args(&self) -> Option<Vec<Declaration>> {
        match self {
            DirectDeclarator::Ident(_) => None,
            DirectDeclarator::Array(direct_declarator, _) => direct_declarator.args(),
            DirectDeclarator::Declarator(declarator) => declarator.direct_declarator.args(),
            DirectDeclarator::Func(_, args) => Some(args.clone()),
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Initializer {
    Expr(Expr),
    Array(Vec<Expr>),
}

impl Initializer {
    pub fn map(
        self,
        mut f: impl FnMut(Expr) -> Result<ConstExpr, CompileError>,
    ) -> Result<ConstInitializer, CompileError> {
        match self {
            Initializer::Expr(expr) => Ok(ConstInitializer::Expr(f(expr)?)),
            Initializer::Array(vec) => Ok(ConstInitializer::Array(
                vec.into_iter()
                    .map(f)
                    .collect::<Result<Vec<ConstExpr>, CompileError>>()?,
            )),
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum TypeSpec {
    Int,
    Char,
    StructOrUnion(StructOrUnionSpec),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum StructOrUnionSpec {
    WithList(Option<String>, Vec<StructDeclaration>),
    WithTag(String),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct StructDeclaration {
    ty_spec: TypeSpec,
    declarator: Declarator,
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

    pub fn new_for(
        init: Option<ForInitKind>,
        cond: Option<Expr>,
        inc: Option<Expr>,
        then: Stmt,
    ) -> Self {
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
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct TypeName {
    // TODO: actually Vec
    spec_quals: SpecQual,
    abstract_declarator: Option<AbstractDeclarator>,
    pos: Position,
}

impl TypeName {
    pub const fn new(
        spec_quals: SpecQual,
        abstract_declarator: Option<AbstractDeclarator>,
        pos: Position,
    ) -> Self {
        Self {
            spec_quals,
            abstract_declarator,
            pos,
        }
    }

    pub fn ty(&self) -> Type {
        // TODO: also consider array
        let base = match self.spec_quals.0 {
            TypeSpec::Int => BaseType::Int,
            TypeSpec::Char => BaseType::Char,
            TypeSpec::StructOrUnion(_) => todo!(),
        };
        let mut ty = Type::Base(base);
        if let Some(ref abstract_declarator) = self.abstract_declarator {
            for _ in 0..abstract_declarator.n_star {
                ty = Type::Ptr(Box::new(ty));
            }
        }
        ty
    }
}

/// \<specifier-qualifier-list\> := \<type-specifiers\>
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct SpecQual(TypeSpec);

/// \<abstract-declarator\> := "\*"\* \<direct-abstract-declarator\>?
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct AbstractDeclarator {
    /// "\*"\*
    n_star: usize,
    /// \<direct-abstract-declarator\>?
    direct_abstract_declarator: Option<DirectAbstractDeclarator>,
}

impl AbstractDeclarator {
    pub const fn new(
        n_star: usize,
        direct_abstract_declarator: Option<DirectAbstractDeclarator>,
    ) -> Self {
        Self {
            n_star,
            direct_abstract_declarator,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum DirectAbstractDeclarator {
    /// <direct-abstract-declarator>? "[]"
    Array(Option<Box<DirectAbstractDeclarator>>),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum StmtKind {
    Expr(Expr),
    Return(Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<ForInitKind>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Declare(Declaration),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ForInitKind {
    Declaration(Declaration),
    Expr(Expr),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Position,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ExprKind {
    Binary(Binary),
    Num(isize),
    StrLit(String),
    Unary(UnaryOp, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    LVar(String),
    Func(String, Vec<Expr>),
    Deref(Box<Expr>),
    Addr(Box<Expr>),
    SizeOf(SizeOfOperandKind),
    Array(Box<Expr>, Box<Expr>),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum SizeOfOperandKind {
    Expr(Box<Expr>),
    Type(TypeName),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    BitInvert,
    LogicalNot,
}

impl Expr {
    pub fn new_array(expr: Expr, index: Expr, pos: Position) -> Self {
        Self {
            kind: ExprKind::Array(Box::new(expr), Box::new(index)),
            pos,
        }
    }

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

    pub const fn new_str_lit(letters: String, pos: Position) -> Self {
        Self {
            kind: ExprKind::StrLit(letters),
            pos,
        }
    }

    pub const fn new_lvar(name: String, pos: Position) -> Self {
        Self {
            kind: ExprKind::LVar(name),
            pos,
        }
    }

    pub fn new_unary(kind: UnaryOp, expr: Expr, pos: Position) -> Self {
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
}
