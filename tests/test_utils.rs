extern crate ironcc;
use ironcc::{
    analyze::{Analyzer, ConvProgram},
    error::CompileError,
    parse::{Parser, Program},
    tokenize::{Token, TokenStream, Tokenizer},
};
use std::fmt::Debug;

#[cfg(test)]
#[macro_export]
macro_rules! tokens {
    ( $( $token_kind:expr ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            let pos = DebugInfo::default();
            temp_vec.push(Token::new($token_kind, pos));
        )*
        temp_vec
    }};
}

#[cfg(test)]
pub fn kind_eq(lhs: &[Token], rhs: &[Token]) -> bool {
    lhs.iter()
        .zip(rhs.iter())
        .fold(true, |acc, (l_token, r_token)| {
            acc && l_token.kind_eq(r_token)
        })
}

#[cfg(test)]
#[macro_export]
macro_rules! token_poses {
    ( $( ($token_kind:expr, $pos:expr) ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push(Token::new($token_kind, $pos));
        )*
        temp_vec
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! token_kinds {
    ( $( $token_kind:expr ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            let pos = DebugInfo::default();
            temp_vec.push(Token::new($token_kind, pos));
        )*
        temp_vec
            .into_iter()
            .map(|token| token.kind())
            .collect::<Vec<_>>()
    }};
}

pub struct CachedProcessor<'a> {
    tokenizer: CachedTokenizer<'a>,
    parser: CachedParser<'a>,
    analyzer: CachedAnalyzer<'a>,
}

impl<'a> CachedProcessor<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            tokenizer: CachedTokenizer::new(src),
            parser: CachedParser::new(src),
            analyzer: CachedAnalyzer::new(src),
        }
    }
}

impl<'a> CachedProcessor<'a> {
    pub fn program(&mut self) -> Result<&Program, CompileError> {
        let stream = self.tokenizer.stream()?;
        self.parser.program(stream)
    }

    pub fn tokens(&mut self) -> Result<&Vec<Token>, CompileError> {
        self.tokenizer.tokens()
    }

    pub fn conv_program(&mut self) -> Result<&ConvProgram, CompileError> {
        let program = self.program()?.clone();
        self.analyzer.conv_program(program)
    }
}

struct CachedTokenizer<'a> {
    src: &'a str,
    tokenizer: Tokenizer<'a>,
    tokens: Option<Result<Vec<Token>, CompileError>>,
    token_stream: Option<Result<TokenStream<'a, std::vec::IntoIter<Token>>, CompileError>>,
}

impl<'a> CachedTokenizer<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            tokenizer: Tokenizer::new(src),
            tokens: None,
            token_stream: None,
        }
    }

    fn tokens(&mut self) -> Result<&Vec<Token>, CompileError> {
        self.tokens
            .get_or_insert_with(|| self.tokenizer.tokenize())
            .as_ref()
            .map_err(|err| err.clone())
    }

    fn stream(&mut self) -> Result<&mut TokenStream<'a, std::vec::IntoIter<Token>>, CompileError> {
        let iter = self.tokens().map(|vec| vec.clone().into_iter())?;
        self.token_stream
            .get_or_insert_with(|| Ok(TokenStream::new(iter, self.src)))
            .as_mut()
            .map_err(|err| err.clone())
    }
}

struct CachedParser<'a> {
    parser: Parser<'a>,
    program: Option<Result<Program, CompileError>>,
}

impl<'a> CachedParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            parser: Parser::new(input),
            program: None,
        }
    }

    pub fn program<I>(&mut self, stream: &mut TokenStream<'a, I>) -> Result<&Program, CompileError>
    where
        I: Iterator<Item = Token> + Clone + Debug,
    {
        self.program
            .get_or_insert_with(|| self.parser.parse_program(stream))
            .as_ref()
            .map_err(|err| err.clone())
    }
}

struct CachedAnalyzer<'a> {
    analyzer: Analyzer<'a>,
    program: Option<Result<ConvProgram, CompileError>>,
}

impl<'a> CachedAnalyzer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            analyzer: Analyzer::new(input),
            program: None,
        }
    }

    pub fn conv_program(&mut self, program: Program) -> Result<&ConvProgram, CompileError> {
        self.program
            .get_or_insert_with(|| self.analyzer.traverse_program(program))
            .as_ref()
            .map_err(|err| err.clone())
    }
}

pub mod ast {
    use ironcc::{parse::*, tokenize::DebugInfo};
    pub fn deref(expr: Expr) -> Expr {
        Expr::new_deref(expr, DebugInfo::default())
    }

    pub fn addr(expr: Expr) -> Expr {
        Expr::new_addr(expr, DebugInfo::default())
    }

    pub fn func_def(
        type_spec: TypeSpec,
        n_star: usize,
        direct_declarator: DirectDeclarator,
        body: Stmt,
        pos: DebugInfo,
    ) -> ProgramComponent {
        ProgramComponent::new(
            ProgramKind::new_funcdef(type_spec, n_star, direct_declarator, body),
            pos,
        )
    }

    pub fn declare_stmt(declaration: Declaration) -> Stmt {
        Stmt::new_declare(declaration)
    }

    pub fn declare(
        ty_spec: TypeSpec,
        n_star: usize,
        direct_declarator: DirectDeclarator,
    ) -> Declaration {
        Declaration::new(
            ty_spec,
            n_star,
            direct_declarator,
            None,
            DebugInfo::default(),
        )
    }

    pub fn declare_with_init(
        ty_spec: TypeSpec,
        n_star: usize,
        direct_declarator: DirectDeclarator,
        init: Initializer,
    ) -> Declaration {
        Declaration::new(
            ty_spec,
            n_star,
            direct_declarator,
            Some(init),
            DebugInfo::default(),
        )
    }

    pub fn func_dd(name: &str, args: Vec<Declaration>, is_flexible: bool) -> DirectDeclarator {
        DirectDeclarator::Func(
            Box::new(DirectDeclarator::Ident(name.to_string())),
            args,
            is_flexible,
        )
    }

    pub fn expr_stmt(expr: Expr) -> Stmt {
        Stmt::expr(expr)
    }

    pub fn ret(expr: Expr) -> Stmt {
        Stmt::ret(expr)
    }

    pub fn block(stmts: Vec<Stmt>) -> Stmt {
        Stmt::new_block(stmts)
    }

    pub fn if_(cond: Expr, then: Stmt, els: Option<Stmt>) -> Stmt {
        Stmt::new_if(cond, then, els)
    }

    pub fn while_(cond: Expr, then: Stmt) -> Stmt {
        Stmt::new_while(cond, then)
    }

    pub fn for_(init: Option<Expr>, cond: Option<Expr>, inc: Option<Expr>, then: Stmt) -> Stmt {
        Stmt::new_for(init.map(ForInitKind::Expr), cond, inc, then)
    }

    pub fn func(name: &str, args: Vec<Expr>) -> Expr {
        Expr::new_func(name.to_string(), args, DebugInfo::default())
    }

    pub fn lvar(name: &str) -> Expr {
        Expr::new_lvar(name.to_string(), DebugInfo::default())
    }

    pub fn assign(lhs: Expr, rhs: Expr) -> Expr {
        Expr::new_assign(lhs, rhs, DebugInfo::default())
    }

    pub fn bin(op: BinOpKind, lhs: Expr, rhs: Expr) -> Expr {
        Expr::new_binary(op, lhs, rhs, DebugInfo::default())
    }

    pub fn num(n: isize) -> Expr {
        Expr::new_num(n, DebugInfo::default())
    }

    pub fn unary(op: UnaryOp, operand: Expr) -> Expr {
        Expr::new_unary(op, operand, DebugInfo::default())
    }
}
