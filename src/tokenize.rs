use crate::error::CompileError;
use crate::parse::{self, BinOpKind, Scope};
use crate::preprocess::{
    Preprocessor, PreprocessorTokenContainerStream, PreprocessorTokenStream, SrcCursor,
};
use crate::unimplemented_err;
use std::iter::Peekable;

#[derive(Debug)]
pub struct Tokenizer {
    stream: PreprocessorTokenContainerStream,
}

impl Tokenizer {
    pub const fn new(stream: PreprocessorTokenContainerStream) -> Self {
        Self { stream }
    }

    #[allow(clippy::too_many_lines)]
    pub fn tokenize(
        &mut self,
        _file_info: &Rc<FileInfo>,
    ) -> Result<Vec<Token<TokenKind>>, CompileError> {
        let mut tokens = Vec::new();

        'tokenize_loop: while !self.stream.is_empty() {
            // skip white spaces
            if self.stream.starts_with(" ")
                || self.stream.starts_with("\t")
                || self.stream.starts_with("\n")
            {
                self.stream.advance(1);
                continue;
            }
            if self.stream.starts_with("//") {
                // TODO: remove unwrap
                self.stream.advance_until('\n').unwrap();
                continue;
            }
            if self.stream.starts_with("/*") {
                while !self.stream.starts_with("*/") {
                    self.stream.advance(1);
                }
                self.stream.advance(2);
                continue;
            }
            if self.stream.starts_with("#") {
                self.stream.advance_until('\n').unwrap();
                continue;
            }

            let symbols = vec![
                ("...", TokenKind::DotDotDot),
                ("<<=", TokenKind::BinOpEq(AssignBinOpToken::LShift)),
                (">>=", TokenKind::BinOpEq(AssignBinOpToken::RShift)),
                ("<<", TokenKind::BinOp(BinOpToken::LShift)),
                (">>", TokenKind::BinOp(BinOpToken::RShift)),
                ("<=", TokenKind::BinOp(BinOpToken::Le)),
                (">=", TokenKind::BinOp(BinOpToken::Ge)),
                ("==", TokenKind::BinOp(BinOpToken::EqEq)),
                ("!=", TokenKind::BinOp(BinOpToken::Ne)),
                ("||", TokenKind::BinOp(BinOpToken::VerticalVertical)),
                ("&&", TokenKind::BinOp(BinOpToken::AndAnd)),
                ("++", TokenKind::PlusPlus),
                ("--", TokenKind::MinusMinus),
                ("->", TokenKind::Arrow),
                ("+=", TokenKind::BinOpEq(AssignBinOpToken::Plus)),
                ("-=", TokenKind::BinOpEq(AssignBinOpToken::Minus)),
                ("*=", TokenKind::BinOpEq(AssignBinOpToken::Star)),
                ("/=", TokenKind::BinOpEq(AssignBinOpToken::Slash)),
                ("&=", TokenKind::BinOpEq(AssignBinOpToken::And)),
                ("%=", TokenKind::BinOpEq(AssignBinOpToken::Percent)),
                ("+", TokenKind::BinOp(BinOpToken::Plus)),
                ("-", TokenKind::BinOp(BinOpToken::Minus)),
                ("*", TokenKind::BinOp(BinOpToken::Star)),
                ("/", TokenKind::BinOp(BinOpToken::Slash)),
                ("&", TokenKind::BinOp(BinOpToken::And)),
                ("%", TokenKind::BinOp(BinOpToken::Percent)),
                ("(", TokenKind::OpenDelim(DelimToken::Paren)),
                ("{", TokenKind::OpenDelim(DelimToken::Brace)),
                ("[", TokenKind::OpenDelim(DelimToken::Bracket)),
                (")", TokenKind::CloseDelim(DelimToken::Paren)),
                ("}", TokenKind::CloseDelim(DelimToken::Brace)),
                ("]", TokenKind::CloseDelim(DelimToken::Bracket)),
                (",", TokenKind::Comma),
                ("<", TokenKind::BinOp(BinOpToken::Lt)),
                (">", TokenKind::BinOp(BinOpToken::Gt)),
                ("~", TokenKind::Tilde),
                ("!", TokenKind::Exclamation),
                ("?", TokenKind::Question),
                (":", TokenKind::Colon),
                (";", TokenKind::Semi),
                ("=", TokenKind::BinOpEq(AssignBinOpToken::Eq)),
                (".", TokenKind::Dot),
            ];

            for (literal, kind) in symbols {
                if self.stream.starts_with(literal) {
                    let this_token_debug_info = self
                        .stream
                        .get_debug_info_and_advance(literal.len())
                        .unwrap();
                    tokens.push(Token::new(kind, this_token_debug_info));
                    continue 'tokenize_loop;
                }
            }

            if self.stream.starts_with("'") {
                // char literal
                let (debug_info, _) = self.stream.next().unwrap(); // -> '
                let ch;
                match self.stream.next() {
                    Some((_, '\\')) => match self.stream.next() {
                        Some((_, 'n')) => ch = '\n',
                        Some((_, 't')) => ch = '\t',
                        Some((_, 'e')) => {
                            ch = 0x1bu8.into();
                        }
                        Some((_, '0')) => ch = '\0',
                        _ => {
                            let debug_info =
                                self.stream.peek().map_or(
                                    self.stream.get_prev_debug_info(),
                                    |(debug_info, _)| debug_info.clone(),
                                );
                            return Err(unimplemented_err!(
                                debug_info,
                                "This type of escape Sequences are not currently implemented."
                            ));
                        }
                    },
                    Some((_, c)) => {
                        ch = c;
                    }
                    None => return Err(CompileError::new_unexpected_eof_tokenize(debug_info)),
                }
                let popped = self.stream.next();
                match popped {
                    Some((_, '\'')) => {
                        tokens.push(Token::new(TokenKind::Num(ch as isize), debug_info));
                        continue;
                    }
                    Some((debug_info, ch)) => {
                        return Err(unimplemented_err!(
                            debug_info,
                            format!("Expected ' , but got {}", ch)
                        ));
                    }
                    None => return Err(CompileError::new_unexpected_eof_tokenize(debug_info)),
                }
            }

            if self.stream.starts_with("\"") {
                // string literal
                let (debug_info, _) = self.stream.next().unwrap(); // -> "
                let mut str_lit = String::new();
                loop {
                    match self.stream.next() {
                        Some((_, '"')) => {
                            break;
                        }
                        Some((_, '\\')) => match self.stream.next() {
                            Some((_, 'n')) => {
                                str_lit.push('\n');
                            }
                            Some((_, 't')) => {
                                str_lit.push('\t');
                            }
                            Some((_, 'e')) => {
                                str_lit.push(0x1bu8.into());
                            }
                            _ => {
                                let debug_info = self.stream.peek().map_or(
                                    self.stream.get_prev_debug_info(),
                                    |(debug_info, _)| debug_info.clone(),
                                );
                                return Err(unimplemented_err!(
                                    debug_info,
                                    "This type of escape Sequences are not currently implemented."
                                ));
                            }
                        },
                        Some((_, c)) => {
                            str_lit.push(c);
                        }
                        None => return Err(CompileError::new_unexpected_eof_tokenize(debug_info)),
                    }
                }
                tokens.push(Token::new(TokenKind::Str(str_lit), debug_info));
                continue;
            }

            if self.stream.starts_with("0b") {
                let debug_info = self.stream.get_debug_info_and_advance(2).unwrap();
                let mut number = String::new();
                while let Some((_, n @ ('0' | '1'))) = self.stream.peek() {
                    number.push(*n);
                    self.stream.next();
                }
                let mut num = 0;
                for c in number.chars() {
                    num *= 2;
                    match c {
                        '0' => {}
                        '1' => {
                            num += 1;
                        }
                        _ => {
                            return Err(CompileError::new_expected_failed(
                                Box::new("'0' | '1'"),
                                Token {
                                    kind: Box::new(TokenKind::Ident(c.to_string())),
                                    debug_info,
                                },
                            ));
                        }
                    }
                }
                tokens.push(Token::new(TokenKind::Num(num as isize), debug_info));
                continue;
            }

            if self.stream.starts_with_number() {
                let (debug_info, ch) = self.stream.next().unwrap();
                let mut number = String::from(ch);
                while self.stream.starts_with_number() {
                    let (_, ch) = self.stream.next().unwrap();
                    number.push(ch);
                }
                let num = number
                    .parse::<usize>()
                    .expect("Currently support only a number literal.");
                tokens.push(Token::new(TokenKind::Num(num as isize), debug_info));
                continue;
            } else if self.stream.starts_with_alphabet() || self.stream.starts_with("_") {
                // Ident or reserved token
                let (debug_info, ch) = self.stream.next().unwrap();
                let mut ident = String::from(ch);
                while self.stream.starts_with_alphabet()
                    || self.stream.starts_with("_")
                    || self.stream.starts_with_number()
                {
                    let (_, ch) = self.stream.next().unwrap();
                    ident.push(ch);
                }
                tokens.push(Token::new(
                    match ident.as_str() {
                        "return" => TokenKind::Return,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "while" => TokenKind::While,
                        "for" => TokenKind::For,
                        "int" => TokenKind::Type(TypeToken::Int),
                        "char" => TokenKind::Type(TypeToken::Char),
                        "void" => TokenKind::Type(TypeToken::Void),
                        "sizeof" => TokenKind::SizeOf,
                        "struct" => TokenKind::Struct,
                        "enum" => TokenKind::Enum,
                        "typedef" => TokenKind::TypeDef,
                        "extern" => TokenKind::Extern,
                        "__asm__" => TokenKind::Asm,
                        "__nullptr" => TokenKind::NullPtr,
                        "switch" => TokenKind::Switch,
                        "default" => TokenKind::Default,
                        "case" => TokenKind::Case,
                        "break" => TokenKind::Break,
                        "continue" => TokenKind::Continue,
                        "const" => {
                            // TODO: support const
                            eprintln!("WARNING: `const` is just ignored this time.");
                            continue;
                        }
                        "static" => {
                            // TODO: support static
                            eprintln!("WARNING: `static` is just ignored this time.");
                            continue;
                        }
                        _ => TokenKind::Ident(ident),
                    },
                    debug_info,
                ));
                continue;
            }
            let (debug_info, ch) = self.stream.next().unwrap();
            return Err(CompileError::new_unexpected_char(debug_info, ch));
        }
        tokens.push(Token::new(
            TokenKind::Eof,
            self.stream.get_prev_debug_info(),
        ));

        Ok(tokens)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    /// Binary operator
    BinOp(BinOpToken),
    /// Binary operator with eq (`=`) such as `+=`
    BinOpEq(AssignBinOpToken),
    /// string literal
    Str(String),
    /// number literal
    Num(isize),
    /// An opening delimiter (e.g., `{`)
    OpenDelim(DelimToken),
    /// An closing delimiter (e.g., `}`)
    CloseDelim(DelimToken),
    /// Semicolon `;`
    Semi,
    /// An ident
    Ident(String),
    /// type specifiers
    Type(TypeToken),
    /// `return`, reserved word
    Return,
    /// `if`, reserved word
    If,
    /// `else`, reserved word
    Else,
    /// `while`, reserved word
    While,
    /// `for`, reserved word
    For,
    /// `sizeof`, reserved word
    SizeOf,
    /// `struct`, reserved word
    Struct,
    /// `enum`, reserved word
    Enum,
    /// `typedef`, reserved word
    TypeDef,
    /// `extern`, reserved word
    Extern,
    /// `switch`, reserved word
    Switch,
    /// `default`, reserved word
    Default,
    /// `case`, reserved word
    Case,
    /// `break`, reserved word
    Break,
    /// `continue`, reserved word
    Continue,
    /// `__asm__`, reserved word (not standard)
    Asm,
    /// `__nullptr`, reserved word (not standard)
    NullPtr,
    /// `,`
    Comma,
    /// `~`
    Tilde,
    /// `!`
    Exclamation,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `.`
    Dot,
    /// `->`
    Arrow,
    /// `...`
    DotDotDot,
    /// `++`
    PlusPlus,
    /// `--`
    MinusMinus,
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeToken {
    /// `int`, type specifier
    Int,
    /// `char`, type specifier
    Char,
    /// `void`, type specifier
    Void,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DelimToken {
    /// A round parenthesis (i.e., `(` or `)`)
    Paren,
    /// A square bracket (i.e., `[` or `]`)
    Bracket,
    /// A curly brase (i.e., `{` or `}`)
    Brace,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum AssignBinOpToken {
    /// `=`
    Eq,
    /// `+=`
    Plus,
    /// `-=`
    Minus,
    /// `*=`
    Star,
    /// `/=`
    Slash,
    /// `%=`
    Percent,
    /// `&=`
    And,
    /// `<<=`
    LShift,
    /// `>>=`
    RShift,
}

impl TryFrom<AssignBinOpToken> for parse::BinOpKind {
    type Error = ();
    fn try_from(op: AssignBinOpToken) -> Result<Self, ()> {
        Ok(match op {
            AssignBinOpToken::Eq => return Err(()),
            AssignBinOpToken::Plus => BinOpKind::Add,
            AssignBinOpToken::Minus => BinOpKind::Sub,
            AssignBinOpToken::Star => BinOpKind::Mul,
            AssignBinOpToken::Slash => BinOpKind::Div,
            AssignBinOpToken::Percent => BinOpKind::Rem,
            AssignBinOpToken::And => BinOpKind::BitWiseAnd,
            AssignBinOpToken::LShift => BinOpKind::LShift,
            AssignBinOpToken::RShift => BinOpKind::RShift,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOpToken {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `&`
    And,
    /// `<` Less than
    Lt,
    /// `<=` Less equal
    Le,
    /// `>` Greater than
    Gt,
    /// `>=` Greater equal
    Ge,
    /// `==` Equal equal
    EqEq,
    /// `!=` Not equal
    Ne,
    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `||` logical or
    VerticalVertical,
    /// `&&` logical and
    AndAnd,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<K: PartialEq + Debug> {
    pub kind: Box<K>,
    pub debug_info: DebugInfo,
}

impl<K: PartialEq + Debug + Eof> Token<K> {
    pub fn new(token_kind: K, debug_info: DebugInfo) -> Self {
        Self {
            kind: Box::new(token_kind),
            debug_info,
        }
    }

    pub fn kind_eq(&self, rhs: &Token<K>) -> bool {
        self.kind == rhs.kind
    }

    // clippy gives incorrect warning
    #[allow(clippy::missing_const_for_fn)]
    pub fn kind(self) -> Box<K> {
        self.kind
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug, Default)]
pub struct DebugInfo {
    file_info: Rc<FileInfo>,
    pos: Position,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Default)]
pub struct FileInfo {
    file_name: String,
    src: String,
}

impl std::fmt::Debug for FileInfo {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl FileInfo {
    pub const fn new(file_name: String, src: String) -> Self {
        Self { file_name, src }
    }

    pub fn get_file_name(&self) -> &str {
        &self.file_name
    }

    pub fn get_file_src(&self) -> &str {
        &self.src
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug, Copy, Default)]
pub struct Position {
    pub n_char: usize,
    pub n_line: usize,
}

impl Position {
    pub const fn new(n_char: usize, n_line: usize) -> Self {
        Self { n_char, n_line }
    }

    pub const fn get_n_char(&self) -> usize {
        self.n_char
    }

    pub const fn get_n_line(&self) -> usize {
        self.n_line
    }

    pub fn get_n_char_mut(&mut self) -> &mut usize {
        &mut self.n_char
    }

    pub fn get_n_line_mut(&mut self) -> &mut usize {
        &mut self.n_line
    }
}

impl DebugInfo {
    pub fn new(file_info: Rc<FileInfo>, n_char: usize, n_line: usize) -> Self {
        Self {
            file_info,
            pos: Position { n_char, n_line },
        }
    }

    pub fn default_with_file_info(file_info: Rc<FileInfo>) -> Self {
        Self {
            file_info,
            pos: Position::default(),
        }
    }

    pub fn to_error_msg_prefix_string(&self) -> String {
        format!("{}:{}", self.file_info.file_name, self.get_n_line(),)
    }

    pub fn get_cloned_file_info(&self) -> Rc<FileInfo> {
        Rc::clone(&self.file_info)
    }

    pub fn get_file_name(&self) -> String {
        self.file_info.file_name.clone()
    }

    pub fn get_file_src(&self) -> String {
        self.file_info.src.clone()
    }

    pub const fn get_n_char(&self) -> usize {
        self.pos.n_char
    }

    pub const fn get_n_line(&self) -> usize {
        self.pos.n_line
    }

    pub fn get_n_char_mut(&mut self) -> &mut usize {
        &mut self.pos.n_char
    }

    pub fn get_n_line_mut(&mut self) -> &mut usize {
        &mut self.pos.n_line
    }

    pub fn advance_line(&mut self) {
        self.pos.n_char = 0;
        self.pos.n_line += 1;
    }

    #[allow(clippy::return_self_not_must_use)]
    pub fn get_pos_and_advance(&mut self, len_token: usize) -> Self {
        let return_struct = self.clone();
        self.pos.n_char += len_token;
        return_struct
    }

    /// just advance `self.n_char` by `len_token`
    pub fn advance(&mut self, len_token: usize) {
        self.pos.n_char += len_token;
    }
}

#[derive(Clone)]
pub struct TokenStream<I, K>
where
    K: PartialEq + Debug + Clone + Eof,
    I: Iterator<Item = Token<K>> + Clone + Debug,
{
    iter: Peekable<I>,
}

pub trait Eof {
    fn is_eof(&self) -> bool;
}

impl Eof for TokenKind {
    fn is_eof(&self) -> bool {
        *self == TokenKind::Eof
    }
}

use std::fmt::Debug;
use std::rc::Rc;
impl<I, K> Debug for TokenStream<I, K>
where
    K: PartialEq + Eof + Debug + Clone,
    I: Iterator<Item = Token<K>> + Clone + Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stream = self.clone();
        write!(
            f,
            "{:?}",
            stream.map(|token| token.kind).collect::<Vec<_>>()
        )
    }
}
impl<I, K> TokenStream<I, K>
where
    K: PartialEq + Eof + Debug + Clone + 'static,
    I: Iterator<Item = Token<K>> + Clone + Debug,
{
    pub fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
        }
    }

    /// if next token is passed kind consume it and return true, else do nothing and return false
    pub fn consume(&mut self, kind: &K) -> bool {
        if let Some(token) = self.peek() {
            if *token.kind == *kind {
                self.next();
                return true;
            }
        }
        false
    }

    /// if next token is passed kind, then return true, otherwise return false (Not consume)
    pub fn peek_expect(&mut self, kind: &K) -> bool {
        if let Some(token) = self.peek() {
            if *token.kind == *kind {
                return true;
            }
        }
        false
    }

    /// if next token is expected kind, do nothing, otherwise `panic`
    pub fn expect(&mut self, kind: &K) -> Result<DebugInfo, CompileError>
    where
        Token<K>: Into<crate::error::Tokens>,
    {
        let peeked_token = self.next();
        match peeked_token {
            Some(Token {
                kind: got,
                debug_info,
            }) if got.is_eof() && !kind.is_eof() => Err(CompileError::new_unexpected_eof(
                Some(debug_info.get_file_src()),
                Box::new(kind.clone()),
            )),
            None => Err(CompileError::new_unexpected_eof(
                None,
                Box::new(kind.clone()),
            )),
            Some(token) => {
                if *kind != *token.kind {
                    return Err(CompileError::new_expected_failed(
                        Box::new(kind.clone()),
                        token,
                    ));
                }
                Ok(token.debug_info)
            }
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.iter.peek()
    }

    pub fn peek_kind(&mut self) -> Option<K> {
        self.iter.peek().map(|token| *token.kind.clone())
    }

    pub fn next_kind(&mut self) -> Option<K> {
        self.next().map(|token| *token.kind)
    }

    /// # Panics
    /// when `token_stream` does not have next token
    pub fn at_eof(&mut self) -> bool {
        match self.peek_kind() {
            Some(token) => token.is_eof(),
            None => panic!("This stream is already used."),
        }
    }
}

impl<I: Iterator<Item = Token<TokenKind>> + Clone + Debug> TokenStream<I, TokenKind> {
    /// Return next token is the beginning of `type-specifier` or not.(Not consume)
    pub fn is_starting_declaration(&mut self, scope: &Scope) -> bool {
        if let Some(token) = self.peek() {
            return matches!(
                *token.kind,
                TokenKind::Type(_) | TokenKind::Struct | TokenKind::Enum | TokenKind::TypeDef
            ) || if let TokenKind::Ident(ref ident) = *token.kind {
                scope.look_up_typedef_name(ident).is_some()
            } else {
                false
            };
        }
        false
    }

    pub fn expect_number(&mut self) -> Result<isize, CompileError> {
        let token = self.next();
        // let next = token.map(|token| *token.kind);
        match token {
            Some(Token {
                kind,
                debug_info: pos,
            }) => match *kind {
                TokenKind::Num(num) => Ok(num),
                _ => Err(CompileError::new_expected_failed(
                    Box::new("TokenKind::Num(_)"),
                    Token::new(*kind, pos),
                )),
            },
            None => Err(CompileError::new_unexpected_eof(
                None,
                Box::new("TokenKind::Num(_)"),
            )),
        }
    }

    /// if next token is ident, then return its name and Position, otherwise return Err(_)
    pub fn consume_ident(&mut self) -> Result<(String, DebugInfo), CompileError> {
        let token = self.peek().cloned();
        match token {
            Some(token) => match *token.kind {
                TokenKind::Ident(name) => {
                    self.next();
                    Ok((name, token.debug_info))
                }
                _ => Err(CompileError::new_ident_expected_failed(token)),
            },
            _ => Err(CompileError::new_unexpected_eof(
                None,
                Box::new("TokenKind::Ident(_)"),
            )),
        }
    }

    pub fn peek_debug_info(&mut self) -> Option<DebugInfo> {
        self.peek().map(|token| token.debug_info.clone())
    }
}

impl<K: PartialEq + Debug + Clone + Eof, I: Iterator<Item = Token<K>> + Clone + Debug> Iterator
    for TokenStream<I, K>
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! tokens {
    ( $( $token_kind:expr ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            let pos = $crate::tokenize::Position::default();
            temp_vec.push(Token::new($token_kind, pos));
        )*
        temp_vec
    }};
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
            let pos = $crate::tokenize::Position::default();
            temp_vec.push(Token::new($token_kind, pos));
        )*
        temp_vec
            .into_iter()
            .map(|token| token.kind())
            .collect::<Vec<_>>()
    }};
}

#[cfg(test)]
pub fn kind_eq(lhs: &[Token<TokenKind>], rhs: &[Token<TokenKind>]) -> bool {
    lhs.iter()
        .zip(rhs.iter())
        .fold(true, |acc, (l_token, r_token)| {
            acc && l_token.kind_eq(r_token)
        })
}

pub fn tokenize_and_kinds(input: &str) -> Result<Vec<Box<TokenKind>>, CompileError> {
    let file_info = Rc::new(FileInfo {
        file_name: String::new(),
        src: input.to_string(),
    });
    let mut preproccor = Preprocessor::new(file_info.clone(), "");
    let tokens = preproccor.preprocess(&mut SrcCursor::new(file_info.clone()), None)?;
    let stream = PreprocessorTokenStream::new(tokens.into_iter());
    let mut tokenizer = Tokenizer::new(PreprocessorTokenContainerStream::new(stream.collect()));
    Ok(tokenizer
        .tokenize(&file_info)?
        .into_iter()
        .map(Token::kind)
        .collect())
}
