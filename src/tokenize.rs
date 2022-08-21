use crate::error::CompileError;
use crate::unimplemented_err;
use std::iter::Peekable;

pub struct Tokenizer {}

impl Tokenizer {
    pub const fn new() -> Self {
        Self {}
    }

    #[allow(clippy::too_many_lines)]
    pub fn tokenize(&self, file_info: &Rc<FileInfo>) -> Result<Vec<Token>, CompileError> {
        let mut tokens = Vec::new();
        let src = file_info.get_file_src();
        let mut pos = DebugInfo::default_with_file_info(file_info.clone()); // 0, 0
        let mut input = <&str>::clone(&src);

        'tokenize_loop: while !input.is_empty() {
            // skip white spaces
            if input.starts_with(' ') || input.starts_with('\t') {
                pos.advance(1);
                input = &input[1..];
                continue;
            } else if input.starts_with('\n') {
                pos.advance_line();
                input = &input[1..];
                continue;
            } else if input.starts_with("//") {
                let mut input_iter = input.chars();
                let mut num_this_line_char = 1;
                while !matches!(input_iter.next(), Some('\n')) {
                    num_this_line_char += 1;
                }
                pos.advance_line();
                input = &input[num_this_line_char..];
                continue;
            }
            if input.starts_with('#') {
                let mut input_iter = input.chars();
                let mut num_this_line_char = 1;
                while !matches!(input_iter.next(), Some('\n')) {
                    num_this_line_char += 1;
                }
                pos.advance_line();
                input = &input[num_this_line_char..];
                continue;
            }

            let symbols = vec![
                ("...", TokenKind::DotDotDot),
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
                ("=", TokenKind::Eq),
                (".", TokenKind::Dot),
            ];

            for (literal, kind) in symbols {
                if input.starts_with(literal) {
                    tokens.push(Token::new(kind, pos.get_pos_and_advance(literal.len())));
                    input = &input[literal.len()..];
                    continue 'tokenize_loop;
                }
            }

            if input.starts_with('"') {
                // string literal
                let mut chars = input.chars().peekable();
                chars.next(); // -> "
                let mut str_lit = String::new();
                let mut len_token = 1;
                loop {
                    match chars.peek() {
                        Some('"') => {
                            len_token += 1;
                            chars.next();
                            break;
                        }
                        Some('\\') => {
                            len_token += 1;
                            chars.next();
                            match chars.next() {
                                Some('n') => {
                                    len_token += 1;
                                    str_lit.push('\n');
                                }
                                Some('e') => {
                                    len_token += 1;
                                    str_lit.push(0x1bu8.into());
                                }
                                _ => {
                                    pos.advance(len_token);
                                    return Err(unimplemented_err!(
                                        self.input,
                                        pos,
                                        "This type of escape Sequences are not currently implemented."
                                    ));
                                }
                            }
                        }
                        Some(c) => {
                            len_token += 1;
                            str_lit.push(*c);
                            chars.next();
                        }
                        None => return Err(CompileError::new_unexpected_eof_tokenize(src, pos)),
                    }
                }
                tokens.push(Token::new(
                    TokenKind::Str(str_lit),
                    pos.get_pos_and_advance(len_token),
                ));
                input = &input[len_token..];
                continue;
            }

            if input.starts_with("0b") {
                input = &input[2..];
                let mut chars = input.chars().peekable();
                let mut number = String::new();
                while let Some(&('0' | '1')) = chars.peek() {
                    number.push(chars.next().unwrap());
                }
                let len_token = number.len();
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
                                    debug_info: pos,
                                },
                            ));
                        }
                    }
                }
                tokens.push(Token::new(
                    TokenKind::Num(num as isize),
                    pos.get_pos_and_advance(len_token),
                ));
                input = &input[len_token..];
                continue;
            }

            if input.starts_with(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) {
                let mut chars = input.chars().peekable();
                let mut number = String::from(chars.next().unwrap());
                while let Some(&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =
                    chars.peek()
                {
                    number.push(chars.next().unwrap());
                }
                let len_token = number.len();
                let num = number
                    .parse::<usize>()
                    .expect("Currently support only a number literal.");
                tokens.push(Token::new(
                    TokenKind::Num(num as isize),
                    pos.get_pos_and_advance(len_token),
                ));
                input = &input[len_token..];
                continue;
            } else if input.starts_with(
                &('a'..='z')
                    .chain('A'..='Z')
                    .chain(vec!['_'].into_iter())
                    .collect::<Vec<_>>()[..],
            ) {
                // Ident or reserved token
                let mut chars = input.chars().peekable();
                let mut ident = String::from(chars.next().unwrap());
                while let Some(
                    &('a'..='z')
                    | &('A'..='Z')
                    | '_'
                    | '0'
                    | '1'
                    | '2'
                    | '3'
                    | '4'
                    | '5'
                    | '6'
                    | '7'
                    | '8'
                    | '9',
                ) = chars.peek()
                {
                    ident.push(chars.next().unwrap());
                }
                let len_token = ident.len();
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
                        "const" => {
                            // TODO: support const
                            eprintln!("WARNING: `const` is just ignored this time.");
                            input = &input[5..];
                            pos.get_pos_and_advance(5);
                            continue;
                        }
                        _ => TokenKind::Ident(ident),
                    },
                    pos.get_pos_and_advance(len_token),
                ));
                input = &input[len_token..];
                continue;
            }
            return Err(CompileError::new_unexpected_char(
                pos,
                input.chars().next().unwrap(),
            ));
        }
        tokens.push(Token::new(TokenKind::Eof, pos.get_pos_and_advance(0)));

        Ok(tokens)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    BinOp(BinOpToken),
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
    /// `=` assign
    Eq,
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
pub struct Token {
    pub kind: Box<TokenKind>,
    pub debug_info: DebugInfo,
}

impl Token {
    pub fn new(token_kind: TokenKind, pos: DebugInfo) -> Self {
        Self {
            kind: Box::new(token_kind),
            debug_info: pos,
        }
    }

    pub fn kind_eq(&self, rhs: &Token) -> bool {
        self.kind == rhs.kind
    }

    // clippy gives incorrect warning
    #[allow(clippy::missing_const_for_fn)]
    pub fn kind(self) -> Box<TokenKind> {
        self.kind
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug, Default)]
pub struct DebugInfo {
    file_info: Rc<FileInfo>,
    pos: Position,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug, Default)]
pub struct FileInfo {
    file_name: String,
    src: String,
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
pub struct TokenStream<'a, I>
where
    I: Iterator<Item = Token> + Clone + Debug,
{
    iter: Peekable<I>,
    input: &'a str,
}

use std::fmt::Debug;
use std::rc::Rc;
impl<'a, I> Debug for TokenStream<'a, I>
where
    I: Iterator<Item = Token> + Clone + Debug,
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

impl<'a, I: Iterator<Item = Token> + Clone + Debug> TokenStream<'a, I> {
    pub fn new(iter: I, input: &'a str) -> Self {
        Self {
            iter: iter.peekable(),
            input,
        }
    }

    /// if next token is passed kind consume it and return true, else do nothing and return false
    pub fn consume(&mut self, kind: &TokenKind) -> bool {
        if let Some(token) = self.peek() {
            if *token.kind == *kind {
                self.next();
                return true;
            }
        }
        false
    }

    /// if next token is passed kind, then return true, otherwise return false (Not consume)
    pub fn peek_expect(&mut self, kind: &TokenKind) -> bool {
        if let Some(token) = self.peek() {
            if *token.kind == *kind {
                return true;
            }
        }
        false
    }

    /// Return next token is `TokenKind::Type` or not.(Not consume)
    pub fn is_type(&mut self) -> bool {
        if let Some(token) = self.peek() {
            return matches!(*token.kind, TokenKind::Type(_) | TokenKind::Struct);
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
        let token = self.next();
        match token {
            Some(token) => match *token.kind {
                TokenKind::Ident(name) => Ok((name, token.debug_info)),
                _ => Err(CompileError::new_expected_failed(
                    Box::new("TokenKind::Ident(_)"),
                    token,
                )),
            },
            _ => Err(CompileError::new_unexpected_eof(
                None,
                Box::new("TokenKind::Ident(_)"),
            )),
        }
    }

    /// if next token is expected kind, do nothing, otherwise `panic`
    pub fn expect(&mut self, kind: TokenKind) -> Result<(), CompileError> {
        let peeked_token = self.next();
        match peeked_token {
            Some(Token {
                kind: got,
                debug_info,
            }) if matches!(*got, TokenKind::Eof) && kind != TokenKind::Eof => Err(
                CompileError::new_unexpected_eof(Some(debug_info.get_file_src()), Box::new(kind)),
            ),
            None => Err(CompileError::new_unexpected_eof(None, Box::new(kind))),
            Some(token) => {
                if kind != *token.kind {
                    return Err(CompileError::new_expected_failed(Box::new(kind), token));
                }
                Ok(())
            }
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.iter.peek()
    }

    pub fn peek_kind(&mut self) -> Option<TokenKind> {
        self.iter.peek().map(|token| *token.kind.clone())
    }

    pub fn next_kind(&mut self) -> Option<TokenKind> {
        self.next().map(|token| *token.kind)
    }

    /// # Panics
    /// when `token_stream` does not have next token
    pub fn at_eof(&mut self) -> bool {
        match self.peek_kind() {
            Some(token) => matches!(token, TokenKind::Eof),
            None => panic!("This stream is already used."),
        }
    }
}

impl<'a, I: Iterator<Item = Token> + Clone + Debug> Iterator for TokenStream<'a, I> {
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
pub fn kind_eq(lhs: &[Token], rhs: &[Token]) -> bool {
    lhs.iter()
        .zip(rhs.iter())
        .fold(true, |acc, (l_token, r_token)| {
            acc && l_token.kind_eq(r_token)
        })
}

pub fn tokenize_and_kinds(input: &str) -> Result<Vec<Box<TokenKind>>, CompileError> {
    let tokenizer = Tokenizer::new();
    Ok(tokenizer
        .tokenize(&Rc::new(FileInfo {
            file_name: String::new(),
            src: input.to_string(),
        }))?
        .into_iter()
        .map(Token::kind)
        .collect())
}
