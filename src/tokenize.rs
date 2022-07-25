use crate::error::{CompileError, CompileErrorKind, ParseErrorKind, TokenizeErrorKind};
use std::iter::Peekable;

pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn tokenize(&self) -> Result<Vec<Token>, CompileError> {
        let mut tokens = Vec::new();
        let mut pos = Position::default(); // 0, 0
        let mut input = <&str>::clone(&self.input);

        'tokenize_loop: while !input.is_empty() {
            // skip white spaces
            if input.starts_with(' ') || input.starts_with('\t') {
                pos.advance_char();
                input = &input[1..];
                continue;
            } else if input.starts_with('\n') {
                pos.advance_line();
                input = &input[1..];
                continue;
            }

            // reserved token
            let two_word_tokens = vec![
                ("<=", TokenKind::Le),
                (">=", TokenKind::Ge),
                ("==", TokenKind::EqEq),
                ("!=", TokenKind::Ne),
                ("+", TokenKind::BinOp(BinOpToken::Plus)),
                ("-", TokenKind::BinOp(BinOpToken::Minus)),
                ("*", TokenKind::BinOp(BinOpToken::Star)),
                ("/", TokenKind::BinOp(BinOpToken::Slash)),
                ("&", TokenKind::BinOp(BinOpToken::And)),
                ("%", TokenKind::BinOp(BinOpToken::Percent)),
                ("(", TokenKind::OpenDelim(DelimToken::Paran)),
                ("{", TokenKind::OpenDelim(DelimToken::Brace)),
                ("[", TokenKind::OpenDelim(DelimToken::Bracket)),
                (")", TokenKind::CloseDelim(DelimToken::Paran)),
                ("}", TokenKind::CloseDelim(DelimToken::Brace)),
                ("]", TokenKind::CloseDelim(DelimToken::Bracket)),
                (",", TokenKind::Comma),
                ("<", TokenKind::Lt),
                (">", TokenKind::Gt),
                (";", TokenKind::Semi),
                ("=", TokenKind::Eq),
            ];

            // <, <=, >, >=, ==, !=
            for (literal, kind) in two_word_tokens {
                if input.starts_with(literal) {
                    tokens.push(Token::new(kind, pos.advance_and_get_token(literal.len())));
                    input = &input[literal.len()..];
                    continue 'tokenize_loop;
                }
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
                    pos.advance_and_get_token(len_token),
                ));
                input = &input[len_token..];
                continue;
            } else if input
                .starts_with(&('a'..='z').chain(vec!['_'].into_iter()).collect::<Vec<_>>()[..])
            {
                // Ident or reserved Token
                let mut chars = input.chars().peekable();
                let mut ident = String::from(chars.next().unwrap());
                while let Some(
                    &('a'..='z') | '_' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9',
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
                        "sizeof" => TokenKind::SizeOf,
                        _ => TokenKind::Ident(ident),
                    },
                    pos.advance_and_get_token(len_token),
                ));
                input = &input[len_token..];
                continue;
            }
            return Err(self.new_unexpected_char(pos, input.chars().next().unwrap()));
        }
        tokens.push(Token::new(TokenKind::Eof, pos.advance_and_get_token(0)));

        Ok(tokens)
    }

    pub fn new_unexpected_char(&self, pos: Position, c: char) -> CompileError {
        CompileError::new(
            self.input,
            CompileErrorKind::TokenizeError(TokenizeErrorKind::UnexpectedChar(pos, c)),
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    BinOp(BinOpToken),
    Num(isize),
    /// An opening delimiter (e.g., `{`)
    OpenDelim(DelimToken),
    /// An closing delimiter (e.g., `}`)
    CloseDelim(DelimToken),
    /// Semicoron `;`
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
    /// `SizeOf`, reserved word
    SizeOf,
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
    /// `=` assign
    Eq,
    /// `,`
    Comma,
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeToken {
    /// `int`, type specifier
    Int,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DelimToken {
    /// A round parenthesis (i.e., `(` or `)`)
    Paran,
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
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: Box<TokenKind>,
    pub pos: Position,
}

impl Token {
    pub fn new(token_kind: TokenKind, pos: Position) -> Self {
        Self {
            kind: Box::new(token_kind),
            pos,
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

#[derive(Debug, PartialEq, Eq, Clone, Default, Copy)]
pub struct Position {
    pub n_char: usize,
    pub n_line: usize,
}

impl Position {
    pub const fn new(n_char: usize, n_line: usize) -> Self {
        Self { n_char, n_line }
    }

    pub fn advance_line(&mut self) {
        self.n_char = 0;
        self.n_line += 1;
    }

    /// increment `self.n_char`
    pub fn advance_char(&mut self) {
        self.n_char += 1;
    }

    #[must_use]
    pub fn advance_and_get_token(&mut self, len_token: usize) -> Self {
        let return_struct = *self;
        self.n_char += len_token;
        return_struct
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
            return matches!(*token.kind, TokenKind::Type(_));
        }
        false
    }

    pub fn expect_number(&mut self) -> Result<isize, CompileError> {
        let token = self.next();
        // let next = token.map(|token| *token.kind);
        match token {
            Some(Token { kind, pos }) => match *kind {
                TokenKind::Num(num) => Ok(num),
                _ => {
                    Err(self
                        .new_expected_failed(Box::new("TokenKind::Num(_)"), Token::new(*kind, pos)))
                }
            },
            _ => Err(self.new_unexpected_eof(Box::new("TokenKind::Num(_)"))),
        }
    }

    pub fn consume_ident(&mut self) -> Result<String, CompileError> {
        let token = self.next();
        match token {
            Some(token) => match *token.kind {
                TokenKind::Ident(name) => Ok(name),
                _ => Err(self.new_expected_failed(Box::new("TokenKind::Ident(_)"), token)),
            },
            _ => Err(self.new_unexpected_eof(Box::new("TokenKind::Ident(_)"))),
        }
    }

    /// if next token is expected kind, do nothing, otherwise `panic`
    pub fn expect(&mut self, kind: TokenKind) -> Result<(), CompileError> {
        let peeked_token = self.next();
        match peeked_token {
            Some(Token { kind: got, pos: _ })
                if matches!(*got, TokenKind::Eof) && kind != TokenKind::Eof =>
            {
                Err(self.new_unexpected_eof(Box::new(kind)))
            }
            None => Err(self.new_unexpected_eof(Box::new(kind))),
            Some(token) => {
                if kind != *token.kind {
                    return Err(self.new_expected_failed(Box::new(kind), token));
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

    /// # Panics
    /// when `token_stream` does not have next token
    pub fn at_eof(&mut self) -> bool {
        match self.peek_kind() {
            Some(token) => matches!(token, TokenKind::Eof),
            None => panic!("This stream is already used."),
        }
    }

    pub fn new_unexpected_eof(&self, kind: Box<dyn Debug>) -> CompileError {
        CompileError::new(
            self.input,
            CompileErrorKind::ParseError(ParseErrorKind::UnexpectedEof(kind)),
        )
    }

    pub fn new_expected_failed(&self, expect: Box<dyn Debug>, got: Token) -> CompileError {
        CompileError::new(
            self.input,
            CompileErrorKind::ParseError(ParseErrorKind::ExpectFailed { expect, got }),
        )
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
    let tokenizer = Tokenizer::new(input);
    Ok(tokenizer.tokenize()?.into_iter().map(Token::kind).collect())
}
