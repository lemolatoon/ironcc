use std::iter::Peekable;

pub fn tokenize(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut input_chars = input.chars().peekable();
    let mut pos = Position::default(); // 0, 0
    while let Some(c) = input_chars.next() {
        match c {
            // skip white spaces
            ' ' | '\t' => {
                pos.next_char();
            }
            '\n' => {
                pos.next_line();
            }
            '+' => tokens.push(Token::new(
                TokenKind::BinOp(BinOpToken::Plus),
                pos.next_char(),
            )),
            '-' => tokens.push(Token::new(
                TokenKind::BinOp(BinOpToken::Minus),
                pos.next_char(),
            )),
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut number = String::from(c);
                while let Some(&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =
                    input_chars.peek()
                {
                    number.push(input_chars.next().unwrap())
                }
                let len_token = number.len();
                let num = number
                    .parse::<usize>()
                    .expect("Currently support only a number literal.");
                tokens.push(Token::new(
                    TokenKind::Num(num as isize),
                    pos.next_token(len_token),
                ))
            }
            _ => panic!("Unexpected char while tokenize: {}", c),
        }
    }
    tokens.push(Token::new(TokenKind::Eof, pos.next_token(0)));

    tokens
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    BinOp(BinOpToken),
    Num(isize),
    Eof,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum BinOpToken {
    Plus,
    Minus,
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

    pub fn kind(self) -> Box<TokenKind> {
        self.kind
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Position {
    n_char: usize,
    n_line: usize,
}

impl Position {
    pub fn new(n_char: usize, n_line: usize) -> Self {
        Self { n_char, n_line }
    }

    pub fn next_line(&mut self) -> Self {
        let return_struct = self.clone();
        self.n_char = 0;
        self.n_line += 1;
        return_struct
    }

    // increment self.n_char and return not incremented, cloned Position struct
    pub fn next_char(&mut self) -> Self {
        let return_struct = self.clone();
        self.n_char += 1;
        return_struct
    }

    pub fn next_token(&mut self, len_token: usize) -> Self {
        let return_struct = self.clone();
        self.n_char += len_token;
        return_struct
    }
}

pub struct TokenStream<I: Iterator<Item = Token>> {
    iter: Peekable<I>,
}

impl<I: Iterator<Item = Token>> TokenStream<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
        }
    }

    pub fn expect_number(&mut self) -> isize {
        let next = self.next().map(|token| *token.kind);
        match next {
            Some(TokenKind::Num(num)) => num,
            _ => panic!("number expected, but got {:?}", next),
        }
    }

    pub fn peek_kind(&mut self) -> Option<Box<TokenKind>> {
        self.iter.peek().map(|token| token.kind.clone())
    }

    pub fn at_eof(&mut self) -> bool {
        match self.peek_kind() {
            Some(token) => matches!(*token, TokenKind::Eof),
            None => panic!("This stream is already used."),
        }
    }
}

impl<I: Iterator<Item = Token>> Iterator for TokenStream<I> {
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
            let pos = crate::tokenize::Position::default();
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
            let pos = crate::tokenize::Position::default();
            temp_vec.push(Token::new($token_kind, pos));
        )*
        temp_vec
            .into_iter()
            .map(|token| token.kind())
            .collect::<Vec<_>>()
    }};
}

#[cfg(test)]
pub fn kind_eq(lhs: &Vec<Token>, rhs: &Vec<Token>) -> bool {
    lhs.into_iter()
        .zip(rhs.into_iter())
        .fold(true, |acc, (l_token, r_token)| {
            acc && l_token.kind_eq(r_token)
        })
}

pub fn tokenize_and_kinds(input: String) -> Vec<Box<TokenKind>> {
    tokenize(input)
        .into_iter()
        .map(|token| token.kind())
        .collect()
}
