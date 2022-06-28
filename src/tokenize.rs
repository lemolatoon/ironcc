use std::iter::Peekable;

pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    pub fn tokenize(&self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut input_chars = self.input.chars().peekable();
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
                '*' => tokens.push(Token::new(
                    TokenKind::BinOp(BinOpToken::Mul),
                    pos.next_char(),
                )),
                '/' => tokens.push(Token::new(
                    TokenKind::BinOp(BinOpToken::Div),
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
                _ => self.error_at(&pos, &format!("Unexpected char while tokenize: {:?}", &pos)),
            }
        }
        tokens.push(Token::new(TokenKind::Eof, pos.next_token(0)));

        tokens
    }

    pub fn error_at(&self, pos: &Position, msg: &str) -> ! {
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
        panic!()
    }
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
    Mul,
    Div,
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
    pub n_char: usize,
    pub n_line: usize,
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

#[derive(Debug, Clone)]
pub struct TokenStream<'a, I: Iterator<Item = Token>>
where
    I: Clone,
{
    iter: Peekable<I>,
    input: &'a str,
}

impl<'a, I: Iterator<Item = Token>> TokenStream<'a, I>
where
    I: Clone,
{
    pub fn new(iter: I, input: &'a str) -> Self {
        Self {
            iter: iter.peekable(),
            input,
        }
    }

    pub fn expect_number(&mut self) -> isize {
        let token = self.next();
        // let next = token.map(|token| *token.kind);
        match token {
            Some(Token { kind, pos }) => match *kind {
                TokenKind::Num(num) => num,
                _ => self.error_at(Some(pos), &format!("number expected, but got {:?}", kind)),
            },
            _ => self.error_at(None, &format!("number expected, but got {:?}", token)),
        }
    }

    pub fn peek(&mut self) -> Option<&I::Item> {
        self.iter.peek()
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

impl<'a, I: Iterator<Item = Token>> Iterator for TokenStream<'a, I>
where
    I: Clone,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.iter.next();
        next
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
    let tokenizer = Tokenizer::new(&input);
    tokenizer
        .tokenize()
        .into_iter()
        .map(|token| token.kind())
        .collect()
}
