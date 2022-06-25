use std::iter::Peekable;

pub fn tokenize(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut input_chars = input.chars().peekable();
    while let Some(c) = input_chars.next() {
        match c {
            // skip white spaces
            ' ' | '\n' | '\t' => continue,
            '+' => tokens.push(Token::new(TokenKind::BinOp(BinOpToken::Plus))),
            '-' => tokens.push(Token::new(TokenKind::BinOp(BinOpToken::Minus))),
            '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut number = String::from(c);
                while let Some(&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =
                    input_chars.peek()
                {
                    number.push(input_chars.next().unwrap())
                }
                let num = number
                    .parse::<usize>()
                    .expect("Currently support only a number literal.");
                tokens.push(Token::new(TokenKind::Num(num as isize)))
            }
            _ => panic!("Unexpected char while tokenize: {}", c),
        }
    }
    tokens.push(Token::new(TokenKind::Eof));

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
}

impl Token {
    pub fn new(token_kind: TokenKind) -> Self {
        Self {
            kind: Box::new(token_kind),
        }
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

trait Expect {
    type Item;

    fn expect(kind: Self::Item) -> bool;
}

#[cfg(test)]
#[macro_export]
macro_rules! tokens {
    ( $( $token_kind:expr ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push(Token::new($token_kind));
        )*
        temp_vec
    }};
}
