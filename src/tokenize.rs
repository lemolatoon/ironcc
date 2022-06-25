pub fn tokenize(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut input_chars = input.chars().peekable();
    while let Some(c) = input_chars.next() {
        match c {
            // skip white spaces
            ' ' | '\n' | '\t' => continue,
            '+' => tokens.push(Token::new(TokenKind::BinOp(BinOpToken::Plus))),
            '-' => tokens.push(Token::new(TokenKind::BinOp(BinOpToken::Minus))),
            '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
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

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    BinOp(BinOpToken),
    Num(isize),
    Eof,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinOpToken {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    kind: Box<TokenKind>,
}

impl Token {
    pub fn new(token_kind: TokenKind) -> Self {
        Self {
            kind: Box::new(token_kind),
        }
    }
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
