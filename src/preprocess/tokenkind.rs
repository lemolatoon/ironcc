use crate::tokenize::tokenize::Eof;

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum TokenKind {
    Eof,
    Define,
    HashTag,
    StrLit(String),
    NumLit(String),
    Punctuator(String),
    Space(String),
    Comment(String),
    Ident(String),
    Rest(String),
}

impl TokenKind {
    pub fn len(&self) -> usize {
        match self {
            TokenKind::Eof => 0,
            TokenKind::Define => 6,
            TokenKind::HashTag => 1,
            TokenKind::Comment(content)
            | TokenKind::StrLit(content)
            | TokenKind::NumLit(content)
            | TokenKind::Punctuator(content)
            | TokenKind::Space(content)
            | TokenKind::Ident(content)
            | TokenKind::Rest(content) => content.len(),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get_content(&self) -> &str {
        match self {
            TokenKind::Eof => "",
            TokenKind::Define => "define",
            TokenKind::HashTag => "#",
            TokenKind::Comment(content)
            | TokenKind::StrLit(content)
            | TokenKind::NumLit(content)
            | TokenKind::Punctuator(content)
            | TokenKind::Space(content)
            | TokenKind::Ident(content)
            | TokenKind::Rest(content) => content,
        }
    }
}

impl Eof for TokenKind {
    fn is_eof(&self) -> bool {
        *self == TokenKind::Eof
    }
}
