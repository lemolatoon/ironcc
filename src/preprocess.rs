use std::collections::VecDeque;
use std::iter::Peekable;
use std::rc::Rc;

use crate::error::CompileError;
use crate::tokenize::DebugInfo;
use crate::tokenize::Eof;
use crate::tokenize::FileInfo;
use crate::tokenize::Token;

pub struct Preprocessor<'b> {
    include_dir: &'b str,
}

impl<'b> Preprocessor<'b> {
    pub const fn new(include_dir: &'b str) -> Self {
        Self { include_dir }
    }

    pub fn preprocess(&self, main_file_info: Rc<FileInfo>) -> Vec<Token<TokenKind>> {
        let src = main_file_info.get_file_src();
        let token = Token::new(
            TokenKind::Rest(src.to_string()),
            DebugInfo::default_with_file_info(Rc::clone(&main_file_info)),
        );
        vec![token]
    }

    // #[allow(clippy::too_many_lines)]
    // pub fn preprocess(&self) -> Vec<Token> {
    //     let mut tokens = Vec::new();
    //     let mut pos = Position::default(); // 0, 0
    //     let mut input = <&str>::clone(&self.input);

    //         'tokenize_loop: while !input.is_empty() {
    //             // skip white spaces
    //             if input.starts_with(' ') || input.starts_with('\t') {
    //                 pos.advance(1);
    //                 input = &input[1..];
    //                 continue;
    //             } else if input.starts_with('\n') {
    //                 pos.advance_line();
    //                 input = &input[1..];
    //                 continue;
    //             } else if input.starts_with("//") {
    //                 let mut input_iter = input.chars();
    //                 let mut num_this_line_char = 1;
    //                 while !matches!(input_iter.next(), Some('\n')) {
    //                     num_this_line_char += 1;
    //                 }
    //                 pos.advance_line();
    //                 input = &input[num_this_line_char..];
    //                 continue;
    //             }
    //             if input.starts_with('#') {
    //                 let mut input_iter = input.chars();
    //                 let mut num_this_line_char = 1;
    //                 while !matches!(input_iter.next(), Some('\n')) {
    //                     num_this_line_char += 1;
    //                 }
    //                 pos.advance_line();
    //                 input = &input[num_this_line_char..];
    //                 continue;
    //             }

    //             let symbols = vec![
    //                 ("...", TokenKind::DotDotDot),
    //                 ("<<", TokenKind::BinOp(BinOpToken::LShift)),
    //                 (">>", TokenKind::BinOp(BinOpToken::RShift)),
    //                 ("<=", TokenKind::BinOp(BinOpToken::Le)),
    //                 (">=", TokenKind::BinOp(BinOpToken::Ge)),
    //                 ("==", TokenKind::BinOp(BinOpToken::EqEq)),
    //                 ("!=", TokenKind::BinOp(BinOpToken::Ne)),
    //                 ("||", TokenKind::BinOp(BinOpToken::VerticalVertical)),
    //                 ("&&", TokenKind::BinOp(BinOpToken::AndAnd)),
    //                 ("++", TokenKind::PlusPlus),
    //                 ("--", TokenKind::MinusMinus),
    //                 ("->", TokenKind::Arrow),
    //                 ("+", TokenKind::BinOp(BinOpToken::Plus)),
    //                 ("-", TokenKind::BinOp(BinOpToken::Minus)),
    //                 ("*", TokenKind::BinOp(BinOpToken::Star)),
    //                 ("/", TokenKind::BinOp(BinOpToken::Slash)),
    //                 ("&", TokenKind::BinOp(BinOpToken::And)),
    //                 ("%", TokenKind::BinOp(BinOpToken::Percent)),
    //                 ("(", TokenKind::OpenDelim(DelimToken::Paren)),
    //                 ("{", TokenKind::OpenDelim(DelimToken::Brace)),
    //                 ("[", TokenKind::OpenDelim(DelimToken::Bracket)),
    //                 (")", TokenKind::CloseDelim(DelimToken::Paren)),
    //                 ("}", TokenKind::CloseDelim(DelimToken::Brace)),
    //                 ("]", TokenKind::CloseDelim(DelimToken::Bracket)),
    //                 (",", TokenKind::Comma),
    //                 ("<", TokenKind::BinOp(BinOpToken::Lt)),
    //                 (">", TokenKind::BinOp(BinOpToken::Gt)),
    //                 ("~", TokenKind::Tilde),
    //                 ("!", TokenKind::Exclamation),
    //                 ("?", TokenKind::Question),
    //                 (":", TokenKind::Colon),
    //                 (";", TokenKind::Semi),
    //                 ("=", TokenKind::Eq),
    //                 (".", TokenKind::Dot),
    //             ];

    //             for (literal, kind) in symbols {
    //                 if input.starts_with(literal) {
    //                     tokens.push(Token::new(kind, pos.get_pos_and_advance(literal.len())));
    //                     input = &input[literal.len()..];
    //                     continue 'tokenize_loop;
    //                 }
    //             }

    //             if input.starts_with('"') {
    //                 // string literal
    //                 let mut chars = input.chars().peekable();
    //                 chars.next(); // -> "
    //                 let mut str_lit = String::new();
    //                 let mut len_token = 1;
    //                 loop {
    //                     match chars.peek() {
    //                         Some('"') => {
    //                             len_token += 1;
    //                             chars.next();
    //                             break;
    //                         }
    //                         Some('\\') => {
    //                             len_token += 1;
    //                             chars.next();
    //                             match chars.next() {
    //                                 Some('n') => {
    //                                     len_token += 1;
    //                                     str_lit.push('\n');
    //                                 }
    //                                 Some('e') => {
    //                                     len_token += 1;
    //                                     str_lit.push(0x1bu8.into());
    //                                 }
    //                                 _ => {
    //                                     pos.advance(len_token);
    //                                     return Err(unimplemented_err!(
    //                                         self.input,
    //                                         pos,
    //                                         "This type of escape Sequences are not currently implemented."
    //                                     ));
    //                                 }
    //                             }
    //                         }
    //                         Some(c) => {
    //                             len_token += 1;
    //                             str_lit.push(*c);
    //                             chars.next();
    //                         }
    //                         None => {
    //                             return Err(CompileError::new_unexpected_eof_tokenize(self.input, pos))
    //                         }
    //                     }
    //                 }
    //                 tokens.push(Token::new(
    //                     TokenKind::Str(str_lit),
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             }

    //             if input.starts_with("0b") {
    //                 input = &input[2..];
    //                 let mut chars = input.chars().peekable();
    //                 let mut number = String::new();
    //                 while let Some(&('0' | '1')) = chars.peek() {
    //                     number.push(chars.next().unwrap());
    //                 }
    //                 let len_token = number.len();
    //                 let mut num = 0;
    //                 for c in number.chars() {
    //                     num *= 2;
    //                     match c {
    //                         '0' => {}
    //                         '1' => {
    //                             num += 1;
    //                         }
    //                         _ => {
    //                             return Err(CompileError::new_expected_failed(
    //                                 self.input,
    //                                 Box::new("'0' | '1'"),
    //                                 Token {
    //                                     kind: Box::new(TokenKind::Ident(c.to_string())),
    //                                     pos,
    //                                 },
    //                             ));
    //                         }
    //                     }
    //                 }
    //                 tokens.push(Token::new(
    //                     TokenKind::Num(num as isize),
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             }

    //             if input.starts_with(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) {
    //                 let mut chars = input.chars().peekable();
    //                 let mut number = String::from(chars.next().unwrap());
    //                 while let Some(&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =
    //                     chars.peek()
    //                 {
    //                     number.push(chars.next().unwrap());
    //                 }
    //                 let len_token = number.len();
    //                 let num = number
    //                     .parse::<usize>()
    //                     .expect("Currently support only a number literal.");
    //                 tokens.push(Token::new(
    //                     TokenKind::Num(num as isize),
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             } else if input.starts_with(
    //                 &('a'..='z')
    //                     .chain('A'..='Z')
    //                     .chain(vec!['_'].into_iter())
    //                     .collect::<Vec<_>>()[..],
    //             ) {
    //                 // Ident or reserved token
    //                 let mut chars = input.chars().peekable();
    //                 let mut ident = String::from(chars.next().unwrap());
    //                 while let Some(
    //                     &('a'..='z')
    //                     | &('A'..='Z')
    //                     | '_'
    //                     | '0'
    //                     | '1'
    //                     | '2'
    //                     | '3'
    //                     | '4'
    //                     | '5'
    //                     | '6'
    //                     | '7'
    //                     | '8'
    //                     | '9',
    //                 ) = chars.peek()
    //                 {
    //                     ident.push(chars.next().unwrap());
    //                 }
    //                 let len_token = ident.len();
    //                 tokens.push(Token::new(
    //                     match ident.as_str() {
    //                         "return" => TokenKind::Return,
    //                         "if" => TokenKind::If,
    //                         "else" => TokenKind::Else,
    //                         "while" => TokenKind::While,
    //                         "for" => TokenKind::For,
    //                         "int" => TokenKind::Type(TypeToken::Int),
    //                         "char" => TokenKind::Type(TypeToken::Char),
    //                         "void" => TokenKind::Type(TypeToken::Void),
    //                         "sizeof" => TokenKind::SizeOf,
    //                         "struct" => TokenKind::Struct,
    //                         _ => TokenKind::Ident(ident),
    //                     },
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             }
    //             return Err(self.new_unexpected_char(pos, input.chars().next().unwrap()));
    //         }
    //         tokens.push(Token::new(TokenKind::Eof, pos.get_pos_and_advance(0)));

    //         Ok(tokens)
    //     }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum TokenKind {
    Eof,
    Define,
    HashTag,
    Ident(String),
    Rest(String),
}

impl TokenKind {
    pub fn len(&self) -> usize {
        match self {
            TokenKind::Eof => 0,
            TokenKind::Define => 6,
            TokenKind::HashTag => 1,
            TokenKind::Ident(content) | TokenKind::Rest(content) => content.len(),
        }
    }

    pub fn get_content(&self) -> &str {
        match self {
            TokenKind::Eof => "",
            TokenKind::Define => "define",
            TokenKind::HashTag => "#",
            TokenKind::Ident(content) | TokenKind::Rest(content) => content,
        }
    }
}

impl Eof for TokenKind {
    fn is_eof(&self) -> bool {
        *self == TokenKind::Eof
    }
}

use std::fmt::Debug;
#[derive(Clone, Debug)]
pub struct PreprocessorTokenStream<I>
where
    I: Iterator<Item = Token<TokenKind>> + Clone + Debug,
{
    iter: Peekable<I>,
    current_token_content: Vec<char>,
    current_token_debug_info: DebugInfo,
    cur_in_token: usize,
}

impl<I> PreprocessorTokenStream<I>
where
    I: Iterator<Item = Token<TokenKind>> + Clone + Debug,
{
    pub fn new(mut iter: I) -> Self {
        let current_token = iter.next();
        match current_token {
            Some(current_token) => {
                let current_token_content = current_token.kind.get_content().to_string();
                let current_token_debug_info = current_token.debug_info; // this clone uses `Rc::clone` internally, so it's light.
                let cur_in_token = 0;
                Self {
                    iter: iter.peekable(),
                    current_token_content: current_token_content.chars().collect(),
                    current_token_debug_info,
                    cur_in_token,
                }
            }
            None => Self {
                iter: iter.peekable(),
                current_token_content: Vec::new(),
                current_token_debug_info: DebugInfo::default(),
                cur_in_token: 0,
            },
        }
    }

    pub fn starts_with(&self, prefix: &str) -> bool {
        let cloned_self = self.clone();
        let len = prefix.len();
        if cloned_self.is_empty() {
            return false;
        }
        cloned_self
            .take(len)
            .map(|(_, ch)| ch)
            .zip(prefix.chars())
            .all(|(a, b)| a == b)
    }

    // Returns true if the next char matches '0'..='9'
    pub fn starts_with_number(&self) -> bool {
        matches!(self.peek(), Some((_, '0'..='9')))
    }

    // Returns true if the next char matches 'a'..='z' | 'A'..='Z'
    pub fn starts_with_alphabet(&self) -> bool {
        matches!(self.peek(), Some((_, 'a'..='z' | 'A'..='Z')))
    }

    pub fn advance(&mut self, times: usize) {
        for _ in 0..times {
            self.next();
        }
    }

    pub fn get_debug_info_and_advance(&mut self, times: usize) -> DebugInfo {
        let debug_info = self.current_token_debug_info.clone();
        self.advance(times);
        debug_info
    }

    /// Advance this iterator (including sentinel) until sentinel
    pub fn advance_until(&mut self, sentinel: char) -> Result<(), CompileError<TokenKind>> {
        loop {
            match self.next() {
                Some((_, ch)) if ch == sentinel => return Ok(()),
                Some(_) => continue,
                None => {
                    return Err(CompileError::new_unexpected_eof_tokenize(
                        self.current_token_debug_info.clone(),
                    ))
                }
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.clone().next().is_none()
    }

    pub fn peek(&self) -> Option<(DebugInfo, char)> {
        let mut cloned = self.clone();
        cloned.next()
    }

    pub fn get_current_debug_info(&self) -> DebugInfo {
        self.current_token_debug_info.clone()
    }
}

#[derive(Clone, Debug)]
pub struct PreprocessorTokenContainerStream {
    chars: VecDeque<(DebugInfo, char)>,
    prev_debug_info: DebugInfo,
}

impl PreprocessorTokenContainerStream {
    pub fn new(chars: VecDeque<(DebugInfo, char)>) -> Self {
        Self {
            chars,
            prev_debug_info: DebugInfo::default(),
        }
    }
}

impl PreprocessorTokenContainerStream {
    pub fn peek(&self) -> Option<&(DebugInfo, char)> {
        self.chars.get(0)
    }

    pub fn starts_with(&self, prefix: &str) -> bool {
        if self.chars.is_empty() {
            return false;
        }
        self.chars
            .iter()
            .map(|(_, ch)| ch)
            .take(prefix.len())
            .zip(prefix.chars())
            .all(|(a, b)| *a == b)
    }

    pub fn is_empty(&self) -> bool {
        self.chars.is_empty()
    }

    // Returns true if the next char matches '0'..='9'
    pub fn starts_with_number(&self) -> bool {
        matches!(self.peek(), Some((_, '0'..='9')))
    }

    // Returns true if the next char matches 'a'..='z' | 'A'..='Z'
    pub fn starts_with_alphabet(&self) -> bool {
        matches!(self.peek(), Some((_, 'a'..='z' | 'A'..='Z')))
    }

    pub fn advance(&mut self, times: usize) {
        for _ in 0..times {
            self.next();
        }
    }

    pub fn get_debug_info_and_advance(&mut self, times: usize) -> Option<DebugInfo> {
        if times == 0 {
            return self.peek().map(|(debug_info, _)| debug_info.clone());
        }
        let (debug_info, _) = self.next()?;
        self.advance(times - 1);
        Some(debug_info)
    }

    pub fn get_prev_debug_info(&self) -> DebugInfo {
        self.prev_debug_info.clone()
    }

    /// Advance this iterator (including sentinel) until sentinel
    pub fn advance_until(&mut self, sentinel: char) -> Result<(), CompileError<TokenKind>> {
        loop {
            match self.next() {
                Some((_, ch)) if ch == sentinel => return Ok(()),
                Some(_) => continue,
                None => {
                    return Err(CompileError::new_unexpected_eof_tokenize(
                        self.prev_debug_info.clone(),
                    ))
                }
            }
        }
    }
}

impl Iterator for PreprocessorTokenContainerStream {
    type Item = (DebugInfo, char);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.chars.pop_front() {
            self.prev_debug_info = item.0.clone();
            Some(item)
        } else {
            None
        }
    }
}

impl<I> Iterator for PreprocessorTokenStream<I>
where
    I: Iterator<Item = Token<TokenKind>> + Clone + Debug,
{
    type Item = (DebugInfo, char);

    fn next(&mut self) -> Option<Self::Item> {
        while self.current_token_content.len() == self.cur_in_token {
            let current_token = self.iter.next()?;
            self.current_token_content = current_token.kind.get_content().chars().collect();
            self.current_token_debug_info = current_token.debug_info.clone(); // this clone uses `Rc::clone` internally, so it's light.
            self.cur_in_token = 0;
        }
        let ch = self.current_token_content.get(self.cur_in_token).unwrap();
        let debug_info = self.current_token_debug_info.clone();
        if *ch == '\n' {
            // DebugInfo::new(
            //     self.current_token_debug_info.get_cloned_file_info(),
            //     0,
            //     self.current_token_debug_info.get_n_line() + 1,
            // );
            *self.current_token_debug_info.get_n_char_mut() = 0;
            *self.current_token_debug_info.get_n_line_mut() =
                self.current_token_debug_info.get_n_line() + 1;
        } else {
            // DebugInfo::new(
            //     self.current_token_debug_info.get_cloned_file_info(),
            //     self.current_token_debug_info.get_n_char() + 1,
            //     self.current_token_debug_info.get_n_line(),
            // );
            *self.current_token_debug_info.get_n_char_mut() =
                self.current_token_debug_info.get_n_char() + 1;
        }
        self.cur_in_token += 1;
        Some((debug_info, *ch))
    }
}
