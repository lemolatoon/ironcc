use core::panic;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::rc::Rc;

use crate::error::CompileError;
use crate::tokenize::DebugInfo;
use crate::tokenize::Eof;
use crate::tokenize::FileInfo;
use crate::tokenize::Token;

pub struct Preprocessor<'b> {
    main_file_info: Rc<FileInfo>,
    include_dir: &'b str,
    main_chars: SrcCursor,
    define_table: BTreeMap<String, String>,
    include_table: BTreeMap<String, SrcCursor>,
}

impl<'b> Preprocessor<'b> {
    pub fn new(main_file_info: Rc<FileInfo>, include_dir: &'b str) -> Self {
        Self {
            main_chars: SrcCursor::new(main_file_info.clone()),
            main_file_info,
            include_dir,
            define_table: BTreeMap::new(),
            include_table: BTreeMap::new(),
        }
    }

    pub fn preprocess(&mut self) -> Vec<Token<TokenKind>> {
        let mut tokens: Vec<Token<TokenKind>> = Vec::new();
        while !self.main_chars.is_empty() {
            if let Some((debug_info, spaces)) = self
                .main_chars
                .get_debug_info_and_skip_white_space_without_new_line()
            {
                tokens.push(Token::new(spaces, debug_info));
                continue;
            }

            if let Some((debug_info, spaces)) = self.main_chars.get_debug_info_and_skip_new_line() {
                tokens.push(Token::new(spaces, debug_info));
                continue;
            }

            if let Some((debug_info, comment)) = self.main_chars.get_debug_info_and_skip_comment() {
                tokens.push(Token::new(comment, debug_info));
                continue;
            }

            if let Some((debug_info, str_lit)) = self.main_chars.get_debug_info_and_read_str_lit() {
                tokens.push(Token::new(TokenKind::StrLit(str_lit), debug_info));
                continue;
            }

            match self.main_chars.get_debug_info_and_read_punctuator() {
                Some((_, TokenKind::HashTag)) => {
                    self.main_chars
                        .get_debug_info_and_skip_white_space_without_new_line();
                    if let Some((_, ident)) = self.main_chars.get_debug_info_and_read_ident() {
                        if ident == "define" {
                            self.main_chars
                                .get_debug_info_and_skip_white_space_without_new_line();
                            let (_, macro_ident) = self
                                .main_chars
                                .get_debug_info_and_read_ident()
                                .expect("arg(ident) of define must exist.");
                            self.main_chars
                                .get_debug_info_and_skip_white_space_without_new_line();

                            let mut macro_value = self.main_chars.get_debug_info_and_read_ident();

                            if macro_value.is_none() {
                                macro_value = self.main_chars.get_debug_info_and_read_number();
                            }
                            let (_, macro_value) =
                                macro_value.expect("arg(value) of define must exist.");
                            self.define_table.insert(macro_ident, macro_value);
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                Some((debug_info, punctuator)) => {
                    tokens.push(Token::new(punctuator, debug_info));
                    continue;
                }
                None => {}
            }

            if let Some((debug_info, ident)) = self.main_chars.get_debug_info_and_read_ident() {
                if let Some(macro_value) = self.define_table.get(&ident) {
                    tokens.push(Token::new(
                        TokenKind::Ident(macro_value.clone()),
                        debug_info,
                    ));
                    continue;
                }
                tokens.push(Token::new(TokenKind::Ident(ident), debug_info));
                continue;
            }

            if let Some((debug_info, rest)) =
                self.main_chars.get_debug_info_and_skip_until_white_space()
            {
                tokens.push(Token::new(rest, debug_info));
                continue;
            }

            let next = self.main_chars.next();
            panic!(
                "Unexpected char while preprocessing: {:?}",
                self.main_chars.next()
            );
        }

        tokens
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
pub struct SrcCursor {
    src: VecDeque<char>,
    debug_info: DebugInfo,
}

impl SrcCursor {
    pub fn new(file_info: Rc<FileInfo>) -> Self {
        let src = file_info.get_file_src().chars().collect();
        SrcCursor {
            src,
            debug_info: DebugInfo::default_with_file_info(file_info),
        }
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }

    pub fn is_empty(&self) -> bool {
        self.src.is_empty()
    }

    pub fn get_debug_info_and_read_ident(&mut self) -> Option<(DebugInfo, String)> {
        if let Some(ch @ ('a'..='z' | 'A'..='Z' | '_')) = self.src.front() {
            let debug_info = self.get_debug_info();
            let mut ident = String::from(*ch);
            self.advance(1);
            while let Some(ch @ ('a'..='z' | 'A'..='Z' | '_' | '0'..='9')) = self.src.front() {
                ident.push(*ch);
                self.advance(1);
            }
            return Some((debug_info, ident));
        }
        None
    }

    pub fn get_debug_info_and_read_str_lit(&mut self) -> Option<(DebugInfo, String)> {
        if self.starts_with("\"") {
            let debug_info = self.get_debug_info();
            // string literal
            self.advance(1); // -> "
            let mut str_lit = String::new();
            str_lit.push('"');
            loop {
                match self.src.front() {
                    Some('"') => {
                        self.advance(1);
                        str_lit.push('"');
                        break;
                    }
                    Some(c) => {
                        str_lit.push(*c);
                        self.advance(1);
                    }
                    None => return None,
                }
            }
            return Some((debug_info, str_lit));
        }
        None
    }

    pub fn get_debug_info_and_read_number(&mut self) -> Option<(DebugInfo, String)> {
        if let Some(ch @ ('0'..='9' | '-')) = self.src.front() {
            let debug_info = self.get_debug_info();
            let mut ident = String::from(*ch);
            self.advance(1);
            while let Some(ch @ ('0'..='9')) = self.src.front() {
                ident.push(*ch);
                self.advance(1);
            }
            return Some((debug_info, ident));
        }
        None
    }

    pub fn get_debug_info_and_skip_white_space_without_new_line(
        &mut self,
    ) -> Option<(DebugInfo, TokenKind)> {
        let debug_info = self.get_debug_info();
        let mut space = String::new();
        while let Some(c) = self.src.front() {
            match *c {
                ch @ (' ' | '\t' | 'r') => {
                    space.push(ch);
                    self.advance(1);
                    continue;
                }
                _ => {
                    break;
                }
            }
        }
        if space.is_empty() {
            return None;
        }
        Some((debug_info, TokenKind::Space(space)))
    }
    pub fn get_debug_info_and_skip_new_line(&mut self) -> Option<(DebugInfo, TokenKind)> {
        let debug_info = self.get_debug_info();
        let mut space = String::new();
        while self.src.front() == Some(&'\n') {
            space.push('\n');
            self.next();
            self.debug_info.advance_line();
        }
        if space.is_empty() {
            return None;
        }
        Some((debug_info, TokenKind::Space(space)))
    }
    pub fn get_debug_info_and_skip_white_space(&mut self) -> Option<(DebugInfo, TokenKind)> {
        let debug_info = self.get_debug_info();
        let mut space = String::new();
        while let Some(c) = self.src.front() {
            if c.is_whitespace() {
                match self.next().unwrap() {
                    ch @ (' ' | '\t' | 'r') => {
                        space.push(ch);
                        self.debug_info.advance(1);
                        continue;
                    }
                    '\n' => {
                        space.push('\n');
                        self.debug_info.advance_line();
                    }
                    _ => {
                        unreachable!()
                    }
                }
            } else {
                break;
            }
        }
        if space.is_empty() {
            return None;
        }
        Some((debug_info, TokenKind::Space(space)))
    }

    pub fn get_debug_info_and_skip_comment(&mut self) -> Option<(DebugInfo, TokenKind)> {
        if self.starts_with("//") {
            let debug_info = self.get_debug_info();
            let mut comment = String::from("//");
            self.advance(2);
            while let Some(&c) = self.src.front() {
                if c == '\n' {
                    comment.push('\n');
                    self.debug_info.advance_line();
                    self.next();
                    break;
                }
                comment.push(c);
                self.advance(1);
            }
            if comment.is_empty() {
                return None;
            }
            return Some((debug_info, TokenKind::Comment(comment)));
        }
        None
    }

    pub fn get_debug_info_and_read_punctuator(&mut self) -> Option<(DebugInfo, TokenKind)> {
        match self.src.front() {
            Some('#') => {
                let debug_info = self.get_debug_info();
                self.advance(1);
                Some((debug_info, TokenKind::HashTag))
            }
            Some(ch @ ('(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | ':')) => {
                let debug_info = self.get_debug_info();
                let punctuator = TokenKind::Punctuator(ch.to_string());
                self.advance(1);
                Some((debug_info, punctuator))
            }
            _ => None,
        }
    }

    pub fn get_debug_info_and_skip_until_white_space(&mut self) -> Option<(DebugInfo, TokenKind)> {
        let debug_info = self.get_debug_info();
        let mut rest = String::new();
        while let Some(c) = self.src.front() {
            if c.is_whitespace() {
                break;
            }
            rest.push(*c);
            self.advance(1);
        }
        if rest.is_empty() {
            return None;
        }
        Some((debug_info, TokenKind::Rest(rest)))
    }

    pub fn starts_with(&self, prefix: &str) -> bool {
        if self.is_empty() {
            return false;
        }
        self.src
            .iter()
            .take(prefix.len())
            .zip(prefix.chars())
            .all(|(a, b)| *a == b)
    }

    pub fn get_debug_info(&self) -> DebugInfo {
        self.debug_info.clone()
    }

    pub fn advance(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
        self.debug_info.advance(n);
    }
}

impl Iterator for SrcCursor {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.src.pop_front()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum TokenKind {
    Eof,
    Define,
    HashTag,
    StrLit(String),
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
            | TokenKind::Punctuator(content)
            | TokenKind::Space(content)
            | TokenKind::Ident(content)
            | TokenKind::Rest(content) => content.len(),
        }
    }

    pub fn get_content(&self) -> &str {
        match self {
            TokenKind::Eof => "",
            TokenKind::Define => "define",
            TokenKind::HashTag => "#",
            TokenKind::Comment(content)
            | TokenKind::StrLit(content)
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
            *self.current_token_debug_info.get_n_char_mut() = 0;
            *self.current_token_debug_info.get_n_line_mut() =
                self.current_token_debug_info.get_n_line() + 1;
        } else {
            *self.current_token_debug_info.get_n_char_mut() =
                self.current_token_debug_info.get_n_char() + 1;
        }
        self.cur_in_token += 1;
        Some((debug_info, *ch))
    }
}
