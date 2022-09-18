use core::panic;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::path::PathBuf;
use std::rc::Rc;

use crate::common::read_file;
use crate::error::CompileError;
use crate::tokenize::DebugInfo;
use crate::tokenize::Eof;
use crate::tokenize::FileInfo;
use crate::tokenize::Token;

pub struct Preprocessor<'b> {
    main_file_info: Rc<FileInfo>,
    include_dir: &'b str,
    define_table: BTreeMap<String, String>,
}
#[derive(Debug, Clone, Default)]
pub struct DerectiveCount {
    pub ifdef: usize,
}

impl DerectiveCount {
    pub fn set_ifdef_flag(&mut self) {
        self.ifdef += 1;
    }

    pub fn unset_ifdef_flag(&mut self) {
        self.ifdef -= 1;
    }
}

pub enum SrcCursorGenerator {
    FromFile(Rc<FileInfo>),
    FromSrcCursor(SrcCursor),
}

impl From<Rc<FileInfo>> for SrcCursorGenerator {
    fn from(file_info: Rc<FileInfo>) -> Self {
        Self::FromFile(file_info)
    }
}

impl From<SrcCursor> for SrcCursorGenerator {
    fn from(src_cursor: SrcCursor) -> Self {
        Self::FromSrcCursor(src_cursor)
    }
}

impl SrcCursorGenerator {
    pub fn into_cursor(self) -> SrcCursor {
        match self {
            SrcCursorGenerator::FromFile(file_info) => SrcCursor::new(file_info),
            SrcCursorGenerator::FromSrcCursor(cursor) => cursor,
        }
    }
}

impl<'b> Preprocessor<'b> {
    pub fn new(main_file_info: Rc<FileInfo>, include_dir: &'b str) -> Self {
        Self {
            main_file_info,
            include_dir,
            define_table: BTreeMap::new(),
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn preprocess(
        &mut self,
        src_cursor_generator: SrcCursorGenerator,
        tokens: Option<Vec<Token<TokenKind>>>,
        derective_count: &mut Option<DerectiveCount>,
    ) -> Result<Vec<Token<TokenKind>>, CompileError> {
        let mut main_chars = src_cursor_generator.into_cursor();
        let mut tokens: Vec<Token<TokenKind>> = tokens.map_or_else(Vec::new, |tokens| tokens);
        'preprocess_loop: while !main_chars.is_empty() {
            // dbg!(&main_chars.clone().collect::<String>());
            if let Some((debug_info, spaces)) =
                main_chars.get_debug_info_and_skip_white_space_without_new_line()
            {
                tokens.push(Token::new(spaces, debug_info));
                continue;
            }

            if let Some((debug_info, spaces)) = main_chars.get_debug_info_and_skip_new_line() {
                tokens.push(Token::new(spaces, debug_info));
                continue;
            }

            if let Some((debug_info, comment)) = main_chars.get_debug_info_and_skip_comment() {
                tokens.push(Token::new(comment, debug_info));
                continue;
            }

            if let Some((debug_info, str_lit)) = main_chars.get_debug_info_and_read_str_lit() {
                tokens.push(Token::new(TokenKind::StrLit(str_lit), debug_info));
                continue;
            }

            match main_chars.get_debug_info_and_read_punctuator() {
                Some((_, TokenKind::HashTag)) => {
                    main_chars.get_debug_info_and_skip_white_space_without_new_line();
                    if let Some((_, ident)) = main_chars.get_debug_info_and_read_ident() {
                        match ident.as_str() {
                            "define" => {
                                main_chars.get_debug_info_and_skip_white_space_without_new_line();
                                let (_, macro_ident) = main_chars
                                    .get_debug_info_and_read_ident()
                                    .expect("arg(ident) of define must exist.");
                                main_chars.get_debug_info_and_skip_white_space_without_new_line();

                                let mut macro_value = main_chars.get_debug_info_and_read_ident();

                                if macro_value.is_none() {
                                    macro_value = main_chars.get_debug_info_and_read_number();
                                }

                                if macro_value.is_none() {
                                    macro_value = main_chars.get_debug_info_and_read_str_lit();
                                }
                                // just one operand define macro should be defined as 1 .
                                let macro_value = macro_value
                                    .map_or("1".to_string(), |(_, macro_value)| macro_value);
                                self.define_table.insert(macro_ident, macro_value);
                                continue;
                            }
                            ifdefkind @ ("ifdef" | "ifndef") => {
                                main_chars.get_debug_info_and_skip_white_space_without_new_line();
                                let (_, macro_arg) = main_chars
                                    .get_debug_info_and_read_ident()
                                    .expect("arg of `ifdef` must exist.");
                                let mut ifdef_flag = self.define_table.contains_key(&macro_arg);
                                if ifdefkind == "ifndef" {
                                    ifdef_flag = !ifdef_flag;
                                }
                                if ifdef_flag {
                                    if let Some(derective_count) = derective_count {
                                        derective_count.set_ifdef_flag();
                                    } else {
                                        let mut new_derective_count = DerectiveCount::default();
                                        new_derective_count.set_ifdef_flag();
                                        *derective_count = Some(new_derective_count)
                                    }
                                    return self.preprocess(
                                        main_chars.into(),
                                        Some(tokens),
                                        derective_count,
                                    );
                                }
                                loop {
                                    match main_chars.skip_until_macro_keyword().as_str() {
                                        "endif" => break,
                                        "else" => {
                                            if let Some(derective_count) = derective_count {
                                                derective_count.set_ifdef_flag();
                                            } else {
                                                let mut new_derective_count =
                                                    DerectiveCount::default();
                                                new_derective_count.set_ifdef_flag();
                                                *derective_count = Some(new_derective_count);
                                            }
                                            return self.preprocess(
                                                main_chars.into(),
                                                Some(tokens),
                                                derective_count,
                                            );
                                        }
                                        _ => continue,
                                    }
                                }
                                continue 'preprocess_loop;
                            }
                            "else" => {
                                if let Some(derective_count) = derective_count {
                                    derective_count.unset_ifdef_flag();
                                } else {
                                    panic!("`ifdef` should be used before `else`");
                                }
                                while "endif" != main_chars.skip_until_macro_keyword().as_str() {}
                                continue 'preprocess_loop;
                            }
                            "endif" => {
                                if let Some(derective_count) = derective_count {
                                    derective_count.unset_ifdef_flag();
                                }
                                continue 'preprocess_loop;
                            }
                            "undef" => {
                                main_chars.get_debug_info_and_skip_white_space_without_new_line();
                                let (_, macro_ident) = main_chars
                                    .get_debug_info_and_read_ident()
                                    .expect("arg(ident) of define must exist.");
                                self.define_table.remove(&macro_ident);
                                continue 'preprocess_loop;
                            }
                            "include" => {
                                // e.g) # include "mylib.h";
                                main_chars.get_debug_info_and_skip_white_space_without_new_line();
                                if let Some((_, mut str_lit)) =
                                    main_chars.get_debug_info_and_read_str_lit()
                                {
                                    str_lit.pop(); // -> "
                                    str_lit.remove(0); // -> "
                                    tokens = self.include_from_file_dir(
                                        &str_lit,
                                        tokens,
                                        derective_count,
                                    )?;
                                    continue 'preprocess_loop;
                                }
                                let (_, mut file_name) = main_chars
                                    .get_debug_info_and_read_include_file()
                                    .expect("arg(ident) of include with `<` `>` must exist.");
                                file_name.pop(); // -> '>'
                                file_name.remove(0); // -> '<'
                                tokens = self.include_from_include_dir(
                                    &file_name,
                                    tokens,
                                    derective_count,
                                )?;
                                continue 'preprocess_loop;
                            }
                            unknown => panic!("unknown derective: {}", unknown),
                        }
                    } else {
                        continue;
                    }
                }
                Some((debug_info, punctuator)) => {
                    tokens.push(Token::new(punctuator, debug_info));
                    continue 'preprocess_loop;
                }
                None => {}
            }

            if let Some((debug_info, ident)) = main_chars.get_debug_info_and_read_ident() {
                if let Some(macro_value) = self.define_table.get(&ident) {
                    tokens.push(Token::new(
                        TokenKind::Ident(macro_value.clone()),
                        debug_info,
                    ));
                    continue;
                }
                tokens.push(Token::new(TokenKind::Ident(ident), debug_info));
                continue 'preprocess_loop;
            }

            if let Some((debug_info, rest)) = main_chars.get_debug_info_and_skip_until_white_space()
            {
                tokens.push(Token::new(rest, debug_info));
                continue;
            }

            panic!(
                "Unexpected char while preprocessing: {:?}",
                main_chars.next()
            );
        }

        Ok(tokens)
    }

    pub fn include_from_file_dir(
        &mut self,
        file_path: &str,
        mut tokens: Vec<Token<TokenKind>>,
        derective_count: &mut Option<DerectiveCount>,
    ) -> Result<Vec<Token<TokenKind>>, CompileError> {
        let main_file = PathBuf::from(self.main_file_info.get_file_name());
        assert!(main_file.is_file());
        let main_file_dir = main_file.parent().expect("parent dir should exist.");
        let included_file_path = main_file_dir.join(file_path);
        let included_file_path = included_file_path.as_path();
        let src = read_file(included_file_path)?;
        let file_info = Rc::new(FileInfo::new(
            included_file_path.to_str().unwrap().to_string(),
            src,
        ));
        tokens = self.preprocess(file_info.clone().into(), Some(tokens), derective_count)?;
        Ok(tokens)
    }

    pub fn include_from_include_dir(
        &mut self,
        file_path: &str,
        mut tokens: Vec<Token<TokenKind>>,
        derective_count: &mut Option<DerectiveCount>,
    ) -> Result<Vec<Token<TokenKind>>, CompileError> {
        let include_dir = PathBuf::from(self.include_dir);
        assert!(include_dir.is_dir(), "include_dir: {:?}", include_dir);
        let included_file_path = include_dir.join(file_path);
        let included_file_path = included_file_path.as_path();
        assert!(included_file_path.exists());
        let src = read_file(included_file_path)?;
        let file_info = Rc::new(FileInfo::new(
            included_file_path.to_str().unwrap().to_string(),
            src,
        ));
        tokens = self.preprocess(file_info.into(), Some(tokens), derective_count)?;
        Ok(tokens)
    }
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

    /// Panics
    /// if this stream does not start with the given prefix.
    pub fn expect(&mut self, expected: &str) {
        if self.starts_with(expected) {
            self.advance_with_new_line(expected.len());
        } else {
            panic!(
                "Expected: {:?}, but got: {:?}",
                expected,
                self.clone().take(expected.len()).collect::<String>()
            );
        }
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

    pub fn skip_until(&mut self, keyword: &str) {
        while !self.starts_with(keyword) {
            self.advance_with_new_line(1);
        }
        self.advance_with_new_line(keyword.len()); // -> keyword
    }

    /// skip until counter this kind of line: `\n# ${keyword}`. this func reads `keyword` itself and return it.
    pub fn skip_until_macro_keyword(&mut self) -> String {
        loop {
            self.skip_until("\n");
            self.get_debug_info_and_skip_white_space_without_new_line();
            if self.starts_with("#") {
                self.advance(1);
                self.get_debug_info_and_skip_white_space_without_new_line();
                if let Some((_, ident)) = self.get_debug_info_and_read_ident() {
                    return ident;
                }
                continue;
            }
            // In the future, this will be replaced with throwing Result.
            #[allow(clippy::manual_assert)]
            if self.is_empty() {
                panic!("While skipping until specific macro keyword, reach EOF.",);
            }
        }
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

    pub fn get_debug_info_and_read_include_file(&mut self) -> Option<(DebugInfo, String)> {
        if self.starts_with("<") {
            let debug_info = self.get_debug_info();
            // string literal
            self.advance(1); // -> "
            let mut str_lit = String::new();
            str_lit.push('<');
            loop {
                match self.src.front() {
                    Some('>') => {
                        self.advance(1);
                        str_lit.push('>');
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
            Some(
                ch @ ('(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | ':' | '+' | '-' | '*' | '/'
                | '<' | '>'),
            ) => {
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

    /// advance `self.src` and `self.debug_info`, including the consideration whether there are `\n` in the advanced src or not.
    pub fn advance_with_new_line(&mut self, n: usize) {
        for _ in 0..n {
            match self.next() {
                Some('\n') => {
                    self.debug_info.advance_line();
                }
                _ => {
                    self.debug_info.advance(1);
                }
            }
        }
    }

    /// advance `self.src` and `self.debug_info`, under the assumption that there are no `\n` in the advanced src.
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
    pub fn advance_until(&mut self, sentinel: char) -> Result<(), CompileError> {
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
    pub fn advance_until(&mut self, sentinel: char) -> Result<(), CompileError> {
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
