use crate::{
    error::CompileError,
    tokenize::{
        debug_infos::{DebugInfo, FileInfo},
        tokenize::Token,
    },
    unimplemented_err,
};
use std::{
    collections::{BTreeMap, VecDeque},
    rc::Rc,
};

use super::tokenkind::TokenKind;

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

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct SrcCursor {
    src: VecDeque<char>,
    debug_info: DebugInfo,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum PPTokenKind {
    HeaderName(String),
    Ident(String),
    Number(String),
    Char(String),
    StrLit(String),
    Punctuator(String),
    Other(String),
}

impl From<PPTokenKind> for String {
    fn from(value: PPTokenKind) -> Self {
        match value {
            PPTokenKind::HeaderName(str)
            | PPTokenKind::Ident(str)
            | PPTokenKind::Number(str)
            | PPTokenKind::Char(str)
            | PPTokenKind::StrLit(str)
            | PPTokenKind::Punctuator(str)
            | PPTokenKind::Other(str) => str,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct PPToken {
    pub kind: PPTokenKind,
    pub debug_info: DebugInfo,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct PPTokens {
    tokens: Vec<PPToken>,
}

impl PPTokens {
    pub fn replace_with(
        self,
        arg_map: &BTreeMap<String, PPTokens>,
    ) -> Result<PPTokens, CompileError> {
        let replaced_tokens: Vec<PPToken> = self
            .tokens
            .into_iter()
            .map(|pp_token| match pp_token {
                PPToken {
                    kind: PPTokenKind::Ident(ident),
                    debug_info,
                } => {
                    if let Some(value) = arg_map.get(&ident) {
                        value.clone().tokens.into_iter()
                    } else {
                        vec![PPToken {
                            kind: PPTokenKind::Ident(ident),
                            debug_info,
                        }]
                        .into_iter()
                    }
                }
                token => vec![token].into_iter(),
            })
            .flatten()
            .collect();
        Ok(Self {
            tokens: replaced_tokens,
        })
    }
}

impl From<PPTokens> for Vec<Token<TokenKind>> {
    fn from(tokens: PPTokens) -> Self {
        tokens
            .tokens
            .into_iter()
            .map(|PPToken { kind, debug_info }| {
                Token::new(TokenKind::Rest(kind.into()), debug_info)
            })
            .collect()
    }
}

impl SrcCursor {
    pub fn new(file_info: Rc<FileInfo>) -> Self {
        let src = file_info.get_file_src().chars().collect();
        SrcCursor {
            src,
            debug_info: DebugInfo::default_with_file_info(file_info),
        }
    }

    pub fn new_raw(chars: VecDeque<char>, initial_debug_info: DebugInfo) -> Self {
        Self {
            src: chars,
            debug_info: initial_debug_info,
        }
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }

    pub fn is_empty(&self) -> bool {
        self.src.is_empty()
    }

    /// Errors
    /// if this stream does not start with the given prefix.
    pub fn expect(&mut self, expected: &str) -> Result<(), CompileError> {
        let debug_info = self.get_debug_info();
        if self.starts_with(expected) {
            self.advance_with_new_line(expected.len());
            Ok(())
        } else {
            Err(unimplemented_err!(
                debug_info,
                format!(
                    "Expected: {:?}, but got: {:?}",
                    expected,
                    self.clone().take(expected.len()).collect::<String>()
                )
            ))
        }
    }

    // Expect ' ' | '\t'
    pub fn expect_spaces(&mut self) -> Result<(), CompileError> {
        let debug_info = self.get_debug_info();
        if self.starts_with(" ") {
            self.advance(1);
            Ok(())
        } else if self.starts_with("\t") {
            self.advance(1);
            Ok(())
        } else {
            Err(unimplemented_err!(
                debug_info,
                format!(
                    "Expected Space: `' ' | '\\t'`, but got: {:?}",
                    self.clone().take(1).collect::<String>()
                )
            ))
        }
    }

    pub fn consume(&mut self, maybe_expected: impl AsRef<str>) -> bool {
        if self.starts_with(maybe_expected.as_ref()) {
            self.advance_with_new_line(maybe_expected.as_ref().len());
            true
        } else {
            false
        }
    }

    pub fn parse_identifier_list(&mut self) -> Result<Vec<(DebugInfo, String)>, CompileError> {
        let mut identifiers = Vec::new();
        self.get_debug_info_and_skip_white_space();
        while let Some(ident) = self.get_debug_info_and_read_ident() {
            identifiers.push(ident);
            self.get_debug_info_and_skip_white_space();
            if self.consume(",") {
                self.get_debug_info_and_skip_white_space();
            } else {
                break;
            }
        }
        Ok(identifiers)
    }

    pub fn parse_comma_splitted_args(&mut self) -> Result<Vec<PPTokens>, CompileError> {
        let mut args = Vec::new();
        self.expect("(")?;
        loop {
            let (tokens, end_of_args) = self.parse_one_arg()?;
            args.push(tokens);
            if end_of_args {
                break;
            }
        }
        Ok(args)
    }

    pub fn parse_one_arg(&mut self) -> Result<(PPTokens, bool), CompileError> {
        let mut tokens = Vec::new();
        let mut end_of_args = false;
        loop {
            let debug_info = self.get_debug_info();
            if let Some((debug_info, ident)) = self.get_debug_info_and_read_ident() {
                tokens.push(PPToken {
                    kind: PPTokenKind::Ident(ident),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, number)) = self.get_debug_info_and_read_number() {
                tokens.push(PPToken {
                    kind: PPTokenKind::Number(number),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, str_lit)) = self.get_debug_info_and_read_str_lit() {
                tokens.push(PPToken {
                    kind: PPTokenKind::StrLit(str_lit),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, punct)) = self.get_debug_info_and_read_punctuator() {
                match punct {
                    TokenKind::Punctuator(punct) if &punct == "(" => {
                        tokens.push(PPToken {
                            kind: PPTokenKind::Punctuator(punct),
                            debug_info,
                        });
                        let inner_tokens = self.tokenize_group()?;
                        tokens.extend(inner_tokens.tokens);
                        continue;
                    }
                    TokenKind::Punctuator(punct) if &punct == ")" => {
                        end_of_args = true;
                        break;
                    }
                    TokenKind::Punctuator(punct) if &punct == "," => {
                        break;
                    }
                    TokenKind::Punctuator(_) => {}
                    token => {
                        return Err(unimplemented_err!(
                            debug_info,
                            format!(
                                "INTERNAL PREPROCESSOR ERROR: expected punctuator but got {}",
                                token.get_content()
                            )
                        ))
                    }
                }
                tokens.push(PPToken {
                    kind: PPTokenKind::Punctuator(punct.get_content().to_string()),
                    debug_info,
                });
                continue;
            }
            match self.src.front() {
                Some('\n') => {
                    self.advance_with_new_line(1);
                    break;
                }
                Some('\\') => {
                    self.advance(1);
                    if Some(&'\n') == self.src.front() {
                        self.advance_with_new_line(1);
                        continue;
                    } else {
                        break;
                    }
                }
                Some(&char) => {
                    self.advance(1);
                    tokens.push(PPToken {
                        kind: PPTokenKind::Other(char.to_string()),
                        debug_info,
                    });
                    continue;
                }
                None => break,
            }
        }
        Ok((PPTokens { tokens }, end_of_args))
    }

    /// Read until end of `)`
    pub fn tokenize_group(&mut self) -> Result<PPTokens, CompileError> {
        let mut tokens = Vec::new();
        loop {
            let debug_info = self.get_debug_info();
            if let Some((debug_info, ident)) = self.get_debug_info_and_read_ident() {
                tokens.push(PPToken {
                    kind: PPTokenKind::Ident(ident),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, number)) = self.get_debug_info_and_read_number() {
                tokens.push(PPToken {
                    kind: PPTokenKind::Number(number),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, str_lit)) = self.get_debug_info_and_read_str_lit() {
                tokens.push(PPToken {
                    kind: PPTokenKind::StrLit(str_lit),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, punct)) = self.get_debug_info_and_read_punctuator() {
                match punct.clone() {
                    TokenKind::Punctuator(punct) if &punct == "(" => {
                        tokens.push(PPToken {
                            kind: PPTokenKind::Punctuator(punct),
                            debug_info,
                        });
                        let inner_tokens = self.tokenize_group()?;
                        tokens.extend(inner_tokens.tokens);
                        continue;
                    }
                    TokenKind::Punctuator(punct) if &punct == ")" => {
                        tokens.push(PPToken {
                            kind: PPTokenKind::Punctuator(punct),
                            debug_info,
                        });
                        break;
                    }
                    TokenKind::Punctuator(_) => {}
                    token => {
                        return Err(unimplemented_err!(
                            debug_info,
                            format!(
                                "INTERNAL PREPROCESSOR ERROR: expected punctuator but got {}",
                                token.get_content()
                            )
                        ))
                    }
                }
                tokens.push(PPToken {
                    kind: PPTokenKind::Punctuator(punct.get_content().to_string()),
                    debug_info,
                });
                continue;
            }
            match self.src.front() {
                Some('\n') => {
                    self.advance_with_new_line(1);
                    break;
                }
                Some('\\') => {
                    self.advance(1);
                    if Some(&'\n') == self.src.front() {
                        self.advance_with_new_line(1);
                        continue;
                    } else {
                        break;
                    }
                }
                Some(&char) => {
                    self.advance(1);
                    tokens.push(PPToken {
                        kind: PPTokenKind::Other(char.to_string()),
                        debug_info,
                    });
                    continue;
                }
                None => break,
            }
        }
        Ok(PPTokens { tokens })
    }

    pub fn tokenize_until_new_line(&mut self) -> Result<PPTokens, CompileError> {
        // preprocessing-token:
        //   header-name
        //   identifier
        //   pp-number
        //   character-constant
        //   string-literal
        //   punctuator
        //   each non-white-space character that cannot be one of the above
        let mut tokens = Vec::new();
        loop {
            let debug_info = self.get_debug_info();
            if let Some((debug_info, ident)) = self.get_debug_info_and_read_ident() {
                tokens.push(PPToken {
                    kind: PPTokenKind::Ident(ident),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, number)) = self.get_debug_info_and_read_number() {
                tokens.push(PPToken {
                    kind: PPTokenKind::Number(number),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, str_lit)) = self.get_debug_info_and_read_str_lit() {
                tokens.push(PPToken {
                    kind: PPTokenKind::StrLit(str_lit),
                    debug_info,
                });
                continue;
            }

            if let Some((debug_info, punct)) = self.get_debug_info_and_read_punctuator() {
                tokens.push(PPToken {
                    kind: PPTokenKind::Punctuator(punct.get_content().to_string()),
                    debug_info,
                });
                continue;
            }
            match self.src.front() {
                Some('\n') => {
                    self.advance_with_new_line(1);
                    break;
                }
                Some('\\') => {
                    self.advance(1);
                    if Some(&'\n') == self.src.front() {
                        self.advance_with_new_line(1);
                        continue;
                    } else {
                        break;
                    }
                }
                Some(&char) => {
                    self.advance(1);
                    tokens.push(PPToken {
                        kind: PPTokenKind::Other(char.to_string()),
                        debug_info,
                    });
                    continue;
                }
                None => break,
            }
        }
        Ok(PPTokens { tokens })
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

    pub fn get_debug_info_and_read_number_literal(
        &mut self,
    ) -> Result<Option<(DebugInfo, isize)>, CompileError> {
        let mut number = String::new();
        let debug_info = self.get_debug_info();
        let mut has_minus = false;
        if let Some('-') = self.src.front() {
            number.push('-');
            has_minus = true;
        }
        if let Some(ch @ ('1'..='9')) = self.src.get(1) {
            number.push(*ch);
            self.advance(if has_minus { 2 } else { 1 });
            while let Some(ch @ ('0'..='9')) = self.src.front() {
                number.push(*ch);
                self.advance(1);
            }
            let number = number.parse().map_err(|e| {
                unimplemented_err!(debug_info.clone(), format!("Number parse failed: {:?}", e))
            })?;
            return Ok(Some((debug_info, number)));
        }
        Ok(None)
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
            if *ch == '0' {
                self.advance(1);
                return Some((debug_info, "0".to_string()));
            }
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

    pub fn get_debug_info_and_skip_white_space_without_new_line_in_preprocessor_token_context(
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
                '\\' => {
                    space.push('\\');
                    self.advance(1);
                    if Some(&'\n') == self.src.front() {
                        space.push('\n');
                        self.advance_with_new_line(1);
                        continue;
                    } else {
                        break;
                    }
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
