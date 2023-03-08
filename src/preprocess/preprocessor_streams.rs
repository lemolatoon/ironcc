use super::tokenkind::TokenKind;
use crate::{
    error::CompileError,
    tokenize::tokenize::{DebugInfo, Token},
};
use std::{collections::VecDeque, fmt::Debug, iter::Peekable};

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
        if self.chars.len() < prefix.len() {
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

    // Consume stream passed times.
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
