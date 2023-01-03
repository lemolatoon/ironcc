use core::panic;
use std::collections::BTreeMap;
use std::collections::VecDeque;
use std::fmt::format;
use std::iter::Peekable;
use std::path::PathBuf;
use std::rc::Rc;

use crate::common::read_file;
use crate::error::CompileError;
use crate::tokenize::DebugInfo;
use crate::tokenize::Eof;
use crate::tokenize::FileInfo;
use crate::tokenize::Token;
use crate::unimplemented_err;

pub struct Preprocessor<'b> {
    main_file_info: Rc<FileInfo>,
    include_dir: &'b str,
    define_table: BTreeMap<String, String>,
    ifdef_depth: usize,
    watching_label: usize,
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
            ifdef_depth: 0,
            watching_label: 0,
        }
    }

    pub fn increment_ifdef_depth(&mut self) {
        self.ifdef_depth += 1;
    }

    pub fn dec_ifdef_lable(&mut self) -> Result<(), CompileError> {
        let next_value = self.ifdef_depth.checked_sub(1);
        if let Some(next_vale) = next_value {
            self.ifdef_depth = next_vale;
            Ok(())
        } else {
            Err(unimplemented_err!("#endif or #else without #ifdef/#ifndef"))
        }
    }

    pub fn increment_watching_depth(&mut self) {
        self.watching_label += 1;
    }

    pub fn decrement_watching_depth(&mut self) {
        self.watching_label -= 1;
    }

    #[allow(clippy::too_many_lines)]
    pub fn preprocess(
        &mut self,
        main_chars: &mut SrcCursor,
        tokens: Option<Vec<Token<TokenKind>>>,
        // derective_count: &mut Option<DerectiveCount>,
    ) -> Result<Vec<Token<TokenKind>>, CompileError> {
        // let mut main_chars = main_chars.into_cursor();
        let mut tokens: Vec<Token<TokenKind>> = tokens.map_or_else(Vec::new, |tokens| tokens);
        'preprocess_loop: while !main_chars.is_empty() {
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
                                // set watching ifdef's depth and increment depth.
                                self.increment_ifdef_depth();
                                self.watching_label = self.ifdef_depth;
                                if ifdef_flag {
                                    tokens = self.preprocess(main_chars.into(), Some(tokens))?;
                                    continue 'preprocess_loop;
                                }
                                // skip until #endif
                                loop {
                                    match main_chars.skip_until_macro_keyword().as_str() {
                                        "endif" => {
                                            self.dec_ifdef_lable()?;
                                            if self.ifdef_depth + 1 == self.watching_label {
                                                // reached same depth
                                                self.decrement_watching_depth();
                                                if self.watching_label != 0 {
                                                    // ifdef or ifndef is preprocessed by recursive function call.
                                                    // so, if depth is not 0, it means that it is not top level.
                                                    // we should return this
                                                    return Ok(tokens);
                                                }
                                                continue 'preprocess_loop;
                                            }
                                            // reached different depth
                                            // has to be higher depth ifdef/ifndef
                                            assert!(
                                                self.watching_label <= self.ifdef_depth,
                                                "watching_label: {}, ifdef_depth: {}",
                                                self.watching_label,
                                                self.ifdef_depth
                                            );
                                            continue;
                                        }
                                        "else" => {
                                            if self.ifdef_depth == self.watching_label {
                                                // reached same depth
                                                // We have just skipped ifdef/ifndef part.
                                                // so, then we should preprocess else part by recursive function call.
                                                tokens = self
                                                    .preprocess(main_chars.into(), Some(tokens))?;
                                                continue 'preprocess_loop;
                                            }
                                            // reached different depth
                                            // has to be higher depth ifdef/ifndef
                                            assert!(self.watching_label < self.ifdef_depth);
                                            continue;
                                        }
                                        "ifdef" | "ifndef" => {
                                            self.increment_ifdef_depth(); // just increment current depth.
                                            continue;
                                        }
                                        _ => continue,
                                    }
                                }
                            }
                            "else" => {
                                if self.ifdef_depth == 0 {
                                    panic!("Countered else without ifdef or ifndef.");
                                }
                                // outer loop is for preproccessing. so, since we catch #else, we should skip from now on.
                                loop {
                                    match main_chars.skip_until_macro_keyword().as_str() {
                                        "ifdef" | "ifndef" => {
                                            self.increment_ifdef_depth();
                                            // just increment depth.
                                        }
                                        "endif" => {
                                            self.dec_ifdef_lable()?;
                                            if self.ifdef_depth + 1 == self.watching_label {
                                                // reached same depth
                                                self.decrement_watching_depth();
                                                if self.watching_label != 0 {
                                                    // ifdef or ifndef is preprocessed by recursive function call.
                                                    // so, if depth is not 0, it means that it is not top level.
                                                    // we should return this
                                                    return Ok(tokens);
                                                }
                                                continue 'preprocess_loop;
                                            }
                                            // reached different depth
                                            // has to be higher depth ifdef/ifndef
                                            assert!(self.watching_label < self.ifdef_depth);
                                            continue;
                                        }
                                        "else" => {
                                            if self.ifdef_depth == self.watching_label {
                                                // reached same depth

                                                tokens = self
                                                    .preprocess(main_chars.into(), Some(tokens))?;
                                                continue 'preprocess_loop;
                                            }
                                            // reached different depth
                                            // has to be higher depth ifdef/ifndef
                                            assert!(self.watching_label < self.ifdef_depth);
                                            continue;
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                            }
                            "endif" => {
                                self.dec_ifdef_lable()?;
                                if self.ifdef_depth + 1 == self.watching_label {
                                    // reached same depth
                                    self.decrement_watching_depth();
                                    if self.watching_label != 0 {
                                        // ifdef or ifndef is preprocessed by recursive function call.
                                        // so, if depth is not 0, it means that it is not top level.
                                        // we should return this
                                        return Ok(tokens);
                                    }
                                } else {
                                    // reached different depth
                                    // has to be higher depth ifdef/ifndef
                                    assert!(
                                        self.watching_label < self.ifdef_depth,
                                        "watching_label: {:?}, ifdef_depth: {:?}",
                                        self.watching_label,
                                        self.ifdef_depth
                                    );
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
                                    tokens = self.include_from_file_dir(&str_lit, tokens)?;
                                    continue 'preprocess_loop;
                                }
                                let (debug_info, mut file_name) = main_chars
                                    .get_debug_info_and_read_include_file()
                                    .expect("arg(ident) of include with `<` `>` must exist.");
                                file_name.pop(); // -> '>'
                                file_name.remove(0); // -> '<'
                                tokens =
                                    self.include_from_include_dir(&debug_info, &file_name, tokens)?;
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

            if let Some((debug_info, number)) = main_chars.get_debug_info_and_read_number() {
                tokens.push(Token::new(TokenKind::NumLit(number), debug_info));
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
        tokens = self.preprocess(
            &mut SrcCursorGenerator::from(file_info).into_cursor(),
            Some(tokens),
        )?;
        Ok(tokens)
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn include_from_include_dir(
        &mut self,
        debug_info: &DebugInfo,
        file_path: &str,
        mut tokens: Vec<Token<TokenKind>>,
    ) -> Result<Vec<Token<TokenKind>>, CompileError> {
        let include_dir = PathBuf::from(self.include_dir);
        assert!(include_dir.is_dir(), "include_dir: {:?}", include_dir);
        let included_file_path = include_dir.join(file_path);
        let included_file_path = included_file_path.as_path();
        if !included_file_path.exists() {
            return Err(unimplemented_err!(
                debug_info.clone(),
                format!("include file does not exist: {:?}", included_file_path)
            ));
        }
        let src = read_file(included_file_path)?;
        let file_info = Rc::new(FileInfo::new(
            included_file_path.to_str().unwrap().to_string(),
            src,
        ));
        tokens = self.preprocess(&mut SrcCursor::new(file_info), Some(tokens))?;
        Ok(tokens)
    }

    #[cfg(target_arch = "wasm32")]
    pub fn include_from_include_dir(
        &mut self,
        debug_info: &DebugInfo,
        file_path: &str,
        mut tokens: Vec<Token<TokenKind>>,
    ) -> Result<Vec<Token<TokenKind>>, CompileError> {
        let src = match file_path {
            "assert.h" => Includer::get_assert_h(),
            "ctype.h" => Includer::get_ctype_h(),
            "stdarg.h" => Includer::get_stdarg_h(),
            "stdbool.h" => Includer::get_stdbool_h(),
            "stddef.h" => Includer::get_stddef_h(),
            "stdio.h" => Includer::get_stdio_h(),
            "stdlib.h" => Includer::get_stdlib_h(),
            "string.h" => Includer::get_string_h(),
            "curses.h" => Includer::get_curses_h(),
            "unistd.h" => Includer::get_unistd_h(),
            "dirent.h" => Includer::get_dirent_h(),
            _ => {
                return Err(unimplemented_err!(
                    debug_info.clone(),
                    format!("include file does not exist: {:?}", file_path)
                ))
            }
        };
        let file_info = Rc::new(FileInfo::new(file_path.to_string(), src.to_string()));
        tokens = self.preprocess(&mut SrcCursor::new(file_info), Some(tokens))?;
        Ok(tokens)
    }
}

#[cfg(target_arch = "wasm32")]
mod Includer {
    macro_rules! gen_include {
        ($func_name:ident, $file_path:expr) => {
            pub const fn $func_name() -> &'static str {
                include_str!($file_path)
            }
        };
    }

    gen_include!(get_assert_h, "../include/assert.h");
    gen_include!(get_ctype_h, "../include/ctype.h");
    gen_include!(get_stdarg_h, "../include/stdarg.h");
    gen_include!(get_stdbool_h, "../include/stdbool.h");
    gen_include!(get_stddef_h, "../include/stddef.h");
    gen_include!(get_stdio_h, "../include/stdio.h");
    gen_include!(get_stdlib_h, "../include/stdlib.h");
    gen_include!(get_string_h, "../include/string.h");
    gen_include!(get_curses_h, "../include/curses.h");
    gen_include!(get_dirent_h, "../include/dirent.h");
    gen_include!(get_unistd_h, "../include/unistd.h");
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
