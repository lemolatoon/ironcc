use crate::analyze::ConvExpr;
use crate::parse::Expr;
use crate::tokenize::Position;
use crate::tokenize::Token;
use crate::tokenize::TokenKind;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;
use std::io;

pub struct CompileError {
    kind: CompileErrorKind,
    src: String,
}

impl CompileError {
    pub fn new(src: &str, kind: CompileErrorKind) -> Self {
        Self {
            kind,
            src: src.to_string(),
        }
    }
}

#[derive(Debug)]
pub enum CompileErrorKind {
    ParseError(ParseErrorKind),
    AnalyzeError(AnalyzeErrorKind),
    GenerateError(GenerateErrorKind),
    Unimplemented(String),
    IOError(io::Error),
}

impl Error for CompileError {}

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self {
        Self {
            kind: CompileErrorKind::IOError(err),
            src: String::new(), // no source
        }
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    /// An error returned when an operation could not be completed because an "end of file" was reached prematurely.
    UnexpectedEof(Box<dyn Debug>),
    ExpectFailed {
        expect: TokenKind,
        got: Token,
    },
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Debug>::fmt(&self, f)
    }
}

impl Debug for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CompileErrorKind::*;
        match &self.kind {
            ParseError(ParseErrorKind::UnexpectedEof(expect)) => {
                error_at_eof(&self.src, f)?;
                writeln!(f, "Expected `{:?}`, but got EOF.", expect)?;
            }
            ParseError(ParseErrorKind::ExpectFailed { expect, got }) => {
                error_at(&self.src, vec![got.pos], f)?;
                writeln!(f, "Expected `{:?}`, but got `{:?}`.", expect, got.kind)?;
            }
            AnalyzeError(_) => todo!(),
            GenerateError(_) => todo!(),
            Unimplemented(_) => todo!(),
            IOError(err) => Display::fmt(err, f)?,
        };
        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum AnalyzeErrorKind {
    RedefinedError(VariabelKind),
    UndeclaredError(VariabelKind),
    TypeError(Expr, Expr),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VariabelKind {
    Local,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum GenerateErrorKind {
    UnexpectedTypeSize(usize),
    LeftValueError(ConvExpr),
    DerefError(ConvExpr),
}

/// write source annotation which indicates `pos` in `src`
fn error_at(
    src: &str,
    mut positions: Vec<Position>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    writeln!(f, "====================")?;
    writeln!(f, "{}", src)?;
    writeln!(f, "====================")?;
    positions.sort_by(|a, b| (a.n_line, a.n_char).cmp(&(b.n_line, b.n_char)));
    let mut same_line_positions = Vec::new();
    let mut tmp_vec = Vec::new();
    // TOOD: handle None well
    let mut tmp_line = positions.first().map(|pos| pos.n_line).unwrap_or(0);
    for pos in positions {
        if pos.n_line == tmp_line {
            tmp_vec.push(pos);
        } else {
            tmp_line = pos.n_line;
            same_line_positions.push(tmp_vec);
            tmp_vec = Vec::new();
        }
    }
    let max_prefix_len = same_line_positions
        .last()
        .unwrap()
        .first()
        .unwrap()
        .n_line
        .to_string()
        .len()
        + 3;
    for same_line_pos in same_line_positions {
        let mut splited = src.split('\n');
        let n_line = same_line_pos.first().unwrap().n_line;
        let mut num_prefix = String::with_capacity(max_prefix_len);
        num_prefix.push_str(&n_line.to_string());
        while num_prefix.len() < max_prefix_len {
            if num_prefix.len() == max_prefix_len - 2 {
                num_prefix.push(':');
                continue;
            }
            num_prefix.push(' ');
        }
        match splited.nth(n_line) {
            Some(line) => writeln!(f, "{}{}", num_prefix, line),
            None => writeln!(
                f,
                "[While Dealing Error, another error occured.]Position is illegal,\nPosition: {:?}",
                same_line_pos.first().unwrap()
            ),
        }?;
        let n_iter = same_line_pos.last().unwrap().n_char + max_prefix_len;
        let mut buffer = String::with_capacity(n_iter + 1);
        let mut point = same_line_pos.into_iter().map(|pos| pos.n_char).peekable();
        for idx in 0..n_iter {
            match point.peek() {
                Some(&next) if next == idx => buffer.push('^'),
                _ => buffer.push(' '),
            }
        }
        writeln!(f, "{}", buffer)?;
    }
    // let pos = positions.first().unwrap();
    // let mut splited = src.split('\n');
    // let prefix = format!("{}:{}:  ", pos.n_line + 1, pos.n_char + 1);
    // match splited.nth(pos.n_line) {
    //     Some(line) => writeln!(f, "{}{}", prefix, line),
    //     None => writeln!(
    //         f,
    //         "[While Dealing Error, another error occured.]Position is illegal,\nPosition: {:?}",
    //         pos
    //     ),
    // }?;
    // let mut buffer = String::with_capacity(pos.n_char + 1);
    // for _ in 0..(pos.n_char + prefix.len()) {
    //     buffer.push(' ');
    // }
    // buffer.push('^');
    // writeln!(f, "{}", buffer)?;
    Ok(())
}

/// write source annotation which indicates `pos` in `src` with previous lines
fn error_at_eof(src: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "====================")?;
    writeln!(f, "{}", src)?;
    writeln!(f, "====================")?;
    let splited: Vec<&str> = src.split('\n').collect();
    let last_n_line = splited.len() - 1;
    let n_char = splited.last().map(|s| s.len()).unwrap_or(0);
    let prefix = format!("{}:{}:  ", last_n_line + 1, n_char + 1);
    for n_line in (last_n_line.checked_sub(2).unwrap_or(0))..=last_n_line {
        let mut num_prefix = String::with_capacity(prefix.len());
        num_prefix.push_str(&n_line.to_string());
        while num_prefix.len() < prefix.len() {
            if num_prefix.len() == prefix.len() - 3 {
                num_prefix.push(':');
                continue;
            }
            num_prefix.push(' ');
        }
        match splited.get(n_line) {
            Some(line) if n_line == last_n_line => writeln!(f, "{}{}", prefix, line),
            Some(line) => writeln!(f, "{}{}", num_prefix, line),
            None => writeln!(
                f,
                "[While Dealing Error, another error occured.]Position is illegal,\nPosition: {:?}",
                Position::new(n_char, n_line)
            ),
        }?;
    }
    let mut buffer = String::with_capacity(n_char + 1);
    for _ in 0..(n_char + prefix.len()) {
        buffer.push(' ');
    }
    buffer.push('^');
    writeln!(f, "{}", buffer)?;
    Ok(())
}
