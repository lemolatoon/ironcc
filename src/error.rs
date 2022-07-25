use crate::analyze::ConvExpr;
use crate::analyze::Type;
use crate::tokenize::Position;
use crate::tokenize::Token;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;
use std::io;

#[derive(Clone)]
pub struct CompileError {
    pub kind: CompileErrorKind,
    pub src: String,
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
    TokenizeError(TokenizeErrorKind),
    ParseError(ParseErrorKind),
    AnalyzeError(AnalyzeErrorKind),
    GenerateError(GenerateErrorKind),
    Unimplemented(Option<Position>, String),
    IOError(Box<dyn Debug>),
}

impl Clone for CompileErrorKind {
    fn clone(&self) -> Self {
        match self {
            Self::TokenizeError(arg0) => Self::TokenizeError(arg0.clone()),
            Self::ParseError(arg0) => Self::ParseError(arg0.clone()),
            Self::AnalyzeError(arg0) => Self::AnalyzeError(arg0.clone()),
            Self::GenerateError(arg0) => Self::GenerateError(arg0.clone()),
            Self::Unimplemented(arg0, arg1) => Self::Unimplemented(*arg0, arg1.clone()),
            Self::IOError(arg0) => Self::IOError(Box::new(format!("{:?}", arg0))),
        }
    }
}

impl Error for CompileError {}

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self {
        Self {
            kind: CompileErrorKind::IOError(Box::new(err)),
            src: String::new(), // no source
        }
    }
}

impl CompileError {
    pub fn new_redefined_variable(
        src: &str,
        name: String,
        pos: Position,
        kind: VariableKind,
    ) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::RedefinedError(name, pos, kind)),
        )
    }

    pub fn new_type_error(
        src: &str,
        expr0: ConvExpr,
        expr1: ConvExpr,
        msg: Option<String>,
    ) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(expr0, expr1, msg)),
        )
    }

    pub fn new_type_expect_failed(src: &str, pos: Position, expected: Type, got: Type) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(pos, expected, got)),
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TokenizeErrorKind {
    UnexpectedChar(Position, char),
}

#[derive(Debug)]
pub enum ParseErrorKind {
    /// An error returned when an operation could not be completed because an "end of file" was reached prematurely.
    UnexpectedEof(Box<dyn Debug>),
    ExpectFailed {
        expect: Box<dyn Debug>,
        got: Token,
    },
}

impl Clone for ParseErrorKind {
    fn clone(&self) -> Self {
        match self {
            Self::UnexpectedEof(arg0) => Self::UnexpectedEof(Box::new(format!("{:?}", arg0))),
            Self::ExpectFailed { expect, got } => Self::ExpectFailed {
                expect: Box::new(format!("{:?}", expect)),
                got: got.clone(),
            },
        }
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Debug>::fmt(self, f)
    }
}

impl Debug for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CompileErrorKind::{
            AnalyzeError, GenerateError, IOError, ParseError, TokenizeError, Unimplemented,
        };
        match &self.kind {
            TokenizeError(TokenizeErrorKind::UnexpectedChar(pos, c)) => {
                error_at(&self.src, vec![*pos], f)?;
                writeln!(f, "Got Unexpected char while tokenizing: `{}`", c)?;
            }
            ParseError(ParseErrorKind::UnexpectedEof(expect)) => {
                error_at_eof(&self.src, f)?;
                writeln!(f, "Expected `{:?}`, but got EOF.", expect)?;
            }
            ParseError(ParseErrorKind::ExpectFailed { expect, got }) => {
                error_at(&self.src, vec![got.pos], f)?;
                writeln!(f, "Expected `{:?}`, but got `{:?}`.", expect, got.kind)?;
            }
            AnalyzeError(AnalyzeErrorKind::UndeclaredError(name, pos, kind)) => {
                error_at(&self.src, vec![*pos], f)?;
                writeln!(f, "{:?} Variable `{}` Undeclared", kind, name)?;
            }
            AnalyzeError(AnalyzeErrorKind::RedefinedError(name, pos, kind)) => {
                error_at(&self.src, vec![*pos], f)?;
                writeln!(f, "{:?} Variable `{}` Redefined", kind, name)?;
            }
            AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(pos, expected, got)) => {
                error_at(&self.src, vec![*pos], f)?;
                writeln!(f, "{:?} type expected, but got {:?}", expected, got)?;
            }
            AnalyzeError(AnalyzeErrorKind::TypeError(exp0, exp1, Some(msg))) => {
                error_at(&self.src, vec![exp0.pos, exp1.pos], f)?;
                writeln!(f, "{:?} and {:?} is not allowed.", exp0.ty, exp1.ty)?;
                writeln!(f, "{}", msg)?;
            }
            AnalyzeError(AnalyzeErrorKind::TypeError(exp0, exp1, None)) => {
                error_at(&self.src, vec![exp0.pos, exp1.pos], f)?;
                writeln!(f, "{:?} and {:?} is not allowed.", exp0.ty, exp1.ty)?;
            }
            GenerateError(_) => todo!(),
            Unimplemented(Some(pos), msg) => {
                error_at(&self.src, vec![*pos], f)?;
                writeln!(f, "{}", msg)?;
            }
            Unimplemented(None, msg) => {
                writeln!(f, "{}", msg)?;
            }
            IOError(err) => Debug::fmt(err, f)?,
        };
        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum AnalyzeErrorKind {
    RedefinedError(String, Position, VariableKind),
    UndeclaredError(String, Position, VariableKind),
    TypeError(ConvExpr, ConvExpr, Option<String>),
    TypeExpectFailed(Position, Type, Type),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VariableKind {
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
    // TODO: fix bug when positions.len() >= 2.
    writeln!(f, "====================")?;
    writeln!(f, "{}", src)?;
    writeln!(f, "====================")?;
    positions.sort_by(|a, b| (a.n_line, a.n_char).cmp(&(b.n_line, b.n_char)));
    let mut same_line_positions = Vec::new();
    let mut tmp_vec = Vec::new();
    // TOOD: handle None well
    let mut tmp_line = positions.first().map_or(0, |pos| pos.n_line);
    for pos in positions {
        if pos.n_line == tmp_line {
            tmp_vec.push(pos);
        } else {
            tmp_line = pos.n_line;
            same_line_positions.push(tmp_vec);
            tmp_vec = Vec::new();
        }
    }
    same_line_positions.push(tmp_vec);
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
        num_prefix.push_str(&(n_line + 1).to_string());
        while num_prefix.len() < max_prefix_len {
            if num_prefix.len() == max_prefix_len - 2 {
                num_prefix.push(':');
                continue;
            }
            num_prefix.push(' ');
        }
        if let Some(line) = splited.nth(n_line) {
            writeln!(f, "{}{}", num_prefix, line)
        } else {
            writeln!(
                f,
                "[While Dealing Error, another error occured.]Position is illegal,\nPosition: {:?}",
                same_line_pos.first().unwrap()
            )
        }?;
        let n_iter = same_line_pos.last().unwrap().n_char + max_prefix_len;
        let mut buffer = String::with_capacity(n_iter + 1);
        let mut point = same_line_pos
            .into_iter()
            .map(|pos| pos.n_char + max_prefix_len)
            .peekable();
        for idx in 0..=n_iter {
            match point.peek() {
                Some(&next) if next == idx => buffer.push('^'),
                _ => buffer.push(' '),
            }
        }
        writeln!(f, "{}", buffer)?;
    }
    Ok(())
}

/// write source annotation which indicates `pos` in `src` with previous lines
fn error_at_eof(src: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "====================")?;
    writeln!(f, "{}", src)?;
    writeln!(f, "====================")?;
    let splited: Vec<&str> = src.split('\n').collect();
    let last_n_line = splited.len() - 1;
    let n_char = splited.last().map_or(0, |s| s.len());
    let prefix = format!("{}:{}:  ", last_n_line + 1, n_char + 1);
    for n_line in last_n_line.saturating_sub(2)..=last_n_line {
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
