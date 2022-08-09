use crate::analyze::ConstExpr;
use crate::analyze::ConvExpr;
use crate::analyze::ConvExprKind;
use crate::analyze::GVar;
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

    pub fn new_undeclared_error(
        src: &str,
        name: String,
        pos: Position,
        kind: VariableKind,
    ) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(name, pos, kind)),
        )
    }

    pub fn new_args_error(
        src: &str,
        name: String,
        pos: Position,
        expected: usize,
        got: usize,
        declared_pos: Position,
    ) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::FuncArgsError(
                name,
                pos,
                expected,
                got,
                declared_pos,
            )),
        )
    }

    pub fn new_type_error<T: Into<String>>(
        src: &str,
        expr0: ConvExpr,
        expr1: ConvExpr,
        msg: Option<T>,
    ) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(
                TypeErrorKind::Expr(expr0, expr1),
                msg.map(std::convert::Into::into),
            )),
        )
    }

    pub fn new_type_error_const<T: Into<String>>(
        src: &str,
        expr0: ConstExpr,
        expr1: ConstExpr,
        msg: Option<T>,
    ) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(
                TypeErrorKind::ConstExpr(expr0, expr1),
                msg.map(std::convert::Into::into),
            )),
        )
    }

    pub fn new_type_error_types<T: Into<String>>(
        src: &str,
        pos0: Position,
        pos1: Position,
        ty0: Type,
        ty1: Type,
        msg: Option<T>,
    ) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(
                TypeErrorKind::Type(pos0, pos1, ty0, ty1),
                msg.map(std::convert::Into::into),
            )),
        )
    }

    pub fn new_type_expect_failed(src: &str, pos: Position, expected: Type, got: Type) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(pos, expected, got)),
        )
    }

    pub fn new_const_expr_error(src: &str, pos: Position, kind: ConvExprKind) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::AnalyzeError(AnalyzeErrorKind::ConstExprError(pos, kind)),
        )
    }

    pub fn new_deref_error(src: &str, expr: ConvExpr) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::GenerateError(GenerateErrorKind::DerefError(expr)),
        )
    }

    pub fn new_lvalue_error(src: &str, expr: ConvExpr) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::GenerateError(GenerateErrorKind::LeftValueError(expr)),
        )
    }

    pub fn new_type_size_error(src: &str, status: UnexpectedTypeSizeStatus) -> Self {
        CompileError::new(
            src,
            CompileErrorKind::GenerateError(GenerateErrorKind::UnexpectedTypeSize(status)),
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
    #[allow(clippy::too_many_lines)]
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
            AnalyzeError(AnalyzeErrorKind::TypeError(error, msg)) => {
                let poses = error.positions();
                error_at(&self.src, vec![poses.0, poses.1], f)?;
                let types = error.types();
                writeln!(f, "{:?} and {:?} is not allowed.", types.0, types.1)?;
                if let Some(msg) = msg {
                    writeln!(f, "{}", msg)?;
                }
            }
            AnalyzeError(AnalyzeErrorKind::FuncArgsError(
                name,
                pos,
                expected,
                got,
                declared_pos,
            )) => {
                error_at(&self.src, vec![*pos], f)?;
                writeln!(
                    f,
                    "In {:?}'s calling, {} args expected, but got {}",
                    name, expected, got
                )?;
                error_at(&self.src, vec![*declared_pos], f)?;
                writeln!(f, "{} is first declared here.", name)?;
            }
            AnalyzeError(AnalyzeErrorKind::ConstExprError(pos, kind)) => {
                error_at(&self.src, vec![*pos], f)?;
                writeln!(
                    f,
                    "This kind({:?}) of Expr cannot be evaluated as constants.",
                    kind
                )?;
            }
            GenerateError(GenerateErrorKind::DerefError(expr)) => {
                error_at(&self.src, vec![expr.pos], f)?;
                writeln!(f, "{:?} cannot be derefered.", expr.ty)?;
            }
            GenerateError(GenerateErrorKind::LeftValueError(expr)) => {
                error_at(&self.src, vec![expr.pos], f)?;
                writeln!(
                    f,
                    "This expr cannot be used for generate address(Not left value). Type: {:?}",
                    expr.ty
                )?;
            }
            GenerateError(GenerateErrorKind::UnexpectedTypeSize(
                UnexpectedTypeSizeStatus::Expr(expr),
            )) => {
                error_at(&self.src, vec![expr.pos], f)?;
                writeln!(
                    f,
                    "This expr's type size is unexpected. This type cannot be treated as any size of ptr which is allowed by CPU itself. Type: {:?}",
                    expr.ty
                )?;
            }
            GenerateError(GenerateErrorKind::UnexpectedTypeSize(
                UnexpectedTypeSizeStatus::FuncArgs(name, ty),
            )) => {
                writeln!(
                    f,
                    "one of func {}'s args is unexpected. This type cannot be treated as any size of ptr which is allowed by CPU itself. Type: {:?}",
                    name,
                    ty
                )?;
            }
            GenerateError(GenerateErrorKind::UnexpectedTypeSize(
                UnexpectedTypeSizeStatus::Global(GVar { name, ty, init: _ }),
            )) => {
                writeln!(
                    f,
                    "This Global Variable's type size is unexpected. name: {}, ty: {:?}, ty.sizeof(): {}", name, ty, ty.size_of()
                )?;
            }
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
    FuncArgsError(String, Position, usize, usize, Position),
    TypeError(TypeErrorKind, Option<String>),
    TypeExpectFailed(Position, Type, Type),
    ConstExprError(Position, ConvExprKind),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeErrorKind {
    Expr(ConvExpr, ConvExpr),
    ConstExpr(ConstExpr, ConstExpr),
    Type(Position, Position, Type, Type),
}

impl TypeErrorKind {
    pub const fn positions(&self) -> (Position, Position) {
        match self {
            TypeErrorKind::Expr(expr0, expr1) => (expr0.pos, expr1.pos),
            TypeErrorKind::ConstExpr(expr0, expr1) => (expr0.pos, expr1.pos),
            TypeErrorKind::Type(pos0, pos1, _, _) => (*pos0, *pos1),
        }
    }

    pub fn types(&self) -> (Type, Type) {
        match self {
            TypeErrorKind::Expr(expr0, expr1) => (expr0.ty.clone(), expr1.ty.clone()),
            TypeErrorKind::ConstExpr(expr0, expr1) => (expr0.ty.clone(), expr1.ty.clone()),
            TypeErrorKind::Type(_, _, ty0, ty1) => (ty0.clone(), ty1.clone()),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum VariableKind {
    Local,
    Global,
    Func,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum GenerateErrorKind {
    UnexpectedTypeSize(UnexpectedTypeSizeStatus),
    LeftValueError(ConvExpr),
    DerefError(ConvExpr),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum UnexpectedTypeSizeStatus {
    Expr(ConvExpr),
    FuncArgs(String, Type),
    Global(GVar),
}

/// write source annotation which indicates `pos` in `src`
fn error_at(
    src: &str,
    mut positions: Vec<Position>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    writeln!(f, "\n")?;
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
            tmp_vec = vec![pos];
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
                Some(&next) if next == idx => {
                    point.next();
                    buffer.push('^');
                }
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
