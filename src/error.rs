use crate::analyze::ConstExpr;
use crate::analyze::ConvExpr;
use crate::analyze::ConvExprKind;
use crate::analyze::GVar;
use crate::analyze::Type;
use crate::preprocess;
use crate::tokenize;
use crate::tokenize::DebugInfo;
use crate::tokenize::Position;
use crate::tokenize::Token;
use std::error::Error;
use std::fmt::Debug;
use std::fmt::Display;
use std::io;

#[derive(Clone)]
pub struct CompileError {
    pub kind: CompileErrorKind,
}

impl CompileError {
    pub const fn new(kind: CompileErrorKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug)]
pub enum CompileErrorKind {
    TokenizeError(TokenizeErrorKind),
    ParseError(ParseErrorKind),
    AnalyzeError(AnalyzeErrorKind),
    GenerateError(GenerateErrorKind),
    Unimplemented(Option<DebugInfo>, String),
    IOError(Box<dyn Debug>),
}

impl Clone for CompileErrorKind {
    fn clone(&self) -> Self {
        match self {
            Self::TokenizeError(arg0) => Self::TokenizeError(arg0.clone()),
            Self::ParseError(arg0) => Self::ParseError(arg0.clone()),
            Self::AnalyzeError(arg0) => Self::AnalyzeError(arg0.clone()),
            Self::GenerateError(arg0) => Self::GenerateError(arg0.clone()),
            Self::Unimplemented(arg0, arg1) => Self::Unimplemented(arg0.clone(), arg1.clone()),
            Self::IOError(arg0) => Self::IOError(Box::new(format!("{:?}", arg0))),
        }
    }
}

impl Error for CompileError {}

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self {
        Self {
            kind: CompileErrorKind::IOError(Box::new(err)),
        }
    }
}

impl From<Token<preprocess::TokenKind>> for Tokens {
    fn from(token: Token<preprocess::TokenKind>) -> Self {
        Tokens::Preprocess(token)
    }
}
impl From<Token<tokenize::TokenKind>> for Tokens {
    fn from(token: Token<tokenize::TokenKind>) -> Self {
        Tokens::Tokenize(token)
    }
}

impl CompileError {
    pub const fn new_unexpected_char(debug_info: DebugInfo, c: char) -> CompileError {
        CompileError::new(CompileErrorKind::TokenizeError(
            TokenizeErrorKind::UnexpectedChar(debug_info, c),
        ))
    }
    pub fn new_unexpected_eof(input: Option<String>, kind: Box<dyn Debug>) -> CompileError {
        CompileError::new(CompileErrorKind::ParseError(ParseErrorKind::UnexpectedEof(
            input, kind,
        )))
    }

    pub fn new_expected_failed<K>(expect: Box<dyn Debug>, got: Token<K>) -> CompileError
    where
        K: PartialEq + Debug + Clone,
        Token<K>: Into<Tokens>,
    {
        CompileError::new(CompileErrorKind::ParseError(ParseErrorKind::ExpectFailed {
            expect,
            got: got.into(),
        }))
    }

    pub fn new_ident_expected_failed<K>(got: Token<K>) -> CompileError
    where
        K: PartialEq + Debug + Clone,
        Token<K>: Into<Tokens>,
    {
        CompileError::new(CompileErrorKind::ParseError(
            ParseErrorKind::IdentExpectFailed { got: got.into() },
        ))
    }

    pub fn new_expected_failed_with_box<K>(expect: Box<dyn Debug>, got: Token<K>) -> CompileError
    where
        K: PartialEq + Debug + Clone,
        Token<K>: Into<Tokens>,
    {
        CompileError::new(CompileErrorKind::ParseError(ParseErrorKind::ExpectFailed {
            expect,
            got: got.into(),
        }))
    }

    pub const fn new_unexpected_eof_tokenize(pos: DebugInfo) -> Self {
        CompileError::new(CompileErrorKind::TokenizeError(
            TokenizeErrorKind::UnexpectedEof(pos),
        ))
    }

    pub const fn new_unexpected_void(pos: DebugInfo, msg: String) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::TypeExpectFailed(TypeExpectedFailedKind::UnexpectedVoid(pos, msg)),
        ))
    }

    pub const fn new_redefined_variable(
        name: String,
        debug_info: DebugInfo,
        kind: VariableKind,
    ) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::RedefinedError(name, debug_info, kind),
        ))
    }

    pub const fn new_no_such_member(
        tag_name: Option<String>,
        pos: DebugInfo,
        got_member_name: String,
    ) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::NoSuchMemberError {
                tag_name,
                pos,
                got_member_name,
            },
        ))
    }

    pub const fn new_undeclared_error(name: String, pos: DebugInfo, kind: VariableKind) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::UndeclaredError(name, pos, kind),
        ))
    }

    pub const fn new_args_error(
        name: String,
        debug_info: DebugInfo,
        expected: usize,
        got: usize,
        declared_debug_info: DebugInfo,
    ) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::FuncArgsError(name, debug_info, expected, got, declared_debug_info),
        ))
    }

    pub fn new_type_error<T: Into<String>>(lhs: ConvExpr, rhs: ConvExpr, msg: Option<T>) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(
            TypeErrorKind::Expr { lhs, rhs },
            msg.map(std::convert::Into::into),
        )))
    }

    pub fn new_type_error_const<T: Into<String>>(
        expr0: ConstExpr,
        expr1: ConstExpr,
        msg: Option<T>,
    ) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(
            TypeErrorKind::ConstExpr(expr0, expr1),
            msg.map(std::convert::Into::into),
        )))
    }

    pub fn new_type_error_types<T: Into<String>>(
        debug_info0: DebugInfo,
        debug_info1: DebugInfo,
        ty0: Type,
        ty1: Type,
        msg: Option<T>,
    ) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(
            TypeErrorKind::Type(debug_info0, debug_info1, ty0, ty1),
            msg.map(std::convert::Into::into),
        )))
    }

    pub const fn new_type_expect_failed(pos: DebugInfo, expected: Type, got: Type) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::TypeExpectFailed(TypeExpectedFailedKind::Type { pos, expected, got }),
        ))
    }

    pub const fn new_type_expect_failed_with_str(
        debug_info: DebugInfo,
        expected: String,
        got: Type,
    ) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::TypeExpectFailed(TypeExpectedFailedKind::TypeWithPatternStr {
                pos: debug_info,
                expected,
                got,
            }),
        ))
    }

    pub const fn new_const_expr_error(debug_info: DebugInfo, kind: ConvExprKind) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::ConstExprError(debug_info, kind),
        ))
    }

    pub const fn new_not_allowed_stmt_error(
        debug_info: DebugInfo,
        kind: NotAllowedStmtKind,
    ) -> Self {
        CompileError::new(CompileErrorKind::AnalyzeError(
            AnalyzeErrorKind::NotAllowedStmtError(debug_info, kind),
        ))
    }

    pub const fn new_deref_error(expr: ConvExpr) -> Self {
        CompileError::new(CompileErrorKind::GenerateError(
            GenerateErrorKind::DerefError(expr),
        ))
    }

    pub const fn new_lvalue_error(expr: ConvExpr) -> Self {
        CompileError::new(CompileErrorKind::GenerateError(
            GenerateErrorKind::LeftValueError(expr),
        ))
    }

    pub const fn new_type_size_error(status: UnexpectedTypeSizeStatus) -> Self {
        CompileError::new(CompileErrorKind::GenerateError(
            GenerateErrorKind::UnexpectedTypeSize(status),
        ))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TokenizeErrorKind {
    UnexpectedChar(DebugInfo, char),
    UnexpectedEof(DebugInfo),
}

#[derive(Debug, Clone)]
pub enum Tokens {
    Preprocess(Token<preprocess::TokenKind>),
    Tokenize(Token<tokenize::TokenKind>),
}

impl Tokens {
    pub fn get_debug_info(&self) -> DebugInfo {
        match self {
            Tokens::Preprocess(token) => token.debug_info.clone(),
            Tokens::Tokenize(token) => token.debug_info.clone(),
        }
    }

    pub fn get_kind_str(&self) -> String {
        match self {
            Tokens::Preprocess(token) => format!("{:?}", token.kind),
            Tokens::Tokenize(token) => format!("{:?}", token.kind),
        }
    }

    pub const fn get_tokenize_token(&self) -> Option<&Token<tokenize::TokenKind>> {
        match self {
            Tokens::Preprocess(_) => None,
            Tokens::Tokenize(token) => Some(token),
        }
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    /// An error returned when an operation could not be completed because an "end of file" was reached prematurely.
    UnexpectedEof(/* src */ Option<String>, Box<dyn Debug>),
    ExpectFailed {
        expect: Box<dyn Debug>,
        got: Tokens,
    },
    IdentExpectFailed {
        got: Tokens,
    },
}

impl Clone for ParseErrorKind {
    fn clone(&self) -> Self {
        match self {
            Self::UnexpectedEof(src, arg0) => {
                Self::UnexpectedEof(src.clone(), Box::new(format!("{:?}", arg0)))
            }
            Self::ExpectFailed { expect, got } => Self::ExpectFailed {
                expect: Box::new(format!("{:?}", expect)),
                got: got.clone(),
            },
            ParseErrorKind::IdentExpectFailed { got } => {
                Self::IdentExpectFailed { got: got.clone() }
            }
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
            TokenizeError(TokenizeErrorKind::UnexpectedEof(debug_info)) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(f, "Got Unexpected Eof while tokenizing")?;
            }
            TokenizeError(TokenizeErrorKind::UnexpectedChar(debug_info, c)) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(f, "Got Unexpected char while tokenizing: `{}`", c)?;
            }
            ParseError(ParseErrorKind::UnexpectedEof(src, expect)) => {
                if let Some(src) = src {
                    error_at_eof(src.as_str(), f)?;
                }
                writeln!(f, "Expected `{:?}`, but got EOF.", expect)?;
            }
            ParseError(ParseErrorKind::ExpectFailed { expect, got }) => {
                error_at(vec![got.get_debug_info()], f)?;
                writeln!(
                    f,
                    "Expected `{:?}`, but got `{:?}`.",
                    expect,
                    got.get_kind_str(),
                )?;
            }
            ParseError(ParseErrorKind::IdentExpectFailed { got }) => {
                error_at(vec![got.get_debug_info()], f)?;
                writeln!(
                    f,
                    "Expected `TokenKind::Ident(_)`, but got `{:?}`.",
                    got.get_kind_str(),
                )?;
            }
            AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(
                TypeExpectedFailedKind::UnexpectedVoid(debug_info, msg),
            )) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(f, "{:?}", msg)?;
            }
            AnalyzeError(AnalyzeErrorKind::UndeclaredError(name, debug_info, kind)) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(f, "{:?} Variable `{}` Undeclared", kind, name)?;
            }
            AnalyzeError(AnalyzeErrorKind::RedefinedError(name, debug_info, kind)) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(f, "{:?} Variable `{}` Redefined", kind, name)?;
            }
            AnalyzeError(AnalyzeErrorKind::NoSuchMemberError {
                tag_name,
                pos: debug_info,
                got_member_name,
            }) => {
                error_at(vec![debug_info.clone()], f)?;
                if let Some(tag_name) = tag_name {
                    writeln!(
                        f,
                        "`struct {}` 's members do not contain `{}`.",
                        tag_name, got_member_name
                    )?;
                } else {
                    writeln!(
                        f,
                        "this struct 's members do not contain `{}`.",
                        got_member_name
                    )?;
                }
            }
            AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(TypeExpectedFailedKind::Type {
                pos: debug_info,
                expected,
                got,
            })) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(f, "{:?} type expected, but got {:?}", expected, got)?;
            }
            AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(
                TypeExpectedFailedKind::TypeWithPatternStr {
                    pos: debug_info,
                    expected,
                    got,
                },
            )) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(f, "{:?} type expected, but got {:?}", expected, got)?;
            }
            AnalyzeError(AnalyzeErrorKind::TypeError(error, msg)) => {
                let poses = error.positions();
                error_at(vec![poses.0, poses.1], f)?;
                let types = error.types();
                writeln!(
                    f,
                    "Type conversion from {:?} into {:?} is not allowed.",
                    types.1, types.0
                )?;
                if let Some(msg) = msg {
                    writeln!(f, "{}", msg)?;
                }
            }
            AnalyzeError(AnalyzeErrorKind::FuncArgsError(
                name,
                debug_info,
                expected,
                got,
                declared_pos,
            )) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(
                    f,
                    "In {:?}'s calling, {} args expected, but got {}",
                    name, expected, got
                )?;
                error_at(vec![declared_pos.clone()], f)?;
                writeln!(f, "{} is first declared here.", name)?;
            }
            AnalyzeError(AnalyzeErrorKind::ConstExprError(debug_info, kind)) => {
                error_at(vec![debug_info.clone()], f)?;
                writeln!(
                    f,
                    "This kind({:?}) of Expr cannot be evaluated as constants.",
                    kind
                )?;
            }
            AnalyzeError(AnalyzeErrorKind::NotAllowedStmtError(debug_info, kind)) => {
                error_at(vec![debug_info.clone()], f)?;
                match kind {
                    NotAllowedStmtKind::Break => {
                        writeln!(f, "`break` statement not in loop or switch statement.")?
                    }
                    NotAllowedStmtKind::Continue => {
                        writeln!(f, "`continue` statement not in loop statement.")?
                    }
                    NotAllowedStmtKind::Case => {
                        writeln!(f, "`case` statement not in loop or switch statement.")?
                    }
                    NotAllowedStmtKind::Default => {
                        writeln!(f, "`default` statement not in loop or switch statement.")?
                    }
                }
            }
            GenerateError(GenerateErrorKind::DerefError(expr)) => {
                error_at(vec![expr.debug_info.clone()], f)?;
                writeln!(f, "{:?} cannot be dereferenced.", expr.ty)?;
            }
            GenerateError(GenerateErrorKind::LeftValueError(expr)) => {
                error_at(vec![expr.debug_info.clone()], f)?;
                writeln!(
                    f,
                    "This expr cannot be used for generate address(Not left value). Type: {:?}",
                    expr.ty
                )?;
            }
            GenerateError(GenerateErrorKind::UnexpectedTypeSize(
                UnexpectedTypeSizeStatus::Expr(expr),
            )) => {
                error_at(vec![expr.debug_info.clone()], f)?;
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
                UnexpectedTypeSizeStatus::Global(GVar {
                    name,
                    ty,
                    init: _,
                    is_extern,
                }),
            )) => {
                writeln!(
                    f,
                    "This Global Variable's type size is unexpected. name: {}, ty: {:?}, ty.sizeof(): {}, is_extern: {}", name, ty, ty.size_of(), is_extern
                )?;
            }
            GenerateError(GenerateErrorKind::UnexpectedTypeSize(
                UnexpectedTypeSizeStatus::Size(size),
            )) => {
                writeln!(f, "this type size is unexpected. size: {}", size)?;
            }
            Unimplemented(Some(debug_info), msg) => {
                error_at(vec![debug_info.clone()], f)?;
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
    RedefinedError(String, DebugInfo, VariableKind),
    UndeclaredError(String, DebugInfo, VariableKind),
    NoSuchMemberError {
        tag_name: Option<String>,
        pos: DebugInfo,
        got_member_name: String,
    },
    FuncArgsError(String, DebugInfo, usize, usize, DebugInfo),
    TypeError(TypeErrorKind, Option<String>),
    TypeExpectFailed(TypeExpectedFailedKind),
    ConstExprError(DebugInfo, ConvExprKind),
    NotAllowedStmtError(DebugInfo, NotAllowedStmtKind),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum NotAllowedStmtKind {
    Break,
    Continue,
    Case,
    Default,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeExpectedFailedKind {
    Type {
        pos: DebugInfo,
        expected: Type,
        got: Type,
    },
    TypeWithPatternStr {
        pos: DebugInfo,
        expected: String,
        got: Type,
    },
    UnexpectedVoid(DebugInfo, String),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeErrorKind {
    Expr { lhs: ConvExpr, rhs: ConvExpr },
    ConstExpr(ConstExpr, ConstExpr),
    Type(DebugInfo, DebugInfo, Type, Type),
}

impl TypeErrorKind {
    pub fn positions(&self) -> (DebugInfo, DebugInfo) {
        match self {
            TypeErrorKind::Expr { lhs, rhs } => (lhs.debug_info.clone(), rhs.debug_info.clone()),
            TypeErrorKind::ConstExpr(expr0, expr1) => {
                (expr0.debug_info.clone(), expr1.debug_info.clone())
            }
            TypeErrorKind::Type(pos0, pos1, _, _) => (pos0.clone(), pos1.clone()),
        }
    }

    pub fn types(&self) -> (Type, Type) {
        match self {
            TypeErrorKind::Expr { lhs, rhs } => (lhs.ty.clone(), rhs.ty.clone()),
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
    Struct,
    Enum,
    Typedef,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum GenerateErrorKind {
    UnexpectedTypeSize(UnexpectedTypeSizeStatus),
    LeftValueError(ConvExpr),
    DerefError(ConvExpr),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum UnexpectedTypeSizeStatus {
    Size(usize),
    Expr(ConvExpr),
    FuncArgs(String, Type),
    Global(GVar),
}

/// write source annotation which indicates `pos` in `src`
fn error_at(mut positions: Vec<DebugInfo>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "\n")?;
    positions.sort_by(|a, b| {
        (a.get_file_name(), a.get_n_line(), a.get_n_char()).cmp(&(
            b.get_file_name(),
            b.get_n_line(),
            b.get_n_char(),
        ))
    });
    let max_prefix_len = positions
        .iter()
        .map(|debug_info| debug_info.to_error_msg_prefix_string().len())
        .max()
        .unwrap_or(0)
        + 3;
    let collect_by_lines = |same_file_name_info_vec: Vec<DebugInfo>| {
        let mut same_line_positions = Vec::new();
        let mut tmp_vec = Vec::new();
        // TOOD: handle None well
        let mut tmp_line = same_file_name_info_vec
            .first()
            .map_or(0, DebugInfo::get_n_line);
        for pos in same_file_name_info_vec {
            if pos.get_n_line() == tmp_line {
                tmp_vec.push(pos);
            } else {
                tmp_line = pos.get_n_line();
                same_line_positions.push(tmp_vec);
                tmp_vec = vec![pos];
            }
        }
        same_line_positions.push(tmp_vec);
        same_line_positions
    };
    let mut collected_by_file_name_by_lines_debug_infos = Vec::new();
    let mut tmp_vec = Vec::new();
    let mut tmp_file_name = positions.first().unwrap().get_file_name();
    for debug_info in positions {
        if debug_info.get_file_name() == tmp_file_name {
            tmp_vec.push(debug_info);
        } else {
            tmp_file_name = debug_info.get_file_name();
            collected_by_file_name_by_lines_debug_infos.push(collect_by_lines(tmp_vec));
            tmp_vec = vec![debug_info];
        }
    }
    collected_by_file_name_by_lines_debug_infos.push(collect_by_lines(tmp_vec));
    // let max_prefix_len = same_line_positions
    //     .last()
    //     .unwrap()
    //     .first()
    //     .unwrap()
    //     .get_n_line()
    //     .to_string()
    //     .len()
    //     + 3;
    for same_line_positions in collected_by_file_name_by_lines_debug_infos {
        let file_info = &same_line_positions[0][0];
        let file_name = file_info.get_file_name();
        let src = file_info.get_file_src();
        for same_line_pos in same_line_positions {
            let mut splited = src.split('\n');
            let n_line = same_line_pos.first().unwrap().get_n_line();
            let mut prefix = String::with_capacity(max_prefix_len);
            prefix.push_str(&format!("{}:{}", &file_name, n_line + 1));
            while prefix.len() < max_prefix_len {
                if prefix.len() == max_prefix_len - 2 {
                    prefix.push(':');
                    continue;
                }
                prefix.push(' ');
            }
            if let Some(line) = splited.nth(n_line) {
                writeln!(f, "{}{}", prefix, line)
            } else {
                writeln!(
                f,
                "[While Dealing Error, another error occured.] DebugInfo is illegal,\nPosition: {:?}",
                same_line_pos.first().unwrap()
            )
            }?;
            let n_iter = same_line_pos.last().unwrap().get_n_char() + max_prefix_len;
            let mut buffer = String::with_capacity(n_iter + 1);
            let mut point = same_line_pos
                .into_iter()
                .map(|pos| pos.get_n_char() + max_prefix_len)
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
    }
    Ok(())
}

/// write source annotation which indicates `pos` in `src` with previous lines
fn error_at_eof(src: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let splited: Vec<&str> = src.split('\n').collect();
    let last_n_line = splited.len() - 1;
    let n_char = splited.last().map_or(0, |s| s.len());
    let prefix = format!("{}:{}:  ", last_n_line + 1, n_char + 1);
    for n_line in last_n_line.saturating_sub(3)..last_n_line {
        let mut num_prefix = String::with_capacity(prefix.len());
        num_prefix.push_str(&(n_line + 1).to_string());
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
                Position { n_char, n_line }
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
