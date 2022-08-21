/// return `CompilerError` whose kind is `Unimplemented`
///  - 1st arg: `input`:&str
///  - 2nd arg: `pos`: Position
///  - 3rd arg: `msg`: error message
///
/// Also no args provided is allowed.
#[macro_export]
macro_rules! unimplemented_err {
    ($input: expr, $pos: expr, $msg: expr) => {
        CompileError::new($crate::error::CompileErrorKind::Unimplemented(
            Some($pos),
            format!("{}{}", $crate::meta!(), $msg),
        ))
    };
    ($msg: expr) => {
        CompileError::new($crate::error::CompileErrorKind::Unimplemented(
            None,
            format!("{} {}", $crate::meta!(), $msg),
        ))
    };
    () => {
        CompileError::new($crate::error::CompileErrorKind::Unimplemented(
            None,
            format!("{} Not yet implemented.", $crate::meta!()),
        ))
    };
}

#[macro_export]
macro_rules! meta {
    () => {
        concat!(file!(), ":", line!(), ":", column!(), ": ")
    };
}
