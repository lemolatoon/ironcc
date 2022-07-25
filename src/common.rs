/// return `CompilerError` whose kind is `Unimplemented`
///  - 1st arg: `self`: where `self.input: &str` is valid
///  - 2nd arg: `pos`: Position
///  - 3rd arg: `msg`: error message
///
/// Also no args provided is allowed.
#[macro_export]
macro_rules! unimplemented_err {
    ($self: ident, $pos: expr, $msg: expr) => {
        CompileError::new(
            $self.input,
            $crate::error::CompileErrorKind::Unimplemented(
                Some($pos),
                format!("{}{}", $crate::meta!(), $msg),
            ),
        )
    };
    () => {
        CompileError::new(
            "",
            $crate::error::CompileErrorKind::Unimplemented(
                None,
                format!("{} Not yet unimplemented.", $crate::meta!()),
            ),
        )
    };
}

#[macro_export]
macro_rules! meta {
    () => {
        concat!(file!(), ":", line!(), ":", column!(), ": ")
    };
}
