use std::{fs::File, io::Read, path::Path};

/// return `CompilerError` whose kind is `Unimplemented`
///  - 1st arg: `input`:&str
///  - 2nd arg: `pos`: Position
///  - 3rd arg: `msg`: error message
///
/// Also no args provided is allowed.
#[macro_export]
macro_rules! unimplemented_err {
    ($pos: expr, $msg: expr) => {
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

pub fn read_file(path: &Path) -> Result<String, std::io::Error> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}
