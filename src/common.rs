use std::{fs::File, io::Read, path::Path};

/// return `CompilerError` whose kind is `Unimplemented`
///  - 1st arg: `debug_info: DebugInfo`
///  - 2rd arg: `msg`: T` `where T: Display`  // error message
///
/// Also no args provided is allowed.
#[macro_export]
macro_rules! unimplemented_err {
    ($debug_info: expr, $msg: expr) => {
        CompileError::new($crate::error::CompileErrorKind::Unimplemented(
            Some($debug_info),
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

/// read file of given path as text and return it as `String`
pub fn read_file(path: &Path) -> Result<String, std::io::Error> {
    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    contents.push('\n');
    Ok(contents)
}
