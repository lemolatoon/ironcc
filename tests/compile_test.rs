pub mod test_utils;

use ironcc::error::{CompileError, CompileErrorKind, ParseErrorKind, TokenizeErrorKind};
use test_utils::CachedProcesser;

#[test]
fn unexpected_char() {
    let src = "int main() {int あいうえお = 5; return 0;}";
    let mut tester = CachedProcesser::new(src);
    assert!(matches!(
        tester.tokens(),
        Err(CompileError {
            kind: CompileErrorKind::TokenizeError(TokenizeErrorKind::UnexpectedChar(_, _)),
            src: _,
        })
    ));
}

#[test]
fn unexpected_eof() {
    let src = "int main() { int k = 4; return 0; ";
    let mut tester = CachedProcesser::new(src);
    assert!(matches!(
        tester.program(),
        Err(CompileError {
            kind: CompileErrorKind::ParseError(ParseErrorKind::UnexpectedEof(_)),
            src: _
        })
    ))
}
