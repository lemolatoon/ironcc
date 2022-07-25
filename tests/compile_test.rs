pub mod test_utils;

use ironcc::error::{
    AnalyzeErrorKind, CompileError, CompileErrorKind, ParseErrorKind, TokenizeErrorKind,
    VariableKind,
};
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

#[test]
fn expected_failed() {
    let src = "int main() { int k = 4 return k; }";
    let mut tester = CachedProcesser::new(src);
    assert!(matches!(
        tester.program(),
        Err(CompileError {
            kind: CompileErrorKind::ParseError(ParseErrorKind::ExpectFailed { expect: _, got: _ }),
            src: _
        })
    ));
}

#[test]
fn variable_undeclared() {
    let src = "int main() { return a; }";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::Local
                )),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn variable_redefined() {
    let src = "int main() { int b; int b; return b; }";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::RedefinedError(
                    _,
                    _,
                    VariableKind::Local
                )),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "int main() { int b = double(5); return b; } int double(int num) {int num = 3; return num*2;}";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::RedefinedError(
                    _,
                    _,
                    VariableKind::Local
                )),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}
