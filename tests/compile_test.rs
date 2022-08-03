pub mod test_utils;

use ironcc::{
    analyze::{BaseType, Type},
    error::{
        AnalyzeErrorKind, CompileError, CompileErrorKind, ParseErrorKind, TokenizeErrorKind,
        VariableKind,
    },
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
    assert!(
        matches!(
            tester.program(),
            Err(CompileError {
                kind: CompileErrorKind::ParseError(ParseErrorKind::UnexpectedEof(_)),
                src: _
            })
        ),
        "{:?}",
        tester.program()
    )
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

    let src = "int main() { int b = double(5); return b; } int double(int num) { return num*2;}";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::Func,
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

    let src = "int double(int arg0);int main() { int b = double(5); return b; } int double(int num) {int num = 3; return num*2;}";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::RedefinedError(
                    _,
                    _,
                    VariableKind::Local,
                )),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn type_error() {
    let src = "int main() { int *a; int *b; int *p = b + b; return 0; }";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(_, _, _,)),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "int main() { int a = 0; int *p = a; return 0; }";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(_, _, _,)),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "int *ret_ptr();int main() { int a = ret_ptr();  return 0; }";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(_, _, _,)),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src =
        "int *take_ptr(int *ptr);int main() {int not_ptr = 0; int a = take_ptr(not_ptr);  return 0; }";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(
                    _,
                    Type::Ptr(_),
                    Type::Base(BaseType::Int),
                )),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn func_arg_error() {
    let src = "int take_3_int(int a, int b, int c); int main() { int a = 2; take_3_int(a, 34); return 0; }";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::FuncArgsError(
                    _,
                    _,
                    3, // expected
                    2, // got
                    _,
                )),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn scopes() {
    let src = "
    int main() {
        {
            int i = 0;
        }
        return i;
    }
    ";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::Local,
                )),
                src: _,
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "
    int main() {
        int sum = 0;
        for (int i = 0; i < 10;i = i + 1) {
            sum = sum + i;
        }
        for (int i = 11; i < 20;i = i + 1) {
            sum = sum + i;
        }
        return sum;
    }
    ";
    let mut tester = CachedProcesser::new(src);
    assert!(
        matches!(tester.conv_program(), Ok(_)),
        "{:?}",
        tester.conv_program()
    );
}
