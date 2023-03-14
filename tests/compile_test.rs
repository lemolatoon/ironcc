pub mod test_utils;

use ironcc::{
    analyze::types::{BaseType, Type},
    error::{
        AnalyzeErrorKind, CompileError, CompileErrorKind, ParseErrorKind, TokenizeErrorKind,
        TypeExpectedFailedKind, VariableKind,
    },
};
use test_utils::CachedProcessor;

#[test]
fn unexpected_char() {
    let src = "int main() {int あいうえお = 5; return 0;}";
    let mut tester = CachedProcessor::new(src);
    assert!(matches!(
        tester.tokens(),
        Err(CompileError {
            kind: CompileErrorKind::TokenizeError(TokenizeErrorKind::UnexpectedChar(_, _)),
        })
    ));
}

#[test]
fn unexpected_eof() {
    let src = "int main() { int k = 4; return 0; ";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.program(),
            Err(CompileError {
                kind: CompileErrorKind::ParseError(ParseErrorKind::UnexpectedEof(..)),
            })
        ),
        "{:?}",
        tester.program()
    )
}

#[test]
fn expected_failed() {
    let src = "int main() { int k = 4 return k; }";
    let mut tester = CachedProcessor::new(src);
    assert!(matches!(
        tester.program(),
        Err(CompileError {
            kind: CompileErrorKind::ParseError(ParseErrorKind::ExpectFailed { expect: _, got: _ }),
        })
    ));
}

#[test]
fn variable_undeclared() {
    let src = "int main() { return a; }";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::LocalOrGlobalOrFunc
                )),
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "int main() { int b = double(5); return b; } int double(int num) { return num*2;}";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::Func,
                )),
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn variable_redefined() {
    let src = "int main() { int b; int b; return b; }";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::RedefinedError(
                    _,
                    _,
                    VariableKind::Local
                )),
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "int double(int arg0);int main() { int b = double(5); return b; } int double(int num) {int num = 3; return num*2;}";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::RedefinedError(
                    _,
                    _,
                    VariableKind::Local,
                )),
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn type_error() {
    let src = "int main() { int *a; int *b; int *p = b + b; return 0; }";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(_, _,)),
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "int main() { int a = 0; int *p = a; return 0; }";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(_, _,)),
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "int *ret_ptr();int main() { int a = ret_ptr();  return 0; }";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(_, _,)),
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src =
        "int *take_ptr(int *ptr);int main() {int not_ptr = 0; int a = take_ptr(not_ptr);  return 0; }";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(
                    TypeExpectedFailedKind::Type {
                        pos: _,
                        expected: Type::Ptr(_),
                        got: Type::Base(BaseType::Int),
                    }
                )),
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "char char_global = 1;int main() {int *int_ptr = &char_global;  return 0; }";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeError(_, _)),
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn func_arg_error() {
    let src = "int take_3_int(int a, int b, int c); int main() { int a = 2; take_3_int(a, 34); return 0; }";
    let mut tester = CachedProcessor::new(src);
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
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::LocalOrGlobalOrFunc,
                )),
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
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(tester.conv_program(), Ok(_)),
        "{:?}",
        tester.conv_program()
    );

    let src = "
    int main() {
        {
            enum A {
                A1,
                A2
            };
            return A1;
        }
        return 0;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(tester.conv_program(), Ok(_)),
        "{:?}",
        tester.conv_program()
    );

    let src = "
    int main() {
        {
            enum A {
                A1,
                A2
            };
        }
        return A2;
    }
    ";

    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::LocalOrGlobalOrFunc,
                )),
            })
        ),
        "{:?}",
        tester.conv_program()
    );

    let src = "
    void f(void) {
        static int a;
    }
    int main(void) {
        f();
        return a;
    }
    ";

    let mut tester = CachedProcessor::new(src);
    assert!(
        matches!(
            tester.conv_program(),
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                    _,
                    _,
                    VariableKind::LocalOrGlobalOrFunc,
                )),
            })
        ),
        "{:?}",
        tester.conv_program()
    );
}

#[test]
fn global_variable() {
    let src = "
    int global1;
    int main() {
        int global1;
    }";
    let mut tester = CachedProcessor::new(src);
    let result = tester.conv_program();
    // This is ok
    assert!(matches!(result, Ok(_),), "{:?}", result,);

    let src = "
    int global1;
    int (*global1)[3];
    int main() {
    }";
    let mut tester = CachedProcessor::new(src);
    let result = tester.conv_program();
    assert!(
        matches!(
            result,
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::RedefinedError(
                    _,
                    _,
                    VariableKind::Global,
                )),
            })
        ),
        "{:?}",
        result,
    );

    let src = "
    int main() {
        int (*global1)[3];
    }
    int global1;
    ";
    let mut tester = CachedProcessor::new(src);
    let result = tester.conv_program();
    assert!(matches!(result, Ok(_)), "{:?}", result,);
}

#[test]
fn flexible_args() {
    let src = "
    void printf(char *msg, ...);
    int main() {
        int a;
        printf(a);
        return 0;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    let result = tester.conv_program();
    assert!(
        matches!(
            result,
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::TypeExpectFailed(_)),
            })
        ),
        "{:?}",
        result,
    );
    let src = r#"
    void printf(char *msg, ...);
    int main() {
        char a[3] = {1, 2, 3};
        printf(a, 1, 2, 3, 3, 4 , "asldkfjl");
        return 0;
    }
    "#;
    let mut tester = CachedProcessor::new(src);
    let result = tester.conv_program();
    assert!(matches!(result, Ok(_),), "{:?}", result,);
    let src = r#"
    int f(void);
    int main() {
        return f(3);
    }
    "#;
    let mut tester = CachedProcessor::new(src);
    let result = tester.conv_program();
    assert!(
        matches!(
            result,
            Err(CompileError {
                kind: CompileErrorKind::AnalyzeError(AnalyzeErrorKind::FuncArgsError(
                    _,
                    _,
                    _,
                    _,
                    _
                )),
            }),
        ),
        "{:?}",
        result,
    );

    let src = r#"
    int f();
    int main() {
        return f(3);
    }
    "#;
    let mut tester = CachedProcessor::new(src);
    let result = tester.conv_program();
    assert!(matches!(result, Ok(_)), "{:?}", result,);
}
