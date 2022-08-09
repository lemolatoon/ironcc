use insta::assert_debug_snapshot;
pub mod test_utils;
use ironcc::error::CompileError;
use test_utils::CachedProcessor;

macro_rules! all {
    ($self: ident) => {
        assert_debug_snapshot!($self.tokens().as_ref().unwrap());
        assert_debug_snapshot!($self.program().as_ref().unwrap());
        assert_debug_snapshot!($self.conv_program().as_ref().unwrap());
    };
}

#[test]
fn insta_tests() -> Result<(), CompileError> {
    let src = "\nint main() {\nint i;\ni = 5;\nint* p; p = &i;\nint *p2; p2 = p + i;\n}";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
    Ok(())
}

#[test]
fn initializer() -> Result<(), CompileError> {
    let src = "int main() {int a = 5; int *p = &a; return 0;}";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
    Ok(())
}

#[test]
fn size_of() -> Result<(), CompileError> {
    let src = "\n\
    int main() {\n \
        int a = 5;\n \
        int b = sizeof (a);\n \
        int c = sizeof (int);\n \
        int d = sizeof (int*);\n \
    }\n\
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
    Ok(())
}

#[test]
fn array_syntax_sugar() -> Result<(), CompileError> {
    let src = "\n\
    int main() {\n \
        int array[5];
        return array[0];
    }\n\
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
    Ok(())
}

#[test]
fn scopes() {
    let src = "\n\
    int main() {\n \
        int a;
        {int *k;}
        {int *i; {int k;}}
        int c[2];
    }\n\
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn implicit_cast() {
    let src = "
    int main() {
        int a = 2;
        char b = a;
        b = 4;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn add_char() {
    let src = "
    int main() {
        int a = 2;
        char b = a;
        a + b;
        b + a;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn string_literal() {
    let src = r#"
    int printf(char *msg);

    int main() {
        char *msg = "Hello World";
        printf(msg);
    }
    "#;
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn string_literal2() {
    let src = r#"
    int printf(char *msg);

    int main() {
        char *msg;
        msg = "Hello World";
        printf(msg);
    }
    "#;
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}
