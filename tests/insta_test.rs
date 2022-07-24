use insta::assert_debug_snapshot;
pub mod test_utils;
use ironcc::error::CompileError;
use test_utils::CachedProcesser;

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
    let mut tester = CachedProcesser::new(src);
    all!(tester);
    Ok(())
}

#[test]
fn initializer() -> Result<(), CompileError> {
    let src = "int main() {int a = 5; int *p = &a; return 0;}";
    let mut tester = CachedProcesser::new(src);
    all!(tester);
    Ok(())
}

#[test]
fn array_syntax_sugar() -> Result<(), CompileError> {
    let src = "\n\
    int main() {\n \
        int a = 5;\n \
        int b = sizeof (a);\n \
        int c = sizeof (int);\n \
        int d = sizeof (int*);\n \
    }\n\
    ";
    let mut tester = CachedProcesser::new(src);
    all!(tester);
    Ok(())
}
