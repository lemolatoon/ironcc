use std::{fs::File, io::Read, path::Path};

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

macro_rules! gen_test {
    ($file_name: ident) => {
        #[test]
        fn $file_name() {
            let path = format!("tests/insta_srcs/{}.c", stringify!($file_name));
            eprintln!("{}", path);
            let src = read_file(path).unwrap();

            let mut tester = CachedProcessor::new(&src);
            all!(tester);
        }
    };
}

fn read_file(path: String) -> Result<String, std::io::Error> {
    let input_file_path = Path::new(&path);
    let mut input_file = File::open(input_file_path)?;

    let mut input = String::new();
    input_file
        .read_to_string(&mut input)
        .expect("This source is not valid UTF8");
    Ok(input)
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

    let src = "\n\
    int main() {\n \
        int array[5];
        return array[3];
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

#[test]
fn tilde() {
    let src = "
    int main() {
        char a = 4; // -> -5
        return ~a;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn exclamation() {
    let src = "
    int main() {
        int a = 1;
        return !a;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn binary_literal() {
    let src = "
    int main() {
        int a = 0b1111;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn bit_wise_and() {
    let src = "
    int main() {
        return 0b111 & 0b101;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn struct_declaration() {
    let src = include_str!("insta_srcs/struct_declaration.c");
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn void_type() {
    let src = "int main() {
        void *p;
        int ****a;
        p = &a;
    }";

    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn conditional_expr() {
    let src = "int main() {
        int a = 1;
        int b = 2;
        int c = a > b ? a : b;
    }";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn logical_or_and() {
    let src = "int main() {
        int a = 1;
        int b = 0;
        int c = a && b && 2; // -> 0
        int d = a || b || 1; // -> 1 
    }";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn flexible_args() {
    let src = r#"
    void printf(char *msg, ...);
    
    int main() {
        int a = 2;
        printf("Hello World:%d\n", a);
    }
    "#;

    let mut tester = CachedProcessor::new(src);
    all!(tester);

    let src = r#"
    void printf(char *msg, ...);
    void printf2(char *msg, ...) {
        printf(msg);
    }
    "#;

    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn postfix_inc_dec() {
    let src = "
    int main() {
        int a = 1;
        a++;
        a--;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

#[test]
fn unary_inc_dec() {
    let src = "
    int main() {
        int a = 1;
        ++a;
        --a;
    }
    ";
    let mut tester = CachedProcessor::new(src);
    all!(tester);
}

gen_test!(struct_declaration2);
gen_test!(struct_member_access);
gen_test!(struct_incomplete_member);
