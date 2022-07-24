use insta::assert_debug_snapshot;

use ironcc::{
    analyze::{Analyzer, ConvProgram},
    error::CompileError,
    parse::{Parser, Program},
    tokenize::{Token, TokenStream, Tokenizer},
};

use std::clone::Clone;
use std::fmt::Debug;

macro_rules! all {
    ($self: ident) => {
        assert_debug_snapshot!($self.tokens());
        assert_debug_snapshot!($self.program().as_ref().unwrap());
        assert_debug_snapshot!($self.conv_program());
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

struct CachedProcesser<'a, I>
where
    I: Iterator<Item = Token> + std::fmt::Debug + std::clone::Clone,
{
    tokens: Vec<Token>,
    token_stream: TokenStream<'a, I>,
    parser: CachedParser<'a>,
    analyzer: CachedAnalyzer<'a>,
}

impl<'a> CachedProcesser<'a, std::vec::IntoIter<Token>> {
    fn new(src: &'a str) -> Self {
        let tokens = Tokenizer::new(src).tokenize();
        let stream = TokenStream::new(tokens.clone().into_iter(), src);
        Self {
            tokens: tokens,
            token_stream: stream,
            parser: CachedParser::new(src),
            analyzer: CachedAnalyzer::new(src),
        }
    }
}

impl<'a, I> CachedProcesser<'a, I>
where
    I: Iterator<Item = Token> + std::fmt::Debug + std::clone::Clone,
{
    fn program(&mut self) -> &Result<Program, CompileError> {
        self.parser.program(&mut self.token_stream)
    }

    fn tokens(&mut self) -> Vec<Token> {
        self.tokens.clone()
    }

    fn conv_program(&mut self) -> &ConvProgram {
        let program = {
            match &self.analyzer.program {
                Some(_) => None,
                None => Some(self.program().as_ref().unwrap().clone()),
            }
        };
        // TODO: Result
        self.analyzer.conv_program(program)
    }
}

struct CachedParser<'a> {
    parser: Parser<'a>,
    program: Option<Result<Program, CompileError>>,
}

impl<'a> CachedParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            parser: Parser::new(input),
            program: None,
        }
    }

    pub fn program<I>(&mut self, stream: &mut TokenStream<'a, I>) -> &Result<Program, CompileError>
    where
        I: Iterator<Item = Token> + Clone + Debug,
    {
        self.program
            .get_or_insert_with(|| self.parser.parse_program(stream))
    }
}

struct CachedAnalyzer<'a> {
    analyzer: Analyzer<'a>,
    program: Option<ConvProgram>,
}

impl<'a> CachedAnalyzer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            analyzer: Analyzer::new(input),
            program: None,
        }
    }

    pub fn conv_program(&mut self, program: Option<Program>) -> &ConvProgram {
        self.program.get_or_insert_with(|| {
            self.analyzer.down_program(
                program.expect("program should not be None if analyzer.program is None"),
            )
        })
    }
}
