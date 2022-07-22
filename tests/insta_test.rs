use insta::assert_debug_snapshot;

use ironcc::{
    analyze::{Analyzer, ConvProgram},
    parse::{Parser, Program},
    tokenize::{Token, TokenStream, Tokenizer},
};

use std::clone::Clone;
use std::fmt::Debug;

#[test]
fn insta_tests() {
    let src = "\nint main() {\nint i;\ni = 5;\nint* p; p = &i;\nint *p2; p2 = p + i;\n}";
    let mut tester = CachedProcesser::new(src);
    tester.test_all();
}

struct CachedProcesser<'a, I>
where
    I: Iterator<Item = Token> + std::fmt::Debug + std::clone::Clone,
{
    src: &'a str,
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
            src,
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
    fn test_all(&mut self) {
        assert_debug_snapshot!(format!("[tokens]{}", self.src), self.tokens());
        assert_debug_snapshot!(format!("[ast]{}", self.src), self.program());
        assert_debug_snapshot!(format!("[conv_ast]{}", self.src), self.conv_program());
    }

    fn program(&mut self) -> &Program {
        self.parser.program(&mut self.token_stream)
    }

    fn tokens(&mut self) -> Vec<Token> {
        self.tokens.clone()
    }

    fn conv_program(&mut self) -> &ConvProgram {
        let program = {
            match &self.analyzer.program {
                Some(_) => None,
                None => Some(self.program().clone()),
            }
        };
        self.analyzer.conv_program(program)
    }
}

struct CachedParser<'a> {
    parser: Parser<'a>,
    program: Option<Program>,
}

impl<'a> CachedParser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            parser: Parser::new(input),
            program: None,
        }
    }

    pub fn program<I>(&mut self, stream: &mut TokenStream<'a, I>) -> &Program
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
