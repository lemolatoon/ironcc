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
    assert_debug_snapshot!(src, tester.program());
    // assert_debug_snapshot!(src, tester.conv_program());
}

fn parse_program(src: &str) -> Program {
    let tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize();
    let parser = Parser::new(src);
    let mut tokens = TokenStream::new(tokens.into_iter(), src);
    parser.parse_program(&mut tokens)
}

struct CachedProcesser<'a, I>
where
    I: Iterator<Item = Token> + std::fmt::Debug + std::clone::Clone,
{
    src: &'a str,
    tokens: Option<Vec<Token>>,
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
            tokens: Some(tokens),
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
    fn program(&mut self) -> Program {
        self.parser.program(&mut self.token_stream)
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

    pub fn program<I>(&mut self, stream: &mut TokenStream<'a, I>) -> Program
    where
        I: Iterator<Item = Token> + Clone + Debug,
    {
        match &self.program {
            Some(program) => program.clone(),
            None => {
                let program = self.parser.parse_program(stream);
                self.program = Some(program);
                self.program.as_ref().unwrap().clone()
            }
        }
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
}
