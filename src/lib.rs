#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::missing_errors_doc, clippy::must_use_candidate)]
#![allow(
    clippy::cast_possible_wrap,
    clippy::missing_panics_doc,
    clippy::use_self,
    clippy::module_name_repetitions,
    clippy::vec_init_then_push,
    clippy::large_enum_variant
)]

pub mod analyze;
pub mod common;
pub mod error;
pub mod generate;
pub mod parse;
pub mod preprocess;
pub mod tokenize;

use crate::generate::generate::Generator;
use analyze::analyze::Analyzer;
use analyze::analyze::ConvProgram;
use error::CompileError;
use parse::parse::Parser;
use parse::parse::Program;
use preprocess::preprocess::Preprocessor;
use preprocess::preprocess::PreprocessorTokenContainerStream;
use preprocess::preprocess::PreprocessorTokenStream;
use preprocess::preprocess::SrcCursor;
use std::io::BufWriter;
use std::rc::Rc;
use tokenize::tokenize::FileInfo;
use tokenize::tokenize::Token as TokenizeToken;
use tokenize::tokenize::TokenKind as TokenizeTokenKind;
use tokenize::tokenize::TokenStream;
use tokenize::tokenize::Tokenizer;

pub fn preprocess_and_compile(mut input: String) -> Result<String, CompileError> {
    use std::io::Write;
    input.push('\n');

    let file_name = "src.c".to_string();
    let main_file_info = Rc::new(FileInfo::new(file_name.clone(), input.clone()));
    let mut preprocessor = Preprocessor::new(main_file_info.clone(), "include");
    let tokens = preprocessor.preprocess(&mut SrcCursor::new(main_file_info), None)?;
    let stream = PreprocessorTokenStream::new(tokens.into_iter());
    let container_stream = PreprocessorTokenContainerStream::new(stream.collect());

    let mut tokenizer = Tokenizer::new(container_stream);
    let file_info = Rc::new(FileInfo::new(file_name, input));
    let tokens = tokenizer.tokenize(&file_info)?;
    let mut token_stream = TokenStream::new(tokens.into_iter());

    let mut parser = Parser::new();
    let program = parser.parse_program(&mut token_stream)?;

    let mut analyzer = Analyzer::new();
    let converted_program = analyzer.traverse_program(program)?;

    let mut buf_writer = BufWriter::new(Vec::new());
    let mut generater = Generator::new();
    generater.gen_head(&mut buf_writer, converted_program)?;
    buf_writer.flush()?;

    let asm = String::from_utf8(buf_writer.into_inner().unwrap()).unwrap();
    Ok(asm)
}

pub fn preprocessed_source(mut input: String) -> Result<String, CompileError> {
    input.push('\n');

    let file_name = "src.c".to_string();
    let main_file_info = Rc::new(FileInfo::new(file_name, input.clone()));
    let mut preprocessor = Preprocessor::new(main_file_info.clone(), "include");
    let tokens = preprocessor.preprocess(&mut SrcCursor::new(main_file_info), None)?;
    let stream = PreprocessorTokenStream::new(tokens.into_iter());
    let container_stream = PreprocessorTokenContainerStream::new(stream.collect());

    Ok(container_stream.map(|(_, ch)| ch).collect())
}

pub fn tokens(mut input: String) -> Result<Vec<TokenizeToken<TokenizeTokenKind>>, CompileError> {
    input.push('\n');

    let file_name = "src.c".to_string();
    let main_file_info = Rc::new(FileInfo::new(file_name.clone(), input.clone()));
    let mut preprocessor = Preprocessor::new(main_file_info.clone(), "include");
    let tokens = preprocessor.preprocess(&mut SrcCursor::new(main_file_info), None)?;
    let stream = PreprocessorTokenStream::new(tokens.into_iter());
    let container_stream = PreprocessorTokenContainerStream::new(stream.collect());

    let mut tokenizer = Tokenizer::new(container_stream);
    let file_info = Rc::new(FileInfo::new(file_name, input));
    tokenizer.tokenize(&file_info)
}

pub fn parsed_ast(input: String) -> Result<Program, CompileError> {
    let tokens = tokens(input)?;
    let mut token_stream = TokenStream::new(tokens.into_iter());

    let mut parser = Parser::new();
    let program = parser.parse_program(&mut token_stream)?;
    Ok(program)
}

pub fn converted_ast(input: String) -> Result<ConvProgram, CompileError> {
    let ast = parsed_ast(input)?;
    let mut analyzer = Analyzer::new();
    analyzer.traverse_program(ast)
}
