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

use error::CompileError;

pub fn preprocess_and_compile(mut input: String) -> Result<String, CompileError> {
    use crate::generate::Generator;
    use analyze::Analyzer;
    use parse::Parser;
    use preprocess::Preprocessor;
    use preprocess::PreprocessorTokenContainerStream;
    use preprocess::PreprocessorTokenStream;
    use preprocess::SrcCursor;
    use std::io::BufWriter;
    use std::rc::Rc;
    use tokenize::FileInfo;
    use tokenize::TokenStream;
    use tokenize::Tokenizer;
    input.push('\n');

    let file_name = "src.c".to_string();
    let main_file_info = Rc::new(FileInfo::new(file_name.clone(), input.clone()));
    let mut preprocessor = Preprocessor::new(main_file_info.clone(), "include");
    let tokens = preprocessor.preprocess(&mut SrcCursor::new(main_file_info), None)?;
    let stream = PreprocessorTokenStream::new(tokens.into_iter());

    let mut tokenizer = Tokenizer::new(PreprocessorTokenContainerStream::new(stream.collect()));
    let file_info = Rc::new(FileInfo::new(file_name.clone(), input)); // TODO: remove this clone, by all input info around substituted with Rc
    let tokens = tokenizer.tokenize(&file_info)?;
    let mut token_stream = TokenStream::new(tokens.into_iter());

    let mut parser = Parser::new();
    let program = parser.parse_program(&mut token_stream)?;

    let mut analyzer = Analyzer::new();
    let converted_program = analyzer.traverse_program(program)?;

    use std::io::Write;
    let mut buf_writer = BufWriter::new(Vec::new());
    let mut generater = Generator::new();
    generater.gen_head(&mut buf_writer, converted_program)?;
    buf_writer.flush()?;

    let asm = String::from_utf8(buf_writer.into_inner().unwrap()).unwrap();
    Ok(asm)
}
