use ironcc::analyze::analyze::Analyzer;
use ironcc::error::CompileError;
use ironcc::preprocess::preprocess::Preprocessor;
use ironcc::preprocess::preprocessor_streams::{
    PreprocessorTokenContainerStream, PreprocessorTokenStream,
};
use ironcc::preprocess::srccursor::SrcCursor;
use ironcc::preprocess::tokenkind::TokenKind as PreprocessTokenKind;
use ironcc::tokenize::debug_infos::FileInfo;
use ironcc::tokenize::tokenize::Token;
use ironcc::tokenize::tokenize::TokenStream;
use ironcc::tokenize::tokenize::Tokenizer;
use ironcc::{generate::generate::Generator, parse::parse::Parser};
use std::env;
use std::ffi::OsString;
use std::fs::File;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use std::rc::Rc;

fn main() {
    let result = preprocess_and_compile();
    match result {
        Ok(_) => {}
        Err(err) => {
            eprintln!("{}", err);
            exit(1);
        }
    }
}

const INCLUDE_DIR: &str = "include";

fn preprocess_and_compile() -> Result<(), CompileError> {
    let args: Vec<String> = env::args().collect();
    // let args = vec!["", "test/test.c"].into_iter().map(|arg| arg.to_string()).collect(); // for cargo profiler
    let (file_name, mut in_f, out_f) = get_io_file(args)?;
    let mut input = String::new();
    in_f.read_to_string(&mut input)
        .expect("This source is not valid UTF8");
    input.push('\n');

    let main_file_info = Rc::new(FileInfo::new(file_name, input));
    let tokens = preprocess(main_file_info, INCLUDE_DIR)?;
    eprintln!("preprocess finished!!");
    let stream = PreprocessorTokenStream::new(tokens.into_iter());
    compile(
        stream,
        String::from("This arg-passed input should not be used."),
        String::from("This arg-passed input should not be used.2"),
        out_f,
    )
}

fn preprocess(
    main_file_info: Rc<FileInfo>,
    include_dir: &str,
) -> Result<Vec<Token<PreprocessTokenKind>>, CompileError> {
    let mut preprocessor = Preprocessor::new(main_file_info.clone(), include_dir);
    preprocessor.preprocess(&mut SrcCursor::new(main_file_info), None)
}

use std::fmt::Debug;
fn compile<I>(
    stream: PreprocessorTokenStream<I>,
    input: String,
    file_name: String,
    out_f: File,
) -> Result<(), CompileError>
where
    I: Iterator<Item = Token<PreprocessTokenKind>> + Clone + Debug,
{
    let mut tokenizer = Tokenizer::new(PreprocessorTokenContainerStream::new(stream.collect()));
    let file_info = Rc::new(FileInfo::new(file_name, input)); // TODO: remove this clone, by all input info around substituted with Rc
    let tokens = tokenizer.tokenize(&file_info)?;
    eprintln!("tokenize finished!!");
    let mut token_stream = TokenStream::new(tokens.into_iter());

    let mut parser = Parser::new();
    let program = parser.parse_program(&mut token_stream)?;
    eprintln!("parse finished!!");

    let mut analyzer = Analyzer::new();
    let converted_program = analyzer.traverse_program(program)?;
    eprintln!("analyze finished!!");

    let mut buf_writer = BufWriter::new(out_f);
    let mut generater = Generator::new();
    generater.gen_head(&mut buf_writer, converted_program)?;
    buf_writer.flush()?;
    eprintln!("generate finished!!");

    Ok(())
}

fn get_io_file(
    args: Vec<String>,
) -> Result<(/* input file name */ String, File, File), std::io::Error> {
    if args.len() < 2 {
        panic!("The number of command-line args is invalid: {}", args.len());
    }
    let path_buf = PathBuf::from(&args[1]);
    let input_file_path = Path::new(&path_buf);
    assert!(input_file_path.is_file());
    assert!(input_file_path.is_file());
    assert!(input_file_path.exists());
    let input_file = File::open(input_file_path).unwrap();

    let mut buffer = OsString::with_capacity(input_file_path.as_os_str().len());
    buffer.push(
        input_file_path
            .file_stem()
            .expect("File name starts with `.` ."),
    );
    buffer.push(".s");
    let output_file_path = Path::new(buffer.as_os_str());
    let output_file = File::create(output_file_path).unwrap();
    Ok((args[1].to_string(), input_file, output_file))
}
