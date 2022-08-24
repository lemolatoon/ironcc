use std::env;
use std::ffi::OsString;
use std::fs::File;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use std::rc::Rc;

use ironcc::analyze::Analyzer;
use ironcc::error::CompileError;
use ironcc::preprocess;
use ironcc::preprocess::Preprocessor;
use ironcc::preprocess::PreprocessorTokenStream;
use ironcc::tokenize::FileInfo;
use ironcc::tokenize::Token;
use ironcc::tokenize::TokenKind;
use ironcc::tokenize::TokenStream;
use ironcc::tokenize::Tokenizer;
use ironcc::{generate::Generator, parse::Parser};

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

fn preprocess_and_compile() -> Result<(), CompileError<TokenKind>> {
    let args: Vec<String> = env::args().collect();
    let (file_name, mut in_f, out_f) = get_io_file(args)?;
    let mut input = String::new();
    in_f.read_to_string(&mut input)
        .expect("This source is not valid UTF8");
    input.push('\n');

    let main_file_info = Rc::new(FileInfo::new(file_name, input));
    let tokens = preprocess(main_file_info, INCLUDE_DIR);
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
) -> Vec<Token<preprocess::TokenKind>> {
    let mut preprocessor = Preprocessor::new(include_dir);
    preprocessor.preprocess(main_file_info)
}

use std::fmt::Debug;
fn compile<I>(
    stream: PreprocessorTokenStream<I>,
    input: String,
    file_name: String,
    out_f: File,
) -> Result<(), CompileError<TokenKind>>
where
    I: Iterator<Item = Token<preprocess::TokenKind>> + Clone + Debug,
{
    let tokenizer = Tokenizer::new(stream);
    let file_info = Rc::new(FileInfo::new(file_name, input)); // TODO: remove this clone, by all input info around substituted with Rc
    let tokens = tokenizer.tokenize(&file_info)?;
    let mut token_stream = TokenStream::new(tokens.into_iter());

    let parser = Parser::new();
    let program = parser.parse_program(&mut token_stream)?;

    let mut analyzer = Analyzer::new();
    let converted_program = analyzer.traverse_program(program)?;

    let mut buf_writer = BufWriter::new(out_f);
    let mut generater = Generator::new();
    generater.gen_head(&mut buf_writer, converted_program)?;
    buf_writer.flush()?;

    Ok(())
}

fn get_io_file(
    args: Vec<String>,
) -> Result<(/* input file name */ String, File, File), std::io::Error> {
    if args.len() < 2 {
        panic!("The number of command-line args is invalid: {}", args.len());
    }
    let input_file_path = Path::new(&args[1]);
    let input_file = File::open(input_file_path)?;

    let mut buffer = OsString::with_capacity(input_file_path.as_os_str().len());
    buffer.push(
        input_file_path
            .file_stem()
            .expect("File name starts with `.` ."),
    );
    buffer.push(".s");
    let output_file_path = Path::new(buffer.as_os_str());
    let output_file = File::create(output_file_path)?;
    Ok((args[1].to_string(), input_file, output_file))
}
