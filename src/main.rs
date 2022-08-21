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
use ironcc::preprocess::Preprocessor;
use ironcc::tokenize::FileInfo;
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

fn preprocess_and_compile() -> Result<(), CompileError> {
    let args: Vec<String> = env::args().collect();
    let (file_name, mut in_f, out_f) = get_io_file(args)?;
    let mut input = String::new();
    in_f.read_to_string(&mut input)
        .expect("This source is not valid UTF8");
    input.push('\n');

    let input = preprocess(&input, INCLUDE_DIR);
    compile(input, file_name, out_f)
}

fn preprocess(input: &str, include_dir: &str) -> String {
    let mut preprocessor = Preprocessor::new(include_dir);
    preprocessor.preprocess(input)
}

fn compile(input: String, file_name: String, out_f: File) -> Result<(), CompileError> {
    let tokenizer = Tokenizer::new();
    let file_info = Rc::new(FileInfo::new(file_name, input.clone())); // TODO: remove this clone, by all input info around substituted with Rc
    let tokens = tokenizer.tokenize(&file_info)?;
    let mut token_stream = TokenStream::new(tokens.into_iter(), &input);

    let parser = Parser::new(&input);
    let program = parser.parse_program(&mut token_stream)?;

    let mut analyzer = Analyzer::new(&input);
    let converted_program = analyzer.traverse_program(program)?;

    let mut buf_writer = BufWriter::new(out_f);
    let mut generater = Generator::new(&input);
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
