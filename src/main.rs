use std::env;
use std::ffi::OsString;
use std::fs::File;
use std::io::BufWriter;
use std::io::Read;
use std::io::Write;
use std::path::Path;

use ironcc::analyze::Analyzer;
use ironcc::tokenize::TokenStream;
use ironcc::tokenize::Tokenizer;
use ironcc::{generate::Generater, parse::Parser};

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let (mut in_f, out_f) = get_io_file(args)?;
    let mut input = String::new();
    in_f.read_to_string(&mut input)
        .expect("This source is not valid UTF8");

    let tokenizer = Tokenizer::new(&input);
    let tokens = tokenizer.tokenize();
    let mut token_stream = TokenStream::new(tokens.into_iter(), &input);
    let parser = Parser::new(&input);
    let expr = parser.parse_expr(&mut token_stream);
    let analyzer = Analyzer::new(&input);
    let expr = analyzer.down_expr(expr);
    let mut buf_writer = BufWriter::new(out_f);
    let generater = Generater::new(&input);
    writeln!(buf_writer, ".intel_syntax noprefix\n")?;
    writeln!(buf_writer, ".global main")?;
    writeln!(buf_writer, "main:")?;
    generater.gen_expr(&mut buf_writer, expr)?;
    writeln!(buf_writer, "  pop rax")?;
    writeln!(buf_writer, "  ret")?;
    buf_writer.flush()?;

    Ok(())
}

fn get_io_file(args: Vec<String>) -> Result<(File, File), std::io::Error> {
    if args.len() < 2 {
        panic!("The number of command-line args is invalid: {}", args.len());
    }
    let input_file_path = Path::new(&args[1]);
    let input_file = File::open(input_file_path)?;

    let mut buffer = OsString::with_capacity(input_file_path.as_os_str().len());
    buffer.push(
        input_file_path
            .file_stem()
            .expect("File name doesn't exsit"),
    );
    buffer.push(".s");
    let output_file_path = Path::new(buffer.as_os_str());
    let output_file = File::create(output_file_path)?;
    Ok((input_file, output_file))
}
