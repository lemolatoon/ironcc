use std::env;
use std::ffi::OsString;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let (mut in_f, mut out_f) = get_io_file(args)?;
    let mut input = String::new();
    in_f.read_to_string(&mut input)
        .expect("This source is not valid UTF8");

    writeln!(&mut out_f, ".intel_syntax noprefix\n")?;
    writeln!(&mut out_f, ".global main")?;
    writeln!(&mut out_f, "main:")?;
    let return_code = input
        .trim_end_matches("\n")
        .parse::<usize>()
        .expect("Currently support only a number literal.");
    writeln!(&mut out_f, "  mov rax, {}", return_code)?;
    writeln!(&mut out_f, "  ret")?;

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
    return Ok((input_file, output_file));
}
