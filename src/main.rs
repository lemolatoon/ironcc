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
    // println!("{:?}", tokenize(input));
    writeln!(&mut out_f, "  ret")?;

    Ok(())
}

fn tokenize(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut input_chars = input.chars().peekable();
    while let Some(c) = input_chars.next() {
        match c {
            // skip white spaces
            ' ' | '\n' | '\t' => continue,
            '+' => tokens.push(Token::new(TokenKind::BinOp(BinOpToken::Plus))),
            '-' => tokens.push(Token::new(TokenKind::BinOp(BinOpToken::Minus))),
            '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let mut number = String::from(c);
                while let Some(&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =
                    input_chars.peek()
                {
                    number.push(input_chars.next().unwrap())
                }
                let num = number
                    .parse::<usize>()
                    .expect("Currently support only a number literal.");
                tokens.push(Token::new(TokenKind::Num(num as isize)))
            }
            _ => panic!("Unexpected char while tokenize: {}", c),
        }
    }
    tokens.push(Token::new(TokenKind::Eof));

    tokens
}

#[derive(Debug)]
enum TokenKind {
    BinOp(BinOpToken),
    Num(isize),
    Eof,
}

#[derive(Debug)]
enum BinOpToken {
    Plus,
    Minus,
}

#[derive(Debug)]
struct Token {
    kind: Box<TokenKind>,
}

impl Token {
    fn new(token_kind: TokenKind) -> Self {
        Self {
            kind: Box::new(token_kind),
        }
    }
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
