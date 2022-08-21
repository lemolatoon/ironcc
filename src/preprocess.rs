use crate::tokenize::DebugInfo;

pub struct Preprocessor<'b> {
    include_dir: &'b str,
}

impl<'b> Preprocessor<'b> {
    pub const fn new(include_dir: &'b str) -> Self {
        Self { include_dir }
    }

    pub fn preprocess(&self, input: &str) -> String {
        return input.to_string();
    }

    // #[allow(clippy::too_many_lines)]
    // pub fn preprocess(&self) -> Vec<Token> {
    //     let mut tokens = Vec::new();
    //     let mut pos = Position::default(); // 0, 0
    //     let mut input = <&str>::clone(&self.input);

    //         'tokenize_loop: while !input.is_empty() {
    //             // skip white spaces
    //             if input.starts_with(' ') || input.starts_with('\t') {
    //                 pos.advance(1);
    //                 input = &input[1..];
    //                 continue;
    //             } else if input.starts_with('\n') {
    //                 pos.advance_line();
    //                 input = &input[1..];
    //                 continue;
    //             } else if input.starts_with("//") {
    //                 let mut input_iter = input.chars();
    //                 let mut num_this_line_char = 1;
    //                 while !matches!(input_iter.next(), Some('\n')) {
    //                     num_this_line_char += 1;
    //                 }
    //                 pos.advance_line();
    //                 input = &input[num_this_line_char..];
    //                 continue;
    //             }
    //             if input.starts_with('#') {
    //                 let mut input_iter = input.chars();
    //                 let mut num_this_line_char = 1;
    //                 while !matches!(input_iter.next(), Some('\n')) {
    //                     num_this_line_char += 1;
    //                 }
    //                 pos.advance_line();
    //                 input = &input[num_this_line_char..];
    //                 continue;
    //             }

    //             let symbols = vec![
    //                 ("...", TokenKind::DotDotDot),
    //                 ("<<", TokenKind::BinOp(BinOpToken::LShift)),
    //                 (">>", TokenKind::BinOp(BinOpToken::RShift)),
    //                 ("<=", TokenKind::BinOp(BinOpToken::Le)),
    //                 (">=", TokenKind::BinOp(BinOpToken::Ge)),
    //                 ("==", TokenKind::BinOp(BinOpToken::EqEq)),
    //                 ("!=", TokenKind::BinOp(BinOpToken::Ne)),
    //                 ("||", TokenKind::BinOp(BinOpToken::VerticalVertical)),
    //                 ("&&", TokenKind::BinOp(BinOpToken::AndAnd)),
    //                 ("++", TokenKind::PlusPlus),
    //                 ("--", TokenKind::MinusMinus),
    //                 ("->", TokenKind::Arrow),
    //                 ("+", TokenKind::BinOp(BinOpToken::Plus)),
    //                 ("-", TokenKind::BinOp(BinOpToken::Minus)),
    //                 ("*", TokenKind::BinOp(BinOpToken::Star)),
    //                 ("/", TokenKind::BinOp(BinOpToken::Slash)),
    //                 ("&", TokenKind::BinOp(BinOpToken::And)),
    //                 ("%", TokenKind::BinOp(BinOpToken::Percent)),
    //                 ("(", TokenKind::OpenDelim(DelimToken::Paren)),
    //                 ("{", TokenKind::OpenDelim(DelimToken::Brace)),
    //                 ("[", TokenKind::OpenDelim(DelimToken::Bracket)),
    //                 (")", TokenKind::CloseDelim(DelimToken::Paren)),
    //                 ("}", TokenKind::CloseDelim(DelimToken::Brace)),
    //                 ("]", TokenKind::CloseDelim(DelimToken::Bracket)),
    //                 (",", TokenKind::Comma),
    //                 ("<", TokenKind::BinOp(BinOpToken::Lt)),
    //                 (">", TokenKind::BinOp(BinOpToken::Gt)),
    //                 ("~", TokenKind::Tilde),
    //                 ("!", TokenKind::Exclamation),
    //                 ("?", TokenKind::Question),
    //                 (":", TokenKind::Colon),
    //                 (";", TokenKind::Semi),
    //                 ("=", TokenKind::Eq),
    //                 (".", TokenKind::Dot),
    //             ];

    //             for (literal, kind) in symbols {
    //                 if input.starts_with(literal) {
    //                     tokens.push(Token::new(kind, pos.get_pos_and_advance(literal.len())));
    //                     input = &input[literal.len()..];
    //                     continue 'tokenize_loop;
    //                 }
    //             }

    //             if input.starts_with('"') {
    //                 // string literal
    //                 let mut chars = input.chars().peekable();
    //                 chars.next(); // -> "
    //                 let mut str_lit = String::new();
    //                 let mut len_token = 1;
    //                 loop {
    //                     match chars.peek() {
    //                         Some('"') => {
    //                             len_token += 1;
    //                             chars.next();
    //                             break;
    //                         }
    //                         Some('\\') => {
    //                             len_token += 1;
    //                             chars.next();
    //                             match chars.next() {
    //                                 Some('n') => {
    //                                     len_token += 1;
    //                                     str_lit.push('\n');
    //                                 }
    //                                 Some('e') => {
    //                                     len_token += 1;
    //                                     str_lit.push(0x1bu8.into());
    //                                 }
    //                                 _ => {
    //                                     pos.advance(len_token);
    //                                     return Err(unimplemented_err!(
    //                                         self.input,
    //                                         pos,
    //                                         "This type of escape Sequences are not currently implemented."
    //                                     ));
    //                                 }
    //                             }
    //                         }
    //                         Some(c) => {
    //                             len_token += 1;
    //                             str_lit.push(*c);
    //                             chars.next();
    //                         }
    //                         None => {
    //                             return Err(CompileError::new_unexpected_eof_tokenize(self.input, pos))
    //                         }
    //                     }
    //                 }
    //                 tokens.push(Token::new(
    //                     TokenKind::Str(str_lit),
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             }

    //             if input.starts_with("0b") {
    //                 input = &input[2..];
    //                 let mut chars = input.chars().peekable();
    //                 let mut number = String::new();
    //                 while let Some(&('0' | '1')) = chars.peek() {
    //                     number.push(chars.next().unwrap());
    //                 }
    //                 let len_token = number.len();
    //                 let mut num = 0;
    //                 for c in number.chars() {
    //                     num *= 2;
    //                     match c {
    //                         '0' => {}
    //                         '1' => {
    //                             num += 1;
    //                         }
    //                         _ => {
    //                             return Err(CompileError::new_expected_failed(
    //                                 self.input,
    //                                 Box::new("'0' | '1'"),
    //                                 Token {
    //                                     kind: Box::new(TokenKind::Ident(c.to_string())),
    //                                     pos,
    //                                 },
    //                             ));
    //                         }
    //                     }
    //                 }
    //                 tokens.push(Token::new(
    //                     TokenKind::Num(num as isize),
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             }

    //             if input.starts_with(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) {
    //                 let mut chars = input.chars().peekable();
    //                 let mut number = String::from(chars.next().unwrap());
    //                 while let Some(&('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =
    //                     chars.peek()
    //                 {
    //                     number.push(chars.next().unwrap());
    //                 }
    //                 let len_token = number.len();
    //                 let num = number
    //                     .parse::<usize>()
    //                     .expect("Currently support only a number literal.");
    //                 tokens.push(Token::new(
    //                     TokenKind::Num(num as isize),
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             } else if input.starts_with(
    //                 &('a'..='z')
    //                     .chain('A'..='Z')
    //                     .chain(vec!['_'].into_iter())
    //                     .collect::<Vec<_>>()[..],
    //             ) {
    //                 // Ident or reserved token
    //                 let mut chars = input.chars().peekable();
    //                 let mut ident = String::from(chars.next().unwrap());
    //                 while let Some(
    //                     &('a'..='z')
    //                     | &('A'..='Z')
    //                     | '_'
    //                     | '0'
    //                     | '1'
    //                     | '2'
    //                     | '3'
    //                     | '4'
    //                     | '5'
    //                     | '6'
    //                     | '7'
    //                     | '8'
    //                     | '9',
    //                 ) = chars.peek()
    //                 {
    //                     ident.push(chars.next().unwrap());
    //                 }
    //                 let len_token = ident.len();
    //                 tokens.push(Token::new(
    //                     match ident.as_str() {
    //                         "return" => TokenKind::Return,
    //                         "if" => TokenKind::If,
    //                         "else" => TokenKind::Else,
    //                         "while" => TokenKind::While,
    //                         "for" => TokenKind::For,
    //                         "int" => TokenKind::Type(TypeToken::Int),
    //                         "char" => TokenKind::Type(TypeToken::Char),
    //                         "void" => TokenKind::Type(TypeToken::Void),
    //                         "sizeof" => TokenKind::SizeOf,
    //                         "struct" => TokenKind::Struct,
    //                         _ => TokenKind::Ident(ident),
    //                     },
    //                     pos.get_pos_and_advance(len_token),
    //                 ));
    //                 input = &input[len_token..];
    //                 continue;
    //             }
    //             return Err(self.new_unexpected_char(pos, input.chars().next().unwrap()));
    //         }
    //         tokens.push(Token::new(TokenKind::Eof, pos.get_pos_and_advance(0)));

    //         Ok(tokens)
    //     }
}

pub enum TokenKind {
    Eof,
    Define,
    HashTag,
    Ident(String),
}
