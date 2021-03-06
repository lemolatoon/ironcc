extern crate ironcc;
pub mod test_utils;
use test_utils::kind_eq;

use ironcc::tokenize::*;

#[test]
fn tokenize_plus_minus_test() {
    // use crate::tokenize::{kind_eq, tokenize_and_kinds, BinOpToken, Token, TokenKind, Tokenizer};

    let input = String::from("0 + 0 + 11  -4");
    let tokenizer = Tokenizer::new(&input);
    assert_eq!(
        tokenizer
            .tokenize()
            .into_iter()
            .map(|token| token.kind())
            .collect::<Vec<_>>(),
        tokens!(
            TokenKind::Num(0),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(0),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(11),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(4),
            TokenKind::Eof
        )
        .into_iter()
        .map(|token| token.kind())
        .collect::<Vec<_>>()
    );

    let input = String::from("10101010101 + 0 + 11  -4");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Num(10101010101),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(0),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(11),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(4),
            TokenKind::Eof
        )
    );
    let input = String::from("(1 +1) -2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Num(1),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(1),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
    let input = String::from("1 + 4 -       909");
    let tokenizer = Tokenizer::new(&input);
    assert!(kind_eq(
        &tokenizer.tokenize(),
        &tokens!(
            TokenKind::Num(1),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(4),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(909),
            TokenKind::Eof
        )
    ));
    let input = String::from("0\t + 5+1+9-3 -  \n     909");
    let tokenizer = Tokenizer::new(&input);
    assert!(kind_eq(
        &tokenizer.tokenize(),
        &tokens!(
            TokenKind::Num(0),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(5),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(1),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(9),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(3),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(909),
            TokenKind::Eof
        )
    ));
}

#[test]
fn tokenize_pos_test() {
    let input = String::from("1 +1");
    let tokenizer = Tokenizer::new(&input);
    assert_eq!(
        tokenizer.tokenize(),
        token_poses!(
            (TokenKind::Num(1), Position::new(0, 0)),
            (TokenKind::BinOp(BinOpToken::Plus), Position::new(2, 0)),
            (TokenKind::Num(1), Position::new(3, 0)),
            (TokenKind::Eof, Position::new(4, 0))
        )
    );

    let input = String::from("1 +1\n\t+5");
    let tokenizer = Tokenizer::new(&input);
    assert_eq!(
        tokenizer.tokenize(),
        token_poses!(
            (TokenKind::Num(1), Position::new(0, 0)),
            (TokenKind::BinOp(BinOpToken::Plus), Position::new(2, 0)),
            (TokenKind::Num(1), Position::new(3, 0)),
            (TokenKind::BinOp(BinOpToken::Plus), Position::new(1, 1)),
            (TokenKind::Num(5), Position::new(2, 1)),
            (TokenKind::Eof, Position::new(3, 1))
        )
    );
}

#[test]
fn tokenize_compare_op_test() {
    let input = String::from("1 == 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Num(1),
            TokenKind::EqEq,
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
    let input = String::from("1 < 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Num(1),
            TokenKind::Lt,
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
    let input = String::from("1 > 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Num(1),
            TokenKind::Gt,
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
    let input = String::from("1 <= 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Num(1),
            TokenKind::Le,
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
    let input = String::from("1 >= 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Num(1),
            TokenKind::Ge,
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
    let input = String::from("1 != 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Num(1),
            TokenKind::Ne,
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
}

#[test]
fn tokenize_tokens_test() {
    let input = String::from("a = 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("a".to_string()),
            TokenKind::Eq,
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );

    let input = String::from("a % 2");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("a".to_string()),
            TokenKind::BinOp(BinOpToken::Percent),
            TokenKind::Num(2),
            TokenKind::Eof
        )
    );
}

#[test]
fn tokenize_ident_test() {
    let input = String::from("a");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(TokenKind::Ident("a".to_string()), TokenKind::Eof)
    );

    let input = String::from("q * z");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("q".to_string()),
            TokenKind::BinOp(BinOpToken::Star),
            TokenKind::Ident("z".to_string()),
            TokenKind::Eof
        )
    );

    let input = String::from("abc = cdf = 8*7");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("abc".to_string()),
            TokenKind::Eq,
            TokenKind::Ident("cdf".to_string()),
            TokenKind::Eq,
            TokenKind::Num(8),
            TokenKind::BinOp(BinOpToken::Star),
            TokenKind::Num(7),
            TokenKind::Eof
        )
    );
}

#[test]
fn tokenize_stmt_test() {
    let input = String::from("a;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("a".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );

    let input = String::from("q * z;\nc +c;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("q".to_string()),
            TokenKind::BinOp(BinOpToken::Star),
            TokenKind::Ident("z".to_string()),
            TokenKind::Semi,
            TokenKind::Ident("c".to_string()),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Ident("c".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );
}

#[test]
fn tokenize_reserved_test() {
    let input = String::from("returnx = 1;\nreturn returnx;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("returnx".to_string()),
            TokenKind::Eq,
            TokenKind::Num(1),
            TokenKind::Semi,
            TokenKind::Return,
            TokenKind::Ident("returnx".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );

    let input = String::from("x = 2;ifx = 1; if (x == 1) ifx = 3;\nreturn ifx;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::Ident("ifx".to_string()),
            TokenKind::Eq,
            TokenKind::Num(1),
            TokenKind::Semi,
            TokenKind::If,
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::EqEq,
            TokenKind::Num(1),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::Ident("ifx".to_string()),
            TokenKind::Eq,
            TokenKind::Num(3),
            TokenKind::Semi,
            TokenKind::Return,
            TokenKind::Ident("ifx".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );

    let input = String::from("x = 2; while(x>0) x = x -1; return x;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::While,
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::Gt,
            TokenKind::Num(0),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Ident("x".to_string()),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(1),
            TokenKind::Semi,
            TokenKind::Return,
            TokenKind::Ident("x".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );

    let input = String::from("x = 2; if(x>0) x = x -1; else x = 5; return x;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::If,
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::Gt,
            TokenKind::Num(0),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Ident("x".to_string()),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(1),
            TokenKind::Semi,
            TokenKind::Else,
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Num(5),
            TokenKind::Semi,
            TokenKind::Return,
            TokenKind::Ident("x".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );

    let input = String::from("for (x = 1; x <= 11; x = x + 1) x = x + 2; return x;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::For,
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Num(1),
            TokenKind::Semi,
            TokenKind::Ident("x".to_string()),
            TokenKind::Le,
            TokenKind::Num(11),
            TokenKind::Semi,
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Ident("x".to_string()),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(1),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Ident("x".to_string()),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::Return,
            TokenKind::Ident("x".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );
    let input = String::from("for (x = 1; x <= 11; x = x + 1) {x = x + 2;} return x;");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::For,
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Num(1),
            TokenKind::Semi,
            TokenKind::Ident("x".to_string()),
            TokenKind::Le,
            TokenKind::Num(11),
            TokenKind::Semi,
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Ident("x".to_string()),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(1),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::OpenDelim(DelimToken::Brace),
            TokenKind::Ident("x".to_string()),
            TokenKind::Eq,
            TokenKind::Ident("x".to_string()),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::CloseDelim(DelimToken::Brace),
            TokenKind::Return,
            TokenKind::Ident("x".to_string()),
            TokenKind::Semi,
            TokenKind::Eof
        )
    );
}

#[test]
fn tokenize_call_func_test() {
    let input = String::from("{return foo(1, 2);}");
    assert_eq!(
        tokenize_and_kinds(&input),
        token_kinds!(
            TokenKind::OpenDelim(DelimToken::Brace),
            TokenKind::Return,
            TokenKind::Ident("foo".to_string()),
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Num(1),
            TokenKind::Comma,
            TokenKind::Num(2),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::Semi,
            TokenKind::CloseDelim(DelimToken::Brace),
            TokenKind::Eof
        )
    );
}
