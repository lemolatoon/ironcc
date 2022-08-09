extern crate ironcc;
pub mod test_utils;

use ironcc::error::CompileError;
use ironcc::parse::*;
use ironcc::tokenize::*;
use test_utils::ast::*;

#[test]
fn parse_test() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(2),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        Expr::new_binary(
            BinOpKind::Add,
            Expr::new_num(1, Position::default()),
            Expr::new_num(2, Position::default()),
            Position::default(),
        )
        .kind
    );

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(2),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Num(3),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        Expr::new_binary(
            BinOpKind::Add,
            Expr::new_num(1, Position::default()),
            Expr::new_binary(
                BinOpKind::Mul,
                Expr::new_num(2, Position::default()),
                Expr::new_num(3, Position::default()),
                Position::default()
            ),
            Position::default(),
        )
        .kind
    );

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(2),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Num(3),
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::Num(4),
        TokenKind::BinOp(BinOpToken::Slash),
        TokenKind::Num(5),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        bin(
            BinOpKind::Sub,
            bin(BinOpKind::Add, num(1), bin(BinOpKind::Mul, num(2), num(3))),
            bin(BinOpKind::Div, num(4), num(5))
        )
        .kind
    );

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(2),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Num(3),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        bin(BinOpKind::Mul, bin(BinOpKind::Add, num(1), num(2)), num(3)).kind
    );

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::Num(2),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Slash),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(31),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Num(4),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(5),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        bin(
            BinOpKind::Add,
            bin(
                BinOpKind::Div,
                bin(BinOpKind::Sub, num(1), num(2)),
                bin(BinOpKind::Mul, num(31), num(4))
            ),
            num(5)
        )
        .kind
    );

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::Num(2),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Slash),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(31),
        TokenKind::BinOp(BinOpToken::Percent),
        TokenKind::Num(4),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(5),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        bin(
            BinOpKind::Add,
            bin(
                BinOpKind::Div,
                bin(BinOpKind::Sub, num(1), num(2)),
                bin(BinOpKind::Rem, num(31), num(4))
            ),
            num(5)
        )
        .kind
    );

    Ok(())
}

#[test]
fn parse_unary_test() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::Num(2),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Slash),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(31),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Num(4),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(5),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        bin(
            BinOpKind::Add,
            bin(
                BinOpKind::Div,
                bin(BinOpKind::Sub, num(1), unary(UnaryOp::Minus, num(2))),
                bin(BinOpKind::Mul, num(31), num(4))
            ),
            num(5)
        )
        .kind
    );

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(1),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(expr.kind, unary(UnaryOp::Plus, num(1)).kind);

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Num(22),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        unary(UnaryOp::Minus, bin(BinOpKind::Mul, num(1), num(22))).kind
    );

    Ok(())
}

#[test]
fn parse_compare_op_test() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::EqEq),
        TokenKind::Num(2),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(3),
        TokenKind::BinOp(BinOpToken::Lt),
        TokenKind::Num(4),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        bin(
            BinOpKind::Eq,
            num(1),
            bin(BinOpKind::Lt, bin(BinOpKind::Add, num(2), num(3)), num(4))
        )
        .kind
    );
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Num(1),
        TokenKind::BinOp(BinOpToken::Ne),
        TokenKind::Num(2),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Num(3),
        TokenKind::BinOp(BinOpToken::Ge),
        TokenKind::Num(4),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(5),
        TokenKind::Eof
    );
    let expr = parser
        .parse_expr(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    assert_eq!(
        expr.kind,
        bin(
            BinOpKind::Ne,
            num(1),
            bin(
                BinOpKind::Ge,
                bin(BinOpKind::Mul, num(2), num(3)),
                bin(BinOpKind::Add, num(4), num(5))
            ),
        )
        .kind
    );

    Ok(())
}

#[test]
fn parse_stmt_test() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Num(1),
        TokenKind::Semi,
        TokenKind::Num(2),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", vec![]),
        block(vec![expr_stmt(num(1)), expr_stmt(num(2))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident("a".to_string()),
        TokenKind::Eq,
        TokenKind::Num(2),
        TokenKind::Semi,
        TokenKind::Return,
        TokenKind::Ident("a".to_string()),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![expr_stmt(assign(lvar("a"), num(2))), ret(lvar("a"))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    Ok(())
}

#[test]
fn parse_assign_expr_test() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident('a'.to_string()),
        TokenKind::Eq,
        TokenKind::Num(2),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![expr_stmt(assign(lvar("a"), num(2)))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident('a'.to_string()),
        TokenKind::Eq,
        TokenKind::Ident('b'.to_string()),
        TokenKind::Eq,
        TokenKind::Num(2),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![expr_stmt(assign(
            lvar("a"),
            assign(lvar("b"), num(2)),
        ))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident('a'.to_string()),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(1),
        TokenKind::Eq,
        TokenKind::Num(2),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![expr_stmt(assign(
            bin(BinOpKind::Add, lvar("a"), num(1)),
            num(2),
        ))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    Ok(())
}

#[test]
fn parse_various_stmts() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident('a'.to_string()),
        TokenKind::Eq,
        TokenKind::Num(22),
        TokenKind::Semi,
        TokenKind::If,
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Ident('a'.to_string()),
        TokenKind::BinOp(BinOpToken::Ge),
        TokenKind::Num(10),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::Return,
        TokenKind::Num(1),
        TokenKind::Semi,
        TokenKind::Else,
        TokenKind::Return,
        TokenKind::Num(0),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![
            expr_stmt(assign(lvar("a"), num(22))),
            if_(
                bin(BinOpKind::Ge, lvar("a"), num(10)),
                ret(num(1)),
                Some(ret(num(0))),
            ),
        ]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident('a'.to_string()),
        TokenKind::Eq,
        TokenKind::Num(22),
        TokenKind::Semi,
        TokenKind::While,
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Ident('a'.to_string()),
        TokenKind::BinOp(BinOpToken::Gt),
        TokenKind::Num(0),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::Ident('a'.to_string()),
        TokenKind::Eq,
        TokenKind::Ident('a'.to_string()),
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::Num(1),
        TokenKind::Semi,
        TokenKind::Return,
        TokenKind::Ident('a'.to_string()),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    ); // -> 1
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![
            expr_stmt(assign(lvar("a"), num(22))),
            while_(
                bin(BinOpKind::Gt, lvar("a"), num(0)),
                expr_stmt(assign(lvar("a"), bin(BinOpKind::Sub, lvar("a"), num(1)))),
            ),
            ret(lvar("a")),
        ]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident('x'.to_string()),
        TokenKind::Eq,
        TokenKind::Num(1),
        TokenKind::Semi,
        TokenKind::For,
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Ident("a".to_string()),
        TokenKind::Eq,
        TokenKind::Num(3),
        TokenKind::Semi,
        TokenKind::Semi,
        TokenKind::Ident("a".to_string()),
        TokenKind::Eq,
        TokenKind::Ident("a".to_string()),
        TokenKind::BinOp(BinOpToken::Minus),
        TokenKind::Num(2),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::If,
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(1),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::Ident("a".to_string()),
        TokenKind::Eq,
        TokenKind::Ident("a".to_string()),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(1),
        TokenKind::Semi,
        TokenKind::Else,
        TokenKind::Return,
        TokenKind::Num(200),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    ); // -> 1
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![
            expr_stmt(assign(lvar("x"), num(1))),
            for_(
                Some(assign(lvar("a"), num(3))),
                None,
                Some(assign(lvar("a"), bin(BinOpKind::Sub, lvar("a"), num(2)))),
                if_(
                    num(1),
                    expr_stmt(assign(lvar("a"), bin(BinOpKind::Add, lvar("a"), num(1)))),
                    Some(ret(num(200))),
                ),
            ),
        ]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);

    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident("a".to_string()),
        TokenKind::Eq,
        TokenKind::Num(13),
        TokenKind::Semi,
        TokenKind::Return,
        TokenKind::Ident("a".to_string()),
        TokenKind::BinOp(BinOpToken::Percent),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(3),
        TokenKind::BinOp(BinOpToken::Percent),
        TokenKind::Num(1),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    ); // -> 1
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![
            expr_stmt(assign(lvar("a"), num(13))),
            ret(bin(
                BinOpKind::Rem,
                lvar("a"),
                bin(BinOpKind::Rem, num(3), num(1)),
            )),
        ]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    Ok(())
}

#[test]
fn parse_call_func() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident("foo".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(3),
        TokenKind::Comma,
        TokenKind::Num(1),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![expr_stmt(func("foo", vec![num(3), num(1)]))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    Ok(())
}

#[test]
fn parse_ptr() -> Result<(), CompileError> {
    let input = String::new();
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Return,
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Ident("a".to_string()),
        TokenKind::Semi,
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![ret(deref(lvar("a")))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    let input = String::from("int main() {a = foo(3, 1); ap = &a; *a = a + 1;}");
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Ident("a".to_string()),
        TokenKind::Eq,
        TokenKind::Ident("foo".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::Num(3),
        TokenKind::Comma,
        TokenKind::Num(1),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::Semi, // a = foo(3, 1);
        TokenKind::Ident("ap".to_string()),
        TokenKind::Eq,
        TokenKind::BinOp(BinOpToken::And),
        TokenKind::Ident("a".to_string()),
        TokenKind::Semi, // ap = &a;
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Ident("a".to_string()),
        TokenKind::Eq,
        TokenKind::Ident("a".to_string()),
        TokenKind::BinOp(BinOpToken::Plus),
        TokenKind::Num(1),
        TokenKind::Semi, // *a = a + 1;
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    assert_eq!(
        tokenize_and_kinds(&input).unwrap(),
        tokens
            .clone()
            .into_iter()
            .map(|k| k.kind)
            .collect::<Vec<_>>()
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![
            expr_stmt(assign(lvar("a"), func("foo", vec![num(3), num(1)]))),
            expr_stmt(assign(lvar("ap"), addr(lvar("a")))),
            expr_stmt(assign(
                deref(lvar("a")),
                bin(BinOpKind::Add, lvar("a"), num(1)),
            )),
        ]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    Ok(())
}

#[test]
fn parse_declaration() -> Result<(), CompileError> {
    let input = String::from("int main() {int a;}");
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("a".to_string()),
        TokenKind::Semi, // int a;
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let tokenized = tokenize_and_kinds(&input).unwrap();
    assert_eq!(
        tokenized,
        tokens
            .clone()
            .into_iter()
            .map(|k| k.kind)
            .collect::<Vec<_>>()
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![declare_stmt(declare(
            TypeSpec::Int,
            0,
            DirectDeclarator::Ident("a".to_string()),
        ))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    let input = String::from("int  main() {int *a;}");
    let parser = Parser::new(&input);
    let tokens = tokens!(
        TokenKind::Type(TypeToken::Int),
        TokenKind::Ident("main".to_string()),
        TokenKind::OpenDelim(DelimToken::Paran),
        TokenKind::CloseDelim(DelimToken::Paran),
        TokenKind::OpenDelim(DelimToken::Brace),
        TokenKind::Type(TypeToken::Int),
        TokenKind::BinOp(BinOpToken::Star),
        TokenKind::Ident("a".to_string()),
        TokenKind::Semi, // int *a;
        TokenKind::CloseDelim(DelimToken::Brace),
        TokenKind::Eof
    );
    let tokenized = tokenize_and_kinds(&input).unwrap();
    assert_eq!(
        tokenized,
        tokens
            .clone()
            .into_iter()
            .map(|k| k.kind)
            .collect::<Vec<_>>()
    );
    let parsed = parser
        .parse_program(&mut TokenStream::new(tokens.into_iter(), &input))
        .unwrap();
    let expected = Program::with_vec(vec![func_def(
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
        block(vec![declare_stmt(declare(
            TypeSpec::Int,
            1,
            DirectDeclarator::Ident("a".to_string()),
        ))]),
        Position::default(),
    )]);

    assert_eq!(parsed, expected);
    Ok(())
}
