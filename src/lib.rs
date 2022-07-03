pub mod analyze;
pub mod generate;
pub mod parse;
pub mod tokenize;

#[cfg(test)]
mod tests {

    use pretty_assertions::assert_eq;
    use std::collections::BTreeMap;

    use crate::{
        analyze::{
            Analyzer, ConvBinOpKind, ConvExpr, ConvFuncDef, ConvProgram, ConvProgramKind, ConvStmt,
            Lvar,
        },
        parse::{BinOpKind, Expr, FuncDef, Parser, Program, ProgramKind, Stmt, UnOp},
        tokenize::{
            tokenize_and_kinds, BinOpToken, DelimToken, Position, Token, TokenKind, TokenStream,
        },
    };

    use super::*;

    #[test]
    fn tokenize_plus_minus_test() {
        use crate::tokenize::{
            kind_eq, tokenize_and_kinds, BinOpToken, Token, TokenKind, Tokenizer,
        };

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
        use crate::tokenize::{BinOpToken, Position, Token, TokenKind, Tokenizer};
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
                TokenKind::BinOp(BinOpToken::Mul),
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
                TokenKind::BinOp(BinOpToken::Mul),
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
                TokenKind::BinOp(BinOpToken::Mul),
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
    #[test]
    fn parse_test() {
        use crate::{
            parse::{BinOpKind, Expr, Parser},
            tokenize::{BinOpToken, Position, Token, TokenKind},
        };
        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
            TokenKind::Num(1),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(2),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
            TokenKind::BinOp(BinOpToken::Mul),
            TokenKind::Num(3),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
            TokenKind::BinOp(BinOpToken::Mul),
            TokenKind::Num(3),
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::Num(4),
            TokenKind::BinOp(BinOpToken::Div),
            TokenKind::Num(5),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
            TokenKind::BinOp(BinOpToken::Mul),
            TokenKind::Num(3),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
            TokenKind::BinOp(BinOpToken::Div),
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Num(31),
            TokenKind::BinOp(BinOpToken::Mul),
            TokenKind::Num(4),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(5),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
            TokenKind::BinOp(BinOpToken::Div),
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Num(31),
            TokenKind::BinOp(BinOpToken::Percent),
            TokenKind::Num(4),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(5),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
    }

    #[test]
    fn parse_unary_test() {
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
            TokenKind::BinOp(BinOpToken::Div),
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Num(31),
            TokenKind::BinOp(BinOpToken::Mul),
            TokenKind::Num(4),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(5),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
        assert_eq!(
            expr.kind,
            bin(
                BinOpKind::Add,
                bin(
                    BinOpKind::Div,
                    bin(BinOpKind::Sub, num(1), unary(UnOp::Minus, num(2))),
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
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
        assert_eq!(expr.kind, unary(UnOp::Plus, num(1)).kind);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
            TokenKind::BinOp(BinOpToken::Minus),
            TokenKind::OpenDelim(DelimToken::Paran),
            TokenKind::Num(1),
            TokenKind::BinOp(BinOpToken::Mul),
            TokenKind::Num(22),
            TokenKind::CloseDelim(DelimToken::Paran),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
        assert_eq!(
            expr.kind,
            unary(UnOp::Minus, bin(BinOpKind::Mul, num(1), num(22))).kind
        );
    }

    #[test]
    fn parse_compare_op_test() {
        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
            TokenKind::Num(1),
            TokenKind::EqEq,
            TokenKind::Num(2),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(3),
            TokenKind::Lt,
            TokenKind::Num(4),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
            TokenKind::Ne,
            TokenKind::Num(2),
            TokenKind::BinOp(BinOpToken::Mul),
            TokenKind::Num(3),
            TokenKind::Ge,
            TokenKind::Num(4),
            TokenKind::BinOp(BinOpToken::Plus),
            TokenKind::Num(5),
            TokenKind::Eof
        );
        let expr = parser.parse_expr(&mut TokenStream::new(tokens.into_iter(), &input));
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
    }

    #[test]
    fn parse_stmt_test() {
        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            vec![],
            block(vec![expr_stmt(num(1)), expr_stmt(num(2))]),
        )]);

        assert_eq!(parsed, expected);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![expr_stmt(assign(lvar("a"), num(2))), ret(lvar("a"))]),
        )]);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_assign_expr_test() {
        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![expr_stmt(assign(lvar("a"), num(2)))]),
        )]);

        assert_eq!(parsed, expected);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![expr_stmt(assign(
                lvar("a"),
                assign(lvar("b"), num(2)),
            ))]),
        )]);

        assert_eq!(parsed, expected);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![expr_stmt(assign(
                bin(BinOpKind::Add, lvar("a"), num(1)),
                num(2),
            ))]),
        )]);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_various_stmts() {
        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
            TokenKind::Ge,
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![
                expr_stmt(assign(lvar("a"), num(22))),
                if_(
                    bin(BinOpKind::Ge, lvar("a"), num(10)),
                    ret(num(1)),
                    Some(ret(num(0))),
                ),
            ]),
        )]);

        assert_eq!(parsed, expected);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
            TokenKind::Gt,
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![
                expr_stmt(assign(lvar("a"), num(22))),
                while_(
                    bin(BinOpKind::Gt, lvar("a"), num(0)),
                    expr_stmt(assign(lvar("a"), bin(BinOpKind::Sub, lvar("a"), num(1)))),
                ),
                ret(lvar("a")),
            ]),
        )]);

        assert_eq!(parsed, expected);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
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
        )]);

        assert_eq!(parsed, expected);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![
                expr_stmt(assign(lvar("a"), num(13))),
                ret(bin(
                    BinOpKind::Rem,
                    lvar("a"),
                    bin(BinOpKind::Rem, num(3), num(1)),
                )),
            ]),
        )]);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_call_func() {
        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
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
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![expr_stmt(func("foo", vec![num(3), num(1)]))]),
        )]);

        assert_eq!(parsed, expected);
    }

    fn func_def(name: &str, args: Vec<&str>, body: Stmt) -> ProgramKind {
        ProgramKind::Func(FuncDef::new(
            name.to_string(),
            args.into_iter().map(|s| s.to_string()).collect(),
            body,
        ))
    }

    fn expr_stmt(expr: Expr) -> Stmt {
        Stmt::expr(expr)
    }

    fn ret(expr: Expr) -> Stmt {
        Stmt::ret(expr)
    }

    fn block(stmts: Vec<Stmt>) -> Stmt {
        Stmt::new_block(stmts)
    }

    fn if_(cond: Expr, then: Stmt, els: Option<Stmt>) -> Stmt {
        Stmt::new_if(cond, then, els)
    }

    fn while_(cond: Expr, then: Stmt) -> Stmt {
        Stmt::new_while(cond, then)
    }

    fn for_(init: Option<Expr>, cond: Option<Expr>, inc: Option<Expr>, then: Stmt) -> Stmt {
        Stmt::new_for(init, cond, inc, then)
    }

    fn func(name: &str, args: Vec<Expr>) -> Expr {
        Expr::new_func(name.to_string(), args, Position::default())
    }

    fn lvar(name: &str) -> Expr {
        Expr::new_lvar(name.to_string(), Position::default())
    }

    fn assign(lhs: Expr, rhs: Expr) -> Expr {
        Expr::new_assign(lhs, rhs, Position::default())
    }

    fn bin(op: BinOpKind, lhs: Expr, rhs: Expr) -> Expr {
        Expr::new_binary(op, lhs, rhs, Position::default())
    }

    fn num(n: isize) -> Expr {
        Expr::new_num(n, Position::default())
    }

    fn unary(op: UnOp, operand: Expr) -> Expr {
        Expr::new_unary(op, operand, Position::default())
    }

    #[test]
    fn analysis_test() {
        let input = String::new();
        let mut analyzer = Analyzer::new(&input);
        let expr = unary(UnOp::Minus, bin(BinOpKind::Mul, num(1), num(22)));
        let mut lvar_map = BTreeMap::new();
        let converted_expr = analyzer.down_expr(expr, &mut lvar_map);
        assert_eq!(
            converted_expr.kind,
            cbin(
                ConvBinOpKind::Sub,
                cnum(0),
                cbin(ConvBinOpKind::Mul, cnum(1), cnum(22))
            )
            .kind
        );

        let input = String::new();
        let mut analyzer = Analyzer::new(&input);
        let expr = bin(BinOpKind::Ge, num(1), num(2));
        let mut lvar_map = BTreeMap::new();
        let converted_expr = analyzer.down_expr(expr, &mut lvar_map);
        assert_eq!(
            converted_expr.kind,
            cbin(ConvBinOpKind::Le, cnum(2), cnum(1),).kind
        );

        let input = String::new();
        let mut analyzer = Analyzer::new(&input);
        let expr = bin(BinOpKind::Gt, num(1), num(2));
        let mut lvar_map = BTreeMap::new();
        let converted_expr = analyzer.down_expr(expr, &mut lvar_map);
        assert_eq!(
            converted_expr.kind,
            cbin(ConvBinOpKind::Lt, cnum(2), cnum(1),).kind
        )
    }

    #[test]
    fn analysis_ident_test() {
        let input = String::new();
        let mut analyzer = Analyzer::new(&input);
        let expr = assign(lvar("a"), num(1));
        let mut lvar_map = BTreeMap::new();
        let converted_expr = analyzer.down_expr(expr, &mut lvar_map);
        assert_eq!(converted_expr.kind, cassign(clvar("a", 0), cnum(1)).kind)
    }

    #[test]
    fn analysis_program_test() {
        let input = String::new();
        let mut analyzer = Analyzer::new(&input);
        let program = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![
                expr_stmt(assign(lvar("a"), bin(BinOpKind::Ge, num(1), lvar("k")))),
                expr_stmt(lvar("b")),
            ]),
        )]);
        let converted_program = analyzer.down_program(program);
        assert_eq!(
            converted_program,
            cprog(vec![cfunc_def(
                "main",
                Vec::new(),
                cblock(vec![
                    cexpr_stmt(cassign(
                        clvar("a", 0),
                        cbin(ConvBinOpKind::Le, clvar("k", 8), cnum(1))
                    )),
                    cexpr_stmt(clvar("b", 16))
                ]),
                vec![
                    clvar_strct("a", 0),
                    clvar_strct("k", 8),
                    clvar_strct("b", 16)
                ]
            )])
        )
    }

    #[test]
    fn analysis_local_variable_test() {
        let input = String::new();
        let mut analyzer = Analyzer::new(&input);
        let program = Program::with_vec(vec![func_def(
            "main",
            Vec::new(),
            block(vec![
                expr_stmt(assign(lvar("a"), assign(lvar("k"), num(1)))),
                expr_stmt(assign(lvar("c"), num(3))),
                expr_stmt(bin(BinOpKind::Div, lvar("a"), lvar("k"))),
            ]),
        )]);
        let converted_program = analyzer.down_program(program);
        assert_eq!(
            converted_program,
            cprog(vec![cfunc_def(
                "main",
                Vec::new(),
                cblock(vec![
                    cexpr_stmt(cassign(clvar("a", 0), cassign(clvar("k", 8), cnum(1)))),
                    cexpr_stmt(cassign(clvar("c", 16), cnum(3))),
                    cexpr_stmt(cbin(ConvBinOpKind::Div, clvar("a", 0), clvar("k", 8))),
                ]),
                vec![
                    clvar_strct("a", 0),
                    clvar_strct("k", 8),
                    clvar_strct("c", 16)
                ]
            )])
        )
    }

    #[test]
    fn analysis_func_def_test() {
        let input = String::new();
        let mut analyzer = Analyzer::new(&input);
        let program = Program::with_vec(vec![func_def(
            "main",
            vec!["a", "b", "c"],
            block(vec![
                expr_stmt(assign(lvar("a"), assign(lvar("k"), num(1)))),
                expr_stmt(assign(lvar("c"), num(3))),
                expr_stmt(bin(BinOpKind::Div, lvar("a"), lvar("k"))),
                ret(bin(BinOpKind::Div, lvar("b"), lvar("c"))),
            ]),
        )]);
        let converted_program = analyzer.down_program(program);
        assert_eq!(
            converted_program,
            cprog(vec![cfunc_def(
                "main",
                vec![
                    clvar_strct("a", 0),
                    clvar_strct("b", 8),
                    clvar_strct("c", 16)
                ],
                cblock(vec![
                    cexpr_stmt(cassign(clvar("a", 0), cassign(clvar("k", 24), cnum(1)))),
                    cexpr_stmt(cassign(clvar("c", 16), cnum(3))),
                    cexpr_stmt(cbin(ConvBinOpKind::Div, clvar("a", 0), clvar("k", 24))),
                    cret(cbin(ConvBinOpKind::Div, clvar("b", 8), clvar("c", 16))),
                ]),
                vec![
                    clvar_strct("a", 0),
                    clvar_strct("b", 8),
                    clvar_strct("c", 16),
                    clvar_strct("k", 24)
                ]
            )])
        )
    }

    fn cprog(components: Vec<ConvProgramKind>) -> ConvProgram {
        ConvProgram::with_vec(components)
    }

    fn cfunc_def(name: &str, args: Vec<Lvar>, body: ConvStmt, lvars: Vec<Lvar>) -> ConvProgramKind {
        ConvProgramKind::Func(ConvFuncDef::new(
            name.to_string(),
            args,
            body,
            lvars.into_iter().collect(),
        ))
    }

    fn cblock(stmts: Vec<ConvStmt>) -> ConvStmt {
        ConvStmt::new_block(stmts)
    }

    fn clvar(name: &str, mut offset: usize) -> ConvExpr {
        let offset = &mut offset;
        let mut empty_lvar_map = BTreeMap::new();
        ConvExpr::new_lvar(
            name.to_string(),
            Position::default(),
            offset,
            &mut empty_lvar_map,
        )
    }

    fn clvar_strct(name: &str, mut offset: usize) -> Lvar {
        let offset = &mut offset;
        let mut empty_lvar_map = BTreeMap::new();
        Lvar::new(name.to_string(), offset, &mut empty_lvar_map)
    }

    fn cexpr_stmt(expr: ConvExpr) -> ConvStmt {
        ConvStmt::new_expr(expr)
    }

    fn cret(expr: ConvExpr) -> ConvStmt {
        ConvStmt::new_ret(expr)
    }

    fn cif_(cond: ConvExpr, then: ConvStmt, els: Option<ConvStmt>) -> ConvStmt {
        ConvStmt::new_if(cond, then, els)
    }

    fn cwhile_(cond: ConvExpr, then: ConvStmt) -> ConvStmt {
        ConvStmt::new_while(cond, then)
    }

    fn cfor_(
        init: Option<ConvExpr>,
        cond: Option<ConvExpr>,
        inc: Option<ConvExpr>,
        then: ConvStmt,
    ) -> ConvStmt {
        ConvStmt::new_for(init, cond, inc, then)
    }

    fn cfunc(name: &str, args: Vec<ConvExpr>) -> ConvExpr {
        ConvExpr::new_func(name.to_string(), args, Position::default())
    }

    fn cassign(lhs: ConvExpr, rhs: ConvExpr) -> ConvExpr {
        ConvExpr::new_assign(lhs, rhs, Position::default())
    }

    fn cbin(op: ConvBinOpKind, lhs: ConvExpr, rhs: ConvExpr) -> ConvExpr {
        ConvExpr::new_binary(op, lhs, rhs, Position::default())
    }

    fn cnum(n: isize) -> ConvExpr {
        ConvExpr::new_num(n, Position::default())
    }
}
