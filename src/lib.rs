pub mod analyze;
pub mod generate;
pub mod parse;
pub mod tokenize;

#[cfg(test)]
mod tests {

    use crate::{
        analyze::{Analyzer, ConvBinOpKind, ConvExpr},
        parse::{BinOpKind, Expr, Parser, Program, ProgramKind, Stmt, UnOp},
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
            TokenKind::Num(1),
            TokenKind::Semi,
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::Eof
        );
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![stmt(num(1)), stmt(num(2))]);

        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_assign_expr_test() {
        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
            TokenKind::Ident('a'.to_string()),
            TokenKind::Eq,
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::Eof
        );
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected = Program::with_vec(vec![stmt(assign(ident("a"), num(2)))]);

        assert_eq!(parsed, expected);

        let input = String::new();
        let parser = Parser::new(&input);
        let tokens = tokens!(
            TokenKind::Ident('a'.to_string()),
            TokenKind::Eq,
            TokenKind::Ident('b'.to_string()),
            TokenKind::Eq,
            TokenKind::Num(2),
            TokenKind::Semi,
            TokenKind::Eof
        );
        let parsed = parser.parse_program(&mut TokenStream::new(tokens.into_iter(), &input));
        let expected =
            Program::with_vec(vec![stmt(assign(ident("a"), assign(ident("b"), num(2))))]);

        assert_eq!(parsed, expected);
    }

    fn stmt(expr: Expr) -> ProgramKind {
        ProgramKind::Stmt(Stmt::expr(expr))
    }

    fn ident(name: &str) -> Expr {
        Expr::new_ident(name.to_string(), Position::default())
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
        let analyzer = Analyzer::new(&input);
        let expr = unary(UnOp::Minus, bin(BinOpKind::Mul, num(1), num(22)));
        let converted_expr = analyzer.down_expr(expr);
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
        let analyzer = Analyzer::new(&input);
        let expr = bin(BinOpKind::Ge, num(1), num(2));
        let converted_expr = analyzer.down_expr(expr);
        assert_eq!(
            converted_expr.kind,
            cbin(ConvBinOpKind::Le, cnum(2), cnum(1),).kind
        );

        let input = String::new();
        let analyzer = Analyzer::new(&input);
        let expr = bin(BinOpKind::Gt, num(1), num(2));
        let converted_expr = analyzer.down_expr(expr);
        assert_eq!(
            converted_expr.kind,
            cbin(ConvBinOpKind::Lt, cnum(2), cnum(1),).kind
        )
    }
    fn cbin(op: ConvBinOpKind, lhs: ConvExpr, rhs: ConvExpr) -> ConvExpr {
        ConvExpr::new_binary(op, lhs, rhs, Position::default())
    }

    fn cnum(n: isize) -> ConvExpr {
        ConvExpr::new_num(n, Position::default())
    }
}
