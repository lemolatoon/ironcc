pub mod parse;
pub mod tokenize;

#[cfg(test)]
mod tests {

    use crate::tokenize::TokenStream;

    use super::*;

    #[test]
    fn tokenize_plus_minus_test() {
        use crate::tokenize::{
            kind_eq, tokenize_and_kinds, BinOpToken, Token, TokenKind, Tokenizer,
        };
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
            tokenize_and_kinds(input),
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
        )
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
        println!("{:?}", tokens);
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
        )
    }
}
