pub mod tokenize;

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn tokenize_test() {
        use crate::tokenize::{tokenize, BinOpToken, Token, TokenKind};
        let input = String::from("1 + 4 -       909");
        assert_eq!(
            tokenize(input),
            tokens!(
                TokenKind::Num(1),
                TokenKind::BinOp(BinOpToken::Plus),
                TokenKind::Num(4),
                TokenKind::BinOp(BinOpToken::Minus),
                TokenKind::Num(909),
                TokenKind::Eof
            )
        );

        let input = String::from("0\t + 5+1+9-3 -  \n     909");
        assert_eq!(
            tokenize(input),
            tokens!(
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
        );
    }
}
