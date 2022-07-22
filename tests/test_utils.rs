extern crate ironcc;
use ironcc::tokenize::Token;

#[cfg(test)]
#[macro_export]
macro_rules! tokens {
    ( $( $token_kind:expr ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            let pos = Position::default();
            temp_vec.push(Token::new($token_kind, pos));
        )*
        temp_vec
    }};
}

#[cfg(test)]
#[macro_export]
pub fn kind_eq(lhs: &Vec<Token>, rhs: &Vec<Token>) -> bool {
    lhs.into_iter()
        .zip(rhs.into_iter())
        .fold(true, |acc, (l_token, r_token)| {
            acc && l_token.kind_eq(r_token)
        })
}

#[cfg(test)]
#[macro_export]
macro_rules! token_poses {
    ( $( ($token_kind:expr, $pos:expr) ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push(Token::new($token_kind, $pos));
        )*
        temp_vec
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! token_kinds {
    ( $( $token_kind:expr ), *) => {{
        let mut temp_vec = Vec::new();
        $(
            let pos = Position::default();
            temp_vec.push(Token::new($token_kind, pos));
        )*
        temp_vec
            .into_iter()
            .map(|token| token.kind())
            .collect::<Vec<_>>()
    }};
}

pub mod ast {
    use ironcc::{parse::*, tokenize::Position};
    pub fn deref(expr: Expr) -> Expr {
        Expr::new_deref(expr, Position::default())
    }

    pub fn addr(expr: Expr) -> Expr {
        Expr::new_addr(expr, Position::default())
    }

    pub fn func_def(declare: Declaration, body: Stmt) -> ProgramKind {
        ProgramKind::Func(declare, body)
    }

    pub fn declare_stmt(declaration: Declaration) -> Stmt {
        Stmt::new_declare(declaration)
    }

    pub fn declare(
        ty_spec: TypeSpec,
        n_star: usize,
        direct_declarator: DirectDeclarator,
    ) -> Declaration {
        Declaration::new(ty_spec, n_star, direct_declarator, Position::default())
    }

    pub fn func_dd(name: &str, args: Vec<Declaration>) -> DirectDeclarator {
        DirectDeclarator::Func(Box::new(DirectDeclarator::Ident(name.to_string())), args)
    }

    pub fn expr_stmt(expr: Expr) -> Stmt {
        Stmt::expr(expr)
    }

    pub fn ret(expr: Expr) -> Stmt {
        Stmt::ret(expr)
    }

    pub fn block(stmts: Vec<Stmt>) -> Stmt {
        Stmt::new_block(stmts)
    }

    pub fn if_(cond: Expr, then: Stmt, els: Option<Stmt>) -> Stmt {
        Stmt::new_if(cond, then, els)
    }

    pub fn while_(cond: Expr, then: Stmt) -> Stmt {
        Stmt::new_while(cond, then)
    }

    pub fn for_(init: Option<Expr>, cond: Option<Expr>, inc: Option<Expr>, then: Stmt) -> Stmt {
        Stmt::new_for(init, cond, inc, then)
    }

    pub fn func(name: &str, args: Vec<Expr>) -> Expr {
        Expr::new_func(name.to_string(), args, Position::default())
    }

    pub fn lvar(name: &str) -> Expr {
        Expr::new_lvar(name.to_string(), Position::default())
    }

    pub fn assign(lhs: Expr, rhs: Expr) -> Expr {
        Expr::new_assign(lhs, rhs, Position::default())
    }

    pub fn bin(op: BinOpKind, lhs: Expr, rhs: Expr) -> Expr {
        Expr::new_binary(op, lhs, rhs, Position::default())
    }

    pub fn num(n: isize) -> Expr {
        Expr::new_num(n, Position::default())
    }

    pub fn unary(op: UnOp, operand: Expr) -> Expr {
        Expr::new_unary(op, operand, Position::default())
    }
}
