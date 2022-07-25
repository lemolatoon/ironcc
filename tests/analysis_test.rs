extern crate ironcc;
pub mod test_utils;

use std::collections::BTreeMap;

use ironcc::analyze::*;
use ironcc::error::CompileError;
use ironcc::parse::*;
use ironcc::tokenize::Position;
use test_utils::ast::*;

#[test]
fn analysis_test() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let expr = unary(UnOp::Minus, bin(BinOpKind::Mul, num(1), num(22)));
    let mut lvar_map = BTreeMap::new();
    let converted_expr = analyzer.down_expr(expr, &mut lvar_map).unwrap();
    assert_eq!(
        converted_expr.kind,
        cbin(
            ConvBinOpKind::Sub,
            cnum(0),
            cbin(
                ConvBinOpKind::Mul,
                cnum(1),
                cnum(22),
                Type::Base(BaseType::Int),
            ),
            Type::Base(BaseType::Int),
        )
        .kind
    );

    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let expr = bin(BinOpKind::Ge, num(1), num(2));
    let mut lvar_map = BTreeMap::new();
    let converted_expr = analyzer.down_expr(expr, &mut lvar_map).unwrap();
    assert_eq!(
        converted_expr.kind,
        cbin(
            ConvBinOpKind::Le,
            cnum(2),
            cnum(1),
            Type::Base(BaseType::Int),
        )
        .kind
    );

    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let expr = bin(BinOpKind::Gt, num(1), num(2));
    let mut lvar_map = BTreeMap::new();
    let converted_expr = analyzer.down_expr(expr, &mut lvar_map).unwrap();
    assert_eq!(
        converted_expr.kind,
        cbin(
            ConvBinOpKind::Lt,
            cnum(2),
            cnum(1),
            Type::Base(BaseType::Int),
        )
        .kind
    );
    Ok(())
}

#[test]
fn analysis_ident_test() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let expr = assign(lvar("a"), num(1));
    // dummy fucn defining
    let mut lvar_map = BTreeMap::new();
    let mut offset = 0;
    Lvar::new(
        "a".to_string(),
        &mut offset,
        Type::Base(BaseType::Int),
        &mut lvar_map,
    )
    .unwrap();
    // dummy fucn defining end
    let converted_expr = analyzer.down_expr(expr, &mut lvar_map).unwrap();
    assert_eq!(
        converted_expr.kind,
        cassign(clvar("a", Type::Base(BaseType::Int), 0), cnum(1)).kind
    );
    Ok(())
}

#[test]
fn analysis_program_test() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let program = Program::with_vec(vec![func_def(
        declare(TypeSpec::Int, 0, func_dd("main", vec![])),
        block(vec![
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("a".to_string()),
            )), // int a;
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("b".to_string()),
            )), // int b;
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("k".to_string()),
            )), // int k;
            expr_stmt(assign(lvar("a"), bin(BinOpKind::Ge, num(1), lvar("k")))),
            expr_stmt(lvar("b")),
        ]),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Base(BaseType::Int),
            "main",
            Vec::new(),
            cblock(vec![
                cblock(vec![]),
                cblock(vec![]),
                cblock(vec![]),
                cexpr_stmt(cassign(
                    clvar("a", Type::Base(BaseType::Int), 0),
                    cbin(
                        ConvBinOpKind::Le,
                        clvar("k", Type::Base(BaseType::Int), 8),
                        cnum(1),
                        Type::Base(BaseType::Int),
                    )
                )),
                cexpr_stmt(clvar("b", Type::Base(BaseType::Int), 4))
            ]),
            vec![
                clvar_strct("a", Type::Base(BaseType::Int), 0),
                clvar_strct("b", Type::Base(BaseType::Int), 4),
                clvar_strct("k", Type::Base(BaseType::Int), 8),
            ]
        )])
    );
    Ok(())
}

#[test]
fn analysis_local_variable_test() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let program = Program::with_vec(vec![func_def(
        declare(TypeSpec::Int, 0, func_dd("main", vec![])),
        block(vec![
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("a".to_string()),
            )), // int a;
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("k".to_string()),
            )), // int k;
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("c".to_string()),
            )), // int c;
            expr_stmt(assign(lvar("a"), assign(lvar("k"), num(1)))),
            expr_stmt(assign(lvar("c"), num(3))),
            expr_stmt(bin(BinOpKind::Div, lvar("a"), lvar("k"))),
        ]),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Base(BaseType::Int),
            "main",
            Vec::new(),
            cblock(vec![
                cblock(vec![]),
                cblock(vec![]),
                cblock(vec![]),
                cexpr_stmt(cassign(
                    clvar("a", Type::Base(BaseType::Int), 0),
                    cassign(clvar("k", Type::Base(BaseType::Int), 4), cnum(1))
                )),
                cexpr_stmt(cassign(clvar("c", Type::Base(BaseType::Int), 8), cnum(3))),
                cexpr_stmt(cbin(
                    ConvBinOpKind::Div,
                    clvar("a", Type::Base(BaseType::Int), 0),
                    clvar("k", Type::Base(BaseType::Int), 4),
                    Type::Base(BaseType::Int),
                )),
            ]),
            vec![
                clvar_strct("a", Type::Base(BaseType::Int), 0),
                clvar_strct("k", Type::Base(BaseType::Int), 4),
                clvar_strct("c", Type::Base(BaseType::Int), 8)
            ]
        )])
    );
    Ok(())
}

#[test]
fn analysis_func_def_test() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let program = Program::with_vec(vec![func_def(
        declare(
            TypeSpec::Int,
            0,
            func_dd(
                "main",
                vec!["a", "b", "c"]
                    .into_iter()
                    .map(|s| declare(TypeSpec::Int, 0, DirectDeclarator::Ident(s.to_string())))
                    .collect(),
            ),
        ),
        block(vec![
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("k".to_string()),
            )), // int k;
            expr_stmt(assign(lvar("a"), assign(lvar("k"), num(1)))),
            expr_stmt(assign(lvar("c"), num(3))),
            expr_stmt(bin(BinOpKind::Div, lvar("a"), lvar("k"))),
            ret(bin(BinOpKind::Div, lvar("b"), lvar("c"))),
        ]),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Base(BaseType::Int),
            "main",
            vec![
                clvar_strct("a", Type::Base(BaseType::Int), 0),
                clvar_strct("b", Type::Base(BaseType::Int), 4),
                clvar_strct("c", Type::Base(BaseType::Int), 8)
            ],
            cblock(vec![
                cblock(vec![]),
                cexpr_stmt(cassign(
                    clvar("a", Type::Base(BaseType::Int), 0),
                    cassign(clvar("k", Type::Base(BaseType::Int), 12), cnum(1))
                )),
                cexpr_stmt(cassign(clvar("c", Type::Base(BaseType::Int), 8), cnum(3))),
                cexpr_stmt(cbin(
                    ConvBinOpKind::Div,
                    clvar("a", Type::Base(BaseType::Int), 0),
                    clvar("k", Type::Base(BaseType::Int), 12),
                    Type::Base(BaseType::Int),
                )),
                cret(
                    cbin(
                        ConvBinOpKind::Div,
                        clvar("b", Type::Base(BaseType::Int), 4),
                        clvar("c", Type::Base(BaseType::Int), 8),
                        Type::Base(BaseType::Int),
                    ),
                    "main"
                ),
            ]),
            vec![
                clvar_strct("a", Type::Base(BaseType::Int), 0),
                clvar_strct("b", Type::Base(BaseType::Int), 4),
                clvar_strct("c", Type::Base(BaseType::Int), 8),
                clvar_strct("k", Type::Base(BaseType::Int), 12)
            ]
        )])
    );
    Ok(())
}

#[test]
fn analysis_declaration() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let program = Program::with_vec(vec![func_def(
        declare(TypeSpec::Int, 0, func_dd("main", vec![])),
        block(vec![
            declare_stmt(declare(
                TypeSpec::Int,
                1,
                DirectDeclarator::Ident("a".to_string()),
            )), // int *a;
            declare_stmt(declare(
                TypeSpec::Int,
                2,
                DirectDeclarator::Ident("c".to_string()),
            )), // int **c;
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("k".to_string()),
            )), // int k;
            declare_stmt(declare(
                TypeSpec::Int,
                0,
                DirectDeclarator::Ident("x".to_string()),
            )), // int x;
            expr_stmt(bin(BinOpKind::Add, lvar("a"), lvar("k"))), // a + k
        ]),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Base(BaseType::Int),
            "main",
            Vec::new(),
            cblock(vec![
                cblock(vec![]),
                cblock(vec![]),
                cblock(vec![]),
                cblock(vec![]),
                cexpr_stmt(cbin(
                    ConvBinOpKind::Add,
                    clvar("a", Type::Ptr(Box::new(Type::Base(BaseType::Int))), 0),
                    cbin(
                        ConvBinOpKind::Mul,
                        clvar("k", Type::Base(BaseType::Int), 16),
                        cnum(4),
                        Type::Base(BaseType::Int),
                    ),
                    Type::Ptr(Box::new(Type::Base(BaseType::Int))),
                )),
            ]),
            vec![
                clvar_strct("a", Type::Ptr(Box::new(Type::Base(BaseType::Int))), 0),
                clvar_strct("k", Type::Base(BaseType::Int), 16),
                clvar_strct(
                    "c",
                    Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Base(BaseType::Int))))),
                    8
                ),
                clvar_strct("x", Type::Base(BaseType::Int), 20)
            ]
        )])
    );
    Ok(())
}

#[test]
fn analysis_ptr_addition() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let program = Program::with_vec(vec![func_def(
        declare(TypeSpec::Int, 0, func_dd("main", Vec::new())),
        block(vec![
            declare_stmt(declare(
                TypeSpec::Int,
                1,
                DirectDeclarator::Ident("k".to_string()),
            )), // int *k;
            declare_stmt(declare(
                TypeSpec::Int,
                1,
                DirectDeclarator::Ident("p".to_string()),
            )), // int *p;
            expr_stmt(assign(lvar("p"), bin(BinOpKind::Add, lvar("k"), num(1)))), // p = k + 1;
            ret(num(0)),
        ]),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Base(BaseType::Int),
            "main",
            Vec::new(),
            cblock(vec![
                cblock(vec![]),
                cblock(vec![]),
                cexpr_stmt(cassign(
                    clvar("p", Type::Ptr(Box::new(Type::Base(BaseType::Int))), 8),
                    cbin(
                        ConvBinOpKind::Add,
                        clvar("k", Type::Ptr(Box::new(Type::Base(BaseType::Int))), 0),
                        cbin(
                            ConvBinOpKind::Mul,
                            cnum(1),
                            cnum(4),
                            Type::Base(BaseType::Int)
                        ),
                        Type::Ptr(Box::new(Type::Base(BaseType::Int))),
                    )
                )),
                cret(cnum(0), "main"),
            ]),
            vec![
                clvar_strct("k", Type::Ptr(Box::new(Type::Base(BaseType::Int))), 0),
                clvar_strct("p", Type::Ptr(Box::new(Type::Base(BaseType::Int))), 8)
            ]
        )])
    );
    Ok(())
}

fn cprog(components: Vec<ConvProgramKind>) -> ConvProgram {
    ConvProgram::with_vec(components)
}

fn cfunc_def(
    ty: Type,
    name: &str,
    args: Vec<Lvar>,
    body: ConvStmt,
    lvars: Vec<Lvar>,
) -> ConvProgramKind {
    ConvProgramKind::Func(ConvFuncDef::new(
        ty,
        name.to_string(),
        args,
        body,
        lvars.into_iter().collect(),
    ))
}

fn cblock(stmts: Vec<ConvStmt>) -> ConvStmt {
    ConvStmt::new_block(stmts)
}

fn clvar(name: &str, ty: Type, mut offset: usize) -> ConvExpr {
    let mut lvar_map = BTreeMap::new();
    let offset = &mut offset;
    Lvar::new(name.to_string(), offset, ty, &mut lvar_map).unwrap();

    let lvar = match &mut lvar_map.get(name) {
        Some(lvar) => lvar.clone(),
        None => panic!("Illegal func args"),
    };
    let ty = lvar.ty.clone();
    ConvExpr::new_lvar_raw(lvar, ty, Position::default())
}

fn clvar_strct(name: &str, ty: Type, mut offset: usize) -> Lvar {
    let offset = &mut offset;
    let mut empty = BTreeMap::new();
    Lvar::new(name.to_string(), offset, ty, &mut empty).unwrap()
}

fn cexpr_stmt(expr: ConvExpr) -> ConvStmt {
    ConvStmt::new_expr(expr)
}

fn cret(expr: ConvExpr, name: &str) -> ConvStmt {
    ConvStmt::new_ret(expr, name.to_string())
}

// TODO: use these 4 func and write tests
fn _cif_(cond: ConvExpr, then: ConvStmt, els: Option<ConvStmt>) -> ConvStmt {
    ConvStmt::new_if(cond, then, els)
}

fn _cwhile_(cond: ConvExpr, then: ConvStmt) -> ConvStmt {
    ConvStmt::new_while(cond, then)
}

fn _cfor_(
    init: Option<ConvExpr>,
    cond: Option<ConvExpr>,
    inc: Option<ConvExpr>,
    then: ConvStmt,
) -> ConvStmt {
    ConvStmt::new_for(init, cond, inc, then)
}

fn _cfunc(name: &str, args: Vec<ConvExpr>) -> ConvExpr {
    ConvExpr::new_func(name.to_string(), args, Position::default())
}

fn cassign(lhs: ConvExpr, rhs: ConvExpr) -> ConvExpr {
    ConvExpr::new_assign(lhs, rhs, Position::default())
}

fn cbin(op: ConvBinOpKind, lhs: ConvExpr, rhs: ConvExpr, ty: Type) -> ConvExpr {
    ConvExpr {
        kind: ConvExprKind::Binary(ConvBinary {
            kind: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }),
        ty,
        pos: Position::default(),
    }
}

fn cnum(n: isize) -> ConvExpr {
    ConvExpr::new_num(n, Position::default())
}
