extern crate ironcc;
pub mod test_utils;

use std::collections::{BTreeMap, BTreeSet};

use ironcc::analyze::{self, *};
use ironcc::error::CompileError;
use ironcc::parse::*;
use ironcc::tokenize::{Position, TokenStream, Tokenizer};
use test_utils::ast::*;

#[test]
fn analysis_test() -> Result<(), CompileError> {
    let input = String::new();
    let mut analyzer = Analyzer::new(&input);
    let expr = unary(UnOp::Minus, bin(BinOpKind::Mul, num(1), num(22)));
    let converted_expr = analyzer.down_expr(expr, BTreeSet::new()).unwrap();
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
    let converted_expr = analyzer.down_expr(expr, BTreeSet::new()).unwrap();
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
    let converted_expr = analyzer.down_expr(expr, BTreeSet::new()).unwrap();
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
    let mut offset = 0;
    analyzer.scope.push_scope();
    analyzer
        .scope
        .register_lvar(
            &input,
            Position::default(),
            &mut offset,
            &"a".to_string(),
            Type::Base(BaseType::Int),
        )
        .unwrap();
    // dummy fucn defining end
    let converted_expr = analyzer.down_expr(expr, BTreeSet::new()).unwrap();
    analyzer.scope.pop_scope(&mut offset);
    assert_eq!(offset, 0);
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
        TypeSpec::Int,
        0,
        func_dd("main", vec![]),
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
        Position::default(),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Func(Box::new(Type::Base(BaseType::Int)), Vec::new()),
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
        TypeSpec::Int,
        0,
        func_dd("main", vec![]),
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
        Position::default(),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Func(Box::new(Type::Base(BaseType::Int)), Vec::new()),
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
        TypeSpec::Int,
        0,
        func_dd(
            "main",
            vec!["a", "b", "c"]
                .into_iter()
                .map(|s| declare(TypeSpec::Int, 0, DirectDeclarator::Ident(s.to_string())))
                .collect(),
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
        Position::default(),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Func(
                Box::new(Type::Base(BaseType::Int)),
                vec![
                    Type::Base(BaseType::Int),
                    Type::Base(BaseType::Int),
                    Type::Base(BaseType::Int)
                ]
            ),
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
        TypeSpec::Int,
        0,
        func_dd("main", vec![]),
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
        Position::default(),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Func(Box::new(Type::Base(BaseType::Int)), Vec::new()),
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
        TypeSpec::Int,
        0,
        func_dd("main", Vec::new()),
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
        Position::default(),
    )]);
    let converted_program = analyzer.down_program(program).unwrap();
    assert_eq!(
        converted_program,
        cprog(vec![cfunc_def(
            Type::Func(Box::new(Type::Base(BaseType::Int)), Vec::new()),
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

#[test]
fn parse_type_test() {
    use analyze::BaseType;
    use analyze::Type;
    let declaration_src = "{int *array[5]; return array;}";
    let ty = extract_ty(declaration_src);
    assert_eq!(
        ty,
        // after array to ptr conversion
        Type::Ptr(Box::new(Type::Ptr(Box::new(Type::Base(BaseType::Int)))))
    );

    let declaration_src =
        "{int *(*(*func)(int arg_f1, int* arg_f2))(int **arg_f_ret_1); return func;}";
    // `func` is a pointer { of function(int, int*) returning { a pointer { of function(int **) { returning int* } } } }
    let ty = extract_ty(declaration_src);
    let int = || Type::Base(BaseType::Int);
    let ptr = |ty| Type::Ptr(Box::new(ty));
    let func = |ty, args| Type::Func(Box::new(ty), args);
    let arr = |ty, size| Type::Array(Box::new(ty), size);
    assert_eq!(
        ty,
        ptr(func(
            ptr(func(ptr(int()), vec![ptr(ptr(int()))])),
            vec![int(), ptr(int())]
        ))
    );

    let src = "{int *(*(*signal)(int arg0, int *(*f)(int arg0)))(int a); return signal;}";
    // `signal` is a pointer to function(int, a pointer to function(int) returning a pointer to int) returning a pointer to function(int) returning a pointer to int
    let ty = extract_ty(src);
    assert_eq!(
        ty,
        ptr(func(
            ptr(func(ptr(int()), vec![int()])),
            vec![int(), ptr(func(ptr(int()), vec![int()]))]
        ))
    );

    let src = "int init_2d_mat(int (*mat)[2], int a, int b, int c, int d) {}";
    let ty = extract_func_ty(src);
    assert_eq!(
        ty,
        func(int(), vec![ptr(arr(int(), 2)), int(), int(), int(), int()])
    );

    let src = "int (*mat_mul_2d(int (*new_mat)[2], int (*lhs)[2], int (*rhs)[2]))[2] {}";
    let ty = extract_func_ty(src);
    assert_eq!(
        ty,
        func(
            ptr(arr(int(), 2)),
            vec![ptr(arr(int(), 2)), ptr(arr(int(), 2)), ptr(arr(int(), 2)),]
        )
    );
}

fn extract_ty(src: &str) -> Type {
    let tokens = Tokenizer::new(src).tokenize().unwrap();
    let mut tokens = TokenStream::new(tokens.into_iter(), src);
    let ast = Parser::new(src).parse_stmt(&mut tokens).unwrap();
    let mut analyzer = Analyzer::new(src);
    let conv_stmt = analyzer.down_stmt(ast, "main".to_string()).unwrap();
    if let ConvStmt {
        kind: ConvStmtKind::Block(vec),
    } = conv_stmt
    {
        let second_stmt = vec.get(1).unwrap();
        if let ConvStmt {
            kind: ConvStmtKind::Return(expr, _),
        } = second_stmt
        {
            expr.ty.clone()
        } else {
            panic!("Return expected.")
        }
    } else {
        panic!("Block expected.");
    }
}

fn extract_func_ty(src: &str) -> Type {
    let tokens = Tokenizer::new(src).tokenize().unwrap();
    let mut tokens = TokenStream::new(tokens.into_iter(), src);
    let ast = Parser::new(src).parse_func_def(&mut tokens).unwrap();
    let (ty_spec, declarator, body, pos) = if let ProgramComponent {
        kind: ProgramKind::FuncDef(ty_spec, declarator, body),
        pos,
    } = ast
    {
        (ty_spec, declarator, body, pos)
    } else {
        panic!("ProgramKind::Func expected.")
    };
    let mut analyzer = Analyzer::new(src);
    // return Declaration::new(ty_spec, declarator.n_star, declarator.d_declrtr, None, pos)
    //     .ty(&analyzer)
    //     .unwrap();
    let conv_program = analyzer
        .down_func_def(&ty_spec, &declarator, body, pos)
        .unwrap();

    match conv_program {
        ConvProgramKind::Func(ConvFuncDef {
            ty,
            name: _,
            args: _,
            body: _,
            stack_size: _,
        }) => ty,
        ConvProgramKind::Global(_) => panic!("Func expected."),
    }
}

fn cprog(components: Vec<ConvProgramKind>) -> ConvProgram {
    ConvProgram::with_vec(components)
}

fn cfunc_def(
    ty: Type,
    name: &str,
    args: Vec<LVar>,
    body: ConvStmt,
    lvars: Vec<LVar>,
) -> ConvProgramKind {
    ConvProgramKind::Func(ConvFuncDef::new(
        ty,
        name.to_string(),
        args,
        body,
        lvars.into_iter().map(|lvar| lvar.offset).max().unwrap_or(0),
    ))
}

fn cblock(stmts: Vec<ConvStmt>) -> ConvStmt {
    ConvStmt::new_block(stmts)
}

fn clvar(name: &str, ty: Type, mut offset: usize) -> ConvExpr {
    let mut lvar_map = BTreeMap::new();
    let offset = &mut offset;
    Analyzer::new_lvar(
        "",
        name.to_string(),
        Position::default(),
        offset,
        ty,
        &mut lvar_map,
    )
    .unwrap();

    let lvar = match &mut lvar_map.get(name) {
        Some(lvar) => lvar.clone(),
        None => panic!("Illegal func args"),
    };
    let ty = lvar.ty.clone();
    ConvExpr::new_lvar_raw(lvar, ty, Position::default())
}

fn clvar_strct(name: &str, ty: Type, mut offset: usize) -> LVar {
    let offset = &mut offset;
    let mut empty = BTreeMap::new();
    Analyzer::new_lvar(
        "",
        name.to_string(),
        Position::default(),
        offset,
        ty,
        &mut empty,
    )
    .unwrap()
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
    ConvExpr::new_func(
        name.to_string(),
        args,
        Type::Base(BaseType::Int),
        Position::default(),
    )
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
