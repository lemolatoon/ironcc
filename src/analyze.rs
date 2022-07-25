use std::collections::{BTreeMap, BTreeSet};

use crate::{
    error::{AnalyzeErrorKind, CompileError, CompileErrorKind, VariableKind},
    parse::{
        BinOpKind, Binary, Declaration, DirectDeclarator, Expr, ExprKind, Initializer, Program,
        ProgramKind, SizeOfOperandKind, Stmt, StmtKind, UnOp,
    },
    tokenize::Position,
    unimplemented_err,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Analyzer<'a> {
    input: &'a str,
    offset: usize,
}

impl<'a> Analyzer<'a> {
    pub const fn new(input: &'a str) -> Self {
        Self { input, offset: 0 }
    }

    pub fn down_program(&mut self, program: Program) -> Result<ConvProgram, CompileError> {
        let mut conv_program = ConvProgram::new();
        for component in program {
            match component {
                ProgramKind::Func(func_declare, body) => {
                    let mut lvar_map = BTreeMap::new();
                    self.offset = 0; // reset another func's offset
                    conv_program.push(self.down_func_declare(
                        &func_declare,
                        body,
                        &mut lvar_map,
                    )?);
                }
            }
        }
        Ok(conv_program)
    }

    pub fn down_func_declare(
        &mut self,
        declare: &Declaration,
        body: Stmt,
        lvar_map: &mut BTreeMap<String, Lvar>,
    ) -> Result<ConvProgramKind, CompileError> {
        let mut lvars = Vec::new();
        let ident = declare.ident_name();
        // TODO: manage func's return type
        let ty = declare.ty();
        let args = match declare.declrtr.clone() {
            DirectDeclarator::Ident(_) => {
                return Err(unimplemented_err!(
                    self,
                    declare.pos,
                    "Currently top-level declaration is not allowed."
                ))
            }
            DirectDeclarator::Func(_, args) => args,
        };
        for arg in args {
            // register func args as lvar
            lvars.push(Self::new_lvar(
                self.input,
                arg.ident_name().to_owned(),
                arg.pos,
                &mut self.offset,
                arg.ty(),
                lvar_map,
            )?);
        }
        Ok(ConvProgramKind::Func(ConvFuncDef::new(
            ty,
            ident.to_owned(),
            lvars,
            self.down_stmt(body, lvar_map, ident.to_owned())?,
            lvar_map.clone().into_values().collect::<BTreeSet<_>>(),
        )))
    }

    pub fn down_stmt(
        &mut self,
        stmt: Stmt,
        lvar_map: &mut BTreeMap<String, Lvar>,
        fn_name: String,
    ) -> Result<ConvStmt, CompileError> {
        Ok(match stmt.kind {
            StmtKind::Expr(expr) => ConvStmt::new_expr(self.down_expr(expr, lvar_map)?),
            StmtKind::Return(expr) => ConvStmt::new_ret(self.down_expr(expr, lvar_map)?, fn_name),
            StmtKind::If(cond, then, els) => ConvStmt::new_if(
                self.down_expr(cond, lvar_map)?,
                self.down_stmt(*then, lvar_map, fn_name.clone())?,
                els.map(|stmt| self.down_stmt(*stmt, lvar_map, fn_name.clone()))
                    .transpose()?,
            ),
            StmtKind::While(cond, then) => ConvStmt::new_while(
                self.down_expr(cond, lvar_map)?,
                self.down_stmt(*then, lvar_map, fn_name)?,
            ),
            StmtKind::For(init, cond, inc, then) => ConvStmt::new_for(
                init.map(|expr| self.down_expr(expr, lvar_map))
                    .transpose()?,
                cond.map(|expr| self.down_expr(expr, lvar_map))
                    .transpose()?,
                inc.map(|expr| self.down_expr(expr, lvar_map)).transpose()?,
                self.down_stmt(*then, lvar_map, fn_name)?,
            ),
            StmtKind::Block(stmts) => ConvStmt::new_block(
                stmts
                    .into_iter()
                    .map(|stmt| self.down_stmt(stmt, lvar_map, fn_name.clone()))
                    .collect::<Result<Vec<_>, CompileError>>()?,
            ),
            StmtKind::Declare(declare) => {
                let ty = declare.ty();
                // TODO: avoid func definition here
                assert!(!matches!(declare.declrtr, DirectDeclarator::Func(_, _)));
                let name = declare.ident_name();
                let lvar = Self::new_lvar(
                    self.input,
                    name.to_owned(),
                    declare.pos,
                    &mut self.offset,
                    ty.clone(),
                    lvar_map,
                )?;
                match declare.initializer {
                    Some(Initializer::Expr(init)) => {
                        let rhs = self.down_expr(init, lvar_map)?;
                        ConvStmt::new_expr(self.new_assign_expr_with_type_check(
                            // Safety:
                            // the lvar is generated by `Self::new_lvar` which initializes lvar_map
                            ConvExpr::new_lvar_raw(lvar, ty, declare.pos),
                            rhs,
                            declare.pos,
                        )?)
                    }
                    // just declaration does nothing
                    _ => ConvStmt::new_block(vec![]),
                }
            }
        })
    }

    // this `&self` will be used when type checking is activated
    #[allow(clippy::unused_self)]
    pub fn new_assign_expr_with_type_check(
        &self,
        lhs: ConvExpr,
        rhs: ConvExpr,
        pos: Position,
    ) -> Result<ConvExpr, CompileError> {
        if lhs.ty != rhs.ty {
            // TODO:
            // return Err here after impl prototype func declaration

            // return Err(CompileError::new_type_error(
            //     self.input,
            //     lhs,
            //     rhs,
            //     Some("Assign expression's lhs and rhs has to have compatible types"),
            // ));
        }
        Ok(ConvExpr::new_assign(lhs, rhs, pos))
    }

    pub fn down_expr(
        &mut self,
        expr: Expr,
        lvar_map: &mut BTreeMap<String, Lvar>,
    ) -> Result<ConvExpr, CompileError> {
        let pos = expr.pos;
        match expr.kind {
            // `a >= b` := `b <= a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Ge,
                lhs,
                rhs,
            }) => self.down_binary(Binary::new(BinOpKind::Le, rhs, lhs), pos, lvar_map),
            // `a > b` := `b < a`
            ExprKind::Binary(Binary {
                kind: BinOpKind::Gt,
                lhs,
                rhs,
            }) => self.down_binary(Binary::new(BinOpKind::Lt, rhs, lhs), pos, lvar_map),
            // do nothing
            ExprKind::Binary(binary) => self.down_binary(binary, pos, lvar_map), // do nothing
            ExprKind::Num(n) => Ok(ConvExpr::new_num(n, pos)),
            // substitute `-x` into `0-x`
            ExprKind::Unary(UnOp::Minus, operand) => self.down_binary(
                Binary::new(BinOpKind::Sub, Box::new(Expr::new_num(0, pos)), operand),
                pos,
                lvar_map,
            ),
            // do nothing
            ExprKind::Unary(UnOp::Plus, operand) => self.down_expr(*operand, lvar_map),
            // do nothing
            ExprKind::Assign(lhs, rhs) => {
                let lhs = self.down_expr(*lhs, lvar_map)?;
                let rhs = self.down_expr(*rhs, lvar_map)?;
                self.new_assign_expr_with_type_check(lhs, rhs, pos)
            }
            // currently all ident is local variable
            ExprKind::LVar(name) => self.fetch_lvar(&name, pos, lvar_map),
            ExprKind::Func(name, args) => Ok(ConvExpr::new_func(
                name,
                args.into_iter()
                    .map(|expr| self.down_expr(expr, lvar_map))
                    .collect::<Result<Vec<_>, CompileError>>()?,
                pos,
            )),
            ExprKind::Deref(expr) => {
                // type check
                let conv_expr = self.down_expr(*expr, lvar_map)?;
                let base_ty = match conv_expr.ty.clone() {
                    Type::Base(base) => {
                        return Err(CompileError::new_type_expect_failed(
                            self.input,
                            conv_expr.pos,
                            // TODO: base type is not a problem of this error
                            Type::Ptr(Box::new(Type::Base(base))),
                            Type::Base(base),
                        ));
                    }
                    Type::Ptr(ptr_base) => ptr_base,
                };
                Ok(ConvExpr::new_deref(conv_expr, *base_ty, pos))
            }
            ExprKind::Addr(expr) => Ok(ConvExpr::new_addr(self.down_expr(*expr, lvar_map)?, pos)),
            ExprKind::SizeOf(SizeOfOperandKind::Expr(expr)) => {
                let size = self.down_expr(*expr, lvar_map)?.ty.size_of() as isize;
                Ok(ConvExpr::new_num(size, pos))
            }
            ExprKind::SizeOf(SizeOfOperandKind::Type(type_name)) => {
                let size = type_name.ty().size_of() as isize;
                Ok(ConvExpr::new_num(size, pos))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn down_binary(
        &mut self,
        Binary { kind, lhs, rhs }: Binary,
        pos: Position,
        lvar_map: &mut BTreeMap<String, Lvar>,
    ) -> Result<ConvExpr, CompileError> {
        let mut rhs = self.down_expr(*rhs, lvar_map)?;
        let mut lhs = self.down_expr(*lhs, lvar_map)?;
        let kind = ConvBinOpKind::new(kind).unwrap();
        match kind {
            ConvBinOpKind::Add | ConvBinOpKind::Sub => match (&lhs.ty, &rhs.ty) {
                (Type::Base(lhs_ty), Type::Base(rhs_ty)) => {
                    if lhs_ty != rhs_ty {
                        return Err(CompileError::new_type_error(
                            self.input,
                            lhs,
                            rhs,
                            Some(
                                "incompatible type addition or subtraction is not allowed"
                                    .to_string(),
                            ),
                        ));
                    }
                    let base_ty = *lhs_ty;
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Base(base_ty),
                        pos,
                    ))
                }
                (Type::Base(base), Type::Ptr(ptr_base)) => {
                    if *base != BaseType::Int {
                        return Err(CompileError::new_type_expect_failed(
                            self.input,
                            lhs.pos,
                            Type::Base(BaseType::Int),
                            Type::Base(*base),
                        ));
                    }
                    lhs = ConvExpr::new_binary(
                        ConvBinOpKind::Mul,
                        ConvExpr::new_num(ptr_base.size_of() as isize, pos),
                        lhs.clone(),
                        Type::Base(BaseType::Int),
                        pos,
                    ); // i + p -> sizeof(*p) * i + p
                    let ptr_base = ptr_base.clone();
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Ptr(ptr_base),
                        pos,
                    ))
                }
                (Type::Ptr(ptr_base), Type::Base(base)) => {
                    if *base != BaseType::Int {
                        return Err(CompileError::new_type_expect_failed(
                            self.input,
                            lhs.pos,
                            Type::Base(*base),
                            Type::Base(BaseType::Int),
                        ));
                    }

                    rhs = ConvExpr::new_binary(
                        ConvBinOpKind::Mul,
                        rhs.clone(),
                        ConvExpr::new_num(ptr_base.size_of() as isize, pos),
                        // Type::Ptr(ptr_base.clone()),
                        Type::Base(BaseType::Int),
                        pos,
                    ); // p + i ->  p + i * sizeof(*p)
                    let ptr_base = ptr_base.clone();
                    Ok(ConvExpr::new_binary(
                        kind,
                        lhs,
                        rhs,
                        Type::Ptr(ptr_base),
                        pos,
                    ))
                }
                (Type::Ptr(lhs_base), Type::Ptr(rhs_base))
                    if matches!(kind, ConvBinOpKind::Sub) =>
                {
                    if *lhs_base != *rhs_base {
                        return Err(CompileError::new_type_error(
                            self.input,
                            lhs,
                            rhs,
                            Some("incompatible type on ptr subtraction is not allowed".to_string()),
                        ));
                    }
                    let base_ty = lhs_base.clone();
                    let size_of = ConvExpr::new_num(base_ty.size_of() as isize, pos);
                    let subtracted = ConvExpr::new_binary(
                        ConvBinOpKind::Sub,
                        lhs,
                        rhs,
                        Type::Ptr(base_ty.clone()),
                        pos,
                    );
                    Ok(ConvExpr::new_binary(
                        ConvBinOpKind::Div,
                        subtracted,
                        size_of,
                        Type::Ptr(base_ty),
                        pos,
                    ))
                }
                (Type::Ptr(_), Type::Ptr(_)) => Err(CompileError::new_type_error(
                    self.input,
                    lhs,
                    rhs,
                    Some("ptr + ptr or ptr - ptr is not allowed. ".to_string()),
                )),
            },
            ConvBinOpKind::Mul | ConvBinOpKind::Div => {
                if lhs.ty != rhs.ty {
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some(
                            "incompatible type multiplication or division is not allowed"
                                .to_string(),
                        ),
                    ));
                }
                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(kind, lhs, rhs, lhs_ty, pos))
            }
            ConvBinOpKind::Rem => {
                if lhs.ty != rhs.ty {
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some("incompatible type take remaint is not allowed".to_string()),
                    ));
                }

                let lhs_ty = lhs.ty.clone();
                Ok(ConvExpr::new_binary(kind, lhs, rhs, lhs_ty, pos))
            }
            ConvBinOpKind::Eq | ConvBinOpKind::Le | ConvBinOpKind::Lt | ConvBinOpKind::Ne => {
                if lhs.ty != rhs.ty {
                    return Err(CompileError::new_type_error(
                        self.input,
                        lhs,
                        rhs,
                        Some("incompatible type binary expr is not allowed".to_string()),
                    ));
                }

                Ok(ConvExpr::new_binary(
                    kind,
                    lhs,
                    rhs,
                    Type::Base(BaseType::Int),
                    pos,
                ))
            }
        }
    }

    /// # Panics
    /// always
    pub fn error_at(&self, pos: impl Into<Option<Position>>, msg: &str) -> ! {
        let pos: Option<Position> = pos.into();
        match pos {
            None => panic!("Passed pos info was None.\n{}", msg),
            Some(pos) => {
                let mut splited = self.input.split('\n');
                let line = splited.nth(pos.n_line).unwrap_or_else(|| {
                    panic!(
                        "Position is illegal, pos: {:?},\n input: {}",
                        pos, self.input
                    )
                });
                eprintln!("{}", line);
                let mut buffer = String::with_capacity(pos.n_char + 1);
                for _ in 0..pos.n_char {
                    buffer.push(' ');
                }
                buffer.push('^');
                eprintln!("{}", buffer);
                eprintln!("{}", msg);
                panic!();
            }
        }
    }

    pub fn fetch_lvar(
        &self,
        name: &str,
        pos: Position,
        lvar_map: &mut BTreeMap<String, Lvar>,
    ) -> Result<ConvExpr, CompileError> {
        let lvar = match lvar_map.get(name) {
            Some(lvar) => lvar.clone(),
            None => {
                return Err(CompileError::new(
                    self.input,
                    CompileErrorKind::AnalyzeError(AnalyzeErrorKind::UndeclaredError(
                        name.to_string(),
                        pos,
                        VariableKind::Local,
                    )),
                ))
            }
        };
        let ty = lvar.ty.clone();
        Ok(ConvExpr::new_lvar_raw(lvar, ty, pos))
    }

    /// create first defined variable
    pub fn new_lvar(
        src: &str,
        name: String,
        pos: Position,
        new_offset: &mut usize,
        ty: Type,
        lvar_map: &mut BTreeMap<String, Lvar>,
    ) -> Result<Lvar, CompileError> {
        let offset = if lvar_map.get(&name).is_some() {
            return Err(CompileError::new_redefined_variable(
                src,
                name,
                pos,
                VariableKind::Local,
            ));
        } else {
            *new_offset += ty.size_of();
            lvar_map.insert(name, Lvar::new_raw(*new_offset, ty.clone()));
            *new_offset
        };
        Ok(Lvar::new_raw(offset, ty))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvProgram {
    components: Vec<ConvProgramKind>,
}

impl ConvProgram {
    pub const fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn with_vec(vec: Vec<ConvProgramKind>) -> Self {
        Self { components: vec }
    }

    pub fn push(&mut self, kind: ConvProgramKind) {
        self.components.push(kind);
    }
}

impl IntoIterator for ConvProgram {
    type Item = ConvProgramKind;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvProgramKind {
    Func(ConvFuncDef),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvFuncDef {
    pub ty: Type,
    pub name: String,
    pub args: Vec<Lvar>,
    pub body: ConvStmt,
    pub lvars: BTreeSet<Lvar>,
}

impl ConvFuncDef {
    pub fn new(
        ty: Type,
        name: String,
        args: Vec<Lvar>,
        body: ConvStmt,
        lvars: BTreeSet<Lvar>,
    ) -> Self {
        Self {
            ty,
            name,
            args,
            body,
            lvars,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvStmt {
    pub kind: ConvStmtKind,
}

impl ConvStmt {
    pub const fn new_expr(expr: ConvExpr) -> Self {
        Self {
            kind: ConvStmtKind::Expr(expr),
        }
    }

    pub const fn new_ret(expr: ConvExpr, name: String) -> Self {
        Self {
            kind: ConvStmtKind::Return(expr, name),
        }
    }

    pub fn new_block(stmts: Vec<ConvStmt>) -> Self {
        Self {
            kind: ConvStmtKind::Block(stmts),
        }
    }

    pub fn new_if(cond: ConvExpr, then: ConvStmt, els: Option<ConvStmt>) -> Self {
        Self {
            kind: ConvStmtKind::If(cond, Box::new(then), els.map(Box::new)),
        }
    }

    pub fn new_while(cond: ConvExpr, then: ConvStmt) -> Self {
        Self {
            kind: ConvStmtKind::While(cond, Box::new(then)),
        }
    }

    pub fn new_for(
        init: Option<ConvExpr>,
        cond: Option<ConvExpr>,
        inc: Option<ConvExpr>,
        then: ConvStmt,
    ) -> Self {
        Self {
            kind: ConvStmtKind::For(init, cond, inc, Box::new(then)),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvStmtKind {
    Expr(ConvExpr),
    Return(ConvExpr, String),
    Block(Vec<ConvStmt>),
    If(ConvExpr, Box<ConvStmt>, Option<Box<ConvStmt>>),
    While(ConvExpr, Box<ConvStmt>),
    For(
        Option<ConvExpr>,
        Option<ConvExpr>,
        Option<ConvExpr>,
        Box<ConvStmt>,
    ),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvExpr {
    pub kind: ConvExprKind,
    pub ty: Type,
    pub pos: Position,
}
impl ConvExpr {
    pub fn new_binary(
        kind: ConvBinOpKind,
        lhs: ConvExpr,
        rhs: ConvExpr,
        ty: Type,
        pos: Position,
    ) -> Self {
        Self {
            kind: ConvExprKind::Binary(ConvBinary::new(kind, Box::new(lhs), Box::new(rhs))),
            ty,
            pos,
        }
    }

    pub fn new_func(name: String, args: Vec<ConvExpr>, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Func(name, args),
            ty: Type::Base(BaseType::Int), // TODO: change this by reading function definition
            pos,
        }
    }

    pub const fn new_num(num: isize, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Num(num),
            ty: Type::Base(BaseType::Int),
            pos,
        }
    }

    pub fn new_assign(lhs: ConvExpr, rhs: ConvExpr, pos: Position) -> Self {
        let ty = lhs.ty.clone();
        ConvExpr {
            kind: ConvExprKind::Assign(Box::new(lhs), Box::new(rhs)),
            ty,
            pos,
        }
    }

    pub const fn new_lvar_raw(lvar: Lvar, ty: Type, pos: Position) -> Self {
        ConvExpr {
            kind: ConvExprKind::Lvar(lvar),
            ty,
            pos,
        }
    }

    pub fn new_deref(expr: ConvExpr, base_ty: Type, pos: Position) -> Self {
        Self {
            kind: ConvExprKind::Deref(Box::new(expr)),
            ty: base_ty,
            pos,
        }
    }

    pub fn new_addr(expr: ConvExpr, pos: Position) -> Self {
        let ty = expr.ty.clone();
        Self {
            kind: ConvExprKind::Addr(Box::new(expr)),
            ty: Type::Ptr(Box::new(ty)),
            pos,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvExprKind {
    Binary(ConvBinary),
    Num(isize),
    Lvar(Lvar),
    Assign(Box<ConvExpr>, Box<ConvExpr>),
    Func(String, Vec<ConvExpr>),
    Deref(Box<ConvExpr>),
    Addr(Box<ConvExpr>),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Lvar {
    /// Used like `mov rax, [rbp - offset]`
    pub offset: usize,
    pub ty: Type,
}

impl Lvar {
    pub const fn new_raw(offset: usize, ty: Type) -> Self {
        Self { offset, ty }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Base(BaseType),
    Ptr(Box<Type>),
}

impl Type {
    pub const fn size_of(&self) -> usize {
        match self {
            Type::Base(BaseType::Int) => 4,
            Type::Ptr(_) => 8,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Debug)]
pub enum BaseType {
    Int,
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ConvBinary {
    pub kind: ConvBinOpKind,
    pub lhs: Box<ConvExpr>,
    pub rhs: Box<ConvExpr>,
}

impl ConvBinary {
    pub fn new(kind: ConvBinOpKind, lhs: Box<ConvExpr>, rhs: Box<ConvExpr>) -> Self {
        Self { kind, lhs, rhs }
    }
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ConvBinOpKind {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (remains)
    Rem,
    /// The `==` operator (equality)
    Eq,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `<` operator (less than)
    Lt,
    /// The `!=` operator (Not equal to)
    Ne,
}

impl ConvBinOpKind {
    pub const fn new(kind: BinOpKind) -> Option<Self> {
        match kind {
            BinOpKind::Add => Some(ConvBinOpKind::Add),
            BinOpKind::Sub => Some(ConvBinOpKind::Sub),
            BinOpKind::Mul => Some(ConvBinOpKind::Mul),
            BinOpKind::Div => Some(ConvBinOpKind::Div),
            BinOpKind::Rem => Some(ConvBinOpKind::Rem),
            BinOpKind::Eq => Some(ConvBinOpKind::Eq),
            BinOpKind::Le => Some(ConvBinOpKind::Le),
            BinOpKind::Lt => Some(ConvBinOpKind::Lt),
            BinOpKind::Ge | BinOpKind::Gt => None,
            BinOpKind::Ne => Some(ConvBinOpKind::Ne),
        }
    }
}
