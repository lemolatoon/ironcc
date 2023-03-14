use crate::{
    analyze::{
        analyze::Analyzer,
        types::{BaseType, InCompleteKind, Type},
    },
    error::{CompileError, CompileErrorKind, ParseErrorKind},
    tokenize::{
        debug_infos::DebugInfo,
        tokenize::{
            AssignBinOpToken, BinOpToken, DelimToken, Token, TokenKind, TokenStream, TypeToken,
        },
    },
    unimplemented_err,
};
use std::fmt::Debug;

use super::{
    declaration::{
        Declaration, DeclarationSpecifier, Declarator, DirectDeclarator, Initializer,
        StorageClassSpecifier, StructDeclaration, StructOrUnionSpec, TypeSpecifier,
    },
    expr::{BinOpKind, Expr, UnaryOp},
    parser_context::{ParserContext, ParserContextKind},
    scope::Scope,
    stmt::Stmt,
};

pub struct Parser {
    scope: Scope,
    pub context: ParserContext,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            scope: Scope::new(),
            context: ParserContext::new(),
        }
    }
    pub fn parse_program<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Program, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut program = Program::new();
        while !tokens.at_eof() {
            if tokens.consume(&TokenKind::Asm) {
                tokens.expect(&TokenKind::OpenDelim(DelimToken::Paren))?;
                let token = tokens.next().ok_or_else(|| {
                    CompileError::new_unexpected_eof(None, Box::new("TokenKind::Str(_)"))
                })?;
                if let TokenKind::Str(string) = *token.kind {
                    let debug_info = token.debug_info;
                    let string = Self::parse_string_literal(string, tokens)?;
                    tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
                    tokens.expect(&TokenKind::Semi)?;
                    program.push(ProgramComponent::new(
                        ProgramKind::InlineAsm(string),
                        debug_info,
                    ));
                } else {
                    return Err(CompileError::new_expected_failed(
                        Box::new("TokenKind::Str(_)"),
                        token,
                    ));
                }
                continue;
            }
            let mut tmp_tokens = tokens.clone();
            let component = {
                let declaration = self.parse_declaration(&mut tmp_tokens, true)?;
                if Some(TokenKind::Semi) == tmp_tokens.peek_kind() {
                    tmp_tokens.expect(&TokenKind::Semi).unwrap();
                    *tokens = tmp_tokens;
                    let debug_info = declaration.debug_info.clone();
                    ProgramComponent::new(ProgramKind::Declaration(declaration), debug_info)
                } else {
                    self.parse_func_def(tokens)?
                }
            };
            program.push(component);
        }
        tokens.expect(&TokenKind::Eof)?;
        assert!(tokens.next().is_none());
        Ok(program)
    }

    pub fn parse_func_def<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<ProgramComponent, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let (type_spec, debug_info) = self.parse_type_specifier(tokens, true)?;
        let n_star = Self::parse_pointer(tokens)?;
        let direct_declarator = self.parse_direct_declarator(tokens)?;
        // have to be block stmt
        tokens.expect(&TokenKind::OpenDelim(DelimToken::Brace))?;
        self.scope.scope_push();
        let mut stmts = Vec::new();
        while !tokens.consume(&TokenKind::CloseDelim(DelimToken::Brace)) {
            stmts.push(self.parse_stmt(tokens)?);
        }
        self.scope.scope_pop();
        let body = Stmt::new_block(stmts, debug_info.clone());
        let kind = ProgramKind::new_funcdef(type_spec, n_star, direct_declarator, body);
        Ok(ProgramComponent::new(kind, debug_info))
    }

    /// arg: `consider_typedef_specifier` should be true when calling out of this function.
    pub fn parse_declaration<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
        consider_typedef_specifier: bool, // default: true
    ) -> Result<Declaration, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut tmp_tokens = tokens.clone();
        // TODO: todo!("impl declaration specifiers");
        // <declaration-specifiers> := <type-specifiers>
        // first element of direct declarator is Ident
        let (declaration_specifiers, debug_info) =
            self.parse_declaration_specifiers(&mut tmp_tokens, consider_typedef_specifier)?;

        // <pointer>*
        let n_star = Self::parse_pointer(&mut tmp_tokens)?;
        if tmp_tokens.peek_expect(&TokenKind::Semi) {
            *tokens = tmp_tokens;
            // InitDeclarator is None
            return Ok(Declaration {
                declaration_specifiers,
                init_declarator: None,
                debug_info,
            });
        }
        let direct_declarator = self.parse_direct_declarator(&mut tmp_tokens);
        let direct_declarator = if matches!(
            &direct_declarator,
            &Err(CompileError {
                kind: CompileErrorKind::ParseError(ParseErrorKind::IdentExpectFailed { got: _ }),
            }),
        ) {
            // redefine typeof ident
            // such as...
            // typedef int int2;
            // { typedef char int2; }
            let mut has_typedef = false;
            let mut has_only_one_typedefed_typespecifier = false;
            for specifier in &declaration_specifiers {
                if DeclarationSpecifier::StorageClass(StorageClassSpecifier::Typedef) == *specifier
                {
                    has_typedef = true;
                } else if let DeclarationSpecifier::Type(TypeSpecifier::TypeDefName(_)) = specifier
                {
                    if has_only_one_typedefed_typespecifier {
                        has_only_one_typedefed_typespecifier = false;
                        break;
                    }
                    has_only_one_typedefed_typespecifier = true;
                }
            }
            if has_typedef && has_only_one_typedefed_typespecifier {
                return self.parse_declaration(tokens, false);
            }
            direct_declarator?; // return Err

            unreachable!()
        } else {
            direct_declarator.unwrap()
        };
        *tokens = tmp_tokens;
        let init = if tokens.consume(&TokenKind::BinOpEq(AssignBinOpToken::Eq)) {
            Some(self.parse_initializer(tokens)?)
        } else {
            None
        };

        if !declaration_specifiers.contains(&DeclarationSpecifier::StorageClass(
            StorageClassSpecifier::Typedef,
        )) {
            return Ok(Declaration::new(
                declaration_specifiers,
                n_star,
                direct_declarator,
                init,
                debug_info,
            ));
        }

        // typedef declaration

        let ident_name = direct_declarator.ident_name().to_string();

        let declaration = Declaration::new(
            declaration_specifiers,
            n_star,
            direct_declarator,
            init,
            debug_info.clone(),
        );

        // register type map
        let ty = declaration.ty(&mut Analyzer::new_for_parser(), debug_info.clone())?;
        self.scope
            .register_typedef_name(ident_name, ty, debug_info)?;

        Ok(declaration)
    }

    pub fn parse_declarator<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Declarator, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        Ok(Declarator::new(
            Self::parse_pointer(tokens)?,
            self.parse_direct_declarator(tokens)?,
        ))
    }

    pub fn parse_direct_declarator<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<DirectDeclarator, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut direct_declarator = if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paren)) {
            // "(" <declaration> ")"
            let direct_declarator =
                DirectDeclarator::Declarator(Box::new(self.parse_declarator(tokens)?));
            tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
            direct_declarator
        } else {
            // <ident>
            DirectDeclarator::Ident(tokens.consume_ident()?.0)
        };
        loop {
            if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paren)) {
                // function declaration
                if tokens.consume(&TokenKind::CloseDelim(DelimToken::Paren)) {
                    // e.g) `main()`
                    // no arg func should be considered as a flexible arg function
                    direct_declarator =
                        DirectDeclarator::Func(Box::new(direct_declarator), Vec::new(), true);
                    continue;
                } else if tokens.peek_expect(&TokenKind::Type(TypeToken::Void)) {
                    let mut tmp_tokens = tokens.clone();
                    tmp_tokens.next();
                    if tmp_tokens.consume(&TokenKind::CloseDelim(DelimToken::Paren)) {
                        // no arg func with void should be considered as a **not** flexible arg function
                        *tokens = tmp_tokens;
                        direct_declarator =
                            DirectDeclarator::Func(Box::new(direct_declarator), Vec::new(), false);
                        continue;
                    }

                    // not flexible arg function
                }
                let mut args = Vec::new();
                args.push(self.parse_declaration(tokens, true)?);
                let mut is_flexible = false;
                while tokens.consume(&TokenKind::Comma) {
                    if tokens.consume(&TokenKind::DotDotDot) {
                        is_flexible = true;
                        break;
                    }
                    args.push(self.parse_declaration(tokens, true)?);
                }
                direct_declarator =
                    DirectDeclarator::Func(Box::new(direct_declarator), args, is_flexible);
                tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
            } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Bracket)) {
                if tokens.consume(&TokenKind::CloseDelim(DelimToken::Bracket)) {
                    direct_declarator = DirectDeclarator::Array(Box::new(direct_declarator), None);
                } else {
                    let expr = self.parse_assign(tokens)?;
                    direct_declarator =
                        DirectDeclarator::Array(Box::new(direct_declarator), Some(expr));
                    tokens.expect(&TokenKind::CloseDelim(DelimToken::Bracket))?;
                }
            } else {
                break;
            }
        }
        Ok(direct_declarator)
    }

    pub fn parse_initializer<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Initializer, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
            let init_list = self.parse_initializer_list(tokens)?;
            tokens.consume(&TokenKind::Comma);
            tokens.expect(&TokenKind::CloseDelim(DelimToken::Brace))?;
            Ok(Initializer::Array(init_list))
        } else {
            self.context
                .push_ctx(ParserContextKind::TopFuncArgs, false)?;
            let expr = self.parse_assign(tokens)?;
            self.context.pop(ParserContextKind::TopFuncArgs)?;
            Ok(Initializer::Expr(expr))
        }
    }

    pub fn parse_initializer_list<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Vec<Initializer>, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut init_list = Vec::new();
        init_list.push(self.parse_initializer(tokens)?);
        while tokens.peek_expect(&TokenKind::Comma) {
            let mut tmp_tokens = tokens.clone();
            tmp_tokens.next();
            if tmp_tokens.peek_expect(&TokenKind::CloseDelim(DelimToken::Brace)) {
                // <initializer> := "{" <initializer-list> , "}""
                break;
            }
            tokens.next(); // -> ","
            init_list.push(self.parse_initializer(tokens)?);
        }
        Ok(init_list)
    }

    pub fn parse_type_specifier<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
        consider_typedef_specifier: bool,
    ) -> Result<(TypeSpecifier, DebugInfo), CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let peeked = tokens.peek().cloned();
        match peeked {
            Some(Token { kind, debug_info }) => match *kind {
                TokenKind::Type(TypeToken::Int) => {
                    tokens.next();
                    Ok((TypeSpecifier::Int, debug_info))
                }
                TokenKind::Type(TypeToken::Char) => {
                    tokens.next();
                    Ok((TypeSpecifier::Char, debug_info))
                }
                TokenKind::Type(TypeToken::Void) => {
                    tokens.next();
                    Ok((TypeSpecifier::Void, debug_info))
                }
                TokenKind::Struct => {
                    tokens.next();
                    Ok((
                        TypeSpecifier::StructOrUnion(self.parse_struct_or_union_specifier(tokens)?),
                        debug_info,
                    ))
                }
                TokenKind::Enum => {
                    tokens.next();
                    Ok((
                        TypeSpecifier::Enum(Self::parse_enum_specifier(tokens)?),
                        debug_info,
                    ))
                }
                TokenKind::Ident(ident) if consider_typedef_specifier => {
                    if let Some(ty) = self.scope.look_up_typedef_name(&ident) {
                        tokens.next();
                        Ok((TypeSpecifier::TypeDefName(ty), debug_info))
                    } else {
                        Err(unimplemented_err!(
                            debug_info,
                            format!("unknown type specifier: {}", ident)
                        ))
                    }
                }
                _ => Err(CompileError::new_expected_failed(
                    Box::new("TokenKind::Type(_) | TokenKind::Struct".to_string()),
                    Token::new(*kind, debug_info),
                )),
            },
            None => Err(CompileError::new_unexpected_eof(
                None,
                Box::new("ToKenKind::Type(_)"),
            )),
        }
    }

    pub fn parse_declaration_specifiers<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
        consider_typedef_specifier: bool,
    ) -> Result<(Vec<DeclarationSpecifier>, DebugInfo), CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut specifiers = Vec::new();
        let mut first_debug_info = None;
        loop {
            let ty_spec = self.parse_type_specifier(tokens, consider_typedef_specifier);
            if let Ok((ty_spec, debug_info)) = ty_spec {
                first_debug_info.get_or_insert(debug_info);
                specifiers.push(DeclarationSpecifier::Type(ty_spec));
                continue;
            }

            let storage_class = Self::parse_storage_class_specifier(tokens);
            if let Ok((storage_class_specifier, debug_info)) = storage_class {
                first_debug_info.get_or_insert(debug_info);
                specifiers.push(DeclarationSpecifier::StorageClass(storage_class_specifier));
                continue;
            }
            break;
        }
        first_debug_info.map_or_else(
            || {
                tokens.peek_debug_info().map_or_else(
                    || {
                        Err(CompileError::new_unexpected_eof(
                            None,
                            Box::new("declaration specifier expected, but got EOF."),
                        ))
                    },
                    |debug_info| {
                        Err(unimplemented_err!(
                            debug_info,
                            "At least one declaration specifier required."
                        ))
                    },
                )
            },
            |first_debug_info| Ok((specifiers, first_debug_info)),
        )
    }

    pub fn parse_storage_class_specifier<I>(
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<(StorageClassSpecifier, DebugInfo), CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let peeked = tokens.peek().cloned();
        match peeked {
            Some(Token { kind, debug_info }) => match *kind {
                TokenKind::TypeDef => {
                    tokens.next();
                    Ok((StorageClassSpecifier::Typedef, debug_info))
                }
                TokenKind::Extern => {
                    tokens.next();
                    Ok((StorageClassSpecifier::Extern, debug_info))
                }
                TokenKind::Static => {
                    tokens.next();
                    Ok((StorageClassSpecifier::Static, debug_info))
                }
                _ => Err(CompileError::new_expected_failed(
                    Box::new("TokenKind::Typedef".to_string()),
                    Token::new(*kind, debug_info),
                )),
            },
            None => Err(CompileError::new_unexpected_eof(
                None,
                Box::new("ToKenKind::Type(_)"),
            )),
        }
    }
    pub fn parse_enum_specifier<I>(
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<EnumSpec, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let peeked = if let Some(peeked) = tokens.peek_kind() {
            peeked
        } else {
            return Err(CompileError::new_unexpected_eof(
                None,
                Box::new("TokenKind::Ident".to_string()),
            ));
        };
        if matches!(peeked, TokenKind::Ident(_)) {
            // with tag
            let (name, _) = tokens.consume_ident()?;
            if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
                // <enumerator-list>
                let list = Self::parse_enumerator_list(tokens)?;
                tokens.expect(&TokenKind::CloseDelim(DelimToken::Brace))?;
                return Ok(EnumSpec::WithList(Some(name), list));
            }
            return Ok(EnumSpec::WithTag(name));
        } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
            // <enumerator-list>
            let list = Self::parse_enumerator_list(tokens)?;
            tokens.expect(&TokenKind::CloseDelim(DelimToken::Brace))?;
            return Ok(EnumSpec::WithList(None, list));
        }

        match tokens.peek() {
            Some(token) => Err(CompileError::new_expected_failed(
                Box::new("TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Brace)"),
                token.clone(),
            )),
            None => Err(CompileError::new_unexpected_eof(
                None,
                Box::new("TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Brace)"),
            )),
        }
    }

    pub fn parse_enumerator_list<I>(
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Vec<EnumConstant>, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut list = Vec::new();
        loop {
            if matches!(tokens.peek_kind(), Some(TokenKind::Ident(_))) {
                let (ident, debug_info) = tokens.consume_ident().unwrap();
                list.push(EnumConstant { ident, debug_info });
                if !tokens.consume(&TokenKind::Comma) {
                    break;
                }
                if tokens.peek_expect(&TokenKind::CloseDelim(DelimToken::Brace)) {
                    break;
                }

                if tokens.peek_expect(&TokenKind::Comma) {
                    let mut tmp_tokens = tokens.clone();
                    tmp_tokens.next();
                    if Some(TokenKind::CloseDelim(DelimToken::Brace)) == tmp_tokens.peek_kind() {
                        break;
                    }
                    continue;
                }
            } else {
                if tokens.peek_expect(&TokenKind::Comma)
                    || tokens.peek_expect(&TokenKind::CloseDelim(DelimToken::Brace))
                {
                    break;
                }
                if let Some(token) = tokens.next() {
                    return Err(CompileError::new_expected_failed(
                        Box::new("TokenKind::Ident(_)"),
                        token,
                    ));
                }
                return Err(CompileError::new_unexpected_eof(
                    None,
                    Box::new("TokenKind::Ident(_)"),
                ));
            };
        }
        if !tokens.peek_expect(&TokenKind::CloseDelim(DelimToken::Brace)) {
            return Err(CompileError::new_expected_failed(
                Box::new(TokenKind::CloseDelim(DelimToken::Brace)),
                tokens.next().unwrap(),
            ));
        }
        Ok(list)
    }

    pub fn parse_struct_or_union_specifier<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<StructOrUnionSpec, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        if let Ok((name, _)) = tokens.consume_ident() {
            if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
                // <struct-declaration-list>
                let list = self.parse_struct_declaration_list(tokens)?;
                tokens.expect(&TokenKind::CloseDelim(DelimToken::Brace))?;
                return Ok(StructOrUnionSpec::WithList(Some(name), list));
            }
            return Ok(StructOrUnionSpec::WithTag(name));
        } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
            // <struct-declaration-list>
            let list = self.parse_struct_declaration_list(tokens)?;
            tokens.expect(&TokenKind::CloseDelim(DelimToken::Brace))?;
            return Ok(StructOrUnionSpec::WithList(None, list));
        }

        match tokens.peek() {
            Some(token) => Err(CompileError::new_expected_failed(
                Box::new("TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Brace)"),
                token.clone(),
            )),
            None => Err(CompileError::new_unexpected_eof(
                None,
                Box::new("TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Brace)"),
            )),
        }
    }
    pub fn parse_struct_declaration_list<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Vec<StructDeclaration>, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let (ty_spec, debug_info) = self.parse_type_specifier(tokens, true)?;
        let ty_spec = vec![ty_spec];
        let declarator = self.parse_declarator(tokens)?;
        let mut list = vec![StructDeclaration {
            debug_info,
            ty_spec,
            declarator,
        }];
        tokens.expect(&TokenKind::Semi)?;
        while let Ok((ty_spec, debug_info)) = self.parse_type_specifier(tokens, true) {
            let ty_spec = vec![ty_spec];
            let declarator = self.parse_declarator(tokens)?;
            list.push(StructDeclaration {
                debug_info,
                ty_spec,
                declarator,
            });
            tokens.expect(&TokenKind::Semi)?;
        }
        Ok(list)
    }

    pub fn parse_pointer<I>(tokens: &mut TokenStream<I, TokenKind>) -> Result<usize, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut n_star = 0;
        while tokens.consume(&TokenKind::BinOp(BinOpToken::Star)) {
            n_star += 1;
        }
        Ok(n_star)
    }

    #[allow(clippy::too_many_lines)]
    pub fn parse_stmt<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Stmt, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let stmt = if tokens.consume(&TokenKind::Return) {
            // return stmt
            let debug_info = tokens
                .peek_debug_info()
                .ok_or_else(|| CompileError::new_unexpected_eof(None, Box::new(TokenKind::Semi)))?;
            if tokens.consume(&TokenKind::Semi) {
                Ok(Stmt::ret(None, debug_info))
            } else {
                let returning_expr = self.parse_expr_with_new_context(tokens, false)?;
                tokens.expect(&TokenKind::Semi)?;
                Ok(Stmt::ret(Some(returning_expr), debug_info))
            }
        } else if tokens.consume(&TokenKind::If) {
            let debug_info = tokens.expect(&TokenKind::OpenDelim(DelimToken::Paren))?;
            let conditional_expr = self.parse_expr_with_new_context(tokens, false)?;
            tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
            let then_stmt = self.parse_stmt(tokens)?;
            let else_stmt = if tokens.consume(&TokenKind::Else) {
                Some(self.parse_stmt(tokens)?)
            } else {
                None
            };
            Ok(Stmt::new_if(
                conditional_expr,
                then_stmt,
                else_stmt,
                debug_info,
            ))
        } else if tokens.consume(&TokenKind::While) {
            let debug_info = tokens.expect(&TokenKind::OpenDelim(DelimToken::Paren))?;
            let conditional_expr = self.parse_expr_with_new_context(tokens, false)?;
            tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
            let then_stmt = self.parse_stmt(tokens)?;
            Ok(Stmt::new_while(conditional_expr, then_stmt, debug_info))
        } else if tokens.consume(&TokenKind::For) {
            let debug_info = tokens.expect(&TokenKind::OpenDelim(DelimToken::Paren))?;
            self.scope.scope_push();
            let init_expr = if tokens.consume(&TokenKind::Semi) {
                None
            } else {
                Some(if tokens.is_starting_declaration(&self.scope) {
                    let declaration = self.parse_declaration(tokens, true)?;
                    tokens.expect(&TokenKind::Semi)?;
                    ForInitKind::Declaration(declaration)
                } else {
                    let expr = self.parse_expr_with_new_context(tokens, false)?;
                    tokens.expect(&TokenKind::Semi)?;
                    ForInitKind::Expr(expr)
                })
            };
            let cond_expr = if tokens.consume(&TokenKind::Semi) {
                None
            } else {
                let expr = self.parse_expr_with_new_context(tokens, false)?;
                tokens.expect(&TokenKind::Semi)?;
                Some(expr)
            };
            let inc_expr = if tokens.consume(&TokenKind::CloseDelim(DelimToken::Paren)) {
                None
            } else {
                let expr = self.parse_expr_with_new_context(tokens, false)?;
                tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
                Some(expr)
            };
            let then_stmt = self.parse_stmt(tokens)?;
            self.scope.scope_pop();
            Ok(Stmt::new_for(
                init_expr, cond_expr, inc_expr, then_stmt, debug_info,
            ))
        } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Brace)) {
            self.scope.scope_push();
            let mut stmts = Vec::new();
            let debug_info = tokens
                .peek_debug_info()
                .ok_or_else(|| CompileError::new_unexpected_eof(None, Box::new("statement")))?;
            while !tokens.consume(&TokenKind::CloseDelim(DelimToken::Brace)) {
                stmts.push(self.parse_stmt(tokens)?);
            }
            self.scope.scope_pop();
            Ok(Stmt::new_block(stmts, debug_info))
        } else if tokens.consume(&TokenKind::Switch) {
            let debug_info = tokens.expect(&TokenKind::OpenDelim(DelimToken::Paren))?;
            let expr = self.parse_expr_with_new_context(tokens, false)?;
            tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
            let stmt = self.parse_stmt(tokens)?;
            Ok(Stmt::new_switch(expr, stmt, debug_info))
        } else if tokens.consume(&TokenKind::Case) {
            let debug_info = tokens
                .peek_debug_info()
                .ok_or_else(|| CompileError::new_unexpected_eof(None, Box::new("Expr")))?;
            let expr = self.parse_expr_with_new_context(tokens, false)?;
            tokens.expect(&TokenKind::Colon)?;
            let stmt = self.parse_stmt(tokens)?;
            Ok(Stmt::new_labeled_stmt(
                LabelKind::Case(expr),
                stmt,
                debug_info,
            ))
        } else if tokens.consume(&TokenKind::Default) {
            let debug_info = tokens.expect(&TokenKind::Colon)?;
            let stmt = self.parse_stmt(tokens)?;
            Ok(Stmt::new_labeled_stmt(
                LabelKind::Default(debug_info.clone()),
                stmt,
                debug_info,
            ))
        } else if tokens.consume(&TokenKind::Break) {
            let debug_info = tokens.peek_debug_info().unwrap();
            tokens.expect(&TokenKind::Semi)?;
            Ok(Stmt {
                kind: StmtKind::Break,
                debug_info,
            })
        } else if tokens.consume(&TokenKind::Continue) {
            let debug_info = tokens.peek_debug_info().unwrap();
            tokens.expect(&TokenKind::Semi)?;
            Ok(Stmt {
                kind: StmtKind::Continue,
                debug_info,
            })
        } else if tokens.is_starting_declaration(&self.scope) {
            let debug_info = tokens
                .peek_debug_info()
                .ok_or_else(|| CompileError::new_unexpected_eof(None, Box::new("Declaration")))?;
            let stmt = Stmt::new_declare(self.parse_declaration(tokens, true)?, debug_info);
            tokens.expect(&TokenKind::Semi)?;
            Ok(stmt)
        } else {
            let debug_info = tokens
                .peek_debug_info()
                .ok_or_else(|| CompileError::new_unexpected_eof(None, Box::new("Expr")))?;
            let expr = self.parse_expr_with_new_context(tokens, false)?;
            tokens.expect(&TokenKind::Semi)?;
            Ok(Stmt::expr(expr, debug_info))
        };
        stmt
    }

    pub fn parse_expr_with_new_context<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
        in_top_func_args: bool,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        self.context
            .push_ctx(ParserContextKind::TopFuncArgs, in_top_func_args)?;
        let expr = self.parse_expr(tokens)?;
        self.context.pop(ParserContextKind::TopFuncArgs)?;
        Ok(expr)
    }
    pub fn parse_expr<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut expr = self.parse_assign(tokens)?;
        if self
            .context
            .in_top_func_args(ParserContextKind::TopFuncArgs)?
        {
            return Ok(expr);
        }
        while tokens.consume(&TokenKind::Comma) {
            let debug_info = expr.debug_info.clone();
            expr = Expr::new_comma(expr, self.parse_assign(tokens)?, debug_info);
        }
        Ok(expr)
    }

    pub fn parse_assign<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let lhs = self.parse_conditional(tokens)?;
        let (kind, debug_info) = match tokens.peek() {
            Some(Token { kind, debug_info }) => (kind, debug_info.clone()),
            None => return Err(CompileError::new_unexpected_eof(None, Box::new("Token"))),
        };
        match **kind {
            TokenKind::BinOpEq(assign_token) => {
                tokens.next();
                Ok(Expr::new_assign(
                    lhs,
                    self.parse_assign(tokens)?,
                    assign_token,
                    debug_info,
                ))
            }
            _ => Ok(lhs),
        }
    }

    pub fn parse_conditional<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let cond = self.parse_logical_or(tokens)?;
        if tokens.consume(&TokenKind::Question) {
            let then_expr = self.parse_expr(tokens)?;
            tokens.expect(&TokenKind::Colon)?;
            let else_expr = self.parse_conditional(tokens)?;
            let cond_debug_info = cond.debug_info.clone();
            Ok(Expr::new_conditional(
                cond,
                then_expr,
                else_expr,
                cond_debug_info,
            ))
        } else {
            Ok(cond)
        }
    }
    pub fn parse_logical_or<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_logical_and(tokens)?;
        while tokens.consume(&TokenKind::BinOp(BinOpToken::VerticalVertical)) {
            let rhs = self.parse_logical_and(tokens)?;
            let debug_info = lhs.debug_info.clone();
            lhs = Expr::new_binary(BinOpKind::LogicalOr, lhs, rhs, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_logical_and<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_bit_wise_and(tokens)?;
        while tokens.consume(&TokenKind::BinOp(BinOpToken::AndAnd)) {
            let rhs = self.parse_bit_wise_and(tokens)?;
            let debug_info = lhs.debug_info.clone();
            lhs = Expr::new_binary(BinOpKind::LogicalAnd, lhs, rhs, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_bit_wise_and<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_equality(tokens)?;
        while let Some(Token { kind, debug_info }) = tokens.peek() {
            let debug_info = debug_info.clone();
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::And) => BinOpKind::BitWiseAnd,
                _ => break,
            };
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_equality(tokens)?, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_equality<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_relational(tokens)?;
        while let Some(Token { kind, debug_info }) = tokens.peek() {
            let debug_info = debug_info.clone();
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::EqEq) => BinOpKind::Eq,
                TokenKind::BinOp(BinOpToken::Ne) => BinOpKind::Ne,
                _ => break,
            };
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_relational(tokens)?, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_relational<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_shift(tokens)?;
        while let Some(Token { kind, debug_info }) = tokens.peek() {
            let debug_info = debug_info.clone();
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::Lt) => BinOpKind::Lt,
                TokenKind::BinOp(BinOpToken::Le) => BinOpKind::Le,
                TokenKind::BinOp(BinOpToken::Gt) => BinOpKind::Gt,
                TokenKind::BinOp(BinOpToken::Ge) => BinOpKind::Ge,
                _ => break,
            };
            let debug_info = debug_info;
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_shift(tokens)?, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_shift<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_add(tokens)?;
        while let Some(Token { kind, debug_info }) = tokens.peek() {
            let debug_info = debug_info.clone();
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::LShift) => BinOpKind::LShift,
                TokenKind::BinOp(BinOpToken::RShift) => BinOpKind::RShift,
                _ => break,
            };
            let debug_info = debug_info;
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_add(tokens)?, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_add<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_mul(tokens)?;
        while let Some(Token { kind, debug_info }) = tokens.peek() {
            let debug_info = debug_info.clone();
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::Plus) => BinOpKind::Add,
                TokenKind::BinOp(BinOpToken::Minus) => BinOpKind::Sub,
                _ => break,
            };
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_mul(tokens)?, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_mul<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut lhs = self.parse_unary(tokens)?;
        while let Some(Token { kind, debug_info }) = tokens.peek() {
            let debug_info = debug_info.clone();
            let op = match &**kind {
                TokenKind::BinOp(BinOpToken::Star) => BinOpKind::Mul,
                TokenKind::BinOp(BinOpToken::Slash) => BinOpKind::Div,
                TokenKind::BinOp(BinOpToken::Percent) => BinOpKind::Rem,
                _ => break,
            };
            let debug_info = debug_info;
            tokens.next();
            lhs = Expr::new_binary(op, lhs, self.parse_unary(tokens)?, debug_info);
        }
        Ok(lhs)
    }

    pub fn parse_unary<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let (kind, debug_info) = match tokens.peek() {
            Some(Token { kind, debug_info }) => (kind, debug_info),
            None => return Err(CompileError::new_unexpected_eof(None, Box::new("Token"))),
        };
        let debug_info = debug_info.clone();
        Ok(match **kind {
            TokenKind::BinOp(BinOpToken::Plus) => {
                tokens.next();
                Expr::new_unary(UnaryOp::Plus, self.parse_unary(tokens)?, debug_info)
            }
            TokenKind::BinOp(BinOpToken::Minus) => {
                tokens.next();
                Expr::new_unary(UnaryOp::Minus, self.parse_unary(tokens)?, debug_info)
            }
            TokenKind::Tilde => {
                tokens.next();
                Expr::new_unary(UnaryOp::BitInvert, self.parse_unary(tokens)?, debug_info)
            }
            TokenKind::Exclamation => {
                tokens.next();
                Expr::new_unary(UnaryOp::LogicalNot, self.parse_unary(tokens)?, debug_info)
            }
            TokenKind::BinOp(BinOpToken::Star) => {
                tokens.next();
                Expr::new_deref(self.parse_unary(tokens)?, debug_info)
            }
            TokenKind::BinOp(BinOpToken::And) => {
                tokens.next();
                Expr::new_addr(self.parse_unary(tokens)?, debug_info)
            }
            TokenKind::SizeOf => {
                tokens.next();
                let mut tmp_tokens = tokens.clone();
                if let (
                    Some(TokenKind::OpenDelim(DelimToken::Paren)),
                    Some(TokenKind::Type(_) | TokenKind::Struct | TokenKind::Ident(_)),
                ) = (
                    tmp_tokens.next().map(|token| *token.kind()),
                    tmp_tokens.next().map(|token| *token.kind()),
                ) {
                    // e.g) sizeof(int)
                    let mut tmp_tokens = tokens.clone();
                    tmp_tokens.next(); // -> TokenKind::OpenDelim(DelimToken::Paran))
                    if let Ok(type_name) = self.parse_type_name(&mut tmp_tokens) {
                        *tokens = tmp_tokens;
                        let expr = Expr::new_type_sizeof(type_name, debug_info);
                        tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
                        expr
                    } else {
                        let expr = Expr::new_expr_sizeof(self.parse_unary(tokens)?, debug_info);
                        expr
                    }
                } else {
                    // e.g) sizeof (5)
                    Expr::new_expr_sizeof(self.parse_unary(tokens)?, debug_info)
                }
            }
            TokenKind::PlusPlus => {
                tokens.next();
                Expr::new_unary(UnaryOp::Increment, self.parse_unary(tokens)?, debug_info)
            }
            TokenKind::MinusMinus => {
                tokens.next();
                Expr::new_unary(UnaryOp::Decrement, self.parse_unary(tokens)?, debug_info)
            }
            _ => self.parse_postfix(tokens)?,
        })
    }

    pub fn parse_postfix<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let mut expr = self.parse_primary(tokens)?;
        let mut debug_info = expr.debug_info.clone();

        loop {
            match tokens.peek_kind() {
                Some(TokenKind::OpenDelim(DelimToken::Bracket)) => {
                    tokens.next();
                    expr = Expr::new_array(expr, self.parse_expr(tokens)?, debug_info);
                    debug_info = expr.debug_info.clone();
                    tokens.expect(&TokenKind::CloseDelim(DelimToken::Bracket))?;
                }
                Some(TokenKind::Dot) => {
                    tokens.next();
                    let (member, debug_info) = tokens.consume_ident()?;
                    expr = Expr::new_member(expr, member, debug_info);
                }
                Some(TokenKind::Arrow) => {
                    tokens.next();
                    let (member, debug_info) = tokens.consume_ident()?;
                    expr = Expr::new_arrow(expr, member, debug_info);
                }
                Some(TokenKind::PlusPlus) => {
                    tokens.next();
                    expr = Expr::new_postfix_increment(expr, debug_info.clone());
                }
                Some(TokenKind::MinusMinus) => {
                    tokens.next();
                    expr = Expr::new_postfix_decrement(expr, debug_info.clone());
                }
                _ => break,
            };
        }
        Ok(expr)
    }

    pub fn parse_primary<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Expr, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        match tokens.next() {
            Some(Token { kind, debug_info }) => Ok(match *kind {
                TokenKind::Num(num) => Expr::new_num(num, debug_info),
                TokenKind::Str(string) => {
                    let string = Self::parse_string_literal(string,  tokens)?;
                    Expr::new_str_lit(string, debug_info)
                },
                TokenKind::OpenDelim(DelimToken::Paren) => {
                    let expr = self.parse_expr_with_new_context(tokens, false)?;
                    tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
                    expr
                }
                TokenKind::Ident(name) => {
                    if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paren)) {
                        // func call
                        let mut args = Vec::new();
                        if tokens.consume(&TokenKind::CloseDelim(DelimToken::Paren)) {
                            return Ok(Expr::new_func(name, args, debug_info));
                        }
                        args.push(self.parse_expr_with_new_context(tokens, true)?);
                        while !tokens.consume(&TokenKind::CloseDelim(DelimToken::Paren)) {
                            tokens.expect(&TokenKind::Comma)?;
                            args.push(self.parse_expr_with_new_context(tokens, true)?);
                        }
                        return Ok(Expr::new_func(name, args, debug_info));
                    }
                    // local variable
                    Expr::new_lvar(name, debug_info)
                }
                TokenKind::BuiltinVaStart => {
                    tokens.expect(&TokenKind::OpenDelim(DelimToken::Paren))?;
                    let ap = self.parse_expr_with_new_context(tokens, false)?;
                    tokens.expect(&TokenKind::Comma)?;
                    let last = self.parse_expr_with_new_context(tokens, false)?;
                    tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
                    // local variable
                    return Ok(Expr::new_built_in_va_start(ap, last));
                }
                TokenKind::Asm => {
                    tokens.expect(&TokenKind::OpenDelim(DelimToken::Paren))?;
                    let token = tokens.next().ok_or_else(|| {
                        CompileError::new_unexpected_eof(None, Box::new("TokenKind::Str(_)"))
                    })?;
                    if let TokenKind::Str(string) = *token.kind {
                        let debug_info = token.debug_info;
                        let string = Self::parse_string_literal(string, tokens)?;
                        tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
                        Expr::new_inline_asm(string, debug_info)
                    } else {
                        return Err(CompileError::new_expected_failed(
                            Box::new("TokenKind::Str(_)"),
                            token,
                        ));
                    }
                }
                TokenKind::NullPtr => Expr::new_null_ptr(debug_info),
                TokenKind::Eof => return Err(CompileError::new_unexpected_eof(Some(debug_info.get_file_src()), Box::new("TokenKind::Num(_) | TokenKind::Ident(_) | TokenKind::OpenDelim(DelimToken::Paran)"))),
                _ => return Err(CompileError::new_expected_failed( Box::new("TokenKind::Num(_) | TokenKind::OpenDelim(DelimToken::Paran) | TokenKind::Ident"), Token::new(*kind, debug_info))),
            }),
            None => Err(CompileError::new_unexpected_eof(None, Box::new(
                "TokenKind::Num(_) | TokenKind::OpenDelim(DelimToken::Paran) | TokenKind::Ident",
            ))),
        }
    }

    /// This function is called after first string literal.
    pub fn parse_string_literal<I>(
        mut string: String,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<String, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        while let Some(TokenKind::Str(letters)) = tokens.peek_kind() {
            tokens.next();
            string.push_str(&letters);
        }
        Ok(string)
    }

    pub fn parse_type_name<I>(
        &mut self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<TypeName, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let (ty_spec, debug_info) = self.parse_type_specifier(tokens, true)?;
        // TODO: support DirectAbstractDeclarator
        let abstract_declarator = self.parse_abstract_declarator(tokens)?;
        Ok(TypeName::new(
            SpecQual(ty_spec),
            abstract_declarator,
            debug_info,
        ))
    }

    pub fn parse_abstract_declarator<I>(
        &self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Option<AbstractDeclarator>, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let n_star = Self::parse_pointer(tokens)?;
        let direct_abstract_declarator =
            if let Some(TokenKind::OpenDelim(DelimToken::Bracket | DelimToken::Paren)) =
                tokens.peek_kind()
            {
                self.parse_direct_abstract_declarator(tokens)?
            } else {
                None
            };
        Ok(Some(AbstractDeclarator::new(
            n_star,
            direct_abstract_declarator,
        )))
    }

    pub fn parse_direct_abstract_declarator<I>(
        &self,
        tokens: &mut TokenStream<I, TokenKind>,
    ) -> Result<Option<DirectAbstractDeclarator>, CompileError>
    where
        I: Clone + Debug + Iterator<Item = Token<TokenKind>>,
    {
        let direct_abstract_declarator = if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paren))
        {
            // "(" <abstract-declarator> ")"
            let mut tmp_tokens = tokens.clone();
            let abstract_declarator = self.parse_abstract_declarator(&mut tmp_tokens);
            if abstract_declarator.is_err()
                || tmp_tokens.peek_kind() == Some(TokenKind::OpenDelim(DelimToken::Bracket))
                || tmp_tokens.peek_kind() == Some(TokenKind::OpenDelim(DelimToken::Paren))
            {
                // [ <assign> ]
                // | ( <parameter-type-list>? )
                todo!()
            } else {
                let abstract_declarator = abstract_declarator?;
                if abstract_declarator.is_none() {
                    return Ok(None);
                }
                let direct_abstract_declarator = DirectAbstractDeclarator::AbstractDeclarator(
                    Box::new(abstract_declarator.unwrap()),
                );
                *tokens = tmp_tokens;
                tokens.expect(&TokenKind::CloseDelim(DelimToken::Paren))?;
                direct_abstract_declarator
            }
        } else {
            // [ <assign> ]
            // | ( <parameter-type-list>? )
            // TODO: support above
            todo!();
        };
        loop {
            if tokens.consume(&TokenKind::OpenDelim(DelimToken::Paren)) {
                // function declaration
                #[allow(clippy::branches_sharing_code)]
                if tokens.consume(&TokenKind::CloseDelim(DelimToken::Paren)) {
                    // e.g) `main()`
                    // direct_abstract_declarator = DirectAbstractDeclarator::Func(
                    //     Box::new(direct_abstract_declarator),
                    //     Vec::new(),
                    // );
                    // TODO: support function type direct_abstract_declarator
                    todo!()
                } else {
                    // let mut args = Vec::new();
                    // args.push(self.parse_declaration(tokens)?);
                    // while tokens.consume(&TokenKind::Comma) {
                    //     args.push(self.parse_declaration(tokens)?);
                    // }
                    // direct_declarator = DirectDeclarator::Func(Box::new(direct_declarator), args);
                    // tokens.expect(TokenKind::CloseDelim(DelimToken::Paran))?;
                    // TODO: support function type direct_abstract_declarator
                    todo!()
                }
            } else if tokens.consume(&TokenKind::OpenDelim(DelimToken::Bracket)) {
                // these branches should be parsed differently, but now both are not yet implemented
                #[allow(clippy::branches_sharing_code)]
                if tokens.consume(&TokenKind::CloseDelim(DelimToken::Bracket)) {
                    // direct_abstract_declarator =
                    //     DirectAbstractDeclarator::Array(Box::new(direct_declarator), None);
                    // TODO: support array type direct_abstract_declarator
                    todo!()
                } else {
                    // let expr = self.parse_assign(tokens)?;
                    // direct_declarator =
                    //     DirectDeclarator::Array(Box::new(direct_declarator), Some(expr));
                    // tokens.expect(TokenKind::CloseDelim(DelimToken::Bracket))?;
                    // TODO: support array type direct_abstract_declarator
                    todo!()
                }
            } else {
                break;
            }
        }
        Ok(Some(direct_abstract_declarator))
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct Program {
    components: Vec<ProgramComponent>,
}

impl Program {
    pub const fn new() -> Self {
        Self {
            components: Vec::new(),
        }
    }

    pub fn with_vec(vec: Vec<ProgramComponent>) -> Self {
        Self { components: vec }
    }

    pub fn push(&mut self, component: ProgramComponent) {
        self.components.push(component);
    }
}

impl IntoIterator for Program {
    type Item = ProgramComponent;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct ProgramComponent {
    pub kind: ProgramKind,
    pub debug_info: DebugInfo,
}

impl ProgramComponent {
    pub const fn new(kind: ProgramKind, debug_info: DebugInfo) -> Self {
        Self { kind, debug_info }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ProgramKind {
    FuncDef(TypeSpecifier, Declarator, Stmt),
    Declaration(Declaration),
    InlineAsm(String),
}

impl ProgramKind {
    pub const fn new_funcdef(
        type_spec: TypeSpecifier,
        n_star: usize,
        direct_declarator: DirectDeclarator,
        body: Stmt,
    ) -> Self {
        ProgramKind::FuncDef(type_spec, Declarator::new(n_star, direct_declarator), body)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnumConstant {
    pub ident: String,
    pub debug_info: DebugInfo,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum EnumSpec {
    WithList(
        Option<String>,
        /* <enumerator-list> */ Vec<EnumConstant>,
    ),
    WithTag(String),
}

/// <type-name> := <specifier-qualifier-list> <abstract-declarator>?
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct TypeName {
    // TODO: actually Vec
    spec_quals: SpecQual,
    abstract_declarator: Option<AbstractDeclarator>,
    debug_info: DebugInfo,
}

impl TypeName {
    pub const fn new(
        spec_quals: SpecQual,
        abstract_declarator: Option<AbstractDeclarator>,
        debug_info: DebugInfo,
    ) -> Self {
        Self {
            spec_quals,
            abstract_declarator,
            debug_info,
        }
    }

    pub fn ty(&self) -> Type {
        // TODO: also consider array
        let mut ty = match &self.spec_quals.0 {
            TypeSpecifier::Int => Type::Base(BaseType::Int),
            TypeSpecifier::Char => Type::Base(BaseType::Char),
            TypeSpecifier::Void => Type::Void,
            TypeSpecifier::StructOrUnion(
                StructOrUnionSpec::WithTag(name) | StructOrUnionSpec::WithList(Some(name), _),
            ) => Type::InComplete(InCompleteKind::Struct(name.clone())),
            TypeSpecifier::StructOrUnion(StructOrUnionSpec::WithList(None, _)) => {
                todo!()
            }
            TypeSpecifier::Enum(EnumSpec::WithTag(name) | EnumSpec::WithList(Some(name), _)) => {
                Type::InComplete(InCompleteKind::Enum(name.clone()))
            }
            TypeSpecifier::Enum(EnumSpec::WithList(None, _)) => todo!(),
            TypeSpecifier::TypeDefName(ty) => ty.clone(),
        };
        if let Some(ref abstract_declarator) = self.abstract_declarator {
            for _ in 0..abstract_declarator.n_star {
                ty = Type::Ptr(Box::new(ty));
            }
        }
        ty
    }
}

/// \<specifier-qualifier-list\> := \<type-specifiers\>
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct SpecQual(TypeSpecifier);

/// \<abstract-declarator\> := "\*"\* \<direct-abstract-declarator\>?
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub struct AbstractDeclarator {
    /// "\*"\*
    n_star: usize,
    /// \<direct-abstract-declarator\>?
    direct_abstract_declarator: Option<DirectAbstractDeclarator>,
}

impl AbstractDeclarator {
    pub const fn new(
        n_star: usize,
        direct_abstract_declarator: Option<DirectAbstractDeclarator>,
    ) -> Self {
        Self {
            n_star,
            direct_abstract_declarator,
        }
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum DirectAbstractDeclarator {
    /// <direct-abstract-declarator>? "[]"
    Array(Option<Box<DirectAbstractDeclarator>>),
    /// "(" <abstract-declarator> ")"
    AbstractDeclarator(Box<AbstractDeclarator>),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum StmtKind {
    Expr(Expr),
    Return(Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<ForInitKind>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Switch(Expr, Box<Stmt>),
    Declare(Declaration),
    Labeled(LabelKind, Box<Stmt>),
    Break,
    Continue,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum LabelKind {
    Ident(String),
    Case(Expr),
    Default(DebugInfo),
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Debug)]
pub enum ForInitKind {
    Declaration(Declaration),
    Expr(Expr),
}
