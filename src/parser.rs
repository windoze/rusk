use crate::ast::*;
use crate::lexer::{FStringPart, LexError, Lexer, Token, TokenKind};
use crate::source::Span;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for ParseError {}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        Self {
            message: err.message,
            span: err.span,
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lookahead: Token,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Result<Self, ParseError> {
        let mut lexer = Lexer::new(src);
        let lookahead = lexer.next_token()?;
        Ok(Self { lexer, lookahead })
    }

    pub fn with_base_offset(src: &'a str, base_offset: usize) -> Result<Self, ParseError> {
        let mut lexer = Lexer::with_base_offset(src, base_offset);
        let lookahead = lexer.next_token()?;
        Ok(Self { lexer, lookahead })
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::Eof) {
            items.push(self.parse_item()?);
        }
        Ok(Program { items })
    }

    pub fn parse_expr_eof(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_expr()?;
        self.expect(TokenKind::Eof)?;
        Ok(expr)
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        let vis = self.parse_visibility()?;
        match self.lookahead.kind {
            TokenKind::KwFn => Ok(Item::Function(self.parse_fn_item(vis)?)),
            TokenKind::KwStruct => Ok(Item::Struct(self.parse_struct_item(vis)?)),
            TokenKind::KwEnum => Ok(Item::Enum(self.parse_enum_item(vis)?)),
            TokenKind::KwInterface => Ok(Item::Interface(self.parse_interface_item(vis)?)),
            TokenKind::KwMod => Ok(Item::Mod(self.parse_mod_item(vis)?)),
            TokenKind::KwUse => Ok(Item::Use(self.parse_use_item(vis)?)),
            TokenKind::KwImpl => {
                if vis.is_public() {
                    return Err(self.error_here("`impl` items cannot be `pub`"));
                }
                Ok(Item::Impl(self.parse_impl_item()?))
            }
            _ => Err(self.error_here("expected item")),
        }
    }

    fn parse_visibility(&mut self) -> Result<Visibility, ParseError> {
        if matches!(self.lookahead.kind, TokenKind::KwPub) {
            let tok = self.bump()?;
            return Ok(Visibility::Public { span: tok.span });
        }
        Ok(Visibility::Private)
    }

    fn parse_fn_item(&mut self, vis: Visibility) -> Result<FnItem, ParseError> {
        let start = self.expect(TokenKind::KwFn)?.span.start;
        let name = self.expect_ident()?;
        let generics = self.parse_generic_params()?;
        self.expect(TokenKind::LParen)?;
        let params = if matches!(self.lookahead.kind, TokenKind::RParen) {
            Vec::new()
        } else {
            self.parse_param_list()?
        };
        let rparen = self.expect(TokenKind::RParen)?;
        let ret = if matches!(self.lookahead.kind, TokenKind::Arrow) {
            self.bump()?;
            self.parse_type()?
        } else {
            // Default return type: `unit`.
            TypeExpr::Prim {
                prim: PrimType::Unit,
                span: rparen.span,
            }
        };
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(FnItem {
            vis,
            name,
            generics,
            params,
            ret,
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_struct_item(&mut self, vis: Visibility) -> Result<StructItem, ParseError> {
        let start = self.expect(TokenKind::KwStruct)?.span.start;
        let name = self.expect_ident()?;
        let generics = self.parse_generic_params()?;
        self.expect(TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::RBrace) {
            let field_name = self.expect_ident()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            let span = Span::new(field_name.span.start, ty.span().end);
            fields.push(FieldDecl {
                name: field_name,
                ty,
                span,
            });
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
            } else {
                break;
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(StructItem {
            vis,
            name,
            generics,
            fields,
            span: Span::new(start, end),
        })
    }

    fn parse_enum_item(&mut self, vis: Visibility) -> Result<EnumItem, ParseError> {
        let start = self.expect(TokenKind::KwEnum)?.span.start;
        let name = self.expect_ident()?;
        let generics = self.parse_generic_params()?;
        self.expect(TokenKind::LBrace)?;
        let mut variants = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::RBrace) {
            let vname = self.expect_ident()?;
            let vname_start = vname.span.start;
            let mut fields = Vec::new();
            let mut end = vname.span.end;
            if matches!(self.lookahead.kind, TokenKind::LParen) {
                self.bump()?;
                if !matches!(self.lookahead.kind, TokenKind::RParen) {
                    loop {
                        let ty = self.parse_type()?;
                        fields.push(ty);
                        if matches!(self.lookahead.kind, TokenKind::Comma) {
                            self.bump()?;
                            if matches!(self.lookahead.kind, TokenKind::RParen) {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
                end = self.expect(TokenKind::RParen)?.span.end;
            }
            variants.push(EnumVariant {
                name: vname,
                fields,
                span: Span::new(vname_start, end),
            });
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
            } else {
                break;
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(EnumItem {
            vis,
            name,
            generics,
            variants,
            span: Span::new(start, end),
        })
    }

    fn parse_interface_item(&mut self, vis: Visibility) -> Result<InterfaceItem, ParseError> {
        let start = self.expect(TokenKind::KwInterface)?.span.start;
        let name = self.expect_ident()?;
        let generics = self.parse_generic_params()?;
        let supers = if matches!(self.lookahead.kind, TokenKind::Colon) {
            self.bump()?;
            let mut supers = Vec::new();
            supers.push(self.parse_path_type()?);
            while matches!(self.lookahead.kind, TokenKind::Plus) {
                self.bump()?;
                supers.push(self.parse_path_type()?);
            }
            supers
        } else {
            Vec::new()
        };
        self.expect(TokenKind::LBrace)?;
        let mut members = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::RBrace) {
            let m_start = self.expect(TokenKind::KwFn)?.span.start;
            let m_name = self.expect_ident()?;
            let m_generics = self.parse_generic_params()?;
            self.expect(TokenKind::LParen)?;
            let params = if matches!(self.lookahead.kind, TokenKind::RParen) {
                Vec::new()
            } else {
                self.parse_param_list()?
            };
            let rparen = self.expect(TokenKind::RParen)?;
            let ret = if matches!(self.lookahead.kind, TokenKind::Arrow) {
                self.bump()?;
                self.parse_type()?
            } else {
                // Default return type: `unit`.
                TypeExpr::Prim {
                    prim: PrimType::Unit,
                    span: rparen.span,
                }
            };
            let m_end = self.expect(TokenKind::Semi)?.span.end;
            members.push(InterfaceMember {
                name: m_name,
                generics: m_generics,
                params,
                ret,
                span: Span::new(m_start, m_end),
            });
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(InterfaceItem {
            vis,
            name,
            generics,
            supers,
            members,
            span: Span::new(start, end),
        })
    }

    fn parse_mod_item(&mut self, vis: Visibility) -> Result<ModItem, ParseError> {
        let start = self.expect(TokenKind::KwMod)?.span.start;
        let name = self.expect_ident()?;
        if matches!(self.lookahead.kind, TokenKind::Semi) {
            let end = self.bump()?.span.end;
            return Ok(ModItem {
                vis,
                name,
                kind: ModKind::File,
                span: Span::new(start, end),
            });
        }
        if matches!(self.lookahead.kind, TokenKind::LBrace) {
            self.bump()?;
            let mut items = Vec::new();
            while !matches!(self.lookahead.kind, TokenKind::RBrace) {
                items.push(self.parse_item()?);
            }
            let end = self.expect(TokenKind::RBrace)?.span.end;
            return Ok(ModItem {
                vis,
                name,
                kind: ModKind::Inline { items },
                span: Span::new(start, end),
            });
        }

        Err(self.error_here("expected `;` or `{` after module name"))
    }

    fn parse_use_item(&mut self, vis: Visibility) -> Result<UseItem, ParseError> {
        let start = self.expect(TokenKind::KwUse)?.span.start;
        let path = self.parse_path_expr()?;
        let alias = if matches!(self.lookahead.kind, TokenKind::KwAs) {
            self.bump()?;
            Some(self.expect_ident()?)
        } else {
            None
        };
        let end = self.expect(TokenKind::Semi)?.span.end;
        Ok(UseItem {
            vis,
            path,
            alias,
            span: Span::new(start, end),
        })
    }

    fn parse_impl_item(&mut self) -> Result<ImplItem, ParseError> {
        let start = self.expect(TokenKind::KwImpl)?.span.start;
        let generics = self.parse_generic_params()?;
        let first = self.parse_path_type()?;
        let header = if matches!(self.lookahead.kind, TokenKind::KwFor) {
            self.bump()?; // for
            let ty = self.parse_path_type()?;
            let span = Span::new(first.span.start, ty.span.end);
            ImplHeader::InterfaceForType {
                interface: first,
                ty,
                span,
            }
        } else {
            ImplHeader::Inherent {
                span: first.span,
                ty: first,
            }
        };
        self.expect(TokenKind::LBrace)?;
        let mut members = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::RBrace) {
            let vis = self.parse_visibility()?;
            if !matches!(self.lookahead.kind, TokenKind::KwFn) {
                return Err(self.error_here("expected `fn` in `impl` body"));
            }
            members.push(self.parse_fn_item(vis)?);
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(ImplItem {
            generics,
            header,
            members,
            span: Span::new(start, end),
        })
    }

    fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, ParseError> {
        if !matches!(self.lookahead.kind, TokenKind::Lt) {
            return Ok(Vec::new());
        }
        let start = self.bump()?.span.start;
        let mut params = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::Gt) {
            let name = self.expect_ident()?;
            let mut arity = 0usize;
            let mut end = name.span.end;
            if matches!(self.lookahead.kind, TokenKind::Lt) {
                self.bump()?;
                // Parse `_` ("," "_")*
                let mut count = 0usize;
                loop {
                    let tok = self.bump()?;
                    match tok.kind {
                        TokenKind::Ident(ref s) if s == "_" => {
                            count += 1;
                        }
                        _ => {
                            return Err(ParseError {
                                message: "expected `_` in higher-kinded parameter".to_string(),
                                span: tok.span,
                            });
                        }
                    }
                    if matches!(self.lookahead.kind, TokenKind::Comma) {
                        self.bump()?;
                        continue;
                    }
                    break;
                }
                end = self.expect(TokenKind::Gt)?.span.end;
                arity = count;
            }
            let bounds = if matches!(self.lookahead.kind, TokenKind::Colon) {
                self.bump()?;
                let mut bounds = Vec::new();
                let first = self.parse_path_type()?;
                end = first.span.end;
                bounds.push(first);
                while matches!(self.lookahead.kind, TokenKind::Plus) {
                    self.bump()?;
                    let b = self.parse_path_type()?;
                    end = b.span.end;
                    bounds.push(b);
                }
                bounds
            } else {
                Vec::new()
            };
            params.push(GenericParam {
                name: name.clone(),
                arity,
                bounds,
                span: Span::new(name.span.start, end),
            });
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
                if matches!(self.lookahead.kind, TokenKind::Gt) {
                    break;
                }
            } else {
                break;
            }
        }
        let end = self.expect(TokenKind::Gt)?.span.end;
        let _span = Span::new(start, end);
        Ok(params)
    }

    fn parse_param_list(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();
        loop {
            let p_start = self.lookahead.span.start;
            // Optional `readonly` parameter marker: treated as `readonly T` type.
            let readonly = matches!(self.lookahead.kind, TokenKind::KwReadonly);
            if readonly {
                self.bump()?;
            }
            let pat = self.parse_pattern()?;
            self.expect(TokenKind::Colon)?;
            let mut ty = self.parse_type()?;
            if readonly {
                let end = ty.span().end;
                ty = TypeExpr::Readonly {
                    inner: Box::new(ty),
                    span: Span::new(p_start, end),
                };
            }
            let span = Span::new(p_start, ty.span().end);
            params.push(Param { pat, ty, span });
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
                if matches!(self.lookahead.kind, TokenKind::RParen) {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(params)
    }

    fn parse_type(&mut self) -> Result<TypeExpr, ParseError> {
        if matches!(self.lookahead.kind, TokenKind::KwReadonly) {
            let start = self.bump()?.span.start;
            let inner = self.parse_type()?;
            let end = inner.span().end;
            return Ok(TypeExpr::Readonly {
                inner: Box::new(inner),
                span: Span::new(start, end),
            });
        }

        match self.lookahead.kind {
            TokenKind::Ident(ref name) => {
                let prim = match name.as_str() {
                    "unit" => Some(PrimType::Unit),
                    "bool" => Some(PrimType::Bool),
                    "int" => Some(PrimType::Int),
                    "float" => Some(PrimType::Float),
                    "string" => Some(PrimType::String),
                    "bytes" => Some(PrimType::Bytes),
                    _ => None,
                };
                if let Some(prim) = prim {
                    let tok = self.bump()?;
                    return Ok(TypeExpr::Prim {
                        prim,
                        span: tok.span,
                    });
                }
                Ok(TypeExpr::Path(self.parse_path_type()?))
            }
            TokenKind::KwFn => self.parse_fn_type(),
            TokenKind::KwCont => self.parse_cont_type(),
            TokenKind::LBracket => self.parse_array_type(),
            TokenKind::LParen => self.parse_paren_type(),
            TokenKind::KwReadonly => unreachable!("handled above"),
            _ => Err(self.error_here("expected type")),
        }
    }

    fn parse_array_type(&mut self) -> Result<TypeExpr, ParseError> {
        let start = self.expect(TokenKind::LBracket)?.span.start;
        let elem = self.parse_type()?;
        let end = self.expect(TokenKind::RBracket)?.span.end;
        Ok(TypeExpr::Array {
            elem: Box::new(elem),
            span: Span::new(start, end),
        })
    }

    fn parse_fn_type(&mut self) -> Result<TypeExpr, ParseError> {
        let start = self.expect(TokenKind::KwFn)?.span.start;
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        if !matches!(self.lookahead.kind, TokenKind::RParen) {
            loop {
                params.push(self.parse_type()?);
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    if matches!(self.lookahead.kind, TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let rparen = self.expect(TokenKind::RParen)?;
        let ret = if matches!(self.lookahead.kind, TokenKind::Arrow) {
            self.bump()?;
            self.parse_type()?
        } else {
            // Default return type: `unit`.
            TypeExpr::Prim {
                prim: PrimType::Unit,
                span: rparen.span,
            }
        };
        let end = ret.span().end;
        Ok(TypeExpr::Fn {
            params,
            ret: Box::new(ret),
            span: Span::new(start, end),
        })
    }

    fn parse_paren_type(&mut self) -> Result<TypeExpr, ParseError> {
        let start = self.expect(TokenKind::LParen)?.span.start;
        if matches!(self.lookahead.kind, TokenKind::RParen) {
            let end = self.bump()?.span.end;
            return Ok(TypeExpr::Tuple {
                items: Vec::new(),
                span: Span::new(start, end),
            });
        }

        let first = self.parse_type()?;
        if !matches!(self.lookahead.kind, TokenKind::Comma) {
            // Parenthesized type.
            self.expect(TokenKind::RParen)?;
            return Ok(first);
        }

        // Tuple type.
        self.bump()?;
        let mut items = vec![first];
        if !matches!(self.lookahead.kind, TokenKind::RParen) {
            loop {
                items.push(self.parse_type()?);
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    if matches!(self.lookahead.kind, TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RParen)?.span.end;
        Ok(TypeExpr::Tuple {
            items,
            span: Span::new(start, end),
        })
    }

    fn parse_cont_type(&mut self) -> Result<TypeExpr, ParseError> {
        let start = self.expect(TokenKind::KwCont)?.span.start;
        self.expect(TokenKind::LParen)?;
        let param = self.parse_type()?;
        self.expect(TokenKind::RParen)?;
        self.expect(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        let end = ret.span().end;
        Ok(TypeExpr::Cont {
            param: Box::new(param),
            ret: Box::new(ret),
            span: Span::new(start, end),
        })
    }

    fn parse_path_type(&mut self) -> Result<PathType, ParseError> {
        let start = self.lookahead.span.start;
        let mut segments = Vec::new();
        let first = self.expect_ident()?;
        let first_start = first.span.start;
        let args = self.parse_generic_args()?;
        let mut end = if let Some(last) = args.last() {
            last.span().end
        } else {
            first.span.end
        };
        segments.push(PathTypeSegment {
            name: first,
            args,
            span: Span::new(first_start, end),
        });

        while matches!(self.lookahead.kind, TokenKind::ColonColon) {
            self.bump()?;
            let name = self.expect_ident()?;
            let name_start = name.span.start;
            let args = self.parse_generic_args()?;
            end = if let Some(last) = args.last() {
                last.span().end
            } else {
                name.span.end
            };
            segments.push(PathTypeSegment {
                name,
                args,
                span: Span::new(name_start, end),
            });
        }

        Ok(PathType {
            segments,
            span: Span::new(start, end),
        })
    }

    fn parse_generic_args(&mut self) -> Result<Vec<TypeExpr>, ParseError> {
        if !matches!(self.lookahead.kind, TokenKind::Lt) {
            return Ok(Vec::new());
        }
        self.bump()?;
        let mut args = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::Gt) {
            args.push(self.parse_type()?);
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
                if matches!(self.lookahead.kind, TokenKind::Gt) {
                    break;
                }
                continue;
            }
            break;
        }
        self.expect(TokenKind::Gt)?;
        Ok(args)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let start = self.expect(TokenKind::LBrace)?.span.start;
        let mut stmts = Vec::new();
        let mut tail = None;
        while !matches!(self.lookahead.kind, TokenKind::RBrace) {
            match self.lookahead.kind {
                TokenKind::KwLet | TokenKind::KwConst | TokenKind::KwReadonly => {
                    stmts.push(self.parse_binding_stmt()?);
                }
                TokenKind::KwReturn => {
                    stmts.push(self.parse_return_stmt()?);
                }
                TokenKind::KwBreak => {
                    let s = self.bump()?;
                    let end = self.expect(TokenKind::Semi)?.span.end;
                    stmts.push(Stmt::Break {
                        span: Span::new(s.span.start, end),
                    });
                }
                TokenKind::KwContinue => {
                    let s = self.bump()?;
                    let end = self.expect(TokenKind::Semi)?.span.end;
                    stmts.push(Stmt::Continue {
                        span: Span::new(s.span.start, end),
                    });
                }
                _ => {
                    let expr = self.parse_expr()?;
                    if matches!(self.lookahead.kind, TokenKind::Semi) {
                        let semi = self.bump()?;
                        stmts.push(Stmt::Expr {
                            span: Span::new(expr.span().start, semi.span.end),
                            expr,
                        });
                    } else if matches!(self.lookahead.kind, TokenKind::RBrace) {
                        tail = Some(Box::new(expr));
                        break;
                    } else {
                        return Err(self.error_here("expected `;` or `}` after expression"));
                    }
                }
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Block {
            stmts,
            tail,
            span: Span::new(start, end),
        })
    }

    fn parse_binding_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.lookahead.span.start;
        let kind = match self.lookahead.kind {
            TokenKind::KwLet => BindingKind::Let,
            TokenKind::KwConst => BindingKind::Const,
            TokenKind::KwReadonly => BindingKind::Readonly,
            _ => return Err(self.error_here("expected binding statement")),
        };
        self.bump()?;
        let name = self.expect_ident()?;
        let ty = if matches!(self.lookahead.kind, TokenKind::Colon) {
            self.bump()?;
            Some(self.parse_type()?)
        } else {
            None
        };
        let init = if matches!(self.lookahead.kind, TokenKind::Assign) {
            self.bump()?;
            Some(self.parse_expr()?)
        } else {
            None
        };

        match kind {
            BindingKind::Const | BindingKind::Readonly if init.is_none() => {
                return Err(ParseError {
                    message: "const/readonly bindings require an initializer".to_string(),
                    span: name.span,
                });
            }
            _ => {}
        }

        let end = self.expect(TokenKind::Semi)?.span.end;
        Ok(Stmt::Let {
            kind,
            name,
            ty,
            init,
            span: Span::new(start, end),
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let start = self.expect(TokenKind::KwReturn)?.span.start;
        let value = if matches!(self.lookahead.kind, TokenKind::Semi) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        let end = self.expect(TokenKind::Semi)?.span.end;
        Ok(Stmt::Return {
            value,
            span: Span::new(start, end),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0)
    }

    /// Parses an expression in a context where a `{ ... }` block immediately follows.
    ///
    /// This disallows parsing a struct literal directly after an identifier (`Foo { ... }`)
    /// because it would be ambiguous with the following block (e.g. `if cond { ... }`).
    ///
    /// Users can still write a struct literal here by parenthesizing it: `if (Foo { ... }) { ... }`.
    fn parse_expr_no_struct_lit(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_bp_no_struct_lit(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix()?;

        loop {
            // Postfix operators: call, field, index.
            match self.lookahead.kind {
                TokenKind::LParen => {
                    let start = lhs.span().start;
                    let (args, end) = self.parse_call_args()?;
                    lhs = Expr::Call {
                        callee: Box::new(lhs),
                        args,
                        span: Span::new(start, end),
                    };
                    continue;
                }
                TokenKind::LBrace => {
                    // Trailing closure syntax sugar:
                    // - `foo(a, b) { ... }`  ==>  `foo(a, b, || { ... })`
                    // - `foo { ... }`       ==>  `foo(|| { ... })`
                    //
                    // Additionally, if the call arguments are all `name = expr`, this becomes
                    // "named binds" sugar for capturing values into the trailing closure:
                    // - `foo(a=expr1, b=expr2) { ... }`
                    //   ==> `{ let a = expr1; let b = expr2; foo(a, b, || { ... }) }`
                    let start = lhs.span().start;
                    let body = self.parse_block()?;
                    let closure_span = body.span;
                    let closure = Expr::Lambda {
                        params: Vec::new(),
                        body,
                        span: closure_span,
                    };

                    lhs = match lhs {
                        Expr::Call { callee, args, span } => {
                            // Detect `name = expr` bind list.
                            let is_named_bind_arg = |arg: &Expr| -> bool {
                                matches!(
                                    arg,
                                    Expr::Assign { target, .. }
                                        if matches!(
                                            target.as_ref(),
                                            Expr::Path { path, .. } if path.segments.len() == 1
                                        )
                                )
                            };

                            let any_named = args.iter().any(is_named_bind_arg);
                            let all_named = args.iter().all(is_named_bind_arg);

                            if any_named {
                                if !all_named {
                                    return Err(ParseError {
                                        message: "trailing-closure named bindings require every argument to be `name = expr`".to_string(),
                                        span,
                                    });
                                }
                                let mut bind_pairs: Vec<(Ident, Expr)> =
                                    Vec::with_capacity(args.len());
                                for arg in args {
                                    let Expr::Assign { target, value, .. } = arg else {
                                        unreachable!("all args checked by all_named");
                                    };
                                    let Expr::Path { path, .. } = *target else {
                                        unreachable!("all args checked by all_named");
                                    };
                                    debug_assert_eq!(path.segments.len(), 1);
                                    bind_pairs.push((path.segments[0].clone(), *value));
                                }

                                let mut seen = std::collections::BTreeSet::<String>::new();
                                let mut stmts = Vec::with_capacity(bind_pairs.len());
                                let mut call_args = Vec::with_capacity(bind_pairs.len() + 1);
                                for (name, init) in bind_pairs {
                                    if !seen.insert(name.name.clone()) {
                                        return Err(ParseError {
                                            message: format!(
                                                "duplicate trailing-closure binding `{}`",
                                                name.name
                                            ),
                                            span: name.span,
                                        });
                                    }
                                    let stmt_span = Span::new(name.span.start, init.span().end);
                                    stmts.push(Stmt::Let {
                                        kind: BindingKind::Let,
                                        name: name.clone(),
                                        ty: None,
                                        init: Some(init),
                                        span: stmt_span,
                                    });
                                    call_args.push(Expr::Path {
                                        path: Path {
                                            segments: vec![name.clone()],
                                            span: name.span,
                                        },
                                        span: name.span,
                                    });
                                }
                                call_args.push(closure);
                                let call_span = Span::new(span.start, closure_span.end);
                                let call_expr = Expr::Call {
                                    callee,
                                    args: call_args,
                                    span: call_span,
                                };
                                Expr::Block {
                                    block: Block {
                                        stmts,
                                        tail: Some(Box::new(call_expr)),
                                        span: call_span,
                                    },
                                    span: call_span,
                                }
                            } else {
                                let mut new_args = args;
                                new_args.push(closure);
                                Expr::Call {
                                    callee,
                                    args: new_args,
                                    span: Span::new(span.start, closure_span.end),
                                }
                            }
                        }
                        other => Expr::Call {
                            callee: Box::new(other),
                            args: vec![closure],
                            span: Span::new(start, closure_span.end),
                        },
                    };
                    continue;
                }
                TokenKind::Dot => {
                    let start = lhs.span().start;
                    self.bump()?;
                    let name = match self.lookahead.kind.clone() {
                        TokenKind::Ident(_) => FieldName::Named(self.expect_ident()?),
                        TokenKind::Int(v) => {
                            let tok = self.bump()?;
                            if v < 0 {
                                return Err(ParseError {
                                    message: "tuple field index must be non-negative".to_string(),
                                    span: tok.span,
                                });
                            }
                            let idx = usize::try_from(v).map_err(|_| ParseError {
                                message: "tuple field index is too large".to_string(),
                                span: tok.span,
                            })?;
                            FieldName::Index {
                                index: idx,
                                span: tok.span,
                            }
                        }
                        _ => return Err(self.error_here("expected field name")),
                    };
                    let end = name.span().end;
                    lhs = Expr::Field {
                        base: Box::new(lhs),
                        name,
                        span: Span::new(start, end),
                    };
                    continue;
                }
                TokenKind::LBracket => {
                    let start = lhs.span().start;
                    self.bump()?;
                    let index = self.parse_expr()?;
                    let end = self.expect(TokenKind::RBracket)?.span.end;
                    lhs = Expr::Index {
                        base: Box::new(lhs),
                        index: Box::new(index),
                        span: Span::new(start, end),
                    };
                    continue;
                }
                _ => {}
            }

            // Infix casts:
            // - `expr as Type`   (interface upcast; chainable)
            // - `expr as? Type`  (checked cast; returns `Option<T>`)
            if matches!(self.lookahead.kind, TokenKind::KwAs) {
                let l_bp = 15;
                if l_bp < min_bp {
                    break;
                }
                let start = lhs.span().start;
                self.bump()?;
                let checked = matches!(self.lookahead.kind, TokenKind::Question);
                if checked {
                    self.bump()?;
                }
                let ty = self.parse_type()?;
                let end = ty.span().end;
                lhs = if checked {
                    Expr::AsQuestion {
                        expr: Box::new(lhs),
                        ty,
                        span: Span::new(start, end),
                    }
                } else {
                    Expr::As {
                        expr: Box::new(lhs),
                        ty,
                        span: Span::new(start, end),
                    }
                };
                // Allow chaining: `x as A as B` (and `x as A as? B`).
                continue;
            }

            // Infix type test: `expr is Type`
            if matches!(self.lookahead.kind, TokenKind::KwIs) {
                let l_bp = 9;
                if l_bp < min_bp {
                    break;
                }
                let start = lhs.span().start;
                self.bump()?;
                let ty = self.parse_type()?;
                let end = ty.span().end;
                lhs = Expr::Is {
                    expr: Box::new(lhs),
                    ty,
                    span: Span::new(start, end),
                };
                continue;
            }

            // Infix / assignment.
            let (l_bp, r_bp, op) = match self.lookahead.kind {
                TokenKind::Assign => (1, 0, Infix::Assign),
                TokenKind::OrOr => (3, 4, Infix::Binary(BinaryOp::Or)),
                TokenKind::AndAnd => (5, 6, Infix::Binary(BinaryOp::And)),
                TokenKind::EqEq => (7, 8, Infix::Binary(BinaryOp::Eq)),
                TokenKind::NotEq => (7, 8, Infix::Binary(BinaryOp::Ne)),
                TokenKind::Lt => (9, 10, Infix::Binary(BinaryOp::Lt)),
                TokenKind::LtEq => (9, 10, Infix::Binary(BinaryOp::Le)),
                TokenKind::Gt => (9, 10, Infix::Binary(BinaryOp::Gt)),
                TokenKind::GtEq => (9, 10, Infix::Binary(BinaryOp::Ge)),
                TokenKind::Plus => (11, 12, Infix::Binary(BinaryOp::Add)),
                TokenKind::Minus => (11, 12, Infix::Binary(BinaryOp::Sub)),
                TokenKind::Star => (13, 14, Infix::Binary(BinaryOp::Mul)),
                TokenKind::Slash => (13, 14, Infix::Binary(BinaryOp::Div)),
                TokenKind::Percent => (13, 14, Infix::Binary(BinaryOp::Mod)),
                _ => break,
            };

            if l_bp < min_bp {
                break;
            }

            let op_tok = self.bump()?;
            let rhs = self.parse_expr_bp(r_bp)?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            lhs = match op {
                Infix::Assign => Expr::Assign {
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                    span: Span::new(op_tok.span.start, span.end),
                },
                Infix::Binary(binop) => Expr::Binary {
                    op: binop,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span,
                },
            };
        }

        Ok(lhs)
    }

    fn parse_expr_bp_no_struct_lit(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix_no_struct_lit()?;

        loop {
            // Postfix operators: call, field, index.
            match self.lookahead.kind {
                TokenKind::LParen => {
                    let start = lhs.span().start;
                    let (args, end) = self.parse_call_args()?;
                    lhs = Expr::Call {
                        callee: Box::new(lhs),
                        args,
                        span: Span::new(start, end),
                    };
                    continue;
                }
                TokenKind::Dot => {
                    let start = lhs.span().start;
                    self.bump()?;
                    let name = match self.lookahead.kind.clone() {
                        TokenKind::Ident(_) => FieldName::Named(self.expect_ident()?),
                        TokenKind::Int(v) => {
                            let tok = self.bump()?;
                            if v < 0 {
                                return Err(ParseError {
                                    message: "tuple field index must be non-negative".to_string(),
                                    span: tok.span,
                                });
                            }
                            let idx = usize::try_from(v).map_err(|_| ParseError {
                                message: "tuple field index is too large".to_string(),
                                span: tok.span,
                            })?;
                            FieldName::Index {
                                index: idx,
                                span: tok.span,
                            }
                        }
                        _ => return Err(self.error_here("expected field name")),
                    };
                    let end = name.span().end;
                    lhs = Expr::Field {
                        base: Box::new(lhs),
                        name,
                        span: Span::new(start, end),
                    };
                    continue;
                }
                TokenKind::LBracket => {
                    let start = lhs.span().start;
                    self.bump()?;
                    let index = self.parse_expr()?;
                    let end = self.expect(TokenKind::RBracket)?.span.end;
                    lhs = Expr::Index {
                        base: Box::new(lhs),
                        index: Box::new(index),
                        span: Span::new(start, end),
                    };
                    continue;
                }
                _ => {}
            }

            // Infix casts:
            // - `expr as Type`   (interface upcast; chainable)
            // - `expr as? Type`  (checked cast; returns `Option<T>`)
            if matches!(self.lookahead.kind, TokenKind::KwAs) {
                let l_bp = 15;
                if l_bp < min_bp {
                    break;
                }
                let start = lhs.span().start;
                self.bump()?;
                let checked = matches!(self.lookahead.kind, TokenKind::Question);
                if checked {
                    self.bump()?;
                }
                let ty = self.parse_type()?;
                let end = ty.span().end;
                lhs = if checked {
                    Expr::AsQuestion {
                        expr: Box::new(lhs),
                        ty,
                        span: Span::new(start, end),
                    }
                } else {
                    Expr::As {
                        expr: Box::new(lhs),
                        ty,
                        span: Span::new(start, end),
                    }
                };
                continue;
            }

            // Infix type test: `expr is Type`
            if matches!(self.lookahead.kind, TokenKind::KwIs) {
                let l_bp = 9;
                if l_bp < min_bp {
                    break;
                }
                let start = lhs.span().start;
                self.bump()?;
                let ty = self.parse_type()?;
                let end = ty.span().end;
                lhs = Expr::Is {
                    expr: Box::new(lhs),
                    ty,
                    span: Span::new(start, end),
                };
                continue;
            }

            // Infix / assignment.
            let (l_bp, r_bp, op) = match self.lookahead.kind {
                TokenKind::Assign => (1, 0, Infix::Assign),
                TokenKind::OrOr => (3, 4, Infix::Binary(BinaryOp::Or)),
                TokenKind::AndAnd => (5, 6, Infix::Binary(BinaryOp::And)),
                TokenKind::EqEq => (7, 8, Infix::Binary(BinaryOp::Eq)),
                TokenKind::NotEq => (7, 8, Infix::Binary(BinaryOp::Ne)),
                TokenKind::Lt => (9, 10, Infix::Binary(BinaryOp::Lt)),
                TokenKind::LtEq => (9, 10, Infix::Binary(BinaryOp::Le)),
                TokenKind::Gt => (9, 10, Infix::Binary(BinaryOp::Gt)),
                TokenKind::GtEq => (9, 10, Infix::Binary(BinaryOp::Ge)),
                TokenKind::Plus => (11, 12, Infix::Binary(BinaryOp::Add)),
                TokenKind::Minus => (11, 12, Infix::Binary(BinaryOp::Sub)),
                TokenKind::Star => (13, 14, Infix::Binary(BinaryOp::Mul)),
                TokenKind::Slash => (13, 14, Infix::Binary(BinaryOp::Div)),
                TokenKind::Percent => (13, 14, Infix::Binary(BinaryOp::Mod)),
                _ => break,
            };

            if l_bp < min_bp {
                break;
            }

            let op_tok = self.bump()?;
            let rhs = self.parse_expr_bp_no_struct_lit(r_bp)?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            lhs = match op {
                Infix::Assign => Expr::Assign {
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                    span: Span::new(op_tok.span.start, span.end),
                },
                Infix::Binary(binop) => Expr::Binary {
                    op: binop,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    span,
                },
            };
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match self.lookahead.kind.clone() {
            TokenKind::Bang => {
                let start = self.bump()?.span.start;
                let expr = self.parse_expr_bp(15)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                })
            }
            TokenKind::Minus => {
                let start = self.bump()?.span.start;
                let expr = self.parse_expr_bp(15)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_prefix_no_struct_lit(&mut self) -> Result<Expr, ParseError> {
        match self.lookahead.kind.clone() {
            TokenKind::Bang => {
                let start = self.bump()?.span.start;
                let expr = self.parse_expr_bp_no_struct_lit(15)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                })
            }
            TokenKind::Minus => {
                let start = self.bump()?.span.start;
                let expr = self.parse_expr_bp_no_struct_lit(15)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    span: Span::new(start, expr.span().end),
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_primary_no_struct_lit(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        match self.lookahead.kind.clone() {
            TokenKind::LBrace => {
                let block = self.parse_block()?;
                Ok(Expr::Block {
                    span: block.span,
                    block,
                })
            }
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::KwIf => self.parse_if_expr(),
            TokenKind::KwMatch => self.parse_match_expr(),
            TokenKind::KwLoop => {
                let start = self.bump()?.span.start;
                let body = self.parse_block()?;
                let end = body.span.end;
                Ok(Expr::Loop {
                    body,
                    span: Span::new(start, end),
                })
            }
            TokenKind::KwWhile => self.parse_while_expr(),
            TokenKind::KwFor => self.parse_for_expr(),
            TokenKind::Pipe => self.parse_lambda(),
            TokenKind::At => self.parse_effect_call(),
            TokenKind::LBracket => self.parse_array_lit(),
            TokenKind::Int(v) => {
                let tok = self.bump()?;
                Ok(Expr::Int {
                    value: v,
                    span: tok.span,
                })
            }
            TokenKind::Float(v) => {
                let tok = self.bump()?;
                Ok(Expr::Float {
                    value: v,
                    span: tok.span,
                })
            }
            TokenKind::String(s) => {
                let tok = self.bump()?;
                Ok(Expr::String {
                    value: s,
                    span: tok.span,
                })
            }
            TokenKind::Bytes(b) => {
                let tok = self.bump()?;
                Ok(Expr::Bytes {
                    value: b,
                    span: tok.span,
                })
            }
            TokenKind::FString(parts) => {
                let tok = self.bump()?;
                self.desugar_fstring(tok.span, parts)
            }
            TokenKind::Ident(ref s) if s == "true" || s == "false" => {
                let tok = self.bump()?;
                Ok(Expr::Bool {
                    value: s == "true",
                    span: tok.span,
                })
            }
            TokenKind::Ident(_) => self.parse_path_or_struct_lit(true),
            _ => Err(self.error_here("expected expression")),
        }
    }

    fn parse_primary_no_struct_lit(&mut self) -> Result<Expr, ParseError> {
        match self.lookahead.kind.clone() {
            TokenKind::LBrace => {
                let block = self.parse_block()?;
                Ok(Expr::Block {
                    span: block.span,
                    block,
                })
            }
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::KwIf => self.parse_if_expr(),
            TokenKind::KwMatch => self.parse_match_expr(),
            TokenKind::KwLoop => {
                let start = self.bump()?.span.start;
                let body = self.parse_block()?;
                let end = body.span.end;
                Ok(Expr::Loop {
                    body,
                    span: Span::new(start, end),
                })
            }
            TokenKind::KwWhile => self.parse_while_expr(),
            TokenKind::KwFor => self.parse_for_expr(),
            TokenKind::Pipe => self.parse_lambda(),
            TokenKind::At => self.parse_effect_call(),
            TokenKind::LBracket => self.parse_array_lit(),
            TokenKind::Int(v) => {
                let tok = self.bump()?;
                Ok(Expr::Int {
                    value: v,
                    span: tok.span,
                })
            }
            TokenKind::Float(v) => {
                let tok = self.bump()?;
                Ok(Expr::Float {
                    value: v,
                    span: tok.span,
                })
            }
            TokenKind::String(s) => {
                let tok = self.bump()?;
                Ok(Expr::String {
                    value: s,
                    span: tok.span,
                })
            }
            TokenKind::Bytes(b) => {
                let tok = self.bump()?;
                Ok(Expr::Bytes {
                    value: b,
                    span: tok.span,
                })
            }
            TokenKind::FString(parts) => {
                let tok = self.bump()?;
                self.desugar_fstring(tok.span, parts)
            }
            TokenKind::Ident(ref s) if s == "true" || s == "false" => {
                let tok = self.bump()?;
                Ok(Expr::Bool {
                    value: s == "true",
                    span: tok.span,
                })
            }
            TokenKind::Ident(_) => self.parse_path_or_struct_lit(false),
            _ => Err(self.error_here("expected expression")),
        }
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::LParen)?.span.start;
        if matches!(self.lookahead.kind, TokenKind::RParen) {
            let end = self.bump()?.span.end;
            return Ok(Expr::Unit {
                span: Span::new(start, end),
            });
        }

        let first = self.parse_expr()?;
        if !matches!(self.lookahead.kind, TokenKind::Comma) {
            // Parenthesized expression.
            self.expect(TokenKind::RParen)?;
            return Ok(first);
        }

        // Tuple expression.
        self.bump()?;
        let mut items = vec![first];
        if !matches!(self.lookahead.kind, TokenKind::RParen) {
            loop {
                let next = self.parse_expr()?;
                items.push(next);
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    if matches!(self.lookahead.kind, TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RParen)?.span.end;
        Ok(Expr::Tuple {
            items,
            span: Span::new(start, end),
        })
    }

    fn parse_array_lit(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::LBracket)?.span.start;
        let mut items = Vec::new();
        if !matches!(self.lookahead.kind, TokenKind::RBracket) {
            loop {
                items.push(self.parse_expr()?);
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    if matches!(self.lookahead.kind, TokenKind::RBracket) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RBracket)?.span.end;
        Ok(Expr::Array {
            items,
            span: Span::new(start, end),
        })
    }

    fn parse_path_or_struct_lit(&mut self, allow_struct_lit: bool) -> Result<Expr, ParseError> {
        let start = self.lookahead.span.start;

        // Parse a path first: `A` or `A::B::C`.
        let mut segments = vec![self.expect_ident()?];
        while matches!(self.lookahead.kind, TokenKind::ColonColon) {
            self.bump()?;
            segments.push(self.expect_ident()?);
        }
        let path_end = segments.last().map(|s| s.span.end).unwrap_or(start);
        let path = Path {
            segments,
            span: Span::new(start, path_end),
        };

        // Struct literal: `Type { field: expr, ... }`
        let looks_like_type_name = path
            .segments
            .last()
            .and_then(|seg| seg.name.chars().next())
            .is_some_and(|ch| ch.is_ascii_uppercase());
        if allow_struct_lit
            && looks_like_type_name
            && matches!(self.lookahead.kind, TokenKind::LBrace)
        {
            self.bump()?;
            let mut fields = Vec::new();
            while !matches!(self.lookahead.kind, TokenKind::RBrace) {
                let fname = self.expect_ident()?;
                self.expect(TokenKind::Colon)?;
                let expr = self.parse_expr()?;
                fields.push((fname, expr));
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                } else {
                    break;
                }
            }
            let end = self.expect(TokenKind::RBrace)?.span.end;
            return Ok(Expr::StructLit {
                type_path: path,
                fields,
                span: Span::new(start, end),
            });
        }

        Ok(Expr::Path {
            span: path.span,
            path,
        })
    }

    fn parse_call_args(&mut self) -> Result<(Vec<Expr>, usize), ParseError> {
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();
        if !matches!(self.lookahead.kind, TokenKind::RParen) {
            loop {
                args.push(self.parse_expr()?);
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    if matches!(self.lookahead.kind, TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RParen)?.span.end;
        Ok((args, end))
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::KwIf)?.span.start;
        let cond = self.parse_expr_no_struct_lit()?;
        let then_block = self.parse_block()?;
        let else_branch = if matches!(self.lookahead.kind, TokenKind::KwElse) {
            self.bump()?;
            Some(Box::new(match self.lookahead.kind {
                TokenKind::KwIf => self.parse_if_expr()?,
                TokenKind::LBrace => {
                    let block = self.parse_block()?;
                    Expr::Block {
                        span: block.span,
                        block,
                    }
                }
                _ => return Err(self.error_here("expected `if` or block after `else`")),
            }))
        } else {
            None
        };
        let end = else_branch
            .as_ref()
            .map(|e| e.span().end)
            .unwrap_or(then_block.span.end);
        Ok(Expr::If {
            cond: Box::new(cond),
            then_block,
            else_branch,
            span: Span::new(start, end),
        })
    }

    fn parse_match_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::KwMatch)?.span.start;
        let scrutinee = self.parse_expr_no_struct_lit()?;
        self.expect(TokenKind::LBrace)?;
        let mut arms = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::RBrace) {
            let pat = if matches!(self.lookahead.kind, TokenKind::At) {
                MatchPat::Effect(self.parse_effect_pat()?)
            } else {
                MatchPat::Value(self.parse_pattern()?)
            };
            self.expect(TokenKind::FatArrow)?;
            let body = if matches!(self.lookahead.kind, TokenKind::LBrace) {
                let block = self.parse_block()?;
                Expr::Block {
                    span: block.span,
                    block,
                }
            } else {
                self.parse_expr()?
            };
            let pat_start = match &pat {
                MatchPat::Value(p) => p.span().start,
                MatchPat::Effect(p) => p.span.start,
            };
            let arm_span = Span::new(pat_start, body.span().end);
            arms.push(MatchArm {
                pat,
                body,
                span: arm_span,
            });
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
            }
        }
        let end = self.expect(TokenKind::RBrace)?.span.end;
        Ok(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            span: Span::new(start, end),
        })
    }

    fn parse_effect_pat(&mut self) -> Result<EffectPattern, ParseError> {
        let start = self.expect(TokenKind::At)?.span.start;
        let interface = self.parse_path_type()?;
        self.expect(TokenKind::Dot)?;
        let method = self.expect_ident()?;
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();
        if !matches!(self.lookahead.kind, TokenKind::RParen) {
            loop {
                args.push(self.parse_pattern()?);
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    if matches!(self.lookahead.kind, TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let mut end = self.expect(TokenKind::RParen)?.span.end;
        let cont = if matches!(self.lookahead.kind, TokenKind::Arrow) {
            self.bump()?;
            let name = self.expect_ident()?;
            end = name.span.end;
            Some(name)
        } else {
            None
        };
        Ok(EffectPattern {
            interface,
            method,
            args,
            cont,
            span: Span::new(start, end),
        })
    }

    fn parse_effect_call(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::At)?.span.start;
        let interface = self.parse_path_type()?;
        self.expect(TokenKind::Dot)?;
        let method = self.expect_ident()?;
        let (args, end) = self.parse_call_args()?;
        Ok(Expr::EffectCall {
            interface,
            method,
            args,
            span: Span::new(start, end),
        })
    }

    fn parse_while_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::KwWhile)?.span.start;
        let cond = self.parse_expr_no_struct_lit()?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(Expr::While {
            cond: Box::new(cond),
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_for_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::KwFor)?.span.start;
        let binding = self.expect_ident()?;
        self.expect(TokenKind::KwIn)?;
        let iter = self.parse_expr_no_struct_lit()?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(Expr::For {
            binding,
            iter: Box::new(iter),
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(TokenKind::Pipe)?.span.start;
        let mut params = Vec::new();
        if !matches!(self.lookahead.kind, TokenKind::Pipe) {
            loop {
                let p_start = self.lookahead.span.start;
                let name = self.expect_ident()?;
                let ty = if matches!(self.lookahead.kind, TokenKind::Colon) {
                    self.bump()?;
                    Some(self.parse_type()?)
                } else {
                    None
                };
                let end = ty.as_ref().map(|t| t.span().end).unwrap_or(name.span.end);
                params.push(LambdaParam {
                    name,
                    ty,
                    span: Span::new(p_start, end),
                });
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    continue;
                }
                break;
            }
        }
        self.expect(TokenKind::Pipe)?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(Expr::Lambda {
            params,
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_path_expr(&mut self) -> Result<Path, ParseError> {
        let start = self.lookahead.span.start;
        let mut segments = Vec::new();
        segments.push(self.expect_ident()?);
        while matches!(self.lookahead.kind, TokenKind::ColonColon) {
            self.bump()?;
            segments.push(self.expect_ident()?);
        }
        let end = segments.last().map(|s| s.span.end).unwrap_or(start);
        Ok(Path {
            segments,
            span: Span::new(start, end),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.lookahead.kind.clone() {
            TokenKind::Ident(ref s) if s == "_" => {
                let tok = self.bump()?;
                Ok(Pattern::Wildcard { span: tok.span })
            }
            TokenKind::LParen => self.parse_paren_pat(),
            TokenKind::Int(v) => {
                let tok = self.bump()?;
                Ok(Pattern::Literal {
                    lit: PatLiteral::Int(v),
                    span: tok.span,
                })
            }
            TokenKind::Float(v) => {
                let tok = self.bump()?;
                Ok(Pattern::Literal {
                    lit: PatLiteral::Float(v),
                    span: tok.span,
                })
            }
            TokenKind::String(s) => {
                let tok = self.bump()?;
                Ok(Pattern::Literal {
                    lit: PatLiteral::String(s),
                    span: tok.span,
                })
            }
            TokenKind::Bytes(b) => {
                let tok = self.bump()?;
                Ok(Pattern::Literal {
                    lit: PatLiteral::Bytes(b),
                    span: tok.span,
                })
            }
            TokenKind::Ident(ref s) if s == "true" || s == "false" => {
                let tok = self.bump()?;
                Ok(Pattern::Literal {
                    lit: PatLiteral::Bool(s == "true"),
                    span: tok.span,
                })
            }
            TokenKind::LBracket => self.parse_array_pat(),
            TokenKind::Ident(_) => self.parse_bind_or_ctor_pat(),
            _ => Err(self.error_here("expected pattern")),
        }
    }

    fn parse_paren_pat(&mut self) -> Result<Pattern, ParseError> {
        let start = self.expect(TokenKind::LParen)?.span.start;
        if matches!(self.lookahead.kind, TokenKind::RParen) {
            let end = self.bump()?.span.end;
            return Ok(Pattern::Literal {
                lit: PatLiteral::Unit,
                span: Span::new(start, end),
            });
        }
        // Tuple pattern with leading rest: `(..rest, x, y)` or `(..)`.
        if matches!(self.lookahead.kind, TokenKind::DotDot) {
            let rest = Some(self.parse_rest_pat(true)?);
            let mut suffix = Vec::new();
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
                if !matches!(self.lookahead.kind, TokenKind::RParen) {
                    loop {
                        if matches!(self.lookahead.kind, TokenKind::DotDot) {
                            return Err(
                                self.error_here("`..` can only appear once in a tuple pattern")
                            );
                        }
                        suffix.push(self.parse_pattern()?);
                        if matches!(self.lookahead.kind, TokenKind::Comma) {
                            self.bump()?;
                            if matches!(self.lookahead.kind, TokenKind::RParen) {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
            }
            let end = self.expect(TokenKind::RParen)?.span.end;
            return Ok(Pattern::Tuple {
                prefix: Vec::new(),
                rest,
                suffix,
                span: Span::new(start, end),
            });
        }

        let first = self.parse_pattern()?;
        if matches!(self.lookahead.kind, TokenKind::RParen) {
            // Parenthesized pattern.
            self.expect(TokenKind::RParen)?;
            return Ok(first);
        }

        if !matches!(self.lookahead.kind, TokenKind::Comma) {
            return Err(self.error_here("expected `,` or `)` in tuple pattern"));
        }

        // Tuple pattern (possibly with `..`).
        self.bump()?;
        let mut prefix = vec![first];
        let mut rest: Option<RestPat> = None;
        let mut suffix = Vec::new();
        if !matches!(self.lookahead.kind, TokenKind::RParen) {
            loop {
                if matches!(self.lookahead.kind, TokenKind::DotDot) {
                    if rest.is_some() {
                        return Err(self.error_here("`..` can only appear once in a tuple pattern"));
                    }
                    rest = Some(self.parse_rest_pat(true)?);
                } else {
                    let p = self.parse_pattern()?;
                    if rest.is_some() {
                        suffix.push(p);
                    } else {
                        prefix.push(p);
                    }
                }

                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                    if matches!(self.lookahead.kind, TokenKind::RParen) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokenKind::RParen)?.span.end;
        Ok(Pattern::Tuple {
            prefix,
            rest,
            suffix,
            span: Span::new(start, end),
        })
    }

    fn parse_array_pat(&mut self) -> Result<Pattern, ParseError> {
        let start = self.expect(TokenKind::LBracket)?.span.start;
        let mut prefix = Vec::new();
        let mut rest: Option<RestPat> = None;
        let mut suffix = Vec::new();
        while !matches!(self.lookahead.kind, TokenKind::RBracket) {
            if matches!(self.lookahead.kind, TokenKind::DotDot) {
                if rest.is_some() {
                    return Err(self.error_here("`..` can only appear once in an array pattern"));
                }
                rest = Some(self.parse_rest_pat(true)?);
            } else {
                let p = self.parse_pattern()?;
                if rest.is_some() {
                    suffix.push(p);
                } else {
                    prefix.push(p);
                }
            }
            if matches!(self.lookahead.kind, TokenKind::Comma) {
                self.bump()?;
                if matches!(self.lookahead.kind, TokenKind::RBracket) {
                    break;
                }
                continue;
            }
            break;
        }
        let end = self.expect(TokenKind::RBracket)?.span.end;
        Ok(Pattern::Array {
            prefix,
            rest,
            suffix,
            span: Span::new(start, end),
        })
    }

    fn parse_rest_pat(&mut self, allow_binding: bool) -> Result<RestPat, ParseError> {
        let dotdot = self.expect(TokenKind::DotDot)?;
        let start = dotdot.span.start;
        let dotdot_end = dotdot.span.end;
        let binding = if allow_binding && matches!(self.lookahead.kind, TokenKind::Ident(_)) {
            Some(self.expect_ident()?)
        } else if !allow_binding && matches!(self.lookahead.kind, TokenKind::Ident(_)) {
            return Err(ParseError {
                message: "struct patterns do not support binding `..name`".to_string(),
                span: self.lookahead.span,
            });
        } else {
            None
        };
        let end = binding.as_ref().map(|b| b.span.end).unwrap_or(dotdot_end);
        Ok(RestPat {
            binding,
            span: Span::new(start, end),
        })
    }

    fn parse_bind_or_ctor_pat(&mut self) -> Result<Pattern, ParseError> {
        let start = self.lookahead.span.start;
        let mut segments = vec![self.expect_ident()?];
        while matches!(self.lookahead.kind, TokenKind::ColonColon) {
            self.bump()?;
            segments.push(self.expect_ident()?);
        }

        // Enum pattern: `EnumPath::Variant(...)` (note that `EnumPath` itself may be qualified).
        if matches!(self.lookahead.kind, TokenKind::LParen) {
            if segments.len() < 2 {
                return Err(self.error_here("expected `Enum::Variant(...)` pattern"));
            }
            let variant = segments.pop().expect("len >= 2");
            let enum_end = segments.last().map(|s| s.span.end).unwrap_or(start);
            let enum_path = Path {
                segments,
                span: Span::new(start, enum_end),
            };

            self.expect(TokenKind::LParen)?;
            let mut fields = Vec::new();
            if !matches!(self.lookahead.kind, TokenKind::RParen) {
                loop {
                    fields.push(self.parse_pattern()?);
                    if matches!(self.lookahead.kind, TokenKind::Comma) {
                        self.bump()?;
                        if matches!(self.lookahead.kind, TokenKind::RParen) {
                            break;
                        }
                        continue;
                    }
                    break;
                }
            }
            let end = self.expect(TokenKind::RParen)?.span.end;
            return Ok(Pattern::Enum {
                enum_path,
                variant,
                fields,
                span: Span::new(start, end),
            });
        }

        // Struct pattern: `TypePath { field, field: pat, .. }` (TypePath may be qualified).
        if matches!(self.lookahead.kind, TokenKind::LBrace) {
            let type_end = segments.last().map(|s| s.span.end).unwrap_or(start);
            let type_path = Path {
                segments,
                span: Span::new(start, type_end),
            };

            self.bump()?;
            let mut fields = Vec::new();
            let mut has_rest = false;
            while !matches!(self.lookahead.kind, TokenKind::RBrace) {
                if matches!(self.lookahead.kind, TokenKind::DotDot) {
                    if has_rest {
                        return Err(
                            self.error_here("`..` can only appear once in a struct pattern")
                        );
                    }
                    let _ = self.parse_rest_pat(false)?;
                    has_rest = true;
                } else {
                    let fname = self.expect_ident()?;
                    if matches!(self.lookahead.kind, TokenKind::Colon) {
                        self.bump()?;
                        let pat = self.parse_pattern()?;
                        fields.push((fname, pat));
                    } else {
                        let pat = Pattern::Bind {
                            name: fname.clone(),
                            span: fname.span,
                        };
                        fields.push((fname, pat));
                    }
                }
                if matches!(self.lookahead.kind, TokenKind::Comma) {
                    self.bump()?;
                } else {
                    break;
                }
            }
            let end = self.expect(TokenKind::RBrace)?.span.end;
            return Ok(Pattern::Struct {
                type_path,
                fields,
                has_rest,
                span: Span::new(start, end),
            });
        }

        // Bare enum variant pattern: `EnumPath::Variant` (zero-field variant).
        //
        // This is syntactic sugar for `EnumPath::Variant()`. The typechecker is responsible for
        // rejecting bare patterns for non-zero-field variants.
        if segments.len() >= 2 {
            let variant = segments.pop().expect("len >= 2");
            let enum_end = segments.last().map(|s| s.span.end).unwrap_or(start);
            let enum_path = Path {
                segments,
                span: Span::new(start, enum_end),
            };
            let end = variant.span.end;
            return Ok(Pattern::Enum {
                enum_path,
                variant,
                fields: Vec::new(),
                span: Span::new(start, end),
            });
        }

        if segments.len() != 1 {
            return Err(self.error_here("expected `(` or `{` after pattern path"));
        }

        // Otherwise: binding.
        let head = segments.pop().expect("len == 1");
        Ok(Pattern::Bind {
            name: head.clone(),
            span: Span::new(start, head.span.end),
        })
    }

    fn desugar_fstring(&mut self, span: Span, parts: Vec<FStringPart>) -> Result<Expr, ParseError> {
        let mut expr: Option<Expr> = None;
        for part in parts {
            match part {
                FStringPart::Text(text) => {
                    let lit = Expr::String { value: text, span };
                    expr = Some(match expr {
                        None => lit,
                        Some(acc) => self.core_intrinsic_call2(span, "string_concat", acc, lit),
                    });
                }
                FStringPart::Expr { src, base_offset } => {
                    let mut nested = Parser::with_base_offset(&src, base_offset)?;
                    let value_expr = nested.parse_expr_eof()?;
                    let to_string = self.core_intrinsic_call1(span, "to_string", value_expr);
                    expr = Some(match expr {
                        None => to_string,
                        Some(acc) => {
                            self.core_intrinsic_call2(span, "string_concat", acc, to_string)
                        }
                    });
                }
            }
        }
        Ok(expr.unwrap_or(Expr::String {
            value: String::new(),
            span,
        }))
    }

    fn core_intrinsic_call1(&self, span: Span, name: &str, arg: Expr) -> Expr {
        let callee = self.core_intrinsic_path_expr(span, name);
        Expr::Call {
            callee: Box::new(callee),
            args: vec![arg],
            span,
        }
    }

    fn core_intrinsic_call2(&self, span: Span, name: &str, a: Expr, b: Expr) -> Expr {
        let callee = self.core_intrinsic_path_expr(span, name);
        Expr::Call {
            callee: Box::new(callee),
            args: vec![a, b],
            span,
        }
    }

    fn core_intrinsic_path_expr(&self, span: Span, name: &str) -> Expr {
        let core = Ident {
            name: "core".to_string(),
            span,
        };
        let intrinsics = Ident {
            name: "intrinsics".to_string(),
            span,
        };
        let func = Ident {
            name: name.to_string(),
            span,
        };
        Expr::Path {
            path: Path {
                segments: vec![core, intrinsics, func],
                span,
            },
            span,
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        let tok = self.bump()?;
        match tok.kind {
            TokenKind::Ident(name) => Ok(Ident {
                name,
                span: tok.span,
            }),
            _ => Err(ParseError {
                message: "expected identifier".to_string(),
                span: tok.span,
            }),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, ParseError> {
        if self.lookahead.kind == expected {
            return self.bump();
        }
        Err(ParseError {
            message: format!("expected {expected:?}"),
            span: self.lookahead.span,
        })
    }

    fn bump(&mut self) -> Result<Token, ParseError> {
        let current = std::mem::replace(
            &mut self.lookahead,
            Token {
                kind: TokenKind::Eof,
                span: Span::new(0, 0),
            },
        );
        self.lookahead = self.lexer.next_token()?;
        Ok(current)
    }

    fn error_here(&self, message: impl Into<String>) -> ParseError {
        ParseError {
            message: message.into(),
            span: self.lookahead.span,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Infix {
    Assign,
    Binary(BinaryOp),
}
