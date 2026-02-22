use crate::ast::{
    Block, DeriveItem, Expr, FieldDecl, FieldName, FnItem, FnItemKind, GenericParam, Ident,
    ImplHeader, ImplItem, ImplMember, Item, MatchArm, MatchPat, MethodReceiverKind, ModKind, Path,
    PathType, PathTypeSegment, Pattern, PrimType, Stmt, StructBody, StructItem, TypeExpr,
    Visibility,
};
use crate::compiler::CompileError;
use crate::source::Span;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug)]
enum NominalTypeDef {
    Struct(StructItem),
    Enum(crate::ast::EnumItem),
}

#[derive(Clone, Debug)]
struct SpanGen {
    next: usize,
}

impl SpanGen {
    fn new(start: usize) -> Self {
        Self { next: start }
    }

    fn fresh(&mut self) -> Span {
        let start = self.next;
        self.next = self.next.saturating_add(1);
        Span::new(start, start.saturating_add(1))
    }
}

pub(crate) fn expand_derives(
    program: &crate::ast::Program,
) -> Result<crate::ast::Program, CompileError> {
    let items = expand_items_in_module(&program.items, &[])?;
    Ok(crate::ast::Program { items })
}

fn expand_items_in_module(
    items: &[Item],
    module_path: &[String],
) -> Result<Vec<Item>, CompileError> {
    // First pass: collect nominal types in this module.
    let mut types: BTreeMap<String, NominalTypeDef> = BTreeMap::new();
    for item in items {
        match item {
            Item::Struct(s) => {
                types.insert(s.name.name.clone(), NominalTypeDef::Struct(s.clone()));
            }
            Item::Enum(e) => {
                types.insert(e.name.name.clone(), NominalTypeDef::Enum(e.clone()));
            }
            _ => {}
        }
    }

    // Second pass: recursively expand and synthesize derive impls.
    let mut out: Vec<Item> = Vec::with_capacity(items.len());
    for item in items {
        match item {
            Item::Mod(m) => {
                let mut next = m.clone();
                if let ModKind::Inline { items: inner } = &m.kind {
                    let mut child_path = module_path.to_vec();
                    child_path.push(m.name.name.clone());
                    let expanded = expand_items_in_module(inner, &child_path)?;
                    next.kind = ModKind::Inline { items: expanded };
                }
                out.push(Item::Mod(next));
            }
            Item::Derive(d) => {
                let ty = types.get(&d.target.name).ok_or_else(|| {
                    CompileError {
                        message: format!(
                            "derive target `{}` not found in module `{}` (derive must be in the same module as the type definition)",
                            d.target.name,
                            display_module_path(module_path),
                        ),
                        span: d.target.span,
                        rendered_location: None,
                    }
                })?;
                let impls = expand_derive_item(d, ty)?;
                out.extend(impls);
            }
            // Items that don't affect expansion.
            Item::Function(_)
            | Item::IntrinsicFn(_)
            | Item::Struct(_)
            | Item::Enum(_)
            | Item::Interface(_)
            | Item::Impl(_)
            | Item::Use(_) => out.push(item.clone()),
        }
    }
    Ok(out)
}

fn display_module_path(path: &[String]) -> String {
    if path.is_empty() {
        "crate".to_string()
    } else {
        path.join("::")
    }
}

fn expand_derive_item(d: &DeriveItem, ty: &NominalTypeDef) -> Result<Vec<Item>, CompileError> {
    let mut out = Vec::new();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for derive in &d.derives {
        if !seen.insert(derive.name.clone()) {
            continue;
        }
        match derive.name.as_str() {
            "Serialize" => out.push(Item::Impl(derive_serialize_impl(ty, d.span)?)),
            "Deserialize" => out.push(Item::Impl(derive_deserialize_impl(ty, d.span)?)),
            other => {
                return Err(CompileError {
                    message: format!(
                        "unknown derive `{other}`; supported derives: Serialize, Deserialize"
                    ),
                    span: derive.span,
                    rendered_location: None,
                });
            }
        }
    }
    Ok(out)
}

fn derive_serialize_impl(ty: &NominalTypeDef, span: Span) -> Result<ImplItem, CompileError> {
    match ty {
        NominalTypeDef::Struct(s) => derive_serialize_impl_for_struct(s, span),
        NominalTypeDef::Enum(e) => derive_serialize_impl_for_enum(e, span),
    }
}

fn derive_deserialize_impl(ty: &NominalTypeDef, span: Span) -> Result<ImplItem, CompileError> {
    match ty {
        NominalTypeDef::Struct(s) => derive_deserialize_impl_for_struct(s, span),
        NominalTypeDef::Enum(e) => derive_deserialize_impl_for_enum(e, span),
    }
}

fn serde_iface_path(name: &str, span: Span) -> PathType {
    PathType {
        segments: vec![
            PathTypeSegment {
                name: ident("core", span),
                args: Vec::new(),
                span,
            },
            PathTypeSegment {
                name: ident("serde", span),
                args: Vec::new(),
                span,
            },
            PathTypeSegment {
                name: ident(name, span),
                args: Vec::new(),
                span,
            },
        ],
        assoc_bindings: Vec::new(),
        span,
    }
}

fn result_type(ok: TypeExpr, err: TypeExpr, span: Span) -> TypeExpr {
    TypeExpr::Path(PathType {
        segments: vec![
            PathTypeSegment {
                name: ident("core", span),
                args: Vec::new(),
                span,
            },
            PathTypeSegment {
                name: ident("result", span),
                args: Vec::new(),
                span,
            },
            PathTypeSegment {
                name: ident("Result", span),
                args: vec![ok, err],
                span,
            },
        ],
        assoc_bindings: Vec::new(),
        span,
    })
}

fn unit_type(span: Span) -> TypeExpr {
    TypeExpr::Prim {
        prim: PrimType::Unit,
        span,
    }
}

fn self_type(span: Span) -> TypeExpr {
    TypeExpr::Path(PathType {
        segments: vec![PathTypeSegment {
            name: ident("Self", span),
            args: Vec::new(),
            span,
        }],
        assoc_bindings: Vec::new(),
        span,
    })
}

fn impl_type_path(name: &str, generics: &[GenericParam], span: Span) -> PathType {
    let args: Vec<TypeExpr> = generics
        .iter()
        .map(|g| {
            TypeExpr::Path(PathType {
                segments: vec![PathTypeSegment {
                    name: g.name.clone(),
                    args: Vec::new(),
                    span: g.name.span,
                }],
                assoc_bindings: Vec::new(),
                span: g.name.span,
            })
        })
        .collect();

    PathType {
        segments: vec![PathTypeSegment {
            name: ident(name, span),
            args,
            span,
        }],
        assoc_bindings: Vec::new(),
        span,
    }
}

fn derive_serialize_impl_for_struct(s: &StructItem, span: Span) -> Result<ImplItem, CompileError> {
    let impl_generics = add_serde_bounds_for_type_generics(
        &s.generics,
        &collect_type_param_uses_in_struct(s, span)?,
        "Serialize",
        span,
    );

    let header = ImplHeader::InterfaceForType {
        interface: serde_iface_path("Serialize", span),
        ty: impl_type_path(&s.name.name, &impl_generics, span),
        span,
    };

    let serializer_ty = TypeExpr::Path(serde_iface_path("Serializer", span));
    let ret_ty = result_type(
        unit_type(span),
        TypeExpr::Path(serde_iface_path("SerdeError", span)),
        span,
    );

    let body = match &s.body {
        StructBody::Named { fields } => {
            let mut ops: Vec<Expr> = Vec::new();
            ops.push(call_serializer(
                "struct_begin",
                vec![
                    expr_string(&s.name.name, span),
                    expr_int(fields.len() as i64, span),
                ],
                span,
            ));
            for FieldDecl { name, .. } in fields {
                ops.push(call_serializer(
                    "struct_field",
                    vec![expr_string(&name.name, span)],
                    span,
                ));
                ops.push(call_serialize(
                    expr_field(expr_self(span), FieldName::Named(name.clone()), span),
                    span,
                ));
            }
            ops.push(call_serializer("struct_end", vec![], span));
            chain_unit_results(ops, span)
        }
        StructBody::NewType { inner: _ } => chain_unit_results(
            vec![
                call_serializer(
                    "struct_begin",
                    vec![expr_string(&s.name.name, span), expr_int(1, span)],
                    span,
                ),
                call_serializer("struct_field", vec![expr_string("0", span)], span),
                call_serialize(
                    expr_field(expr_self(span), FieldName::Index { index: 0, span }, span),
                    span,
                ),
                call_serializer("struct_end", vec![], span),
            ],
            span,
        ),
    };

    let mut method = FnItem {
        vis: Visibility::Private,
        kind: FnItemKind::Method {
            receiver: MethodReceiverKind::Instance { readonly: true },
        },
        name: ident("serialize", span),
        generics: Vec::new(),
        params: vec![param("s", serializer_ty, span)],
        ret: ret_ty,
        body: Block {
            stmts: Vec::new(),
            tail: Some(Box::new(body)),
            span,
        },
        span,
    };
    respan_block(&mut method.body, &mut SpanGen::new(span.start));

    Ok(ImplItem {
        generics: impl_generics,
        header,
        members: vec![ImplMember::Method(method)],
        span,
    })
}

fn derive_deserialize_impl_for_struct(
    s: &StructItem,
    span: Span,
) -> Result<ImplItem, CompileError> {
    let impl_generics = add_serde_bounds_for_type_generics(
        &s.generics,
        &collect_type_param_uses_in_struct(s, span)?,
        "Deserialize",
        span,
    );

    let header = ImplHeader::InterfaceForType {
        interface: serde_iface_path("Deserialize", span),
        ty: impl_type_path(&s.name.name, &impl_generics, span),
        span,
    };

    let deserializer_ty = TypeExpr::Path(serde_iface_path("Deserializer", span));
    let ret_ty = result_type(
        self_type(span),
        TypeExpr::Path(serde_iface_path("SerdeError", span)),
        span,
    );

    let body = match &s.body {
        StructBody::Named { fields } => {
            let mut steps: Vec<DeserializeStep> = Vec::new();

            steps.push(DeserializeStep::Unit {
                expr: call_deserializer(
                    "struct_begin",
                    vec![
                        expr_string(&s.name.name, span),
                        expr_int(fields.len() as i64, span),
                    ],
                    span,
                ),
            });

            let mut field_binds: Vec<(Ident, Expr)> = Vec::new();
            for (idx, FieldDecl { name, ty, .. }) in fields.iter().enumerate() {
                let bind = ident(&format!("__rusk_serde_f{idx}"), span);
                steps.push(DeserializeStep::Unit {
                    expr: call_deserializer(
                        "struct_field",
                        vec![expr_string(&name.name, span)],
                        span,
                    ),
                });
                steps.push(DeserializeStep::Value {
                    bind: bind.clone(),
                    expr: call_deserialize(ty.clone(), vec![expr_path(&["d"], span)], span),
                });
                field_binds.push((name.clone(), expr_path(&[&bind.name], span)));
            }

            steps.push(DeserializeStep::Unit {
                expr: call_deserializer("struct_end", vec![], span),
            });

            let ok_value = Expr::StructLit {
                type_path: Path {
                    segments: vec![ident(&s.name.name, span)],
                    span,
                },
                fields: field_binds,
                span,
            };
            chain_deserialize_steps(steps, expr_result_ok(ok_value, span), span)
        }
        StructBody::NewType { inner } => {
            let mut steps: Vec<DeserializeStep> = Vec::new();
            steps.push(DeserializeStep::Unit {
                expr: call_deserializer(
                    "struct_begin",
                    vec![expr_string(&s.name.name, span), expr_int(1, span)],
                    span,
                ),
            });
            steps.push(DeserializeStep::Unit {
                expr: call_deserializer("struct_field", vec![expr_string("0", span)], span),
            });

            let bind = ident("__rusk_serde_inner", span);
            steps.push(DeserializeStep::Value {
                bind: bind.clone(),
                expr: call_deserialize(inner.clone(), vec![expr_path(&["d"], span)], span),
            });
            steps.push(DeserializeStep::Unit {
                expr: call_deserializer("struct_end", vec![], span),
            });

            let ok_value = Expr::Call {
                callee: Box::new(expr_path(&[&s.name.name], span)),
                type_args: Vec::new(),
                args: vec![expr_path(&[&bind.name], span)],
                span,
            };
            chain_deserialize_steps(steps, expr_result_ok(ok_value, span), span)
        }
    };

    let mut method = FnItem {
        vis: Visibility::Private,
        kind: FnItemKind::Method {
            receiver: MethodReceiverKind::Static,
        },
        name: ident("deserialize", span),
        generics: Vec::new(),
        params: vec![param("d", deserializer_ty, span)],
        ret: ret_ty,
        body: Block {
            stmts: Vec::new(),
            tail: Some(Box::new(body)),
            span,
        },
        span,
    };
    respan_block(&mut method.body, &mut SpanGen::new(span.start));

    Ok(ImplItem {
        generics: impl_generics,
        header,
        members: vec![ImplMember::Method(method)],
        span,
    })
}

fn derive_serialize_impl_for_enum(
    e: &crate::ast::EnumItem,
    span: Span,
) -> Result<ImplItem, CompileError> {
    let impl_generics = add_serde_bounds_for_type_generics(
        &e.generics,
        &collect_type_param_uses_in_enum(e, span)?,
        "Serialize",
        span,
    );

    let header = ImplHeader::InterfaceForType {
        interface: serde_iface_path("Serialize", span),
        ty: impl_type_path(&e.name.name, &impl_generics, span),
        span,
    };

    let serializer_ty = TypeExpr::Path(serde_iface_path("Serializer", span));
    let ret_ty = result_type(
        unit_type(span),
        TypeExpr::Path(serde_iface_path("SerdeError", span)),
        span,
    );

    let mut arms: Vec<MatchArm> = Vec::new();
    let variant_count = e.variants.len() as i64;
    for (idx, variant) in e.variants.iter().enumerate() {
        let mut binds: Vec<Ident> = Vec::new();
        let mut pat_fields: Vec<Pattern> = Vec::new();
        for field_idx in 0..variant.fields.len() {
            let b = ident(&format!("__rusk_serde_v{idx}_f{field_idx}"), span);
            binds.push(b.clone());
            pat_fields.push(Pattern::Bind { name: b, span });
        }

        let pat = Pattern::Ctor {
            path: Path {
                segments: vec![ident(&e.name.name, span), variant.name.clone()],
                span,
            },
            args: pat_fields,
            span,
        };

        let mut ops: Vec<Expr> = Vec::new();
        ops.push(call_serializer(
            "enum_begin",
            vec![
                expr_string(&e.name.name, span),
                expr_int(variant_count, span),
            ],
            span,
        ));
        ops.push(call_serializer(
            "enum_variant",
            vec![
                expr_string(&variant.name.name, span),
                expr_int(idx as i64, span),
                expr_int(variant.fields.len() as i64, span),
            ],
            span,
        ));
        for b in &binds {
            ops.push(call_serialize(expr_path(&[&b.name], span), span));
        }
        ops.push(call_serializer("enum_end", vec![], span));

        arms.push(MatchArm {
            pat: MatchPat::Value(pat),
            body: chain_unit_results(ops, span),
            span,
        });
    }

    let body = Expr::Match {
        scrutinee: Box::new(expr_self(span)),
        arms,
        span,
    };

    let mut method = FnItem {
        vis: Visibility::Private,
        kind: FnItemKind::Method {
            receiver: MethodReceiverKind::Instance { readonly: true },
        },
        name: ident("serialize", span),
        generics: Vec::new(),
        params: vec![param("s", serializer_ty, span)],
        ret: ret_ty,
        body: Block {
            stmts: Vec::new(),
            tail: Some(Box::new(body)),
            span,
        },
        span,
    };
    respan_block(&mut method.body, &mut SpanGen::new(span.start));

    Ok(ImplItem {
        generics: impl_generics,
        header,
        members: vec![ImplMember::Method(method)],
        span,
    })
}

fn derive_deserialize_impl_for_enum(
    e: &crate::ast::EnumItem,
    span: Span,
) -> Result<ImplItem, CompileError> {
    let impl_generics = add_serde_bounds_for_type_generics(
        &e.generics,
        &collect_type_param_uses_in_enum(e, span)?,
        "Deserialize",
        span,
    );

    let header = ImplHeader::InterfaceForType {
        interface: serde_iface_path("Deserialize", span),
        ty: impl_type_path(&e.name.name, &impl_generics, span),
        span,
    };

    let deserializer_ty = TypeExpr::Path(serde_iface_path("Deserializer", span));
    let ret_ty = result_type(
        self_type(span),
        TypeExpr::Path(serde_iface_path("SerdeError", span)),
        span,
    );

    let variant_count = e.variants.len() as i64;
    let tag_expr = call_deserializer(
        "enum_begin",
        vec![
            expr_string(&e.name.name, span),
            expr_int(variant_count, span),
        ],
        span,
    );

    // `match enum_begin(...) { Err(e) => Err(e), Ok(tag) => match tag { ... } }`
    let err_ident = ident("e", span);
    let tag_ident = ident("__rusk_serde_tag", span);

    let mut tag_arms: Vec<MatchArm> = Vec::new();
    for (idx, variant) in e.variants.iter().enumerate() {
        let mut steps: Vec<DeserializeStep> = Vec::new();
        steps.push(DeserializeStep::Unit {
            expr: call_deserializer(
                "enum_variant",
                vec![
                    expr_string(&variant.name.name, span),
                    expr_int(idx as i64, span),
                    expr_int(variant.fields.len() as i64, span),
                ],
                span,
            ),
        });

        let mut field_exprs: Vec<Expr> = Vec::new();
        for (field_idx, field_ty) in variant.fields.iter().enumerate() {
            let bind = ident(&format!("__rusk_serde_f{idx}_{field_idx}"), span);
            steps.push(DeserializeStep::Value {
                bind: bind.clone(),
                expr: call_deserialize(field_ty.clone(), vec![expr_path(&["d"], span)], span),
            });
            field_exprs.push(expr_path(&[&bind.name], span));
        }

        steps.push(DeserializeStep::Unit {
            expr: call_deserializer("enum_end", vec![], span),
        });

        let ok_value = Expr::Call {
            callee: Box::new(expr_path(&[&e.name.name, &variant.name.name], span)),
            type_args: Vec::new(),
            args: field_exprs,
            span,
        };

        let arm_body = chain_deserialize_steps(steps, expr_result_ok(ok_value, span), span);
        tag_arms.push(MatchArm {
            pat: MatchPat::Value(Pattern::Literal {
                lit: crate::ast::PatLiteral::Int(idx as i64),
                span,
            }),
            body: arm_body,
            span,
        });
    }

    // Default: unknown tag.
    tag_arms.push(MatchArm {
        pat: MatchPat::Value(Pattern::Wildcard { span }),
        body: expr_result_err(
            Expr::Call {
                callee: Box::new(expr_path(
                    &["core", "serde", "SerdeError", "UnknownEnumTag"],
                    span,
                )),
                type_args: Vec::new(),
                args: vec![
                    expr_string(&e.name.name, span),
                    expr_path(&[&tag_ident.name], span),
                ],
                span,
            },
            span,
        ),
        span,
    });

    let match_tag = Expr::Match {
        scrutinee: Box::new(expr_path(&[&tag_ident.name], span)),
        arms: tag_arms,
        span,
    };

    let body = Expr::Match {
        scrutinee: Box::new(tag_expr),
        arms: vec![
            MatchArm {
                pat: MatchPat::Value(Pattern::Ctor {
                    path: result_ctor_path("Err", span),
                    args: vec![Pattern::Bind {
                        name: err_ident.clone(),
                        span,
                    }],
                    span,
                }),
                body: expr_result_err(expr_path(&[&err_ident.name], span), span),
                span,
            },
            MatchArm {
                pat: MatchPat::Value(Pattern::Ctor {
                    path: result_ctor_path("Ok", span),
                    args: vec![Pattern::Bind {
                        name: tag_ident.clone(),
                        span,
                    }],
                    span,
                }),
                body: match_tag,
                span,
            },
        ],
        span,
    };

    let mut method = FnItem {
        vis: Visibility::Private,
        kind: FnItemKind::Method {
            receiver: MethodReceiverKind::Static,
        },
        name: ident("deserialize", span),
        generics: Vec::new(),
        params: vec![param("d", deserializer_ty, span)],
        ret: ret_ty,
        body: Block {
            stmts: Vec::new(),
            tail: Some(Box::new(body)),
            span,
        },
        span,
    };
    respan_block(&mut method.body, &mut SpanGen::new(span.start));

    Ok(ImplItem {
        generics: impl_generics,
        header,
        members: vec![ImplMember::Method(method)],
        span,
    })
}

fn collect_type_param_uses_in_struct(
    s: &StructItem,
    span: Span,
) -> Result<BTreeSet<String>, CompileError> {
    let names: BTreeMap<String, usize> = s
        .generics
        .iter()
        .map(|g| (g.name.name.clone(), g.arity))
        .collect();
    let mut used = BTreeSet::new();
    match &s.body {
        StructBody::Named { fields } => {
            for f in fields {
                collect_type_param_uses_in_type_expr(&f.ty, &names, &mut used, span)?;
            }
        }
        StructBody::NewType { inner } => {
            collect_type_param_uses_in_type_expr(inner, &names, &mut used, span)?;
        }
    }
    Ok(used)
}

fn collect_type_param_uses_in_enum(
    e: &crate::ast::EnumItem,
    span: Span,
) -> Result<BTreeSet<String>, CompileError> {
    let names: BTreeMap<String, usize> = e
        .generics
        .iter()
        .map(|g| (g.name.name.clone(), g.arity))
        .collect();
    let mut used = BTreeSet::new();
    for v in &e.variants {
        for fty in &v.fields {
            collect_type_param_uses_in_type_expr(fty, &names, &mut used, span)?;
        }
    }
    Ok(used)
}

fn collect_type_param_uses_in_type_expr(
    ty: &TypeExpr,
    params: &BTreeMap<String, usize>,
    out: &mut BTreeSet<String>,
    span: Span,
) -> Result<(), CompileError> {
    match ty {
        TypeExpr::Readonly { inner, .. } => {
            collect_type_param_uses_in_type_expr(inner, params, out, span)
        }
        TypeExpr::Prim { .. } => Ok(()),
        TypeExpr::Array { elem, .. } => {
            collect_type_param_uses_in_type_expr(elem, params, out, span)
        }
        TypeExpr::Tuple { items, .. } => {
            for item in items {
                collect_type_param_uses_in_type_expr(item, params, out, span)?;
            }
            Ok(())
        }
        TypeExpr::Fn { .. } | TypeExpr::Cont { .. } => Ok(()),
        TypeExpr::Path(p) => {
            for seg in &p.segments {
                for arg in &seg.args {
                    collect_type_param_uses_in_type_expr(arg, params, out, span)?;
                }
            }
            for binding in &p.assoc_bindings {
                collect_type_param_uses_in_type_expr(&binding.ty, params, out, span)?;
            }

            if p.segments.len() == 1 {
                let name = p.segments[0].name.name.as_str();
                if let Some(arity) = params.get(name) {
                    if *arity != 0 {
                        return Err(CompileError {
                            message: format!(
                                "derive does not support higher-kinded type parameter `{name}` (arity {arity})"
                            ),
                            span,
                            rendered_location: None,
                        });
                    }
                    out.insert(name.to_string());
                }
            }
            Ok(())
        }
    }
}

fn add_serde_bounds_for_type_generics(
    original: &[GenericParam],
    used: &BTreeSet<String>,
    iface: &str,
    span: Span,
) -> Vec<GenericParam> {
    let mut out = original.to_vec();
    let bound = serde_iface_path(iface, span);
    for g in &mut out {
        if !used.contains(&g.name.name) {
            continue;
        }
        if !g.bounds.contains(&bound) {
            g.bounds.push(bound.clone());
        }
    }
    out
}

fn ident(name: &str, span: Span) -> Ident {
    Ident {
        name: name.to_string(),
        span,
    }
}

fn param(name: &str, ty: TypeExpr, span: Span) -> crate::ast::Param {
    crate::ast::Param {
        pat: Pattern::Bind {
            name: ident(name, span),
            span,
        },
        ty,
        span,
    }
}

fn expr_path(segments: &[&str], span: Span) -> Expr {
    Expr::Path {
        path: Path {
            segments: segments.iter().map(|s| ident(s, span)).collect(),
            span,
        },
        span,
    }
}

fn expr_self(span: Span) -> Expr {
    expr_path(&["self"], span)
}

fn expr_int(value: i64, span: Span) -> Expr {
    Expr::Int { value, span }
}

fn expr_string(value: &str, span: Span) -> Expr {
    Expr::String {
        value: value.to_string(),
        span,
    }
}

fn expr_field(base: Expr, name: FieldName, span: Span) -> Expr {
    Expr::Field {
        base: Box::new(base),
        name,
        span,
    }
}

fn call_serialize(value: Expr, span: Span) -> Expr {
    Expr::Call {
        callee: Box::new(expr_path(
            &["core", "serde", "Serialize", "serialize"],
            span,
        )),
        type_args: Vec::new(),
        args: vec![value, expr_path(&["s"], span)],
        span,
    }
}

fn call_deserialize(self_ty: TypeExpr, args: Vec<Expr>, span: Span) -> Expr {
    Expr::Call {
        callee: Box::new(expr_path(
            &["core", "serde", "Deserialize", "deserialize"],
            span,
        )),
        type_args: vec![self_ty],
        args,
        span,
    }
}

fn call_serializer(name: &str, args: Vec<Expr>, span: Span) -> Expr {
    let segs = vec!["core", "serde", "Serializer", name];
    Expr::Call {
        callee: Box::new(expr_path(&segs, span)),
        type_args: Vec::new(),
        args: {
            let mut out = Vec::with_capacity(args.len() + 1);
            out.push(expr_path(&["s"], span));
            out.extend(args);
            out
        },
        span,
    }
}

fn call_deserializer(name: &str, args: Vec<Expr>, span: Span) -> Expr {
    let segs = vec!["core", "serde", "Deserializer", name];
    Expr::Call {
        callee: Box::new(expr_path(&segs, span)),
        type_args: Vec::new(),
        args: {
            let mut out = Vec::with_capacity(args.len() + 1);
            out.push(expr_path(&["d"], span));
            out.extend(args);
            out
        },
        span,
    }
}

fn result_ctor_path(variant: &str, span: Span) -> Path {
    Path {
        segments: vec![
            ident("core", span),
            ident("result", span),
            ident("Result", span),
            ident(variant, span),
        ],
        span,
    }
}

fn expr_result_ok(value: Expr, span: Span) -> Expr {
    Expr::Call {
        callee: Box::new(expr_path(&["core", "result", "Result", "Ok"], span)),
        type_args: Vec::new(),
        args: vec![value],
        span,
    }
}

fn expr_result_err(value: Expr, span: Span) -> Expr {
    Expr::Call {
        callee: Box::new(expr_path(&["core", "result", "Result", "Err"], span)),
        type_args: Vec::new(),
        args: vec![value],
        span,
    }
}

fn chain_unit_results(mut ops: Vec<Expr>, span: Span) -> Expr {
    let Some(last) = ops.pop() else {
        return expr_result_ok(Expr::Unit { span }, span);
    };
    ops.into_iter().rev().fold(last, |acc, op| Expr::Match {
        scrutinee: Box::new(op),
        arms: vec![
            MatchArm {
                pat: MatchPat::Value(Pattern::Ctor {
                    path: result_ctor_path("Err", span),
                    args: vec![Pattern::Bind {
                        name: ident("e", span),
                        span,
                    }],
                    span,
                }),
                body: expr_result_err(expr_path(&["e"], span), span),
                span,
            },
            MatchArm {
                pat: MatchPat::Value(Pattern::Ctor {
                    path: result_ctor_path("Ok", span),
                    args: vec![Pattern::Wildcard { span }],
                    span,
                }),
                body: acc,
                span,
            },
        ],
        span,
    })
}

#[derive(Clone, Debug)]
enum DeserializeStep {
    Unit { expr: Expr },
    Value { bind: Ident, expr: Expr },
}

fn chain_deserialize_steps(steps: Vec<DeserializeStep>, done: Expr, span: Span) -> Expr {
    steps.into_iter().rev().fold(done, |acc, step| match step {
        DeserializeStep::Unit { expr } => Expr::Match {
            scrutinee: Box::new(expr),
            arms: vec![
                MatchArm {
                    pat: MatchPat::Value(Pattern::Ctor {
                        path: result_ctor_path("Err", span),
                        args: vec![Pattern::Bind {
                            name: ident("e", span),
                            span,
                        }],
                        span,
                    }),
                    body: expr_result_err(expr_path(&["e"], span), span),
                    span,
                },
                MatchArm {
                    pat: MatchPat::Value(Pattern::Ctor {
                        path: result_ctor_path("Ok", span),
                        args: vec![Pattern::Wildcard { span }],
                        span,
                    }),
                    body: acc,
                    span,
                },
            ],
            span,
        },
        DeserializeStep::Value { bind, expr } => Expr::Match {
            scrutinee: Box::new(expr),
            arms: vec![
                MatchArm {
                    pat: MatchPat::Value(Pattern::Ctor {
                        path: result_ctor_path("Err", span),
                        args: vec![Pattern::Bind {
                            name: ident("e", span),
                            span,
                        }],
                        span,
                    }),
                    body: expr_result_err(expr_path(&["e"], span), span),
                    span,
                },
                MatchArm {
                    pat: MatchPat::Value(Pattern::Ctor {
                        path: result_ctor_path("Ok", span),
                        args: vec![Pattern::Bind { name: bind, span }],
                        span,
                    }),
                    body: acc,
                    span,
                },
            ],
            span,
        },
    })
}

fn respan_block(block: &mut Block, span_gen: &mut SpanGen) {
    block.span = span_gen.fresh();
    for stmt in &mut block.stmts {
        respan_stmt(stmt, span_gen);
    }
    if let Some(expr) = &mut block.tail {
        respan_expr(expr, span_gen);
    }
}

fn respan_stmt(stmt: &mut Stmt, span_gen: &mut SpanGen) {
    match stmt {
        Stmt::Let {
            init: Some(init),
            span,
            ..
        } => {
            *span = span_gen.fresh();
            respan_expr(init, span_gen);
        }
        Stmt::Let { span, .. } => {
            *span = span_gen.fresh();
        }
        Stmt::Return {
            value: Some(value),
            span,
        } => {
            *span = span_gen.fresh();
            respan_expr(value, span_gen);
        }
        Stmt::Return { span, .. }
        | Stmt::Break { span }
        | Stmt::Continue { span }
        | Stmt::Expr { span, .. } => {
            *span = span_gen.fresh();
            if let Stmt::Expr { expr, .. } = stmt {
                respan_expr(expr, span_gen);
            }
        }
    }
}

fn respan_expr(expr: &mut Expr, span_gen: &mut SpanGen) {
    match expr {
        Expr::Unit { span }
        | Expr::Bool { span, .. }
        | Expr::Int { span, .. }
        | Expr::Float { span, .. }
        | Expr::Char { span, .. }
        | Expr::String { span, .. }
        | Expr::Bytes { span, .. } => {
            *span = span_gen.fresh();
        }

        Expr::Path { span, .. } => {
            *span = span_gen.fresh();
        }
        Expr::Array { items, span } | Expr::Tuple { items, span } => {
            *span = span_gen.fresh();
            for item in items {
                respan_expr(item, span_gen);
            }
        }
        Expr::StructLit { fields, span, .. } => {
            *span = span_gen.fresh();
            for (_name, value) in fields {
                respan_expr(value, span_gen);
            }
        }
        Expr::EffectCall { args, span, .. } => {
            *span = span_gen.fresh();
            for arg in args {
                respan_expr(arg, span_gen);
            }
        }

        Expr::Lambda { body, span, .. } => {
            *span = span_gen.fresh();
            respan_block(body, span_gen);
        }
        Expr::If {
            cond,
            then_block,
            else_branch,
            span,
        } => {
            *span = span_gen.fresh();
            respan_expr(cond, span_gen);
            respan_block(then_block, span_gen);
            if let Some(else_branch) = else_branch {
                respan_expr(else_branch, span_gen);
            }
        }
        Expr::Match {
            scrutinee,
            arms,
            span,
        } => {
            *span = span_gen.fresh();
            respan_expr(scrutinee, span_gen);
            for arm in arms {
                arm.span = span_gen.fresh();
                respan_expr(&mut arm.body, span_gen);
            }
        }
        Expr::Loop { body, span } => {
            *span = span_gen.fresh();
            respan_block(body, span_gen);
        }
        Expr::While { cond, body, span } => {
            *span = span_gen.fresh();
            respan_expr(cond, span_gen);
            respan_block(body, span_gen);
        }
        Expr::For {
            iter, body, span, ..
        } => {
            *span = span_gen.fresh();
            respan_expr(iter, span_gen);
            respan_block(body, span_gen);
        }
        Expr::Block { block, span } => {
            *span = span_gen.fresh();
            respan_block(block, span_gen);
        }

        Expr::Call {
            callee, args, span, ..
        } => {
            *span = span_gen.fresh();
            respan_expr(callee, span_gen);
            for arg in args {
                respan_expr(arg, span_gen);
            }
        }
        Expr::Field { base, span, .. } => {
            *span = span_gen.fresh();
            respan_expr(base, span_gen);
        }
        Expr::Index { base, index, span } => {
            *span = span_gen.fresh();
            respan_expr(base, span_gen);
            respan_expr(index, span_gen);
        }
        Expr::Unary { expr, span, .. } => {
            *span = span_gen.fresh();
            respan_expr(expr, span_gen);
        }
        Expr::Binary {
            left, right, span, ..
        } => {
            *span = span_gen.fresh();
            respan_expr(left, span_gen);
            respan_expr(right, span_gen);
        }
        Expr::Assign {
            target,
            value,
            span,
            ..
        } => {
            *span = span_gen.fresh();
            respan_expr(target, span_gen);
            respan_expr(value, span_gen);
        }
        Expr::As { expr, span, .. }
        | Expr::AsQuestion { expr, span, .. }
        | Expr::Is { expr, span, .. } => {
            *span = span_gen.fresh();
            respan_expr(expr, span_gen);
        }
    }
}
