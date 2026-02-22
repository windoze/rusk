use super::*;

#[derive(Clone, Debug)]
struct LocalInfo {
    ty: Ty,
    kind: BindingKind,
    span: Span,
}

pub(crate) fn typecheck_program(
    program: &Program,
    env: &ProgramEnv,
) -> Result<TypeInfo, TypeError> {
    let mut info = TypeInfo::default();
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Function(func) => {
                let fn_name = module.qualify(&func.name.name);
                let sig = env.functions.get(&fn_name).expect("declared");
                let fn_info = typecheck_function_body(env, module, sig, func)?;
                if info.functions.insert(fn_name, fn_info).is_some() {
                    return Err(TypeError {
                        message: "internal error: duplicate function type info".to_string(),
                        span: func.span,
                    });
                }
                Ok(())
            }
            Item::Interface(_iface) => Ok(()),
            Item::Impl(imp) => {
                let fn_info = typecheck_impl_item(env, module, imp)?;
                info.functions.extend(fn_info.functions);
                Ok(())
            }
            _ => Ok(()),
        },
    )?;
    Ok(info)
}

fn typecheck_impl_item(
    env: &ProgramEnv,
    module: &ModulePath,
    item: &ImplItem,
) -> Result<TypeInfo, TypeError> {
    let mut info = TypeInfo::default();
    let impl_generics = lower_generic_params(env, module, &item.generics, true)?;
    let impl_scope = GenericScope::new(&impl_generics)?;
    match &item.header {
        ImplHeader::Inherent { ty, .. } => {
            let ty = lower_path_type(env, module, &impl_scope, ty)?;
            validate_nominal_bounds_well_formed_in_ty(env, &impl_generics, &ty, item.span)?;
            let type_name = match ty {
                Ty::App(TyCon::Named(name), _args) => name,
                other => {
                    return Err(TypeError {
                        message: format!(
                            "inherent impl target must be a nominal type, got `{other}`"
                        ),
                        span: item.span,
                    });
                }
            };
            for member in &item.members {
                let ImplMember::Method(method) = member else {
                    continue;
                };
                let fn_name = format!("{type_name}::{}", method.name.name);
                let sig = env.functions.get(&fn_name).ok_or(TypeError {
                    message: format!("internal error: missing method signature for `{fn_name}`"),
                    span: method.name.span,
                })?;
                let fn_info = typecheck_function_body(env, module, sig, method)?;
                info.functions.insert(fn_name, fn_info);
            }
        }
        ImplHeader::InterfaceForType { interface, ty, .. } => {
            let iface_ty = lower_path_type(env, module, &impl_scope, interface)?;
            validate_nominal_bounds_well_formed_in_ty(env, &impl_generics, &iface_ty, item.span)?;
            let iface_name = match iface_ty {
                Ty::App(TyCon::Named(name), _args) => name,
                other => {
                    return Err(TypeError {
                        message: format!(
                            "impl interface must be a nominal interface type, got `{other}`"
                        ),
                        span: item.span,
                    });
                }
            };
            let ty = lower_path_type(env, module, &impl_scope, ty)?;
            validate_nominal_bounds_well_formed_in_ty(env, &impl_generics, &ty, item.span)?;
            let type_name = match ty {
                Ty::App(TyCon::Named(name), _args) => name,
                other => {
                    return Err(TypeError {
                        message: format!("impl target must be a nominal type, got `{other}`"),
                        span: item.span,
                    });
                }
            };

            for member in &item.members {
                let ImplMember::Method(method) = member else {
                    continue;
                };
                let mname = &method.name.name;
                let fn_name = format!("impl::{iface_name}::for::{type_name}::{mname}");
                let sig = env.functions.get(&fn_name).ok_or(TypeError {
                    message: format!("internal error: missing method signature for `{fn_name}`"),
                    span: method.name.span,
                })?;
                let fn_info = typecheck_function_body(env, module, sig, method)?;
                info.functions.insert(fn_name, fn_info);
            }

            // Typecheck per-impl specialized default method bodies.
            let iface_def = env.interfaces.get(&iface_name).ok_or(TypeError {
                message: format!("internal error: missing interface `{iface_name}`"),
                span: item.span,
            })?;
            let implemented: BTreeSet<&str> = item
                .members
                .iter()
                .filter_map(|m| match m {
                    ImplMember::Method(method) => Some(method.name.name.as_str()),
                    ImplMember::AssocType(_) => None,
                })
                .collect();
            for (mname, method_info) in &iface_def.all_methods {
                if !method_info.has_default || implemented.contains(mname.as_str()) {
                    continue;
                }
                let Some(template) = &method_info.default_template else {
                    return Err(TypeError {
                        message: format!(
                            "internal error: missing default template for `{}`",
                            method_info.origin
                        ),
                        span: item.span,
                    });
                };

                let origin_module = env
                    .modules
                    .def(&method_info.origin)
                    .map(|d| d.defining_module.clone())
                    .ok_or(TypeError {
                        message: format!(
                            "internal error: missing module for interface `{}`",
                            method_info.origin
                        ),
                        span: item.span,
                    })?;

                let fn_name = format!("impl::{iface_name}::for::{type_name}::{mname}");
                let sig = env.functions.get(&fn_name).ok_or(TypeError {
                    message: format!("internal error: missing method signature for `{fn_name}`"),
                    span: item.span,
                })?;

                let synthesized = synthesize_default_method_fn_item(method_info.receiver, template);
                let origin_iface_def =
                    env.interfaces.get(&method_info.origin).ok_or(TypeError {
                        message: format!(
                            "internal error: missing interface `{}`",
                            method_info.origin
                        ),
                        span: item.span,
                    })?;
                let origin_iface_arity = origin_iface_def.generics.len();
                let method_generics_len = method_info.sig.generics.len();
                let impl_arity =
                    sig.generics
                        .len()
                        .checked_sub(method_generics_len)
                        .ok_or(TypeError {
                            message: "internal error: synthesized method generics length mismatch"
                                .to_string(),
                            span: item.span,
                        })?;
                if origin_iface_arity > impl_arity {
                    return Err(TypeError {
                        message: "internal error: origin interface arity exceeds impl arity"
                            .to_string(),
                        span: item.span,
                    });
                }

                let mut type_expr_generics_override = sig.generics.clone();
                #[allow(clippy::needless_range_loop)]
                for idx in 0..impl_arity {
                    if idx < origin_iface_arity {
                        type_expr_generics_override[idx].name =
                            origin_iface_def.generics[idx].name.clone();
                    } else {
                        // Not visible to the interface template; assign a reserved name to avoid
                        // collisions with origin interface generic names.
                        type_expr_generics_override[idx].name = format!("$implgen#{idx}");
                    }
                }

                let fn_info = typecheck_function_body_with_options(
                    env,
                    &origin_module,
                    sig,
                    &synthesized,
                    TypecheckFunctionBodyOptions {
                        type_expr_generics_override: Some(type_expr_generics_override),
                        prefer_interface_methods_for_self: Some(method_info.origin.clone()),
                    },
                )?;
                info.functions.insert(fn_name, fn_info);
            }
        }
    }
    Ok(info)
}

fn typecheck_function_body(
    env: &ProgramEnv,
    module: &ModulePath,
    sig: &FnSig,
    func: &FnItem,
) -> Result<FnTypeInfo, TypeError> {
    typecheck_function_body_with_options(
        env,
        module,
        sig,
        func,
        TypecheckFunctionBodyOptions::default(),
    )
}

#[derive(Clone, Debug, Default)]
struct TypecheckFunctionBodyOptions {
    type_expr_generics_override: Option<Vec<GenericParamInfo>>,
    prefer_interface_methods_for_self: Option<String>,
}

fn typecheck_function_body_with_options(
    env: &ProgramEnv,
    module: &ModulePath,
    sig: &FnSig,
    func: &FnItem,
    options: TypecheckFunctionBodyOptions,
) -> Result<FnTypeInfo, TypeError> {
    let mut tc = FnTypechecker::new(env, module.clone(), sig);
    tc.type_expr_generics_override = options.type_expr_generics_override;
    tc.prefer_interface_methods_for_self = options.prefer_interface_methods_for_self;

    let has_implicit_receiver = matches!(
        func.kind,
        FnItemKind::Method {
            receiver: MethodReceiverKind::Instance { .. }
        }
    );
    if matches!(func.kind, FnItemKind::Method { .. }) {
        tc.reserved_names.insert("self".to_string());
    }
    if has_implicit_receiver && let Some(recv_ty) = sig.params.first() {
        tc.self_ty_subst = Some(strip_readonly(recv_ty).clone());
    }

    // Parameters are immutable bindings.
    if has_implicit_receiver {
        if sig.params.is_empty() || sig.params.len() != func.params.len() + 1 {
            return Err(TypeError {
                message: "internal error: method signature arity mismatch".to_string(),
                span: func.span,
            });
        }
        tc.bind_reserved_local(
            "self",
            LocalInfo {
                ty: sig.params[0].clone(),
                kind: BindingKind::Const,
                span: func.span,
            },
        )?;
        for (param, ty) in func.params.iter().zip(sig.params.iter().skip(1)) {
            let binds = tc.typecheck_pattern(&param.pat, ty.clone())?;
            for (name, bind_ty) in binds {
                tc.bind_local(
                    &name,
                    LocalInfo {
                        ty: bind_ty,
                        kind: BindingKind::Const,
                        span: name.span,
                    },
                )?;
            }
        }
    } else {
        if func.params.len() != sig.params.len() {
            return Err(TypeError {
                message: "internal error: function signature arity mismatch".to_string(),
                span: func.span,
            });
        }
        for (param, ty) in func.params.iter().zip(sig.params.iter()) {
            let binds = tc.typecheck_pattern(&param.pat, ty.clone())?;
            for (name, bind_ty) in binds {
                tc.bind_local(
                    &name,
                    LocalInfo {
                        ty: bind_ty,
                        kind: BindingKind::Const,
                        span: name.span,
                    },
                )?;
            }
        }
    }

    let body_ty = tc.typecheck_block(&func.body, ExprUse::Value)?;
    tc.unify_expected(sig.ret.clone(), body_ty, func.body.span)
        .map_err(|e| TypeError {
            message: format!("in function `{}`: {}", sig.name, e.message),
            span: e.span,
        })?;

    tc.finish()?;
    let expr_types = tc
        .expr_types
        .into_iter()
        .map(|(span, ty)| (span, tc.infer.resolve_ty(ty)))
        .collect();
    let call_type_args = tc
        .call_type_args
        .into_iter()
        .map(|(key, args)| {
            (
                key,
                args.into_iter().map(|ty| tc.infer.resolve_ty(ty)).collect(),
            )
        })
        .collect();
    let method_type_args = tc
        .method_type_args
        .into_iter()
        .map(|(key, args)| {
            (
                key,
                args.into_iter().map(|ty| tc.infer.resolve_ty(ty)).collect(),
            )
        })
        .collect();
    let effect_interface_args = tc
        .effect_interface_args
        .into_iter()
        .map(|(key, args)| {
            (
                key,
                args.into_iter().map(|ty| tc.infer.resolve_ty(ty)).collect(),
            )
        })
        .collect();
    let static_iface_self_tys = tc
        .static_iface_self_tys
        .into_iter()
        .map(|(key, ty)| (key, tc.infer.resolve_ty(ty)))
        .collect();
    Ok(FnTypeInfo {
        expr_types,
        call_type_args,
        method_type_args,
        static_iface_self_tys,
        effect_interface_args,
    })
}

struct FnTypechecker<'a> {
    env: &'a ProgramEnv,
    module: ModulePath,
    sig: &'a FnSig,
    return_ty: Ty,
    self_ty_subst: Option<Ty>,
    /// If set, `self.m(...)` method-call sugar prefers this interface's method set over inherent
    /// methods on `Self` (used for interface default method bodies; see proposal §4.5).
    prefer_interface_methods_for_self: Option<String>,
    /// Optional override for resolving generic names inside `TypeExpr`s.
    ///
    /// This is used when typechecking specialized interface default method bodies: the AST was
    /// written in the origin interface's generic naming context, but the synthesized impl
    /// function's signature uses the impl's generic parameter names.
    type_expr_generics_override: Option<Vec<GenericParamInfo>>,
    infer: InferCtx,
    scopes: Vec<HashMap<String, LocalInfo>>,
    all_bindings: Vec<(String, Span, Ty)>,
    expr_types: HashMap<Span, Ty>,
    call_type_args: HashMap<(Span, String), Vec<Ty>>,
    method_type_args: HashMap<(Span, String), Vec<Ty>>,
    static_iface_self_tys: HashMap<(Span, String), Ty>,
    effect_interface_args: HashMap<(Span, String), Vec<Ty>>,
    stmt_exprs: HashSet<Span>,
    loop_depth: usize,
    reserved_names: BTreeSet<String>,
}

impl<'a> FnTypechecker<'a> {
    fn new(env: &'a ProgramEnv, module: ModulePath, sig: &'a FnSig) -> Self {
        Self {
            env,
            module,
            sig,
            return_ty: sig.ret.clone(),
            self_ty_subst: None,
            prefer_interface_methods_for_self: None,
            type_expr_generics_override: None,
            infer: InferCtx::new(),
            scopes: vec![HashMap::new()],
            all_bindings: Vec::new(),
            expr_types: HashMap::new(),
            call_type_args: HashMap::new(),
            method_type_args: HashMap::new(),
            static_iface_self_tys: HashMap::new(),
            effect_interface_args: HashMap::new(),
            stmt_exprs: HashSet::new(),
            loop_depth: 0,
            reserved_names: BTreeSet::from(["resume".to_string()]),
        }
    }

    fn finish(&mut self) -> Result<(), TypeError> {
        self.infer.default_unbound_int_literals();

        // Ensure constraints are satisfied.
        for (&var, ifaces) in self.infer.type_constraints.clone().iter() {
            let resolved = self.infer.resolve_ty(Ty::Var(var));
            if matches!(resolved, Ty::Var(_)) {
                return Err(TypeError {
                    message: "cannot infer a constrained type".to_string(),
                    span: self.sig.span,
                });
            }
            for bound in ifaces {
                let bound = self.infer.resolve_ty(bound.clone());
                if !self.type_implements_interface_type(&resolved, &bound) {
                    return Err(TypeError {
                        message: format!("type `{resolved}` does not implement `{bound}`"),
                        span: self.sig.span,
                    });
                }
            }
        }

        // Ensure no local types contain unresolved inference variables.
        for (name, span, ty) in &self.all_bindings {
            let ty = self.infer.resolve_ty(ty.clone());
            if contains_infer_vars(&ty) {
                return Err(TypeError {
                    message: format!("cannot infer type of `{name}`"),
                    span: *span,
                });
            }
        }

        // Ensure no expression types contain unresolved inference variables.
        //
        // v0.4 used erased generics, so some unconstrained inference variables could slip through
        // in "value-only" expression positions (e.g. `Option::None;`). With reified generics, the
        // compiler must be able to reify instantiated nominal types at runtime, so these become
        // hard type errors.
        if let Some((span, _ty)) = self
            .expr_types
            .iter()
            .map(|(span, ty)| (*span, self.infer.resolve_ty(ty.clone())))
            .filter(|(span, ty)| !self.stmt_exprs.contains(span) && contains_infer_vars(ty))
            .min_by_key(|(span, _ty)| (span.start, span.end))
        {
            return Err(TypeError {
                message: "cannot infer type of expression; add a type annotation".to_string(),
                span,
            });
        }

        // Ensure we can reify generic type arguments at call sites.
        if let Some(span) = self
            .call_type_args
            .iter()
            .filter(|(_span, args)| {
                args.iter()
                    .any(|ty| contains_infer_vars(&self.infer.resolve_ty(ty.clone())))
            })
            .map(|((span, _name), _args)| *span)
            .min_by_key(|span| (span.start, span.end))
        {
            return Err(TypeError {
                message: "cannot infer type arguments for generic call; add a type annotation"
                    .to_string(),
                span,
            });
        }
        if let Some(span) = self
            .method_type_args
            .iter()
            .filter(|(_span, args)| {
                args.iter()
                    .any(|ty| contains_infer_vars(&self.infer.resolve_ty(ty.clone())))
            })
            .map(|((span, _method_id), _args)| *span)
            .min_by_key(|span| (span.start, span.end))
        {
            return Err(TypeError {
                message: "cannot infer type arguments for generic method; add a type annotation"
                    .to_string(),
                span,
            });
        }
        if let Some(span) = self
            .static_iface_self_tys
            .iter()
            .filter(|(_key, ty)| contains_infer_vars(&self.infer.resolve_ty((*ty).clone())))
            .map(|((span, _method_id), _ty)| *span)
            .min_by_key(|span| (span.start, span.end))
        {
            return Err(TypeError {
                message: "cannot infer implementing type for static interface method call; add a type annotation"
                    .to_string(),
                span,
            });
        }

        Ok(())
    }

    /// Joins two expression types.
    ///
    /// This is like unification, but with one extra coercion rule:
    /// `!` (never) can join with any type `T` and yields `T`.
    fn join_types(&mut self, a: Ty, b: Ty, span: Span) -> Result<Ty, TypeError> {
        let a = self.infer.resolve_ty(a);
        let b = self.infer.resolve_ty(b);
        match (a.clone(), b.clone()) {
            (Ty::Never, other) | (other, Ty::Never) => Ok(other),
            (a, b) => self.infer.unify(a, b, span),
        }
    }

    /// Checks that a value of type `got` can be used where `expected` is required.
    ///
    /// This is directional (unlike plain unification):
    /// - `!` can coerce to any `expected` type.
    /// - Non-`!` types cannot coerce to `!`.
    /// - For `fn` types, the return type is treated covariantly with the same `!` rule so
    ///   `fn(...) -> !` can be used where `fn(...) -> T` is expected.
    fn unify_expected(&mut self, expected: Ty, got: Ty, span: Span) -> Result<Ty, TypeError> {
        let expected = self.infer.resolve_ty(expected);
        let got = self.infer.resolve_ty(got);

        if got == Ty::Never {
            // `!` can be used in any value position and should not constrain inference.
            return Ok(expected);
        }

        if expected == Ty::Never {
            return Err(TypeError {
                message: format!("type mismatch: expected `!`, got `{got}`"),
                span,
            });
        }

        match (expected.clone(), got) {
            (
                Ty::Fn {
                    params: eparams,
                    ret: eret,
                },
                Ty::Fn {
                    params: gparams,
                    ret: gret,
                },
            ) => {
                if eparams.len() != gparams.len() {
                    return Err(TypeError {
                        message: format!(
                            "type mismatch: expected `fn({}) -> {}`, got `fn({}) -> {}`",
                            eparams
                                .iter()
                                .map(|t| t.to_string())
                                .collect::<Vec<_>>()
                                .join(", "),
                            eret,
                            gparams
                                .iter()
                                .map(|t| t.to_string())
                                .collect::<Vec<_>>()
                                .join(", "),
                            gret
                        ),
                        span,
                    });
                }
                let mut params = Vec::with_capacity(eparams.len());
                for (e, g) in eparams.into_iter().zip(gparams.into_iter()) {
                    params.push(self.infer.unify(e, g, span)?);
                }
                let ret = self.unify_expected(*eret, *gret, span)?;
                Ok(Ty::Fn {
                    params,
                    ret: Box::new(ret),
                })
            }
            (expected, got) => self.infer.unify(expected, got, span),
        }
    }

    fn lower_type_expr_in_fn(&self, ty_expr: &TypeExpr) -> Result<Ty, TypeError> {
        let generics = self
            .type_expr_generics_override
            .as_deref()
            .unwrap_or(&self.sig.generics);
        let scope = GenericScope::new(generics)?;
        let ty = lower_type_expr(self.env, &self.module, &scope, ty_expr)?;
        let ty = self.resolve_self_type_in_ty(ty, ty_expr.span())?;
        validate_assoc_projs_in_ty(self.env, &ty, ty_expr.span())?;
        validate_assoc_projs_well_formed_in_ty(self.env, generics, &ty, ty_expr.span())?;
        validate_nominal_bounds_well_formed_in_ty(self.env, generics, &ty, ty_expr.span())?;
        Ok(ty)
    }

    fn resolve_self_type_in_ty(&self, ty: Ty, span: Span) -> Result<Ty, TypeError> {
        if !contains_self_type(&ty) {
            return Ok(ty);
        }
        let Some(self_ty) = &self.self_ty_subst else {
            return Err(TypeError {
                message: "`Self` can only be used in methods".to_string(),
                span,
            });
        };
        Ok(subst_self_type(ty, self_ty))
    }

    fn normalize_assoc_projs_in_ty(&mut self, ty: Ty) -> Ty {
        let ty = self.infer.resolve_ty(ty);
        match ty {
            Ty::Array(elem) => Ty::Array(Box::new(self.normalize_assoc_projs_in_ty(*elem))),
            Ty::Tuple(items) => Ty::Tuple(
                items
                    .into_iter()
                    .map(|t| self.normalize_assoc_projs_in_ty(t))
                    .collect(),
            ),
            Ty::Fn { params, ret } => Ty::Fn {
                params: params
                    .into_iter()
                    .map(|p| self.normalize_assoc_projs_in_ty(p))
                    .collect(),
                ret: Box::new(self.normalize_assoc_projs_in_ty(*ret)),
            },
            Ty::Cont { param, ret } => Ty::Cont {
                param: Box::new(self.normalize_assoc_projs_in_ty(*param)),
                ret: Box::new(self.normalize_assoc_projs_in_ty(*ret)),
            },
            Ty::Readonly(inner) => Ty::Readonly(Box::new(self.normalize_assoc_projs_in_ty(*inner))),
            Ty::App(con, args) => Ty::App(
                con,
                args.into_iter()
                    .map(|a| self.normalize_assoc_projs_in_ty(a))
                    .collect(),
            ),
            Ty::Iface {
                iface,
                args,
                assoc_bindings,
            } => Ty::Iface {
                iface,
                args: args
                    .into_iter()
                    .map(|a| self.normalize_assoc_projs_in_ty(a))
                    .collect(),
                assoc_bindings: assoc_bindings
                    .into_iter()
                    .map(|(name, ty)| (name, self.normalize_assoc_projs_in_ty(ty)))
                    .collect(),
            },
            Ty::AssocProj {
                iface,
                iface_args,
                assoc,
                self_ty,
            } => {
                let iface_args = iface_args
                    .into_iter()
                    .map(|a| self.normalize_assoc_projs_in_ty(a))
                    .collect::<Vec<_>>();
                let self_ty = self.normalize_assoc_projs_in_ty(*self_ty);

                // Normalize `<nominal as Iface>::Assoc` when the impl is known.
                let self_ty_stripped = strip_readonly(&self_ty);
                let dyn_self = match self_ty_stripped {
                    Ty::App(TyCon::Named(type_name), type_args) => {
                        Some((type_name.clone(), type_args.clone()))
                    }
                    Ty::Array(elem) => Some(("array".to_string(), vec![*elem.clone()])),
                    Ty::Unit => Some(("unit".to_string(), Vec::new())),
                    Ty::Bool => Some(("bool".to_string(), Vec::new())),
                    Ty::Int => Some(("int".to_string(), Vec::new())),
                    Ty::Float => Some(("float".to_string(), Vec::new())),
                    Ty::Byte => Some(("byte".to_string(), Vec::new())),
                    Ty::Char => Some(("char".to_string(), Vec::new())),
                    Ty::String => Some(("string".to_string(), Vec::new())),
                    Ty::Bytes => Some(("bytes".to_string(), Vec::new())),
                    _ => None,
                };
                if let Some((type_name, type_args)) = dyn_self
                    && let Some(template) = self
                        .env
                        .interface_assoc_types
                        .get(&(type_name, iface.clone(), assoc.clone()))
                        .cloned()
                {
                    let ty_subst: HashMap<GenId, Ty> =
                        type_args.iter().cloned().enumerate().collect();
                    let con_subst: HashMap<GenId, TyCon> = HashMap::new();
                    let inst = subst_ty(template, &ty_subst, &con_subst);
                    return self.normalize_assoc_projs_in_ty(inst);
                }

                Ty::AssocProj {
                    iface,
                    iface_args,
                    assoc,
                    self_ty: Box::new(self_ty),
                }
            }
            other => other,
        }
    }

    fn subst_self_assoc_projs_from_bindings(
        &mut self,
        ty: Ty,
        assoc_bindings: &BTreeMap<String, Ty>,
    ) -> Ty {
        match self.infer.resolve_ty(ty) {
            Ty::AssocProj {
                iface,
                iface_args,
                assoc,
                self_ty,
            } => {
                if matches!(self_ty.as_ref(), Ty::SelfType)
                    && let Some(bound) = assoc_bindings.get(&assoc).cloned()
                {
                    self.normalize_assoc_projs_in_ty(bound)
                } else {
                    Ty::AssocProj {
                        iface,
                        iface_args: iface_args
                            .into_iter()
                            .map(|a| self.subst_self_assoc_projs_from_bindings(a, assoc_bindings))
                            .collect(),
                        assoc,
                        self_ty: Box::new(
                            self.subst_self_assoc_projs_from_bindings(*self_ty, assoc_bindings),
                        ),
                    }
                }
            }
            Ty::Array(elem) => Ty::Array(Box::new(
                self.subst_self_assoc_projs_from_bindings(*elem, assoc_bindings),
            )),
            Ty::Tuple(items) => Ty::Tuple(
                items
                    .into_iter()
                    .map(|t| self.subst_self_assoc_projs_from_bindings(t, assoc_bindings))
                    .collect(),
            ),
            Ty::Fn { params, ret } => Ty::Fn {
                params: params
                    .into_iter()
                    .map(|p| self.subst_self_assoc_projs_from_bindings(p, assoc_bindings))
                    .collect(),
                ret: Box::new(self.subst_self_assoc_projs_from_bindings(*ret, assoc_bindings)),
            },
            Ty::Cont { param, ret } => Ty::Cont {
                param: Box::new(self.subst_self_assoc_projs_from_bindings(*param, assoc_bindings)),
                ret: Box::new(self.subst_self_assoc_projs_from_bindings(*ret, assoc_bindings)),
            },
            Ty::Readonly(inner) => Ty::Readonly(Box::new(
                self.subst_self_assoc_projs_from_bindings(*inner, assoc_bindings),
            )),
            Ty::App(con, args) => Ty::App(
                con,
                args.into_iter()
                    .map(|a| self.subst_self_assoc_projs_from_bindings(a, assoc_bindings))
                    .collect(),
            ),
            Ty::Iface {
                iface,
                args,
                assoc_bindings: inner,
            } => Ty::Iface {
                iface,
                args: args
                    .into_iter()
                    .map(|a| self.subst_self_assoc_projs_from_bindings(a, assoc_bindings))
                    .collect(),
                assoc_bindings: inner
                    .into_iter()
                    .map(|(name, ty)| {
                        (
                            name,
                            self.subst_self_assoc_projs_from_bindings(ty, assoc_bindings),
                        )
                    })
                    .collect(),
            },
            other => other,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn bind_local(&mut self, ident: &Ident, info: LocalInfo) -> Result<(), TypeError> {
        if self.reserved_names.contains(&ident.name) {
            return Err(TypeError {
                message: format!("`{}` is reserved and cannot be bound here", ident.name),
                span: ident.span,
            });
        }
        let scope = self.scopes.last_mut().expect("at least one scope");
        if scope.contains_key(&ident.name) {
            return Err(TypeError {
                message: format!("duplicate binding `{}`", ident.name),
                span: ident.span,
            });
        }
        scope.insert(ident.name.clone(), info);
        let inserted = scope.get(&ident.name).expect("just inserted");
        self.all_bindings
            .push((ident.name.clone(), inserted.span, inserted.ty.clone()));
        Ok(())
    }

    fn bind_reserved_local(&mut self, name: &str, info: LocalInfo) -> Result<(), TypeError> {
        let scope = self.scopes.last_mut().expect("at least one scope");
        if scope.contains_key(name) {
            return Err(TypeError {
                message: format!("duplicate binding `{name}`"),
                span: info.span,
            });
        }
        scope.insert(name.to_string(), info);
        Ok(())
    }

    fn lookup_local(&self, name: &str) -> Option<&LocalInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v);
            }
        }
        None
    }

    fn typecheck_block(&mut self, block: &Block, use_kind: ExprUse) -> Result<Ty, TypeError> {
        self.push_scope();
        for stmt in &block.stmts {
            self.typecheck_stmt(stmt)?;
        }
        let ty = if let Some(tail) = &block.tail {
            self.typecheck_expr(tail, use_kind)?
        } else {
            Ty::Unit
        };
        self.pop_scope();
        Ok(ty)
    }

    fn typecheck_stmt(&mut self, stmt: &crate::ast::Stmt) -> Result<(), TypeError> {
        match stmt {
            crate::ast::Stmt::Let {
                kind,
                pat,
                ty,
                init,
                span,
            } => {
                if init.is_none() {
                    let message = match kind {
                        BindingKind::Let => "`let` bindings require an initializer",
                        BindingKind::Const => "`const` bindings require an initializer",
                        BindingKind::Readonly => "`readonly` bindings require an initializer",
                    };
                    return Err(TypeError {
                        message: message.to_string(),
                        span: *span,
                    });
                }

                let declared = if let Some(ty_expr) = ty {
                    Some(self.lower_type_expr_in_fn(ty_expr)?)
                } else {
                    None
                };

                let init_expr = init.as_ref().expect("init checked above");
                let init_ty = self.typecheck_expr(init_expr, ExprUse::Value)?;
                let scrutinee_ty = if let Some(decl) = declared {
                    self.unify_expected(decl, init_ty, *span)?
                } else {
                    init_ty
                };

                let binds = self.typecheck_pattern(pat, scrutinee_ty)?;
                for (name, ty) in binds {
                    let ty = if matches!(kind, BindingKind::Readonly) {
                        ty.as_readonly_view()
                    } else {
                        ty
                    };
                    self.bind_local(
                        &name,
                        LocalInfo {
                            ty,
                            kind: *kind,
                            span: name.span,
                        },
                    )?;
                }
            }
            crate::ast::Stmt::Return { value, span } => {
                let got = if let Some(expr) = value {
                    self.typecheck_expr(expr, ExprUse::Value)?
                } else {
                    Ty::Unit
                };
                let _ = self.unify_expected(self.return_ty.clone(), got, *span)?;
            }
            crate::ast::Stmt::Break { span } => {
                if self.loop_depth == 0 {
                    return Err(TypeError {
                        message: "`break` outside of a loop".to_string(),
                        span: *span,
                    });
                }
            }
            crate::ast::Stmt::Continue { span } => {
                if self.loop_depth == 0 {
                    return Err(TypeError {
                        message: "`continue` outside of a loop".to_string(),
                        span: *span,
                    });
                }
            }
            crate::ast::Stmt::Expr { expr, .. } => {
                let _ = self.typecheck_expr(expr, ExprUse::Stmt)?;
            }
        }
        Ok(())
    }

    fn typecheck_expr(&mut self, expr: &Expr, use_kind: ExprUse) -> Result<Ty, TypeError> {
        let ty = match expr {
            Expr::Unit { .. } => Ty::Unit,
            Expr::Bool { .. } => Ty::Bool,
            Expr::Int { value, .. } => self.infer.fresh_int_lit_var(*value),
            Expr::Float { .. } => Ty::Float,
            Expr::Char { .. } => Ty::Char,
            Expr::String { .. } => Ty::String,
            Expr::Bytes { .. } => Ty::Bytes,

            Expr::Path { path, span } => self.typecheck_path_expr(path, *span)?,
            Expr::Array { items, span: _ } => {
                let elem_ty = self.infer.fresh_type_var();
                for item in items {
                    let t = self.typecheck_expr(item, ExprUse::Value)?;
                    let _ = self.unify_expected(elem_ty.clone(), t, item.span())?;
                }
                Ty::Array(Box::new(self.infer.resolve_ty(elem_ty)))
            }
            Expr::Tuple { items, span: _ } => {
                let mut tys = Vec::with_capacity(items.len());
                for item in items {
                    tys.push(self.typecheck_expr(item, ExprUse::Value)?);
                }
                if tys.is_empty() {
                    Ty::Unit
                } else {
                    Ty::Tuple(tys.into_iter().map(|t| self.infer.resolve_ty(t)).collect())
                }
            }
            Expr::StructLit {
                type_path,
                fields,
                span,
            } => self.typecheck_struct_lit(type_path, fields, *span)?,
            Expr::EffectCall {
                interface,
                method,
                args,
                span,
            } => self.typecheck_effect_call(interface, method, args, *span)?,

            Expr::Lambda { params, body, span } => self.typecheck_lambda(params, body, *span)?,
            Expr::If {
                cond,
                then_block,
                else_branch,
                span,
            } => self.typecheck_if(cond, then_block, else_branch.as_deref(), use_kind, *span)?,
            Expr::Match {
                scrutinee,
                arms,
                span,
            } => self.typecheck_match(scrutinee, arms, *span)?,
            Expr::Loop { body, .. } => {
                self.loop_depth += 1;
                let _ = self.typecheck_block(body, ExprUse::Stmt)?;
                self.loop_depth -= 1;
                Ty::Unit
            }
            Expr::While { cond, body, span } => {
                let cond_ty = self.typecheck_expr(cond, ExprUse::Value)?;
                let _ = self.unify_expected(Ty::Bool, cond_ty, *span)?;
                self.loop_depth += 1;
                let _ = self.typecheck_block(body, ExprUse::Stmt)?;
                self.loop_depth -= 1;
                Ty::Unit
            }
            Expr::For {
                pat,
                iter,
                body,
                span,
            } => self.typecheck_for(pat, iter, body, *span)?,
            Expr::Block { block, .. } => self.typecheck_block(block, use_kind)?,

            Expr::Call {
                callee,
                type_args,
                args,
                span,
            } => self.typecheck_call(callee, type_args, args, use_kind, *span)?,
            Expr::Field { base, name, span } => self.typecheck_field(base, name, *span)?,
            Expr::Index { base, index, span } => self.typecheck_index(base, index, *span)?,
            Expr::Unary { op, expr, span } => self.typecheck_unary(*op, expr, *span)?,
            Expr::Binary {
                op,
                left,
                right,
                span,
            } => self.typecheck_binary(*op, left, right, *span)?,
            Expr::Assign {
                target,
                value,
                span,
            } => self.typecheck_assign(target, value, *span)?,
            Expr::As { expr, ty, span } => self.typecheck_as(expr, ty, *span)?,
            Expr::AsQuestion { expr, ty, span } => self.typecheck_as_question(expr, ty, *span)?,
            Expr::Is { expr, ty, span } => self.typecheck_is(expr, ty, *span)?,
        };

        let resolved = self.infer.resolve_ty(ty);
        if use_kind == ExprUse::Stmt {
            self.stmt_exprs.insert(expr.span());
        }
        self.expr_types.insert(expr.span(), resolved.clone());
        Ok(resolved)
    }

    fn typecheck_path_expr(
        &mut self,
        path: &crate::ast::Path,
        span: Span,
    ) -> Result<Ty, TypeError> {
        if path.segments.len() == 1
            && let Some(local) = self.lookup_local(path.segments[0].name.as_str())
        {
            return Ok(local.ty.clone());
        }

        // Prelude-like `Option` constructors: `None` is treated as `Option::None`.
        if path.segments.len() == 1 && path.segments[0].name == "None" {
            return self.typecheck_enum_lit("Option", &path.segments[0], &[], span);
        }

        let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();
        let mut func_name: Option<String> = None;

        // Bare enum variant value: `Enum::Variant` where `Variant` has zero fields.
        //
        // This is sugar for `Enum::Variant()` (and is only allowed for zero-field variants).
        if segments.len() >= 2 {
            let prefix = &segments[..segments.len() - 1];
            let last = segments.last().expect("len >= 2");
            let last_ident = path.segments.last().expect("len >= 2");
            if let Some((kind, type_fqn)) = self
                .env
                .modules
                .try_resolve_type_fqn(&self.module, prefix, span)
                .map_err(|e| TypeError {
                    message: e.message,
                    span: e.span,
                })?
                && kind == crate::modules::DefKind::Enum
                && let Some(def) = self.env.enums.get(&type_fqn)
                && def
                    .variants
                    .get(last)
                    .is_some_and(|variant_fields| variant_fields.is_empty())
            {
                return self.typecheck_enum_lit(&type_fqn, last_ident, &[], span);
            }
        }

        // Allow taking an inherent method as a value: `Type::method`.
        if segments.len() >= 2 {
            let prefix = &segments[..segments.len() - 1];
            let last = segments.last().expect("len >= 2");
            let prefix_ty = self
                .env
                .modules
                .try_resolve_type_fqn(&self.module, prefix, span)
                .map_err(|e| TypeError {
                    message: e.message,
                    span: e.span,
                })?;
            if let Some((kind, type_fqn)) = prefix_ty
                && kind != crate::modules::DefKind::Interface
            {
                let candidate = format!("{type_fqn}::{last}");
                if self.env.functions.contains_key(&candidate) {
                    func_name = Some(candidate);
                }
            }
        }

        // Allow UFCS-style primitive inherent method calls: `int::to_byte(...)`, `bytes::from_array(...)`, etc.
        //
        // Primitives do not exist as nominal types in the module resolver, so we resolve these
        // directly against the built-in function table.
        if func_name.is_none() && segments.len() == 2 {
            let prim = segments[0].as_str();
            let last = segments[1].as_str();
            if matches!(
                prim,
                "unit" | "bool" | "int" | "float" | "byte" | "char" | "string" | "bytes"
            ) {
                let candidate = format!("{prim}::{last}");
                if self.env.functions.contains_key(&candidate) {
                    func_name = Some(candidate);
                }
            }
        }

        let func_name = match func_name {
            Some(name) => name,
            None => self
                .env
                .modules
                .resolve_value_fqn(&self.module, &segments, span)
                .map_err(|e| TypeError {
                    message: e.message,
                    span: e.span,
                })?,
        };

        // Function item used as a value.
        let Some(sig) = self.env.functions.get(&func_name) else {
            return Err(TypeError {
                message: format!("unknown function `{func_name}`"),
                span,
            });
        };
        if !sig.vis.is_public() && !self.module.is_descendant_of(&sig.defining_module) {
            return Err(TypeError {
                message: format!("function `{}` is private", sig.name),
                span,
            });
        }
        if !sig.generics.is_empty() {
            return Err(TypeError {
                message: format!(
                    "cannot use generic function `{}` as a value without an expected type",
                    sig.name
                ),
                span,
            });
        }
        Ok(Ty::Fn {
            params: sig.params.clone(),
            ret: Box::new(sig.ret.clone()),
        })
    }

    fn typecheck_struct_lit(
        &mut self,
        type_path: &crate::ast::Path,
        fields: &[(Ident, Expr)],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let segments: Vec<String> = type_path.segments.iter().map(|s| s.name.clone()).collect();
        let (kind, struct_name) = self
            .env
            .modules
            .resolve_type_fqn(&self.module, &segments, type_path.span)
            .map_err(|e| TypeError {
                message: e.message,
                span: e.span,
            })?;
        if kind != crate::modules::DefKind::Struct {
            return Err(TypeError {
                message: format!("expected a struct type, got `{struct_name}`"),
                span: type_path.span,
            });
        }
        let Some(def) = self.env.structs.get(&struct_name) else {
            return Err(TypeError {
                message: format!("unknown struct `{struct_name}`"),
                span: type_path.span,
            });
        };
        if def.is_newtype {
            return Err(TypeError {
                message: format!(
                    "new-type struct `{}` must be constructed with call syntax: `{}`(value)",
                    def.name, def.name
                ),
                span: type_path.span,
            });
        }
        if def.generics.iter().any(|g| g.arity != 0) {
            return Err(TypeError {
                message: "higher-kinded generics on structs are not supported in v0.4".to_string(),
                span,
            });
        }

        let mut type_args: Vec<Ty> = Vec::with_capacity(def.generics.len());
        let mut ty_subst: HashMap<GenId, Ty> = HashMap::new();
        let con_subst: HashMap<GenId, TyCon> = HashMap::new();

        for (idx, gp) in def.generics.iter().enumerate() {
            let ty = self.infer.fresh_type_var();
            let ty_for_args = ty.clone();

            if let Ty::Var(var_id) = ty {
                for bound in &gp.bounds {
                    let bound = subst_ty(bound.clone(), &ty_subst, &con_subst);
                    self.infer.add_constraint(var_id, bound);
                }
            }

            type_args.push(ty_for_args.clone());
            ty_subst.insert(idx, ty_for_args);
        }

        let instantiated_fields = def
            .fields
            .iter()
            .map(|(n, t)| (n.clone(), subst_ty(t.clone(), &ty_subst, &con_subst)))
            .collect::<BTreeMap<_, _>>();

        let mut seen = BTreeSet::new();
        for (field_name, value_expr) in fields {
            if !seen.insert(field_name.name.clone()) {
                return Err(TypeError {
                    message: format!("duplicate field initializer `{}`", field_name.name),
                    span: field_name.span,
                });
            }
            let Some(expected) = instantiated_fields.get(&field_name.name) else {
                return Err(TypeError {
                    message: format!(
                        "unknown field `{}` on struct `{}`",
                        field_name.name, def.name
                    ),
                    span: field_name.span,
                });
            };
            let got = self.typecheck_expr(value_expr, ExprUse::Value)?;
            let _ = self.unify_expected(expected.clone(), got, value_expr.span())?;
        }

        // Require all fields to be provided (Rust-like literal).
        if fields.len() != instantiated_fields.len() {
            return Err(TypeError {
                message: format!(
                    "struct literal for `{}` must initialize all fields",
                    def.name
                ),
                span,
            });
        }

        Ok(Ty::App(TyCon::Named(def.name.clone()), type_args))
    }

    fn typecheck_enum_lit(
        &mut self,
        enum_name: &str,
        variant: &Ident,
        fields: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let Some(def) = self.env.enums.get(enum_name) else {
            return Err(TypeError {
                message: format!("unknown enum `{enum_name}`"),
                span,
            });
        };
        if def.generics.iter().any(|g| g.arity != 0) {
            return Err(TypeError {
                message: "higher-kinded generics on enums are not supported in v0.4".to_string(),
                span,
            });
        }

        let Some(variant_fields) = def.variants.get(&variant.name) else {
            return Err(TypeError {
                message: format!("unknown variant `{}` on enum `{}`", variant.name, def.name),
                span: variant.span,
            });
        };

        if fields.len() != variant_fields.len() {
            return Err(TypeError {
                message: format!(
                    "wrong number of fields for `{}::{}`: expected {}, got {}",
                    def.name,
                    variant.name,
                    variant_fields.len(),
                    fields.len()
                ),
                span,
            });
        }

        let mut type_args: Vec<Ty> = Vec::with_capacity(def.generics.len());
        let mut ty_subst: HashMap<GenId, Ty> = HashMap::new();
        let con_subst: HashMap<GenId, TyCon> = HashMap::new();

        for (idx, gp) in def.generics.iter().enumerate() {
            let ty = self.infer.fresh_type_var();
            let ty_for_args = ty.clone();

            if let Ty::Var(var_id) = ty {
                for bound in &gp.bounds {
                    let bound = subst_ty(bound.clone(), &ty_subst, &con_subst);
                    self.infer.add_constraint(var_id, bound);
                }
            }

            type_args.push(ty_for_args.clone());
            ty_subst.insert(idx, ty_for_args);
        }

        for (expr, expected) in fields.iter().zip(variant_fields.iter()) {
            let expected = subst_ty(expected.clone(), &ty_subst, &con_subst);
            let got = self.typecheck_expr(expr, ExprUse::Value)?;
            let _ = self.unify_expected(expected, got, expr.span())?;
        }

        Ok(Ty::App(TyCon::Named(def.name.clone()), type_args))
    }

    fn typecheck_effect_call(
        &mut self,
        interface: &crate::ast::PathType,
        method: &Ident,
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        if interface.segments.is_empty() {
            return Err(TypeError {
                message: "empty interface path in effect call".to_string(),
                span: interface.span,
            });
        }
        if !interface.assoc_bindings.is_empty() {
            return Err(TypeError {
                message: "associated type bindings are not supported in effect calls yet"
                    .to_string(),
                span: interface.span,
            });
        }

        let segments: Vec<String> = interface
            .segments
            .iter()
            .map(|s| s.name.name.clone())
            .collect();
        let (kind, iface_name) = self
            .env
            .modules
            .resolve_type_fqn(&self.module, &segments, interface.span)
            .map_err(|e| TypeError {
                message: e.message,
                span: e.span,
            })?;
        if kind != crate::modules::DefKind::Interface {
            return Err(TypeError {
                message: format!("expected an interface, got `{iface_name}`"),
                span: interface.span,
            });
        }
        let Some(iface_def) = self.env.interfaces.get(&iface_name) else {
            return Err(TypeError {
                message: format!("unknown interface `{iface_name}`"),
                span: interface.span,
            });
        };

        let iface_arity = iface_def.generics.len();
        let iface_args_count: usize = interface.segments.iter().map(|seg| seg.args.len()).sum();
        let explicit_iface_args = iface_args_count != 0;

        let mut iface_args: Vec<Ty> = if explicit_iface_args {
            if iface_args_count != iface_arity {
                return Err(TypeError {
                    message: format!(
                        "interface `{iface_name}` expects {iface_arity} type argument(s), got {}",
                        iface_args_count
                    ),
                    span: interface.span,
                });
            }
            let mut iface_args = Vec::with_capacity(iface_args_count);
            for seg in &interface.segments {
                for a in &seg.args {
                    iface_args.push(self.lower_type_expr_in_fn(a)?);
                }
            }
            iface_args
        } else {
            // Inference case: `@I.foo(...)` where `I` is generic.
            (0..iface_arity)
                .map(|_| self.infer.fresh_type_var())
                .collect()
        };

        let Some(method_info) = iface_def.all_methods.get(&method.name) else {
            return Err(TypeError {
                message: format!("unknown effect/method `{}.{}`", iface_name, method.name),
                span: method.span,
            });
        };
        if matches!(method_info.receiver, MethodReceiverKind::Static) {
            return Err(TypeError {
                message: format!(
                    "static interface method `{}::{}` is not an effect operation",
                    method_info.origin, method.name
                ),
                span: method.span,
            });
        }
        if !method_info.sig.generics.is_empty() {
            return Err(TypeError {
                message: "method-generic effect operations are not supported yet".to_string(),
                span: method.span,
            });
        }
        if interface_method_sig_mentions_self_or_assoc(&method_info.sig) {
            let method_id = format!("{}::{}", method_info.origin, method.name);
            return Err(TypeError {
                message: format!(
                    "effect calls must not target methods that mention `Self` or associated types in their signature; `{method_id}` is not allowed as an effect operation"
                ),
                span: method.span,
            });
        }

        let sig = instantiate_interface_method_sig(
            &method_info.sig,
            &iface_args,
            iface_arity,
            None,
            &mut self.infer,
        );

        if args.len() != sig.params.len() {
            return Err(TypeError {
                message: format!(
                    "arity mismatch for {}.{}: expected {}, got {}",
                    iface_name,
                    method.name,
                    sig.params.len(),
                    args.len()
                ),
                span,
            });
        }
        for (arg, expected) in args.iter().zip(sig.params.iter()) {
            let got = self.typecheck_expr(arg, ExprUse::Value)?;
            let _ = self.unify_expected(expected.clone(), got, arg.span())?;
        }

        // If interface args were omitted, require inference to fully determine them.
        if !explicit_iface_args {
            for arg in &mut iface_args {
                *arg = self.infer.resolve_ty(arg.clone());
                if self.infer.contains_infer_vars_except_int_lits(arg) {
                    return Err(TypeError {
                        message: format!(
                            "cannot infer interface type arguments for effect call `@{iface_name}.{}(...)`; use `@{iface_name}<...>.{}(...)`",
                            method.name, method.name
                        ),
                        span: interface.span,
                    });
                }
            }
        }

        let origin_args = infer_super_interface_args(self.env, &iface_name, &iface_args, &method_info.origin)
            .ok_or(TypeError {
                message: format!(
                    "internal error: cannot infer `{}` type arguments for effect call `@{iface_name}.{}(...)`",
                    method_info.origin, method.name
                ),
                span: interface.span,
            })?;
        let effect_id = format!("{}::{}", method_info.origin, method.name);
        self.effect_interface_args
            .insert((span, effect_id), origin_args);

        Ok(sig.ret)
    }

    fn typecheck_lambda(
        &mut self,
        params: &[crate::ast::LambdaParam],
        body: &Block,
        _span: Span,
    ) -> Result<Ty, TypeError> {
        let mut param_tys = Vec::with_capacity(params.len());

        // A lambda introduces a new function boundary:
        // - `return` returns from the lambda itself
        // - `break`/`continue` do not target outer loops
        let saved_loop_depth = self.loop_depth;
        let saved_return_ty = self.return_ty.clone();
        let lambda_ret_ty = self.infer.fresh_type_var();
        self.loop_depth = 0;
        self.return_ty = lambda_ret_ty.clone();

        self.push_scope();
        for p in params {
            let ty = if let Some(ty_expr) = &p.ty {
                self.lower_type_expr_in_fn(ty_expr)?
            } else {
                self.infer.fresh_type_var()
            };
            param_tys.push(ty.clone());
            self.bind_local(
                &p.name,
                LocalInfo {
                    ty,
                    kind: BindingKind::Const,
                    span: p.span,
                },
            )?;
        }
        let body_ty = self.typecheck_block(body, ExprUse::Value)?;
        self.pop_scope();

        self.infer
            .unify(lambda_ret_ty.clone(), body_ty, body.span)?;

        self.loop_depth = saved_loop_depth;
        self.return_ty = saved_return_ty;

        Ok(Ty::Fn {
            params: param_tys,
            ret: Box::new(self.infer.resolve_ty(lambda_ret_ty)),
        })
    }

    fn typecheck_if(
        &mut self,
        cond: &Expr,
        then_block: &Block,
        else_branch: Option<&Expr>,
        use_kind: ExprUse,
        span: Span,
    ) -> Result<Ty, TypeError> {
        let cond_ty = self.typecheck_expr(cond, ExprUse::Value)?;
        let _ = self.unify_expected(Ty::Bool, cond_ty, cond.span())?;
        let then_ty = self.typecheck_block(then_block, ExprUse::Value)?;
        let Some(else_expr) = else_branch else {
            if use_kind == ExprUse::Value {
                return Err(TypeError {
                    message: "missing `else` branch in value position".to_string(),
                    span,
                });
            }
            return Ok(Ty::Unit);
        };
        let else_ty = self.typecheck_expr(else_expr, ExprUse::Value)?;
        self.join_types(then_ty, else_ty, span)
    }

    fn typecheck_match(
        &mut self,
        scrutinee: &Expr,
        arms: &[MatchArm],
        span: Span,
    ) -> Result<Ty, TypeError> {
        if arms.is_empty() {
            return Err(TypeError {
                message: "`match` must have at least one arm".to_string(),
                span,
            });
        }

        let scrutinee_ty = self.typecheck_expr(scrutinee, ExprUse::Value)?;
        let result_ty = self.infer.fresh_type_var();

        let mut saw_value_arm = false;

        for arm in arms {
            match &arm.pat {
                MatchPat::Value(pat) => {
                    saw_value_arm = true;
                    let binds = self.typecheck_pattern(pat, scrutinee_ty.clone())?;
                    self.push_scope();
                    for (name, ty) in binds {
                        self.bind_local(
                            &name,
                            LocalInfo {
                                ty,
                                kind: BindingKind::Const,
                                span: name.span,
                            },
                        )?;
                    }
                    let body_ty = self.typecheck_expr(&arm.body, ExprUse::Value)?;
                    let _ = self.unify_expected(result_ty.clone(), body_ty, arm.span)?;
                    self.pop_scope();
                }
                MatchPat::Effect(effect_pat) => {
                    if effect_pat.interface.segments.is_empty() {
                        return Err(TypeError {
                            message: "empty interface path in effect pattern".to_string(),
                            span: effect_pat.interface.span,
                        });
                    }
                    if !effect_pat.interface.assoc_bindings.is_empty() {
                        return Err(TypeError {
                            message:
                                "associated type bindings are not supported in effect patterns yet"
                                    .to_string(),
                            span: effect_pat.interface.span,
                        });
                    }

                    let iface_segments: Vec<String> = effect_pat
                        .interface
                        .segments
                        .iter()
                        .map(|s| s.name.name.clone())
                        .collect();
                    let (kind, iface_name) = self
                        .env
                        .modules
                        .resolve_type_fqn(&self.module, &iface_segments, effect_pat.interface.span)
                        .map_err(|e| TypeError {
                            message: e.message,
                            span: e.span,
                        })?;
                    if kind != crate::modules::DefKind::Interface {
                        return Err(TypeError {
                            message: format!("expected an interface, got `{iface_name}`"),
                            span: effect_pat.interface.span,
                        });
                    }
                    let Some(iface_def) = self.env.interfaces.get(&iface_name) else {
                        return Err(TypeError {
                            message: format!("unknown interface `{iface_name}`"),
                            span: effect_pat.interface.span,
                        });
                    };

                    let iface_arity = iface_def.generics.len();
                    let iface_args_count: usize = effect_pat
                        .interface
                        .segments
                        .iter()
                        .map(|seg| seg.args.len())
                        .sum();
                    let explicit_iface_args = iface_args_count != 0;
                    let scope = GenericScope::new(&self.sig.generics)?;
                    let mut iface_args: Vec<Ty> = if explicit_iface_args {
                        if iface_args_count != iface_arity {
                            return Err(TypeError {
                                message: format!(
                                    "interface `{iface_name}` expects {iface_arity} type argument(s), got {}",
                                    iface_args_count
                                ),
                                span: effect_pat.interface.span,
                            });
                        }
                        let mut iface_args = Vec::with_capacity(iface_args_count);
                        for seg in &effect_pat.interface.segments {
                            for a in &seg.args {
                                iface_args.push(lower_type_expr(
                                    self.env,
                                    &self.module,
                                    &scope,
                                    a,
                                )?);
                            }
                        }
                        iface_args
                    } else {
                        (0..iface_arity)
                            .map(|_| self.infer.fresh_type_var())
                            .collect()
                    };

                    let Some(method_info) = iface_def.all_methods.get(&effect_pat.method.name)
                    else {
                        return Err(TypeError {
                            message: format!(
                                "unknown effect/method `{}.{}`",
                                iface_name, effect_pat.method.name
                            ),
                            span: effect_pat.method.span,
                        });
                    };
                    if !method_info.sig.generics.is_empty() {
                        return Err(TypeError {
                            message: "method-generic effect operations are not supported yet"
                                .to_string(),
                            span: effect_pat.method.span,
                        });
                    }
                    if interface_method_sig_mentions_self_or_assoc(&method_info.sig) {
                        let method_id =
                            format!("{}::{}", method_info.origin, effect_pat.method.name);
                        return Err(TypeError {
                            message: format!(
                                "effect patterns must not target methods that mention `Self` or associated types in their signature; `{method_id}` is not allowed as an effect operation"
                            ),
                            span: effect_pat.method.span,
                        });
                    }

                    let msig = instantiate_interface_method_sig(
                        &method_info.sig,
                        &iface_args,
                        iface_arity,
                        None,
                        &mut self.infer,
                    );

                    if effect_pat.args.len() != msig.params.len() {
                        return Err(TypeError {
                            message: format!(
                                "arity mismatch for handler pattern @{}.{}: expected {}, got {}",
                                iface_name,
                                effect_pat.method.name,
                                msig.params.len(),
                                effect_pat.args.len()
                            ),
                            span: effect_pat.span,
                        });
                    }

                    let mut binds = Vec::new();
                    for (pat, expected) in effect_pat.args.iter().zip(msig.params.iter()) {
                        binds.extend(self.typecheck_pattern(pat, expected.clone())?);
                    }

                    self.push_scope();
                    for (name, ty) in binds {
                        self.bind_local(
                            &name,
                            LocalInfo {
                                ty,
                                kind: BindingKind::Const,
                                span: name.span,
                            },
                        )?;
                    }
                    let cont_name = effect_pat
                        .cont
                        .as_ref()
                        .map(|c| c.name.as_str())
                        .unwrap_or("resume");
                    let cont_span = effect_pat
                        .cont
                        .as_ref()
                        .map(|c| c.span)
                        .unwrap_or(effect_pat.span);
                    // `cont: cont(effect_ret) -> match_result` (defaults to `resume`).
                    let cont_ty = Ty::Cont {
                        param: Box::new(msig.ret.clone()),
                        ret: Box::new(result_ty.clone()),
                    };
                    let cont_info = LocalInfo {
                        ty: cont_ty,
                        kind: BindingKind::Const,
                        span: cont_span,
                    };
                    if cont_name == "resume" {
                        self.bind_reserved_local("resume", cont_info)?;
                    } else {
                        self.bind_local(
                            &crate::ast::Ident {
                                name: cont_name.to_string(),
                                span: cont_span,
                            },
                            cont_info,
                        )?;
                    }

                    let body_ty = self.typecheck_expr(&arm.body, ExprUse::Value)?;
                    let _ = self.unify_expected(result_ty.clone(), body_ty, arm.span)?;
                    self.pop_scope();

                    // If interface args were omitted, allow the handler body to constrain them
                    // via the bound pattern variables (type inference works across the whole arm).
                    for arg in &mut iface_args {
                        *arg = self.infer.resolve_ty(arg.clone());
                    }
                    if !explicit_iface_args {
                        for arg in &iface_args {
                            if contains_infer_vars(arg) {
                                return Err(TypeError {
                                    message: format!(
                                        "cannot infer interface type arguments for handler pattern `@{iface_name}.{}(...)`; use `@{iface_name}<...>.{}(...)`",
                                        effect_pat.method.name, effect_pat.method.name
                                    ),
                                    span: effect_pat.interface.span,
                                });
                            }
                        }
                    }

                    let origin_args = infer_super_interface_args(
                        self.env,
                        &iface_name,
                        &iface_args,
                        &method_info.origin,
                    )
                    .ok_or(TypeError {
                        message: format!(
                            "internal error: cannot infer `{}` type arguments for handler pattern `@{iface_name}.{}(...)`",
                            method_info.origin, effect_pat.method.name
                        ),
                        span: effect_pat.interface.span,
                    })?;
                    let effect_id = format!("{}::{}", method_info.origin, effect_pat.method.name);
                    self.effect_interface_args
                        .insert((effect_pat.span, effect_id), origin_args);
                }
            }
        }

        if !saw_value_arm {
            return Err(TypeError {
                message: "`match` must have at least one value arm".to_string(),
                span,
            });
        }

        self.check_match_value_exhaustive(scrutinee_ty, arms, span)?;

        Ok(self.infer.resolve_ty(result_ty))
    }

    fn check_match_value_exhaustive(
        &mut self,
        scrutinee_ty: Ty,
        arms: &[MatchArm],
        span: Span,
    ) -> Result<(), TypeError> {
        let value_pats: Vec<&Pattern> = arms
            .iter()
            .filter_map(|arm| match &arm.pat {
                MatchPat::Value(pat) => Some(pat),
                MatchPat::Effect(_) => None,
            })
            .collect();

        let scrutinee_ty = self.infer.resolve_ty(scrutinee_ty);

        // 5.2: Accept immediately if any value arm is irrefutable for the scrutinee type.
        if value_pats
            .iter()
            .any(|pat| self.pattern_is_irrefutable(pat, scrutinee_ty.clone()))
        {
            return Ok(());
        }

        let scrutinee_ty = self.infer.resolve_ty(scrutinee_ty);
        let (is_ro, inner_scrutinee) = match scrutinee_ty {
            Ty::Readonly(inner) => (true, *inner),
            other => (false, other),
        };

        match inner_scrutinee {
            // `match` on a diverging expression is unreachable, so treat it as vacuously
            // exhaustive (the value arms will never run).
            Ty::Never => Ok(()),
            // 5.3: Finite coverage check (unit / bool / enum).
            Ty::Unit => {
                let has_unit = value_pats.iter().any(|pat| {
                    matches!(
                        pat,
                        Pattern::Literal {
                            lit: PatLiteral::Unit,
                            ..
                        }
                    )
                });
                if has_unit {
                    Ok(())
                } else {
                    Err(TypeError {
                        message: "non-exhaustive match: missing `()`".to_string(),
                        span,
                    })
                }
            }
            Ty::Bool => {
                let mut saw_true = false;
                let mut saw_false = false;
                for pat in &value_pats {
                    match pat {
                        Pattern::Literal {
                            lit: PatLiteral::Bool(true),
                            ..
                        } => saw_true = true,
                        Pattern::Literal {
                            lit: PatLiteral::Bool(false),
                            ..
                        } => saw_false = true,
                        _ => {}
                    }
                }
                if saw_true && saw_false {
                    Ok(())
                } else {
                    let message = match (saw_true, saw_false) {
                        (true, false) => "non-exhaustive match: missing `false`".to_string(),
                        (false, true) => "non-exhaustive match: missing `true`".to_string(),
                        (false, false) => {
                            "non-exhaustive match: missing `true` and `false`".to_string()
                        }
                        (true, true) => unreachable!("saw_true && saw_false handled above"),
                    };
                    Err(TypeError { message, span })
                }
            }
            Ty::App(TyCon::Named(enum_name), enum_args)
                if self.env.enums.contains_key(&enum_name) =>
            {
                let Some(def) = self.env.enums.get(&enum_name) else {
                    // Should not happen: the match arm patterns typechecked against this enum.
                    return Err(TypeError {
                        message: format!("internal error: unknown enum `{enum_name}`"),
                        span,
                    });
                };

                let enum_args: Vec<Ty> = enum_args
                    .into_iter()
                    .map(|ty| self.infer.resolve_ty(ty))
                    .collect();
                let ty_subst: HashMap<GenId, Ty> = enum_args.iter().cloned().enumerate().collect();
                let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                let mut covered = BTreeSet::<String>::new();
                for pat in value_pats {
                    match pat {
                        Pattern::Enum { variant, .. } => {
                            if def
                                .variants
                                .get(&variant.name)
                                .is_some_and(|fields| fields.is_empty())
                            {
                                covered.insert(variant.name.clone());
                            }
                        }
                        Pattern::Ctor { path, args, .. } => {
                            let Some(variant) = path.segments.last() else {
                                continue;
                            };
                            let Some(fields) = def.variants.get(&variant.name) else {
                                continue;
                            };
                            if fields.len() != args.len() {
                                continue;
                            }

                            let mut all_irrefutable = true;
                            for (subpat, fty) in args.iter().zip(fields.iter()) {
                                let mut fty = subst_ty(fty.clone(), &ty_subst, &con_subst);
                                if is_ro {
                                    fty = fty.as_readonly_view();
                                }
                                if !self.pattern_is_irrefutable(subpat, fty) {
                                    all_irrefutable = false;
                                    break;
                                }
                            }
                            if all_irrefutable {
                                covered.insert(variant.name.clone());
                            }
                        }
                        _ => {}
                    }
                }

                let mut missing = Vec::new();
                for variant in def.variants.keys() {
                    if !covered.contains(variant) {
                        missing.push(format!("{enum_name}::{variant}"));
                    }
                }
                if missing.is_empty() {
                    Ok(())
                } else {
                    Err(TypeError {
                        message: format!(
                            "non-exhaustive match: missing variants: {}",
                            missing.join(", ")
                        ),
                        span,
                    })
                }
            }
            other => Err(TypeError {
                message: format!(
                    "non-exhaustive match: cannot prove exhaustiveness for type `{other}`; add `_ => ...`"
                ),
                span,
            }),
        }
    }

    fn pattern_is_irrefutable(&mut self, pat: &Pattern, expected: Ty) -> bool {
        let expected = self.infer.resolve_ty(expected);
        let (is_ro, expected) = match expected {
            Ty::Readonly(inner) => (true, *inner),
            other => (false, other),
        };

        match pat {
            Pattern::Wildcard { .. } | Pattern::Bind { .. } => true,

            Pattern::Literal {
                lit: PatLiteral::Unit,
                ..
            } => matches!(expected, Ty::Unit),

            Pattern::Literal { .. } => false,

            Pattern::Tuple {
                prefix,
                rest,
                suffix,
                ..
            } => {
                let Ty::Tuple(items) = expected else {
                    return false;
                };
                let prefix_len = prefix.len();
                let suffix_len = suffix.len();
                if rest.is_some() {
                    if items.len() < prefix_len + suffix_len {
                        return false;
                    }
                } else if items.len() != prefix_len + suffix_len {
                    return false;
                }

                for (idx, subpat) in prefix.iter().enumerate() {
                    let mut elem_ty = items[idx].clone();
                    if is_ro {
                        elem_ty = elem_ty.as_readonly_view();
                    }
                    if !self.pattern_is_irrefutable(subpat, elem_ty) {
                        return false;
                    }
                }

                for (idx, subpat) in suffix.iter().enumerate() {
                    let item_idx = items.len() - suffix_len + idx;
                    let mut elem_ty = items[item_idx].clone();
                    if is_ro {
                        elem_ty = elem_ty.as_readonly_view();
                    }
                    if !self.pattern_is_irrefutable(subpat, elem_ty) {
                        return false;
                    }
                }

                true
            }

            Pattern::Struct { fields, .. } => {
                let Ty::App(TyCon::Named(type_name), type_args) = expected else {
                    return false;
                };
                let Some(def) = self.env.structs.get(&type_name) else {
                    return false;
                };

                let type_args: Vec<Ty> = type_args
                    .into_iter()
                    .map(|ty| self.infer.resolve_ty(ty))
                    .collect();
                let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
                let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                for (fname, subpat) in fields {
                    let Some((_n, field_ty)) = def.fields.iter().find(|(n, _)| n == &fname.name)
                    else {
                        return false;
                    };
                    let mut field_ty = subst_ty(field_ty.clone(), &ty_subst, &con_subst);
                    if is_ro {
                        field_ty = field_ty.as_readonly_view();
                    }
                    if !self.pattern_is_irrefutable(subpat, field_ty) {
                        return false;
                    }
                }

                true
            }

            Pattern::Ctor { args, .. } => {
                let Ty::App(TyCon::Named(type_name), type_args) = expected else {
                    return false;
                };
                let Some(def) = self.env.structs.get(&type_name) else {
                    return false;
                };
                if !def.is_newtype || args.len() != 1 {
                    return false;
                }

                let type_args: Vec<Ty> = type_args
                    .into_iter()
                    .map(|ty| self.infer.resolve_ty(ty))
                    .collect();
                let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
                let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                let Some((_fname, field_ty)) = def.fields.first() else {
                    return false;
                };
                let mut field_ty = subst_ty(field_ty.clone(), &ty_subst, &con_subst);
                if is_ro {
                    field_ty = field_ty.as_readonly_view();
                }

                self.pattern_is_irrefutable(&args[0], field_ty)
            }

            Pattern::Array {
                prefix,
                rest,
                suffix,
                ..
            } => {
                let Ty::Array(_elem) = expected else {
                    return false;
                };
                rest.is_some() && prefix.is_empty() && suffix.is_empty()
            }

            Pattern::Enum { .. } => false,
        }
    }

    fn typecheck_for(
        &mut self,
        pat: &Pattern,
        iter: &Expr,
        body: &Block,
        span: Span,
    ) -> Result<Ty, TypeError> {
        let iter_ty = self.typecheck_expr(iter, ExprUse::Value)?;
        let iter_ty = self.infer.resolve_ty(iter_ty);

        // Case 1: `iter` already implements `core::iter::Iterator`.
        let iter_iface = "core::iter::Iterator";
        let elem_ty = if let Some(iface_args) =
            self.infer_interface_args_for_receiver(&iter_ty, iter_iface)
        {
            if matches!(iter_ty, Ty::Readonly(_)) {
                return Err(TypeError {
                    message:
                        "cannot iterate over a readonly iterator (`Iterator::next` is mutable)"
                            .to_string(),
                    span,
                });
            }

            let Some(iface_def) = self.env.interfaces.get(iter_iface) else {
                return Err(TypeError {
                    message: "internal error: missing built-in interface `core::iter::Iterator`"
                        .to_string(),
                    span,
                });
            };
            let Some(method_info) = iface_def.all_methods.get("next") else {
                return Err(TypeError {
                    message: "internal error: missing method `core::iter::Iterator::next`"
                        .to_string(),
                    span,
                });
            };

            // Infer `Item` by instantiating `Iterator::next` and extracting `Option<Item>`.
            let recv_for_dispatch = strip_readonly(&iter_ty).clone();
            let self_ty_for_inst: Option<&Ty> = match &recv_for_dispatch {
                Ty::Iface { .. } => None,
                Ty::App(TyCon::Named(name), _) if self.env.interfaces.contains_key(name) => None,
                other => Some(other),
            };
            let mut inst = instantiate_interface_method_sig(
                &method_info.sig,
                &iface_args,
                iface_def.generics.len(),
                self_ty_for_inst,
                &mut self.infer,
            );
            inst.ret = self.normalize_assoc_projs_in_ty(inst.ret);
            if let Ty::Iface { assoc_bindings, .. } = &recv_for_dispatch {
                inst.ret = self.subst_self_assoc_projs_from_bindings(inst.ret, assoc_bindings);
                inst.ret = self.normalize_assoc_projs_in_ty(inst.ret);
            }

            match inst.ret {
                Ty::App(TyCon::Named(name), mut args) if name == "Option" && args.len() == 1 => {
                    args.pop().expect("len == 1")
                }
                other => {
                    return Err(TypeError {
                        message: format!(
                            "internal error: `core::iter::Iterator::next` must return `Option<T>`, got `{other}`"
                        ),
                        span,
                    });
                }
            }
        } else {
            // Case 2: built-in iterables.
            let (is_readonly, inner) = match iter_ty {
                Ty::Readonly(inner) => (true, *inner),
                other => (false, other),
            };

            match inner {
                Ty::Array(elem) => {
                    if is_readonly {
                        elem.as_ref().as_readonly_view()
                    } else {
                        *elem
                    }
                }
                Ty::String => Ty::Char,
                Ty::Bytes => Ty::Byte,
                other => {
                    return Err(TypeError {
                        message: format!(
                            "`for` expects an iterable (`Iterator`, `[T]`, `string`, or `bytes`), got `{other}`"
                        ),
                        span,
                    });
                }
            }
        };

        self.push_scope();
        let binds = self.typecheck_pattern(pat, elem_ty)?;
        for (name, ty) in binds {
            self.bind_local(
                &name,
                LocalInfo {
                    ty,
                    kind: BindingKind::Const,
                    span: name.span,
                },
            )?;
        }
        self.loop_depth += 1;
        let _ = self.typecheck_block(body, ExprUse::Stmt)?;
        self.loop_depth -= 1;
        self.pop_scope();

        let _ = span;
        Ok(Ty::Unit)
    }

    fn typecheck_pattern(
        &mut self,
        pat: &Pattern,
        expected: Ty,
    ) -> Result<Vec<(Ident, Ty)>, TypeError> {
        // `!` is uninhabited, so value-pattern matching is unreachable when the scrutinee type is
        // `!`. Still accept the pattern (for ergonomics and to allow `match panic(...) { ... }`),
        // and conservatively type all binds as `!`.
        if self.infer.resolve_ty(expected.clone()) == Ty::Never {
            fn collect_binds(pat: &Pattern, out: &mut Vec<Ident>) {
                match pat {
                    Pattern::Wildcard { .. } | Pattern::Literal { .. } => {}
                    Pattern::Bind { name, .. } => out.push(name.clone()),
                    Pattern::Tuple {
                        prefix,
                        rest,
                        suffix,
                        ..
                    } => {
                        for p in prefix {
                            collect_binds(p, out);
                        }
                        if let Some(rest) = rest
                            && let Some(binding) = &rest.binding
                        {
                            out.push(binding.clone());
                        }
                        for p in suffix {
                            collect_binds(p, out);
                        }
                    }
                    Pattern::Enum { fields, .. } => {
                        for p in fields {
                            collect_binds(p, out);
                        }
                    }
                    Pattern::Ctor { args, .. } => {
                        for p in args {
                            collect_binds(p, out);
                        }
                    }
                    Pattern::Struct { fields, .. } => {
                        for (_name, p) in fields {
                            collect_binds(p, out);
                        }
                    }
                    Pattern::Array {
                        prefix,
                        rest,
                        suffix,
                        ..
                    } => {
                        for p in prefix {
                            collect_binds(p, out);
                        }
                        if let Some(rest) = rest
                            && let Some(binding) = &rest.binding
                        {
                            out.push(binding.clone());
                        }
                        for p in suffix {
                            collect_binds(p, out);
                        }
                    }
                }
            }

            let mut binds = Vec::new();
            collect_binds(pat, &mut binds);
            return Ok(binds.into_iter().map(|id| (id, Ty::Never)).collect());
        }

        match pat {
            Pattern::Wildcard { .. } => Ok(Vec::new()),
            Pattern::Bind { name, .. } => Ok(vec![(name.clone(), expected)]),
            Pattern::Literal { lit, span } => {
                let lit_ty = match lit {
                    PatLiteral::Unit => Ty::Unit,
                    PatLiteral::Bool(_) => Ty::Bool,
                    PatLiteral::Int(_) => Ty::Int,
                    PatLiteral::Float(_) => Ty::Float,
                    PatLiteral::String(_) => Ty::String,
                    PatLiteral::Bytes(_) => Ty::Bytes,
                };
                let _ = self.infer.unify(expected, lit_ty, *span)?;
                Ok(Vec::new())
            }
            Pattern::Tuple {
                prefix,
                rest,
                suffix,
                span,
            } => {
                let (is_ro, inner) = match expected {
                    Ty::Readonly(inner) => (true, *inner),
                    other => (false, other),
                };

                let inner_resolved = self.infer.resolve_ty(inner.clone());
                let items = match inner_resolved {
                    Ty::Tuple(items) => items,
                    Ty::Var(_) if rest.is_none() => {
                        let total = prefix.len() + suffix.len();
                        let mut items = Vec::with_capacity(total);
                        for _ in 0..total {
                            items.push(self.infer.fresh_type_var());
                        }
                        let _ = self.infer.unify(inner, Ty::Tuple(items.clone()), *span)?;
                        items
                    }
                    other => {
                        return Err(TypeError {
                            message: format!("tuple pattern requires a tuple, got `{other}`"),
                            span: *span,
                        });
                    }
                };

                let prefix_len = prefix.len();
                let suffix_len = suffix.len();
                if rest.is_some() {
                    if items.len() < prefix_len + suffix_len {
                        return Err(TypeError {
                            message: format!(
                                "tuple pattern expects at least {} element(s), got {}",
                                prefix_len + suffix_len,
                                items.len()
                            ),
                            span: *span,
                        });
                    }
                } else if items.len() != prefix_len + suffix_len {
                    return Err(TypeError {
                        message: format!(
                            "tuple arity mismatch: expected {}, got {}",
                            prefix_len + suffix_len,
                            items.len()
                        ),
                        span: *span,
                    });
                }

                let mut binds = Vec::new();
                for (idx, subpat) in prefix.iter().enumerate() {
                    let mut elem_ty = items[idx].clone();
                    if is_ro {
                        elem_ty = elem_ty.as_readonly_view();
                    }
                    binds.extend(self.typecheck_pattern(subpat, elem_ty)?);
                }

                if let Some(rest_pat) = rest
                    && let Some(binding) = &rest_pat.binding
                {
                    let start = prefix_len;
                    let end = items.len().saturating_sub(suffix_len);
                    let mut rest_items = Vec::new();
                    for item_ty in &items[start..end] {
                        let mut ty = item_ty.clone();
                        if is_ro {
                            ty = ty.as_readonly_view();
                        }
                        rest_items.push(ty);
                    }
                    let rest_ty = if rest_items.is_empty() {
                        Ty::Unit
                    } else {
                        Ty::Tuple(rest_items)
                    };
                    binds.push((binding.clone(), rest_ty));
                }

                for (idx, subpat) in suffix.iter().enumerate() {
                    let item_idx = items.len() - suffix_len + idx;
                    let mut elem_ty = items[item_idx].clone();
                    if is_ro {
                        elem_ty = elem_ty.as_readonly_view();
                    }
                    binds.extend(self.typecheck_pattern(subpat, elem_ty)?);
                }

                Ok(binds)
            }
            Pattern::Array {
                prefix,
                rest,
                suffix,
                span,
            } => {
                let (is_ro, inner) = match expected {
                    Ty::Readonly(inner) => (true, *inner),
                    other => (false, other),
                };
                let elem = self.infer.fresh_type_var();
                let _ = self
                    .infer
                    .unify(inner, Ty::Array(Box::new(elem.clone())), *span)?;
                let elem_expected = if is_ro { elem.as_readonly_view() } else { elem };
                let mut binds = Vec::new();
                for it in prefix {
                    binds.extend(self.typecheck_pattern(it, elem_expected.clone())?);
                }
                if let Some(rest_pat) = rest
                    && let Some(binding) = &rest_pat.binding
                {
                    binds.push((binding.clone(), Ty::Array(Box::new(elem_expected.clone()))));
                }
                for it in suffix {
                    binds.extend(self.typecheck_pattern(it, elem_expected.clone())?);
                }
                Ok(binds)
            }
            Pattern::Struct {
                type_path,
                fields,
                has_rest,
                span,
            } => {
                let segments: Vec<String> =
                    type_path.segments.iter().map(|s| s.name.clone()).collect();
                let (kind, struct_name) = self
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, type_path.span)
                    .map_err(|e| TypeError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != crate::modules::DefKind::Struct {
                    return Err(TypeError {
                        message: format!("expected a struct type, got `{struct_name}`"),
                        span: type_path.span,
                    });
                }
                let Some(def) = self.env.structs.get(&struct_name) else {
                    return Err(TypeError {
                        message: format!("unknown struct `{struct_name}`"),
                        span: type_path.span,
                    });
                };

                let mut seen_fields = BTreeSet::<String>::new();
                for (fname, _) in fields.iter() {
                    if !seen_fields.insert(fname.name.clone()) {
                        return Err(TypeError {
                            message: format!(
                                "duplicate field `{}` in pattern for `{}`",
                                fname.name, def.name
                            ),
                            span: fname.span,
                        });
                    }
                }
                if !*has_rest {
                    let mut missing = Vec::new();
                    for (def_field, _) in def.fields.iter() {
                        if !seen_fields.contains(def_field) {
                            missing.push(def_field.clone());
                        }
                    }
                    if !missing.is_empty() {
                        return Err(TypeError {
                            message: format!(
                                "missing fields in pattern for `{}`: {} (add `..` to ignore remaining fields)",
                                def.name,
                                missing.join(", ")
                            ),
                            span: *span,
                        });
                    }
                }

                let mut type_args = Vec::with_capacity(def.generics.len());
                for _ in &def.generics {
                    type_args.push(self.infer.fresh_type_var());
                }
                let expected_ty = Ty::App(TyCon::Named(def.name.clone()), type_args.clone());
                let (ro, inner_expected) = match expected {
                    Ty::Readonly(inner) => (true, *inner),
                    other => (false, other),
                };
                let _ = self.infer.unify(inner_expected, expected_ty, *span)?;

                let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
                let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                let mut binds = Vec::new();
                for (fname, subpat) in fields {
                    let Some((_, fty)) = def.fields.iter().find(|(n, _)| n == &fname.name) else {
                        return Err(TypeError {
                            message: format!("unknown field `{}` on `{}`", fname.name, def.name),
                            span: fname.span,
                        });
                    };
                    let mut fty = subst_ty(fty.clone(), &ty_subst, &con_subst);
                    if ro {
                        fty = fty.as_readonly_view();
                    }
                    binds.extend(self.typecheck_pattern(subpat, fty)?);
                }
                Ok(binds)
            }
            Pattern::Ctor { path, args, span } => {
                // First attempt: enum variant constructor `EnumPath::Variant(...)`.
                if path.segments.len() >= 2 {
                    let enum_segments: Vec<String> = path.segments[..path.segments.len() - 1]
                        .iter()
                        .map(|s| s.name.clone())
                        .collect();
                    let variant = path.segments.last().expect("len >= 2");

                    if let Some((kind, enum_name)) = self
                        .env
                        .modules
                        .try_resolve_type_fqn(&self.module, &enum_segments, path.span)
                        .map_err(|e| TypeError {
                            message: e.message,
                            span: e.span,
                        })?
                        && kind == crate::modules::DefKind::Enum
                    {
                        let Some(def) = self.env.enums.get(&enum_name) else {
                            return Err(TypeError {
                                message: format!("unknown enum `{enum_name}`"),
                                span: path.span,
                            });
                        };
                        let Some(variant_fields) = def.variants.get(&variant.name) else {
                            return Err(TypeError {
                                message: format!(
                                    "unknown variant `{}` on enum `{}`",
                                    variant.name, def.name
                                ),
                                span: variant.span,
                            });
                        };
                        if args.len() != variant_fields.len() {
                            return Err(TypeError {
                                message: format!(
                                    "wrong number of fields for pattern `{}::{}`: expected {}, got {}",
                                    def.name,
                                    variant.name,
                                    variant_fields.len(),
                                    args.len()
                                ),
                                span: *span,
                            });
                        }

                        let mut type_args = Vec::with_capacity(def.generics.len());
                        for _ in &def.generics {
                            type_args.push(self.infer.fresh_type_var());
                        }
                        let expected_ty =
                            Ty::App(TyCon::Named(def.name.clone()), type_args.clone());
                        let (ro, inner_expected) = match expected {
                            Ty::Readonly(inner) => (true, *inner),
                            other => (false, other),
                        };
                        let _ = self.infer.unify(inner_expected, expected_ty, *span)?;

                        let ty_subst: HashMap<GenId, Ty> =
                            type_args.iter().cloned().enumerate().collect();
                        let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                        let mut binds = Vec::new();
                        for (subpat, fty) in args.iter().zip(variant_fields.iter()) {
                            let mut fty = subst_ty(fty.clone(), &ty_subst, &con_subst);
                            if ro {
                                fty = fty.as_readonly_view();
                            }
                            binds.extend(self.typecheck_pattern(subpat, fty)?);
                        }
                        return Ok(binds);
                    }
                }

                // Prelude-like `Option` constructors in patterns: `Some(p)` / `None`.
                //
                // This is intentionally limited to `Option` to avoid ambiguity for user enums.
                if path.segments.len() == 1
                    && matches!(path.segments[0].name.as_str(), "Some" | "None")
                    && let Some(def) = self.env.enums.get("Option")
                {
                    let variant = &path.segments[0];
                    let Some(variant_fields) = def.variants.get(&variant.name) else {
                        return Err(TypeError {
                            message: format!(
                                "unknown variant `{}` on enum `{}`",
                                variant.name, def.name
                            ),
                            span: variant.span,
                        });
                    };
                    if args.len() != variant_fields.len() {
                        return Err(TypeError {
                            message: format!(
                                "wrong number of fields for pattern `{}::{}`: expected {}, got {}",
                                def.name,
                                variant.name,
                                variant_fields.len(),
                                args.len()
                            ),
                            span: *span,
                        });
                    }

                    let mut type_args = Vec::with_capacity(def.generics.len());
                    for _ in &def.generics {
                        type_args.push(self.infer.fresh_type_var());
                    }
                    let expected_ty = Ty::App(TyCon::Named(def.name.clone()), type_args.clone());
                    let (ro, inner_expected) = match expected {
                        Ty::Readonly(inner) => (true, *inner),
                        other => (false, other),
                    };
                    let _ = self.infer.unify(inner_expected, expected_ty, *span)?;

                    let ty_subst: HashMap<GenId, Ty> =
                        type_args.iter().cloned().enumerate().collect();
                    let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                    let mut binds = Vec::new();
                    for (subpat, fty) in args.iter().zip(variant_fields.iter()) {
                        let mut fty = subst_ty(fty.clone(), &ty_subst, &con_subst);
                        if ro {
                            fty = fty.as_readonly_view();
                        }
                        binds.extend(self.typecheck_pattern(subpat, fty)?);
                    }
                    return Ok(binds);
                }

                // Fallback: new-type struct constructor `TypePath(value)`.
                let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();
                let (kind, struct_name) = self
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, path.span)
                    .map_err(|e| TypeError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != crate::modules::DefKind::Struct {
                    return Err(TypeError {
                        message: format!(
                            "expected an enum variant or new-type struct constructor, got `{struct_name}`"
                        ),
                        span: path.span,
                    });
                }
                let Some(def) = self.env.structs.get(&struct_name) else {
                    return Err(TypeError {
                        message: format!("unknown struct `{struct_name}`"),
                        span: path.span,
                    });
                };
                if !def.is_newtype {
                    return Err(TypeError {
                        message: format!(
                            "constructor pattern requires a new-type struct, got `{}`",
                            def.name
                        ),
                        span: path.span,
                    });
                }
                if args.len() != 1 {
                    return Err(TypeError {
                        message: format!(
                            "wrong number of fields for pattern `{}`: expected 1, got {}",
                            def.name,
                            args.len()
                        ),
                        span: *span,
                    });
                }

                let mut type_args = Vec::with_capacity(def.generics.len());
                for _ in &def.generics {
                    type_args.push(self.infer.fresh_type_var());
                }
                let expected_ty = Ty::App(TyCon::Named(def.name.clone()), type_args.clone());
                let (ro, inner_expected) = match expected {
                    Ty::Readonly(inner) => (true, *inner),
                    other => (false, other),
                };
                let _ = self.infer.unify(inner_expected, expected_ty, *span)?;

                let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
                let con_subst: HashMap<GenId, TyCon> = HashMap::new();
                let Some((_fname, field_ty)) = def.fields.first() else {
                    return Err(TypeError {
                        message: "internal error: malformed new-type struct definition".to_string(),
                        span: path.span,
                    });
                };
                let mut fty = subst_ty(field_ty.clone(), &ty_subst, &con_subst);
                if ro {
                    fty = fty.as_readonly_view();
                }
                self.typecheck_pattern(&args[0], fty)
            }
            Pattern::Enum {
                enum_path,
                variant,
                fields,
                span,
            } => {
                let segments: Vec<String> =
                    enum_path.segments.iter().map(|s| s.name.clone()).collect();
                let (kind, enum_name) = self
                    .env
                    .modules
                    .resolve_type_fqn(&self.module, &segments, enum_path.span)
                    .map_err(|e| TypeError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != crate::modules::DefKind::Enum {
                    return Err(TypeError {
                        message: format!("expected an enum type, got `{enum_name}`"),
                        span: enum_path.span,
                    });
                }
                let Some(def) = self.env.enums.get(&enum_name) else {
                    return Err(TypeError {
                        message: format!("unknown enum `{enum_name}`"),
                        span: enum_path.span,
                    });
                };
                let Some(variant_fields) = def.variants.get(&variant.name) else {
                    return Err(TypeError {
                        message: format!(
                            "unknown variant `{}` on enum `{}`",
                            variant.name, def.name
                        ),
                        span: variant.span,
                    });
                };
                if fields.len() != variant_fields.len() {
                    return Err(TypeError {
                        message: format!(
                            "wrong number of fields for pattern `{}::{}`: expected {}, got {}",
                            def.name,
                            variant.name,
                            variant_fields.len(),
                            fields.len()
                        ),
                        span: *span,
                    });
                }

                let mut type_args = Vec::with_capacity(def.generics.len());
                for _ in &def.generics {
                    type_args.push(self.infer.fresh_type_var());
                }
                let expected_ty = Ty::App(TyCon::Named(def.name.clone()), type_args.clone());
                let (ro, inner_expected) = match expected {
                    Ty::Readonly(inner) => (true, *inner),
                    other => (false, other),
                };
                let _ = self.infer.unify(inner_expected, expected_ty, *span)?;

                let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
                let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                let mut binds = Vec::new();
                for (subpat, fty) in fields.iter().zip(variant_fields.iter()) {
                    let mut fty = subst_ty(fty.clone(), &ty_subst, &con_subst);
                    if ro {
                        fty = fty.as_readonly_view();
                    }
                    binds.extend(self.typecheck_pattern(subpat, fty)?);
                }
                Ok(binds)
            }
        }
    }

    fn typecheck_call(
        &mut self,
        callee: &Expr,
        explicit_type_args: &[TypeExpr],
        args: &[Expr],
        _use_kind: ExprUse,
        span: Span,
    ) -> Result<Ty, TypeError> {
        // Direct call by path.
        if let Expr::Path {
            path,
            span: callee_span,
        } = callee
        {
            if path.segments.len() == 1 {
                let local_name = path.segments[0].name.as_str();
                if let Some(local) = self.lookup_local(local_name) {
                    if !explicit_type_args.is_empty() {
                        return Err(TypeError {
                            message:
                                "type arguments are only allowed on named function and method calls"
                                    .to_string(),
                            span,
                        });
                    }
                    let local_ty = local.ty.clone();
                    self.expr_types.insert(*callee_span, local_ty.clone());
                    return self.typecheck_call_via_fn_value(local_ty, args, span);
                }
            }

            // Prelude-like `Option` constructors: `Some(v)` / `None()`.
            if path.segments.len() == 1 && matches!(path.segments[0].name.as_str(), "Some" | "None")
            {
                if !explicit_type_args.is_empty() {
                    return Err(TypeError {
                        message:
                            "type arguments are only allowed on named function and method calls"
                                .to_string(),
                        span,
                    });
                }
                return self.typecheck_enum_lit("Option", &path.segments[0], args, span);
            }

            let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();
            let mut func_name: Option<String> = None;

            // New-type struct constructor call: `Name(v)` / `mod::Name(v)`.
            if let Some((kind, type_fqn)) = self
                .env
                .modules
                .try_resolve_type_fqn(&self.module, &segments, span)
                .map_err(|e| TypeError {
                    message: e.message,
                    span: e.span,
                })?
                && kind == crate::modules::DefKind::Struct
                && let Some(def) = self.env.structs.get(&type_fqn)
                && def.is_newtype
            {
                return self.typecheck_newtype_ctor_call(&type_fqn, explicit_type_args, args, span);
            }

            if segments.len() >= 2 {
                let prefix = &segments[..segments.len() - 1];
                let last = segments.last().expect("len >= 2");
                let last_ident = path.segments.last().expect("len >= 2");

                if let Some((kind, type_fqn)) = self
                    .env
                    .modules
                    .try_resolve_type_fqn(&self.module, prefix, span)
                    .map_err(|e| TypeError {
                        message: e.message,
                        span: e.span,
                    })?
                {
                    match kind {
                        crate::modules::DefKind::Enum => {
                            if let Some(def) = self.env.enums.get(&type_fqn)
                                && def.variants.contains_key(last)
                            {
                                return self.typecheck_enum_lit(&type_fqn, last_ident, args, span);
                            }

                            // Not a variant: allow inherent enum methods via `Enum::method(...)`.
                            let candidate = format!("{type_fqn}::{last}");
                            if self.env.functions.contains_key(&candidate) {
                                func_name = Some(candidate);
                            } else {
                                let self_type_expr = TypeExpr::Path(crate::ast::PathType {
                                    segments: path.segments[..path.segments.len() - 1]
                                        .iter()
                                        .map(|seg| crate::ast::PathTypeSegment {
                                            name: seg.clone(),
                                            args: Vec::new(),
                                            span: seg.span,
                                        })
                                        .collect(),
                                    assoc_bindings: Vec::new(),
                                    span: path.span,
                                });
                                if let Some(ty) = self
                                    .try_typecheck_static_iface_method_sugar_call(
                                        &type_fqn,
                                        self_type_expr,
                                        last_ident,
                                        explicit_type_args,
                                        args,
                                        span,
                                    )?
                                {
                                    return Ok(ty);
                                }
                            }
                        }
                        crate::modules::DefKind::Interface => {
                            return self.typecheck_interface_method_call(
                                &type_fqn,
                                last_ident,
                                explicit_type_args,
                                args,
                                span,
                            );
                        }
                        crate::modules::DefKind::Struct => {
                            let candidate = format!("{type_fqn}::{last}");
                            if self.env.functions.contains_key(&candidate) {
                                func_name = Some(candidate);
                            } else {
                                let self_type_expr = TypeExpr::Path(crate::ast::PathType {
                                    segments: path.segments[..path.segments.len() - 1]
                                        .iter()
                                        .map(|seg| crate::ast::PathTypeSegment {
                                            name: seg.clone(),
                                            args: Vec::new(),
                                            span: seg.span,
                                        })
                                        .collect(),
                                    assoc_bindings: Vec::new(),
                                    span: path.span,
                                });
                                if let Some(ty) = self
                                    .try_typecheck_static_iface_method_sugar_call(
                                        &type_fqn,
                                        self_type_expr,
                                        last_ident,
                                        explicit_type_args,
                                        args,
                                        span,
                                    )?
                                {
                                    return Ok(ty);
                                }
                            }
                        }
                    }
                }
            }

            // Allow UFCS-style primitive inherent method calls: `int::to_byte(...)`,
            // `bytes::from_array(...)`, etc.
            //
            // Primitives do not exist as nominal types in the module resolver, so we resolve
            // these directly against the built-in function table.
            if func_name.is_none() && segments.len() == 2 {
                let prim = segments[0].as_str();
                let last = segments[1].as_str();
                if matches!(
                    prim,
                    "unit" | "bool" | "int" | "float" | "byte" | "char" | "string" | "bytes"
                ) {
                    let candidate = format!("{prim}::{last}");
                    if self.env.functions.contains_key(&candidate) {
                        func_name = Some(candidate);
                    } else {
                        let prim_ty = match prim {
                            "unit" => PrimType::Unit,
                            "bool" => PrimType::Bool,
                            "int" => PrimType::Int,
                            "float" => PrimType::Float,
                            "byte" => PrimType::Byte,
                            "char" => PrimType::Char,
                            "string" => PrimType::String,
                            "bytes" => PrimType::Bytes,
                            _ => PrimType::Unit,
                        };
                        let self_type_expr = TypeExpr::Prim {
                            prim: prim_ty,
                            span: path.span,
                        };
                        if let Some(ty) = self.try_typecheck_static_iface_method_sugar_call(
                            prim,
                            self_type_expr,
                            path.segments.last().expect("len == 2"),
                            explicit_type_args,
                            args,
                            span,
                        )? {
                            return Ok(ty);
                        }
                    }
                }
            }

            let func_name = match func_name {
                Some(name) => name,
                None => self
                    .env
                    .modules
                    .resolve_value_fqn(&self.module, &segments, span)
                    .map_err(|e| TypeError {
                        message: e.message,
                        span: e.span,
                    })?,
            };

            let Some(sig) = self.env.functions.get(&func_name) else {
                return Err(TypeError {
                    message: format!("unknown function `{func_name}`"),
                    span,
                });
            };
            if !sig.vis.is_public() && !self.module.is_descendant_of(&sig.defining_module) {
                return Err(TypeError {
                    message: format!("function `{}` is private", sig.name),
                    span,
                });
            }
            let inst = instantiate_fn(sig, &mut self.infer);
            self.apply_explicit_type_args(
                explicit_type_args,
                &sig.generics,
                &inst.reified_type_args,
                &sig.name,
                span,
            )?;
            if args.len() != inst.params.len() {
                return Err(TypeError {
                    message: format!(
                        "arity mismatch for `{}`: expected {}, got {}",
                        sig.name,
                        inst.params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(inst.params.iter()) {
                let got = self.typecheck_expr(arg, ExprUse::Value)?;
                let _ = self.unify_expected(expected.clone(), got, arg.span())?;
            }

            if !inst.reified_type_args.is_empty() {
                self.call_type_args
                    .insert((span, sig.name.clone()), inst.reified_type_args.clone());
            }
            return Ok(inst.ret);
        }

        // Method call sugar: receiver.method(args...)
        if let Expr::Field {
            base,
            name: FieldName::Named(name),
            ..
        } = callee
        {
            return self.typecheck_method_call(base, name, explicit_type_args, args, span);
        }

        if !explicit_type_args.is_empty() {
            return Err(TypeError {
                message: "type arguments are only allowed on named function and method calls"
                    .to_string(),
                span,
            });
        }
        let callee_ty = self.typecheck_expr(callee, ExprUse::Value)?;
        self.typecheck_call_via_fn_value(callee_ty, args, span)
    }

    fn apply_explicit_type_args(
        &mut self,
        explicit_type_args: &[TypeExpr],
        generics: &[GenericParamInfo],
        reified_type_args: &[Ty],
        target: &str,
        span: Span,
    ) -> Result<(), TypeError> {
        if explicit_type_args.is_empty() {
            return Ok(());
        }

        if generics.iter().any(|g| g.arity != 0) {
            return Err(TypeError {
                message:
                    "explicit type arguments for higher-kinded generics are not supported in this stage"
                        .to_string(),
                span,
            });
        }

        if reified_type_args.len() != explicit_type_args.len() {
            return Err(TypeError {
                message: format!(
                    "type argument arity mismatch for `{target}`: expected {}, got {}",
                    reified_type_args.len(),
                    explicit_type_args.len()
                ),
                span,
            });
        }

        for (idx, ty_expr) in explicit_type_args.iter().enumerate() {
            let explicit_ty = self.lower_type_expr_in_fn(ty_expr)?;
            self.infer
                .unify(reified_type_args[idx].clone(), explicit_ty, ty_expr.span())?;
        }
        Ok(())
    }

    fn typecheck_newtype_ctor_call(
        &mut self,
        struct_name: &str,
        explicit_type_args: &[TypeExpr],
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let Some(def) = self.env.structs.get(struct_name) else {
            return Err(TypeError {
                message: format!("unknown struct `{struct_name}`"),
                span,
            });
        };
        if !def.is_newtype {
            return Err(TypeError {
                message: format!("`{struct_name}` is not a new-type struct"),
                span,
            });
        }
        if args.len() != 1 {
            return Err(TypeError {
                message: format!(
                    "arity mismatch for `{}`: expected 1, got {}",
                    def.name,
                    args.len()
                ),
                span,
            });
        }

        let Some((_fname, field_ty_template)) = def.fields.first() else {
            return Err(TypeError {
                message: "internal error: malformed new-type struct definition".to_string(),
                span,
            });
        };

        let mut type_args: Vec<Ty> = Vec::with_capacity(def.generics.len());
        for _ in &def.generics {
            type_args.push(self.infer.fresh_type_var());
        }
        self.apply_explicit_type_args(
            explicit_type_args,
            &def.generics,
            &type_args,
            &def.name,
            span,
        )?;

        let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
        let con_subst: HashMap<GenId, TyCon> = HashMap::new();
        let expected_field = subst_ty(field_ty_template.clone(), &ty_subst, &con_subst);
        let got = self.typecheck_expr(&args[0], ExprUse::Value)?;
        let _ = self.unify_expected(expected_field, got, args[0].span())?;

        Ok(Ty::App(TyCon::Named(def.name.clone()), type_args))
    }

    fn typecheck_call_via_fn_value(
        &mut self,
        callee_ty: Ty,
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let callee_ty = self.infer.resolve_ty(callee_ty);
        match callee_ty {
            Ty::Fn { params, ret } => {
                if args.len() != params.len() {
                    return Err(TypeError {
                        message: format!(
                            "arity mismatch: expected {}, got {}",
                            params.len(),
                            args.len()
                        ),
                        span,
                    });
                }
                for (arg, expected) in args.iter().zip(params.iter()) {
                    let got = self.typecheck_expr(arg, ExprUse::Value)?;
                    let _ = self.unify_expected(expected.clone(), got, arg.span())?;
                }
                Ok(*ret)
            }
            Ty::Cont { param, ret } => {
                if args.len() != 1 {
                    return Err(TypeError {
                        message: format!("arity mismatch: expected 1, got {}", args.len()),
                        span,
                    });
                }
                let got = self.typecheck_expr(&args[0], ExprUse::Value)?;
                let _ = self.unify_expected(*param, got, args[0].span())?;
                Ok(*ret)
            }
            other => Err(TypeError {
                message: format!("attempted to call a non-function value of type `{other}`"),
                span,
            }),
        }
    }

    fn typecheck_interface_method_call(
        &mut self,
        iface_name: &str,
        method: &Ident,
        explicit_type_args: &[TypeExpr],
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let method_name = method.name.as_str();
        let Some(iface) = self.env.interfaces.get(iface_name) else {
            return Err(TypeError {
                message: format!("unknown interface `{iface_name}`"),
                span,
            });
        };
        let Some(method_info) = iface.all_methods.get(method_name) else {
            return Err(TypeError {
                message: format!("unknown interface method `{iface_name}::{method_name}`"),
                span,
            });
        };
        match method_info.receiver {
            MethodReceiverKind::Instance {
                readonly: method_readonly,
            } => {
                if args.is_empty() {
                    return Err(TypeError {
                        message: format!(
                            "interface method call `{iface_name}::{method_name}` requires an explicit receiver argument"
                        ),
                        span,
                    });
                }
                if args.len() != method_info.sig.params.len() + 1 {
                    return Err(TypeError {
                        message: format!(
                            "arity mismatch for `{iface_name}::{method_name}`: expected {}, got {}",
                            method_info.sig.params.len() + 1,
                            args.len()
                        ),
                        span,
                    });
                }

                let recv_ty = self.typecheck_expr(&args[0], ExprUse::Value)?;
                let mut recv_ty_resolved = self.infer.resolve_ty(recv_ty.clone());
                if let Ty::Var(id) = strip_readonly(&recv_ty_resolved)
                    && self.infer.int_lit_vars.contains_key(id)
                {
                    // We don't support deferring interface-impl selection on an unconstrained receiver in
                    // this stage. Integer literals can be either `int` or `byte`, so apply the standard
                    // fallback to `int` when they need to act as an interface receiver (e.g. f-strings).
                    let _ = self.infer.unify(Ty::Var(*id), Ty::Int, args[0].span())?;
                    recv_ty_resolved = self.infer.resolve_ty(recv_ty.clone());
                }
                if !method_readonly && matches!(recv_ty_resolved, Ty::Readonly(_)) {
                    return Err(TypeError {
                        message: format!(
                            "cannot call mutable interface method `{iface_name}::{method_name}` on a readonly receiver"
                        ),
                        span: args[0].span(),
                    });
                }

                // For interface-typed receivers, only dyn-dispatchable methods are callable.
                //
                // Constrained generics (`T: I`) are also dynamically dispatched, but they can still call
                // "Self-only" methods since `Self` is statically the same type parameter at the call site.
                let recv_for_dispatch = strip_readonly(&recv_ty_resolved);
                let recv_is_iface_object = matches!(recv_for_dispatch, Ty::Iface { .. })
                    || matches!(recv_for_dispatch, Ty::App(TyCon::Named(name), _) if self.env.interfaces.contains_key(name));
                if recv_is_iface_object
                    && !interface_method_sig_is_dyn_dispatchable(&method_info.sig)
                {
                    let method_id = format!("{}::{method_name}", method_info.origin);
                    return Err(TypeError {
                        message: format!(
                            "method `{method_id}` is not dynamically dispatchable because it mentions `Self` in its signature; call requires a concrete receiver type"
                        ),
                        span: method.span,
                    });
                }

                let iface_args = self
                    .infer_interface_args_for_receiver(&recv_ty_resolved, iface_name)
                    .ok_or(TypeError {
                        message: format!(
                            "type `{recv_ty_resolved}` does not implement interface `{iface_name}`"
                        ),
                        span: args[0].span(),
                    })?;
                let iface_arity = iface.generics.len();
                let self_ty_for_inst = match recv_for_dispatch {
                    Ty::Iface { .. } => None,
                    Ty::App(TyCon::Named(name), _) if self.env.interfaces.contains_key(name) => {
                        None
                    }
                    _ => Some(recv_for_dispatch),
                };
                let mut inst = instantiate_interface_method_sig(
                    &method_info.sig,
                    &iface_args,
                    iface_arity,
                    self_ty_for_inst,
                    &mut self.infer,
                );
                inst.params = inst
                    .params
                    .into_iter()
                    .map(|t| self.normalize_assoc_projs_in_ty(t))
                    .collect();
                inst.ret = self.normalize_assoc_projs_in_ty(inst.ret);

                if let Ty::Iface { assoc_bindings, .. } = recv_for_dispatch {
                    inst.params = inst
                        .params
                        .into_iter()
                        .map(|t| self.subst_self_assoc_projs_from_bindings(t, assoc_bindings))
                        .collect();
                    inst.ret = self.subst_self_assoc_projs_from_bindings(inst.ret, assoc_bindings);
                }
                let target_name = format!("{iface_name}::{method_name}");
                self.apply_explicit_type_args(
                    explicit_type_args,
                    &method_info.sig.generics,
                    &inst.reified_type_args,
                    &target_name,
                    span,
                )?;
                let target_iface = Ty::App(TyCon::Named(iface_name.to_string()), iface_args);
                self.ensure_implements_interface_type(
                    &recv_ty_resolved,
                    &target_iface,
                    args[0].span(),
                )?;

                for (arg, expected) in args[1..].iter().zip(inst.params.iter()) {
                    let got = self.typecheck_expr(arg, ExprUse::Value)?;
                    let _ = self.unify_expected(expected.clone(), got, arg.span())?;
                }
                if !inst.reified_type_args.is_empty() {
                    let method_id = format!("{}::{method_name}", method_info.origin);
                    self.method_type_args
                        .insert((span, method_id), inst.reified_type_args.clone());
                }
                Ok(inst.ret)
            }
            MethodReceiverKind::Static => {
                if explicit_type_args.is_empty() {
                    return Err(TypeError {
                        message: format!(
                            "static interface method call `{iface_name}::{method_name}` requires an explicit implementing type argument; use `{iface_name}::{method_name}::<Type>(...)`"
                        ),
                        span,
                    });
                }

                let self_ty = self.lower_type_expr_in_fn(&explicit_type_args[0])?;
                let self_ty_resolved = self.infer.resolve_ty(self_ty.clone());
                let iface_args = self
                    .infer_interface_args_for_receiver(&self_ty_resolved, iface_name)
                    .ok_or(TypeError {
                        message: format!(
                            "type `{self_ty_resolved}` does not implement interface `{iface_name}`"
                        ),
                        span,
                    })?;
                let iface_arity = iface.generics.len();
                let mut inst = instantiate_interface_method_sig(
                    &method_info.sig,
                    &iface_args,
                    iface_arity,
                    Some(strip_readonly(&self_ty_resolved)),
                    &mut self.infer,
                );
                inst.params = inst
                    .params
                    .into_iter()
                    .map(|t| self.normalize_assoc_projs_in_ty(t))
                    .collect();
                inst.ret = self.normalize_assoc_projs_in_ty(inst.ret);

                let target_name = format!("{iface_name}::{method_name}");
                self.apply_explicit_type_args(
                    &explicit_type_args[1..],
                    &method_info.sig.generics,
                    &inst.reified_type_args,
                    &target_name,
                    span,
                )?;

                if args.len() != inst.params.len() {
                    return Err(TypeError {
                        message: format!(
                            "arity mismatch for `{iface_name}::{method_name}`: expected {}, got {}",
                            inst.params.len(),
                            args.len()
                        ),
                        span,
                    });
                }
                for (arg, expected) in args.iter().zip(inst.params.iter()) {
                    let got = self.typecheck_expr(arg, ExprUse::Value)?;
                    let _ = self.unify_expected(expected.clone(), got, arg.span())?;
                }

                let method_id = format!("{}::{method_name}", method_info.origin);
                self.static_iface_self_tys.insert(
                    (span, method_id.clone()),
                    strip_readonly(&self_ty_resolved).clone(),
                );
                if !inst.reified_type_args.is_empty() {
                    self.method_type_args
                        .insert((span, method_id), inst.reified_type_args.clone());
                }

                Ok(inst.ret)
            }
        }
    }

    fn try_typecheck_static_iface_method_sugar_call(
        &mut self,
        self_type_name: &str,
        self_type_expr: TypeExpr,
        method: &Ident,
        explicit_type_args: &[TypeExpr],
        args: &[Expr],
        span: Span,
    ) -> Result<Option<Ty>, TypeError> {
        let mut candidates: BTreeSet<String> = BTreeSet::new();

        for (ty_name, origin_iface, method_name) in self.env.interface_static_methods.keys() {
            if ty_name != self_type_name || method_name != &method.name {
                continue;
            }

            // Respect interface visibility (private interfaces should not be callable across modules).
            if let Some(def) = self.env.modules.def(origin_iface)
                && !def.vis.is_public()
                && !self.module.is_descendant_of(&def.defining_module)
            {
                continue;
            }

            // Defensive: ensure this really is a static interface method.
            if let Some(iface_def) = self.env.interfaces.get(origin_iface)
                && let Some(info) = iface_def.all_methods.get(&method.name)
                && matches!(info.receiver, MethodReceiverKind::Static)
            {
                candidates.insert(origin_iface.clone());
            }
        }

        if candidates.is_empty() {
            return Ok(None);
        }
        if candidates.len() != 1 {
            let mut names: Vec<String> = candidates.into_iter().collect();
            names.sort();
            return Err(TypeError {
                message: format!(
                    "ambiguous static interface method call `{self_type_name}::{}`; candidates: {}. Use fully-qualified syntax `Iface::{}::<{self_type_name}>(...)`",
                    method.name,
                    names.join(", "),
                    method.name,
                ),
                span: method.span,
            });
        }

        let origin_iface = candidates.into_iter().next().expect("len == 1");
        let mut combined_type_args: Vec<TypeExpr> =
            Vec::with_capacity(explicit_type_args.len() + 1);
        combined_type_args.push(self_type_expr);
        combined_type_args.extend_from_slice(explicit_type_args);
        let ty = self.typecheck_interface_method_call(
            &origin_iface,
            method,
            &combined_type_args,
            args,
            span,
        )?;
        Ok(Some(ty))
    }

    fn typecheck_method_call(
        &mut self,
        receiver: &Expr,
        method: &Ident,
        explicit_type_args: &[TypeExpr],
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let recv_ty = self.typecheck_expr(receiver, ExprUse::Value)?;
        let mut recv_ty_resolved = self.infer.resolve_ty(recv_ty.clone());
        if let Ty::Var(id) = strip_readonly(&recv_ty_resolved)
            && self.infer.int_lit_vars.contains_key(id)
        {
            // Integer literals can be `int` or `byte`. When a literal is used as a method-call
            // receiver, we need a concrete receiver type to resolve inherent methods / interface
            // impls in this stage.
            //
            // Prefer the unique inherent-method owner when possible (e.g. `42.to_char()` should
            // choose `int::to_char`). Otherwise, fall back to `int`.
            let int_inherent = format!("int::{}", method.name);
            let byte_inherent = format!("byte::{}", method.name);
            let choose = if self.env.functions.contains_key(&byte_inherent)
                && !self.env.functions.contains_key(&int_inherent)
            {
                Ty::Byte
            } else {
                Ty::Int
            };
            let _ = self.infer.unify(Ty::Var(*id), choose, receiver.span())?;
            recv_ty_resolved = self.infer.resolve_ty(recv_ty.clone());
        }

        let receiver_is_readonly = matches!(recv_ty_resolved, Ty::Readonly(_));
        let mut static_inherent: Option<String> = None;

        // In interface default method bodies, `self.m(...)` should resolve within the origin
        // interface method set before considering inherent methods on `Self`. (Proposal §4.5.)
        let receiver_is_self = matches!(
            receiver,
            Expr::Path { path, .. } if path.segments.len() == 1 && path.segments[0].name == "self"
        );
        if receiver_is_self
            && let Some(prefer_iface) = self.prefer_interface_methods_for_self.clone()
            && let Some(iface_def) = self.env.interfaces.get(&prefer_iface)
            && let Some(info) = iface_def.all_methods.get(&method.name)
            && let MethodReceiverKind::Instance {
                readonly: method_readonly,
            } = info.receiver
        {
            if receiver_is_readonly && !method_readonly {
                return Err(TypeError {
                    message: format!(
                        "cannot call mutable method `{}` on a readonly receiver `{recv_ty_resolved}`",
                        method.name
                    ),
                    span: method.span,
                });
            }
            let iface_args = self
                .infer_interface_args_for_receiver(&recv_ty_resolved, &prefer_iface)
                .ok_or_else(|| TypeError {
                    message: format!(
                        "internal error: failed to infer interface args for `{prefer_iface}`"
                    ),
                    span: method.span,
                })?;
            let mut inst = instantiate_interface_method_sig(
                &info.sig,
                &iface_args,
                iface_def.generics.len(),
                Some(strip_readonly(&recv_ty_resolved)),
                &mut self.infer,
            );
            inst.params = inst
                .params
                .into_iter()
                .map(|t| self.normalize_assoc_projs_in_ty(t))
                .collect();
            inst.ret = self.normalize_assoc_projs_in_ty(inst.ret);
            let target_name = format!("{}::{}", info.origin, method.name);
            self.apply_explicit_type_args(
                explicit_type_args,
                &info.sig.generics,
                &inst.reified_type_args,
                &target_name,
                span,
            )?;

            if args.len() != inst.params.len() {
                return Err(TypeError {
                    message: format!(
                        "arity mismatch for `{prefer_iface}::{}`: expected {}, got {}",
                        method.name,
                        inst.params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(inst.params.iter()) {
                let got = self.typecheck_expr(arg, ExprUse::Value)?;
                let _ = self.unify_expected(expected.clone(), got, arg.span())?;
            }
            if !inst.reified_type_args.is_empty() {
                let method_id = format!("{}::{}", info.origin, method.name);
                self.method_type_args
                    .insert((span, method_id), inst.reified_type_args.clone());
            }
            return Ok(inst.ret);
        }

        // Inherent method wins whenever the receiver has a nominal name.
        if let Some(type_name) = nominal_type_name(&recv_ty_resolved) {
            let inherent_name = format!("{type_name}::{}", method.name);
            let inherent_kind = self.env.inherent_method_kinds.get(&inherent_name).copied();
            if matches!(inherent_kind, Some(InherentMethodKind::Static)) {
                static_inherent = Some(inherent_name);
            } else if let Some(sig) = self.env.functions.get(&inherent_name) {
                if !sig.vis.is_public() && !self.module.is_descendant_of(&sig.defining_module) {
                    return Err(TypeError {
                        message: format!("method `{inherent_name}` is private"),
                        span: method.span,
                    });
                }
                let inst = instantiate_fn(sig, &mut self.infer);
                self.apply_explicit_type_args(
                    explicit_type_args,
                    &sig.generics,
                    &inst.reified_type_args,
                    &inherent_name,
                    span,
                )?;
                if inst.params.is_empty() {
                    return Err(TypeError {
                        message: format!(
                            "inherent method `{inherent_name}` is not an instance method"
                        ),
                        span,
                    });
                }
                let readonly_receiver = matches!(
                    inherent_kind,
                    Some(InherentMethodKind::Instance { readonly: true })
                );
                if !readonly_receiver && receiver_is_readonly {
                    return Err(TypeError {
                        message: format!(
                            "cannot call mutable method `{inherent_name}` on a readonly receiver"
                        ),
                        span: receiver.span(),
                    });
                }
                if readonly_receiver {
                    let expected = strip_readonly(&inst.params[0]).clone();
                    let got = strip_readonly(&recv_ty_resolved).clone();
                    let _ = self.unify_expected(expected, got, receiver.span())?;
                } else {
                    let receiver_expected = inst.params[0].clone();
                    let _ = self.unify_expected(receiver_expected, recv_ty, receiver.span())?;
                }
                if args.len() != inst.params.len() - 1 {
                    return Err(TypeError {
                        message: format!(
                            "arity mismatch for `{inherent_name}`: expected {}, got {}",
                            inst.params.len() - 1,
                            args.len()
                        ),
                        span,
                    });
                }
                for (arg, expected) in args.iter().zip(inst.params[1..].iter()) {
                    let got = self.typecheck_expr(arg, ExprUse::Value)?;
                    let _ = self.unify_expected(expected.clone(), got, arg.span())?;
                }
                if !inst.reified_type_args.is_empty() {
                    self.call_type_args.insert(
                        (span, inherent_name.clone()),
                        inst.reified_type_args.clone(),
                    );
                }
                return Ok(inst.ret);
            }
        }

        // Interface method resolution depends on the *static* receiver type.
        let recv_for_dispatch = match &recv_ty_resolved {
            Ty::Readonly(inner) => inner.as_ref(),
            other => other,
        };

        // Constrained generic receiver: resolve under bounds `T: B1 + ... + Bn`.
        if let Ty::Gen(id) = recv_for_dispatch {
            let Some(gp) = self.sig.generics.get(*id) else {
                return Err(TypeError {
                    message: "internal error: unknown generic parameter id".to_string(),
                    span: method.span,
                });
            };

            // Collect candidates named `method`, dedup by canonical origin interface id.
            let mut candidates: BTreeMap<String, (String, Vec<Ty>, InterfaceMethodSig)> =
                BTreeMap::new();
            let mut saw_mutable_candidate = false;
            for bound in &gp.bounds {
                let (bound_iface, bound_args) = match bound {
                    Ty::App(TyCon::Named(bound_iface), bound_args) => {
                        (bound_iface.clone(), bound_args.clone())
                    }
                    Ty::Iface { iface, args, .. } => (iface.clone(), args.clone()),
                    _ => continue,
                };
                let Some(iface_def) = self.env.interfaces.get(&bound_iface) else {
                    continue;
                };
                let Some(info) = iface_def.all_methods.get(&method.name) else {
                    continue;
                };
                let MethodReceiverKind::Instance {
                    readonly: method_readonly,
                } = info.receiver
                else {
                    continue;
                };
                if receiver_is_readonly && !method_readonly {
                    saw_mutable_candidate = true;
                    continue;
                }
                candidates
                    .entry(info.origin.clone())
                    .or_insert_with(|| (bound_iface.clone(), bound_args.clone(), info.sig.clone()));
            }

            if candidates.is_empty() {
                if receiver_is_readonly && saw_mutable_candidate {
                    return Err(TypeError {
                        message: format!(
                            "cannot call mutable method `{}` on a readonly receiver `{recv_ty_resolved}`",
                            method.name
                        ),
                        span: method.span,
                    });
                }
                return Err(TypeError {
                    message: format!("unknown method `{}` on `{recv_ty_resolved}`", method.name),
                    span: method.span,
                });
            }
            if candidates.len() != 1 {
                let candidates: Vec<String> = candidates.into_keys().collect();
                return Err(TypeError {
                    message: format!(
                        "ambiguous method `{}` on `{recv_ty_resolved}`; candidates: {}",
                        method.name,
                        candidates.join(", ")
                    ),
                    span: method.span,
                });
            }

            let (origin, (iface_name, iface_args, sig_template)) =
                candidates.into_iter().next().expect("len == 1");
            let iface_def = self.env.interfaces.get(&iface_name).expect("exists");
            let mut inst = instantiate_interface_method_sig(
                &sig_template,
                &iface_args,
                iface_def.generics.len(),
                Some(recv_for_dispatch),
                &mut self.infer,
            );
            inst.params = inst
                .params
                .into_iter()
                .map(|t| self.normalize_assoc_projs_in_ty(t))
                .collect();
            inst.ret = self.normalize_assoc_projs_in_ty(inst.ret);
            let target_name = format!("{origin}::{}", method.name);
            self.apply_explicit_type_args(
                explicit_type_args,
                &sig_template.generics,
                &inst.reified_type_args,
                &target_name,
                span,
            )?;

            if args.len() != inst.params.len() {
                return Err(TypeError {
                    message: format!(
                        "arity mismatch for `{iface_name}::{}`: expected {}, got {}",
                        method.name,
                        inst.params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(inst.params.iter()) {
                let got = self.typecheck_expr(arg, ExprUse::Value)?;
                let _ = self.unify_expected(expected.clone(), got, arg.span())?;
            }
            if !inst.reified_type_args.is_empty() {
                let method_id = format!("{origin}::{}", method.name);
                self.method_type_args
                    .insert((span, method_id), inst.reified_type_args.clone());
            }
            return Ok(inst.ret);
        }

        // Interface-typed receiver with associated type bindings.
        if let Ty::Iface {
            iface: iface_name,
            args: iface_args,
            assoc_bindings,
        } = recv_for_dispatch
            && let Some(iface_def) = self.env.interfaces.get(iface_name)
        {
            let Some(info) = iface_def.all_methods.get(&method.name) else {
                return Err(TypeError {
                    message: format!("unknown method `{}` on `{iface_name}`", method.name),
                    span: method.span,
                });
            };
            let MethodReceiverKind::Instance {
                readonly: method_readonly,
            } = info.receiver
            else {
                return Err(TypeError {
                    message: format!(
                        "method `{iface_name}::{}` is a static interface method and cannot be called with a receiver",
                        method.name
                    ),
                    span: method.span,
                });
            };
            if !interface_method_sig_is_dyn_dispatchable(&info.sig) {
                let method_id = format!("{}::{}", info.origin, method.name);
                return Err(TypeError {
                    message: format!(
                        "method `{method_id}` is not dynamically dispatchable because it mentions `Self` in its signature; call requires a concrete receiver type"
                    ),
                    span: method.span,
                });
            }
            if receiver_is_readonly && !method_readonly {
                return Err(TypeError {
                    message: format!(
                        "cannot call mutable method `{}` on a readonly receiver `{recv_ty_resolved}`",
                        method.name
                    ),
                    span: method.span,
                });
            }

            let mut inst = instantiate_interface_method_sig(
                &info.sig,
                iface_args,
                iface_def.generics.len(),
                None,
                &mut self.infer,
            );
            inst.params = inst
                .params
                .into_iter()
                .map(|t| self.subst_self_assoc_projs_from_bindings(t, assoc_bindings))
                .collect();
            inst.ret = self.subst_self_assoc_projs_from_bindings(inst.ret, assoc_bindings);

            let target_name = format!("{}::{}", info.origin, method.name);
            self.apply_explicit_type_args(
                explicit_type_args,
                &info.sig.generics,
                &inst.reified_type_args,
                &target_name,
                span,
            )?;

            if args.len() != inst.params.len() {
                return Err(TypeError {
                    message: format!(
                        "arity mismatch for `{iface_name}::{}`: expected {}, got {}",
                        method.name,
                        inst.params.len(),
                        args.len()
                    ),
                    span: method.span,
                });
            }
            for (arg, expected) in args.iter().zip(inst.params.iter()) {
                let got = self.typecheck_expr(arg, ExprUse::Value)?;
                let _ = self.unify_expected(expected.clone(), got, arg.span())?;
            }
            if !inst.reified_type_args.is_empty() {
                let method_id = format!("{}::{}", info.origin, method.name);
                self.method_type_args
                    .insert((span, method_id), inst.reified_type_args.clone());
            }
            return Ok(inst.ret);
        }

        // Interface-typed receiver: resolve within that interface instantiation (including inherited methods).
        if let Ty::App(TyCon::Named(iface_name), iface_args) = recv_for_dispatch
            && let Some(iface_def) = self.env.interfaces.get(iface_name)
        {
            let Some(info) = iface_def.all_methods.get(&method.name) else {
                return Err(TypeError {
                    message: format!("unknown method `{}` on `{iface_name}`", method.name),
                    span: method.span,
                });
            };
            let MethodReceiverKind::Instance {
                readonly: method_readonly,
            } = info.receiver
            else {
                return Err(TypeError {
                    message: format!(
                        "method `{iface_name}::{}` is a static interface method and cannot be called with a receiver",
                        method.name
                    ),
                    span: method.span,
                });
            };
            if !interface_method_sig_is_dyn_dispatchable(&info.sig) {
                let method_id = format!("{}::{}", info.origin, method.name);
                return Err(TypeError {
                    message: format!(
                        "method `{method_id}` is not dynamically dispatchable because it mentions `Self` in its signature; call requires a concrete receiver type"
                    ),
                    span: method.span,
                });
            }
            if receiver_is_readonly && !method_readonly {
                return Err(TypeError {
                    message: format!(
                        "cannot call mutable method `{}` on a readonly receiver `{recv_ty_resolved}`",
                        method.name
                    ),
                    span: method.span,
                });
            }
            let inst = instantiate_interface_method_sig(
                &info.sig,
                iface_args,
                iface_def.generics.len(),
                None,
                &mut self.infer,
            );
            let target_name = format!("{}::{}", info.origin, method.name);
            self.apply_explicit_type_args(
                explicit_type_args,
                &info.sig.generics,
                &inst.reified_type_args,
                &target_name,
                span,
            )?;
            if args.len() != inst.params.len() {
                return Err(TypeError {
                    message: format!(
                        "arity mismatch for `{iface_name}::{}`: expected {}, got {}",
                        method.name,
                        inst.params.len(),
                        args.len()
                    ),
                    span: method.span,
                });
            }
            for (arg, expected) in args.iter().zip(inst.params.iter()) {
                let got = self.typecheck_expr(arg, ExprUse::Value)?;
                let _ = self.unify_expected(expected.clone(), got, arg.span())?;
            }
            if !inst.reified_type_args.is_empty() {
                let method_id = format!("{}::{}", info.origin, method.name);
                self.method_type_args
                    .insert((span, method_id), inst.reified_type_args.clone());
            }
            return Ok(inst.ret);
        }

        // Concrete receiver: search interfaces implemented by the receiver, but treat
        // diamond-inherited methods as a single canonical method id.
        let Some(type_name) = nominal_type_name(&recv_ty_resolved) else {
            return Err(TypeError {
                message: format!(
                    "method call receiver is not a nominal type: `{recv_ty_resolved}`"
                ),
                span: receiver.span(),
            });
        };

        let mut origins: BTreeMap<String, (String, Vec<Ty>)> = BTreeMap::new();
        let mut saw_mutable_candidate = false;
        for (iface_name, iface) in &self.env.interfaces {
            if let Some(def) = self.env.modules.def(iface_name)
                && !def.vis.is_public()
                && !self.module.is_descendant_of(&def.defining_module)
            {
                continue;
            }
            let Some(method_info) = iface.all_methods.get(&method.name) else {
                continue;
            };
            let MethodReceiverKind::Instance {
                readonly: method_readonly,
            } = method_info.receiver
            else {
                continue;
            };
            if receiver_is_readonly && !method_readonly {
                saw_mutable_candidate = true;
                continue;
            }
            if let Some(iface_args) =
                self.infer_interface_args_for_receiver(&recv_ty_resolved, iface_name)
            {
                origins
                    .entry(method_info.origin.clone())
                    .or_insert_with(|| (iface_name.clone(), iface_args));
            }
        }

        if origins.is_empty() {
            if let Some(static_name) = static_inherent {
                return Err(TypeError {
                    message: format!(
                        "static method `{static_name}` must be called as `{static_name}(...)`"
                    ),
                    span: method.span,
                });
            }
            if receiver_is_readonly && saw_mutable_candidate {
                return Err(TypeError {
                    message: format!(
                        "cannot call mutable method `{}` on a readonly receiver `{recv_ty_resolved}`",
                        method.name
                    ),
                    span: method.span,
                });
            }
            return Err(TypeError {
                message: format!("unknown method `{}` on `{type_name}`", method.name),
                span: method.span,
            });
        }
        if origins.len() != 1 {
            let candidates: Vec<String> = origins.into_keys().collect();
            return Err(TypeError {
                message: format!(
                    "ambiguous method `{}` on `{type_name}`; candidates: {}",
                    method.name,
                    candidates.join(", ")
                ),
                span: method.span,
            });
        }

        let (iface, iface_args) = origins.into_values().next().expect("origins.len()==1");
        let iface_def = self.env.interfaces.get(&iface).expect("exists");
        let info = iface_def
            .all_methods
            .get(&method.name)
            .expect("method exists for chosen interface");
        let mut inst = instantiate_interface_method_sig(
            &info.sig,
            &iface_args,
            iface_def.generics.len(),
            Some(strip_readonly(&recv_ty_resolved)),
            &mut self.infer,
        );
        inst.params = inst
            .params
            .into_iter()
            .map(|t| self.normalize_assoc_projs_in_ty(t))
            .collect();
        inst.ret = self.normalize_assoc_projs_in_ty(inst.ret);
        let target_name = format!("{}::{}", info.origin, method.name);
        self.apply_explicit_type_args(
            explicit_type_args,
            &info.sig.generics,
            &inst.reified_type_args,
            &target_name,
            span,
        )?;
        if args.len() != inst.params.len() {
            return Err(TypeError {
                message: format!(
                    "arity mismatch for `{iface}::{}`: expected {}, got {}",
                    method.name,
                    inst.params.len(),
                    args.len()
                ),
                span,
            });
        }
        for (arg, expected) in args.iter().zip(inst.params.iter()) {
            let got = self.typecheck_expr(arg, ExprUse::Value)?;
            let _ = self.unify_expected(expected.clone(), got, arg.span())?;
        }
        if !inst.reified_type_args.is_empty() {
            let method_id = format!("{}::{}", info.origin, method.name);
            self.method_type_args
                .insert((span, method_id), inst.reified_type_args.clone());
        }
        Ok(inst.ret)
    }

    fn typecheck_field(
        &mut self,
        base: &Expr,
        name: &FieldName,
        span: Span,
    ) -> Result<Ty, TypeError> {
        let base_ty = self.typecheck_expr(base, ExprUse::Value)?;
        let base_ty = self.infer.resolve_ty(base_ty);
        let (is_readonly, inner) = match base_ty {
            Ty::Readonly(inner) => (true, *inner),
            other => (false, other),
        };

        match name {
            FieldName::Named(name) => {
                let Ty::App(TyCon::Named(type_name), type_args) = inner else {
                    return Err(TypeError {
                        message: format!("field access on non-struct value of type `{inner}`"),
                        span,
                    });
                };
                let Some(def) = self.env.structs.get(&type_name) else {
                    return Err(TypeError {
                        message: format!("field access requires a struct type, got `{type_name}`"),
                        span,
                    });
                };
                if type_args.len() != def.generics.len() {
                    return Err(TypeError {
                        message: format!(
                            "internal error: struct `{}` expected {} type argument(s), got {}",
                            def.name,
                            def.generics.len(),
                            type_args.len()
                        ),
                        span,
                    });
                }
                let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
                let con_subst: HashMap<GenId, TyCon> = HashMap::new();

                let Some((_, field_ty)) = def.fields.iter().find(|(n, _)| n == &name.name) else {
                    return Err(TypeError {
                        message: format!("unknown field `{}` on `{}`", name.name, def.name),
                        span: name.span,
                    });
                };
                let field_ty = subst_ty(field_ty.clone(), &ty_subst, &con_subst);
                let out = if is_readonly {
                    field_ty.as_readonly_view()
                } else {
                    field_ty
                };
                Ok(out)
            }
            FieldName::Index {
                index,
                span: idx_span,
            } => {
                let idx = *index;
                let out = match inner {
                    Ty::Tuple(items) => items.get(idx).cloned().ok_or(TypeError {
                        message: format!("tuple index {idx} out of bounds (len={})", items.len()),
                        span: *idx_span,
                    })?,
                    Ty::App(TyCon::Named(type_name), type_args) => {
                        let Some(def) = self.env.structs.get(&type_name) else {
                            return Err(TypeError {
                                message: format!(
                                    "tuple field access requires a tuple or new-type struct, got `{type_name}`"
                                ),
                                span,
                            });
                        };
                        if !def.is_newtype {
                            return Err(TypeError {
                                message: format!(
                                    "tuple field access on non-tuple value of type `{type_name}`"
                                ),
                                span,
                            });
                        }
                        if idx != 0 {
                            return Err(TypeError {
                                message: format!("tuple index {idx} out of bounds (len=1)"),
                                span: *idx_span,
                            });
                        }
                        let Some((_fname, field_ty)) = def.fields.first() else {
                            return Err(TypeError {
                                message: "internal error: malformed new-type struct definition"
                                    .to_string(),
                                span,
                            });
                        };
                        let ty_subst: HashMap<GenId, Ty> =
                            type_args.iter().cloned().enumerate().collect();
                        let con_subst: HashMap<GenId, TyCon> = HashMap::new();
                        subst_ty(field_ty.clone(), &ty_subst, &con_subst)
                    }
                    Ty::Var(_) => {
                        // Inference fallback: access `t.0` constrains `t` to a tuple of at least
                        // `idx + 1` elements, but tuples have fixed arity in Rusk; we pick the
                        // minimal arity that satisfies the access.
                        let mut items = Vec::with_capacity(idx + 1);
                        for _ in 0..=idx {
                            items.push(self.infer.fresh_type_var());
                        }
                        let expected_tuple = Ty::Tuple(items.clone());
                        let _ = self.infer.unify(inner, expected_tuple, span)?;
                        items[idx].clone()
                    }
                    other => {
                        return Err(TypeError {
                            message: format!(
                                "tuple field access on non-tuple value of type `{other}`"
                            ),
                            span,
                        });
                    }
                };
                Ok(if is_readonly {
                    out.as_readonly_view()
                } else {
                    out
                })
            }
        }
    }

    fn typecheck_index(&mut self, base: &Expr, index: &Expr, span: Span) -> Result<Ty, TypeError> {
        let base_ty = self.typecheck_expr(base, ExprUse::Value)?;
        let idx_ty = self.typecheck_expr(index, ExprUse::Value)?;
        let _ = self.unify_expected(Ty::Int, idx_ty, index.span())?;

        let base_ty = self.infer.resolve_ty(base_ty);
        let (is_readonly, inner) = match base_ty {
            Ty::Readonly(inner) => (true, *inner),
            other => (false, other),
        };
        match inner {
            Ty::Array(elem) => {
                let elem_ty = if is_readonly {
                    elem.as_readonly_view()
                } else {
                    *elem
                };
                Ok(elem_ty)
            }
            Ty::Bytes => Ok(Ty::Byte),
            other => Err(TypeError {
                message: format!("indexing requires an array or `bytes`, got `{other}`"),
                span,
            }),
        }
    }

    fn typecheck_unary(&mut self, op: UnaryOp, expr: &Expr, span: Span) -> Result<Ty, TypeError> {
        let t = self.typecheck_expr(expr, ExprUse::Value)?;
        let t_resolved = self.infer.resolve_ty(t.clone());
        match op {
            UnaryOp::Not => {
                match strip_readonly(&t_resolved) {
                    Ty::Bool | Ty::Never => {
                        let _ = self.unify_expected(Ty::Bool, t, span)?;
                        return Ok(Ty::Bool);
                    }
                    Ty::Int => {
                        let _ = self.unify_expected(Ty::Int, t, span)?;
                        return Ok(Ty::Int);
                    }
                    Ty::Byte => {
                        let _ = self.unify_expected(Ty::Byte, t, span)?;
                        return Ok(Ty::Byte);
                    }
                    Ty::Var(id) if self.infer.int_lit_vars.contains_key(id) => {
                        return Ok(Ty::Var(*id));
                    }
                    _ => {}
                }

                let recv_for_dispatch = strip_readonly(&t_resolved);
                let Ty::App(TyCon::Named(type_name), _type_args) = recv_for_dispatch else {
                    return Err(TypeError {
                        message: format!(
                            "unary `!` requires `bool` or an impl of `core::ops::Not` for a concrete type, got `{t_resolved}`"
                        ),
                        span,
                    });
                };
                if self.env.interfaces.contains_key(type_name) {
                    return Err(TypeError {
                        message: "unary `!` is not supported on interface-typed receivers in this stage (method is Self-only)"
                            .to_string(),
                        span,
                    });
                }

                self.ensure_implements_interface_type(
                    recv_for_dispatch,
                    &Ty::App(TyCon::Named("core::ops::Not".to_string()), vec![]),
                    span,
                )?;
                Ok(recv_for_dispatch.clone())
            }
            UnaryOp::Neg => match strip_readonly(&t_resolved) {
                Ty::Int => Ok(Ty::Int),
                Ty::Float => Ok(Ty::Float),
                Ty::Var(id) if self.infer.int_lit_vars.contains_key(id) => {
                    let _ = self.unify_expected(Ty::Int, t, span)?;
                    Ok(Ty::Int)
                }
                _ => {
                    let recv_for_dispatch = strip_readonly(&t_resolved);
                    let Ty::App(TyCon::Named(type_name), _type_args) = recv_for_dispatch else {
                        return Err(TypeError {
                            message: format!(
                                "unary `-` requires `int`, `float`, or an impl of `core::ops::Neg` for a concrete type, got `{t_resolved}`"
                            ),
                            span,
                        });
                    };
                    if self.env.interfaces.contains_key(type_name) {
                        return Err(TypeError {
                                message: "unary `-` is not supported on interface-typed receivers in this stage (method is Self-only)"
                                    .to_string(),
                                span,
                            });
                    }

                    self.ensure_implements_interface_type(
                        recv_for_dispatch,
                        &Ty::App(TyCon::Named("core::ops::Neg".to_string()), vec![]),
                        span,
                    )?;
                    Ok(recv_for_dispatch.clone())
                }
            },
        }
    }

    fn typecheck_binary(
        &mut self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        span: Span,
    ) -> Result<Ty, TypeError> {
        match op {
            BinaryOp::And | BinaryOp::Or => {
                let lt = self.typecheck_expr(left, ExprUse::Value)?;
                let rt = self.typecheck_expr(right, ExprUse::Value)?;
                let _ = self.unify_expected(Ty::Bool, lt, left.span())?;
                let _ = self.unify_expected(Ty::Bool, rt, right.span())?;
                Ok(Ty::Bool)
            }
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                let lt = self.typecheck_expr(left, ExprUse::Value)?;
                let rt = self.typecheck_expr(right, ExprUse::Value)?;
                let lt = self.infer.resolve_ty(lt);
                let rt = self.infer.resolve_ty(rt);
                let lt_base = strip_readonly(&lt).clone();
                let rt_base = strip_readonly(&rt).clone();
                let base = self.join_types(lt_base, rt_base, span)?;
                let base = self.infer.resolve_ty(base);

                match base {
                    Ty::Var(id) if self.infer.int_lit_vars.contains_key(&id) => {
                        let _ = self.infer.unify(Ty::Var(id), Ty::Int, span)?;
                        Ok(Ty::Int)
                    }
                    Ty::Int | Ty::Float => Ok(base),
                    _ => {
                        let iface = match op {
                            BinaryOp::Add => "core::ops::Add",
                            BinaryOp::Sub => "core::ops::Sub",
                            BinaryOp::Mul => "core::ops::Mul",
                            BinaryOp::Div => "core::ops::Div",
                            BinaryOp::Mod => "core::ops::Rem",
                            _ => unreachable!("covered by match arm"),
                        };

                        let Some(type_name) = nominal_type_name(&base) else {
                            return Err(TypeError {
                                message: format!(
                                    "operator `{op:?}` requires a concrete nominal type receiver in this stage, got `{base}`"
                                ),
                                span,
                            });
                        };
                        if self.env.interfaces.contains_key(type_name) {
                            return Err(TypeError {
                                message: format!(
                                    "operator `{op:?}` is not supported on interface-typed receivers in this stage (method is Self-only)"
                                ),
                                span,
                            });
                        }

                        self.ensure_implements_interface_type(
                            &base,
                            &Ty::App(TyCon::Named(iface.to_string()), vec![]),
                            span,
                        )?;
                        Ok(base)
                    }
                }
            }
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                let lt = self.typecheck_expr(left, ExprUse::Value)?;
                let rt = self.typecheck_expr(right, ExprUse::Value)?;
                let lt = self.infer.resolve_ty(lt);
                let rt = self.infer.resolve_ty(rt);
                let lt_base = strip_readonly(&lt).clone();
                let rt_base = strip_readonly(&rt).clone();
                let base = self.join_types(lt_base, rt_base, span)?;
                let base = self.infer.resolve_ty(base);

                match base {
                    Ty::Var(id) if self.infer.int_lit_vars.contains_key(&id) => Ok(Ty::Var(id)),
                    Ty::Int | Ty::Byte => Ok(base),
                    _ => {
                        let iface = match op {
                            BinaryOp::BitAnd => "core::ops::BitAnd",
                            BinaryOp::BitOr => "core::ops::BitOr",
                            BinaryOp::BitXor => "core::ops::BitXor",
                            _ => unreachable!("covered by match arm"),
                        };

                        let Some(type_name) = nominal_type_name(&base) else {
                            return Err(TypeError {
                                message: format!(
                                    "operator `{op:?}` requires a concrete nominal type receiver in this stage, got `{base}`"
                                ),
                                span,
                            });
                        };
                        if self.env.interfaces.contains_key(type_name) {
                            return Err(TypeError {
                                message: format!(
                                    "operator `{op:?}` is not supported on interface-typed receivers in this stage (method is Self-only)"
                                ),
                                span,
                            });
                        }

                        self.ensure_implements_interface_type(
                            &base,
                            &Ty::App(TyCon::Named(iface.to_string()), vec![]),
                            span,
                        )?;
                        Ok(base)
                    }
                }
            }
            BinaryOp::Shl | BinaryOp::Shr | BinaryOp::UShr => {
                let lt = self.typecheck_expr(left, ExprUse::Value)?;
                let rt = self.typecheck_expr(right, ExprUse::Value)?;
                let _ = self.unify_expected(Ty::Int, rt, right.span())?;

                let lt = self.infer.resolve_ty(lt);
                let base = strip_readonly(&lt).clone();

                if matches!(base, Ty::Int | Ty::Byte)
                    || matches!(&base, Ty::Var(id) if self.infer.int_lit_vars.contains_key(id))
                {
                    return Ok(base);
                }

                let iface = match op {
                    BinaryOp::Shl => "core::ops::Shl",
                    BinaryOp::Shr => "core::ops::Shr",
                    BinaryOp::UShr => "core::ops::UShr",
                    _ => unreachable!("covered by match arm"),
                };

                let Ty::App(TyCon::Named(type_name), _) = &base else {
                    return Err(TypeError {
                        message: format!(
                            "operator `{op:?}` requires a concrete nominal type receiver in this stage, got `{base}`"
                        ),
                        span,
                    });
                };
                if self.env.interfaces.contains_key(type_name) {
                    return Err(TypeError {
                        message: format!(
                            "operator `{op:?}` is not supported on interface-typed receivers in this stage (method is Self-only)"
                        ),
                        span,
                    });
                }

                self.ensure_implements_interface_type(
                    &base,
                    &Ty::App(TyCon::Named(iface.to_string()), vec![]),
                    span,
                )?;
                Ok(base)
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge => {
                let lt = self.typecheck_expr(left, ExprUse::Value)?;
                let rt = self.typecheck_expr(right, ExprUse::Value)?;
                let lt = self.infer.resolve_ty(lt);
                let rt = self.infer.resolve_ty(rt);
                let lt_base = strip_readonly(&lt).clone();
                let rt_base = strip_readonly(&rt).clone();
                let base = self.join_types(lt_base, rt_base, span)?;
                let base = self.infer.resolve_ty(base);

                if let Ty::Var(id) = &base
                    && self.infer.int_lit_vars.contains_key(id)
                {
                    // Integer literal comparisons fall back to `int` unless constrained by the
                    // surrounding context. For ordering comparisons, `byte` is not supported, so
                    // eagerly constrain to `int`.
                    if matches!(
                        op,
                        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge
                    ) {
                        let _ = self.infer.unify(base.clone(), Ty::Int, span)?;
                    }
                    return Ok(Ty::Bool);
                }

                let prim_ok = match op {
                    BinaryOp::Eq | BinaryOp::Ne => matches!(
                        base,
                        Ty::Unit
                            | Ty::Bool
                            | Ty::Int
                            | Ty::Float
                            | Ty::Byte
                            | Ty::Char
                            | Ty::String
                            | Ty::Bytes
                    ),
                    BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                        matches!(base, Ty::Int | Ty::Float)
                    }
                    _ => false,
                };
                if prim_ok {
                    return Ok(Ty::Bool);
                }

                let iface = match op {
                    BinaryOp::Eq => "core::ops::Eq",
                    BinaryOp::Ne => "core::ops::Ne",
                    BinaryOp::Lt => "core::ops::Lt",
                    BinaryOp::Le => "core::ops::Le",
                    BinaryOp::Gt => "core::ops::Gt",
                    BinaryOp::Ge => "core::ops::Ge",
                    _ => unreachable!("covered by match arm"),
                };

                let Ty::App(TyCon::Named(type_name), _) = &base else {
                    return Err(TypeError {
                        message: format!(
                            "operator `{op:?}` requires a concrete nominal type receiver in this stage, got `{base}`"
                        ),
                        span,
                    });
                };
                if self.env.interfaces.contains_key(type_name) {
                    return Err(TypeError {
                        message: format!(
                            "operator `{op:?}` is not supported on interface-typed receivers in this stage (method is Self-only)"
                        ),
                        span,
                    });
                }

                self.ensure_implements_interface_type(
                    &base,
                    &Ty::App(TyCon::Named(iface.to_string()), vec![]),
                    span,
                )?;
                Ok(Ty::Bool)
            }
        }
    }

    fn typecheck_assign(
        &mut self,
        target: &Expr,
        value: &Expr,
        span: Span,
    ) -> Result<Ty, TypeError> {
        match target {
            Expr::Path { path, .. } => {
                if path.segments.len() != 1 {
                    return Err(TypeError {
                        message: "assignment target must be a local name".to_string(),
                        span,
                    });
                }
                let name = path.segments[0].name.as_str();
                let Some(info) = self.lookup_local(name) else {
                    return Err(TypeError {
                        message: format!("unknown name `{name}`"),
                        span: path.segments[0].span,
                    });
                };
                let dst_ty = info.ty.clone();
                let dst_kind = info.kind;
                if !matches!(dst_kind, BindingKind::Let) {
                    return Err(TypeError {
                        message: format!("cannot assign to `{name}` (not a `let` binding)"),
                        span: path.segments[0].span,
                    });
                }
                let rhs = self.typecheck_expr(value, ExprUse::Value)?;
                let _ = self.unify_expected(dst_ty, rhs, value.span())?;
                Ok(Ty::Unit)
            }
            Expr::Field { base, name, .. } => {
                let base_ty = self.typecheck_expr(base, ExprUse::Value)?;
                let base_ty = self.infer.resolve_ty(base_ty);
                let inner = match base_ty {
                    Ty::Readonly(_) => {
                        return Err(TypeError {
                            message: "cannot assign through a readonly view".to_string(),
                            span: base.span(),
                        });
                    }
                    other => other,
                };

                match name {
                    FieldName::Named(name) => {
                        let Ty::App(TyCon::Named(type_name), type_args) = inner else {
                            return Err(TypeError {
                                message: format!(
                                    "field assignment requires a struct, got `{inner}`"
                                ),
                                span: base.span(),
                            });
                        };
                        let Some(def) = self.env.structs.get(&type_name) else {
                            return Err(TypeError {
                                message: format!(
                                    "field assignment requires a struct type, got `{type_name}`"
                                ),
                                span: base.span(),
                            });
                        };
                        if type_args.len() != def.generics.len() {
                            return Err(TypeError {
                                message: "internal error: struct type argument mismatch"
                                    .to_string(),
                                span,
                            });
                        }
                        let Some((_, field_ty)) = def.fields.iter().find(|(n, _)| n == &name.name)
                        else {
                            return Err(TypeError {
                                message: format!("unknown field `{}` on `{}`", name.name, def.name),
                                span: name.span,
                            });
                        };
                        let ty_subst: HashMap<GenId, Ty> =
                            type_args.iter().cloned().enumerate().collect();
                        let con_subst: HashMap<GenId, TyCon> = HashMap::new();
                        let expected = subst_ty(field_ty.clone(), &ty_subst, &con_subst);
                        let rhs = self.typecheck_expr(value, ExprUse::Value)?;
                        let _ = self.unify_expected(expected, rhs, value.span())?;
                        Ok(Ty::Unit)
                    }
                    FieldName::Index {
                        index,
                        span: idx_span,
                    } => {
                        let idx = *index;
                        let expected = match inner {
                            Ty::Tuple(items) => items.get(idx).cloned().ok_or(TypeError {
                                message: format!(
                                    "tuple index {idx} out of bounds (len={})",
                                    items.len()
                                ),
                                span: *idx_span,
                            })?,
                            Ty::App(TyCon::Named(type_name), type_args) => {
                                let Some(def) = self.env.structs.get(&type_name) else {
                                    return Err(TypeError {
                                        message: format!(
                                            "tuple field assignment requires a tuple or new-type struct, got `{type_name}`"
                                        ),
                                        span: base.span(),
                                    });
                                };
                                if !def.is_newtype {
                                    return Err(TypeError {
                                        message: format!(
                                            "tuple field assignment requires a tuple, got `{type_name}`"
                                        ),
                                        span: base.span(),
                                    });
                                }
                                if idx != 0 {
                                    return Err(TypeError {
                                        message: format!("tuple index {idx} out of bounds (len=1)"),
                                        span: *idx_span,
                                    });
                                }
                                let Some((_fname, field_ty)) = def.fields.first() else {
                                    return Err(TypeError {
                                        message:
                                            "internal error: malformed new-type struct definition"
                                                .to_string(),
                                        span: base.span(),
                                    });
                                };
                                let ty_subst: HashMap<GenId, Ty> =
                                    type_args.iter().cloned().enumerate().collect();
                                let con_subst: HashMap<GenId, TyCon> = HashMap::new();
                                subst_ty(field_ty.clone(), &ty_subst, &con_subst)
                            }
                            other => {
                                return Err(TypeError {
                                    message: format!(
                                        "tuple field assignment requires a tuple, got `{other}`"
                                    ),
                                    span: base.span(),
                                });
                            }
                        };
                        let rhs = self.typecheck_expr(value, ExprUse::Value)?;
                        let _ = self.unify_expected(expected, rhs, value.span())?;
                        Ok(Ty::Unit)
                    }
                }
            }
            Expr::Index { base, index, .. } => {
                let base_ty = self.typecheck_expr(base, ExprUse::Value)?;
                let idx_ty = self.typecheck_expr(index, ExprUse::Value)?;
                let _ = self.unify_expected(Ty::Int, idx_ty, index.span())?;
                let base_ty = self.infer.resolve_ty(base_ty);
                let inner = match base_ty {
                    Ty::Readonly(_) => {
                        return Err(TypeError {
                            message: "cannot assign through a readonly view".to_string(),
                            span: base.span(),
                        });
                    }
                    other => other,
                };
                let Ty::Array(elem) = inner else {
                    return Err(TypeError {
                        message: format!("index assignment requires an array, got `{inner}`"),
                        span: base.span(),
                    });
                };
                let rhs = self.typecheck_expr(value, ExprUse::Value)?;
                let _ = self.unify_expected(*elem, rhs, value.span())?;
                Ok(Ty::Unit)
            }
            _ => Err(TypeError {
                message: "invalid assignment target".to_string(),
                span,
            }),
        }
    }

    fn typecheck_as(&mut self, expr: &Expr, ty: &TypeExpr, span: Span) -> Result<Ty, TypeError> {
        let src_ty = self.typecheck_expr(expr, ExprUse::Value)?;
        let src_ty = self.infer.resolve_ty(src_ty);
        let (src_is_readonly, src_inner) = match src_ty {
            Ty::Readonly(inner) => (true, *inner),
            other => (false, other),
        };

        let target_ty = self.lower_type_expr_in_fn(ty)?;
        validate_value_ty(self.env, &target_ty, ty.span())?;

        let out = match target_ty {
            Ty::App(TyCon::Named(iface_name), args) => {
                let target_iface = Ty::App(TyCon::Named(iface_name.clone()), args);
                if !self.env.interfaces.contains_key(&iface_name) {
                    return Err(TypeError {
                        message: format!(
                            "`as` target must be an interface type, got `{iface_name}`"
                        ),
                        span,
                    });
                }
                if !self.type_implements_interface_type(&src_inner, &target_iface) {
                    return Err(TypeError {
                        message: format!("type `{src_inner}` does not implement `{target_iface}`"),
                        span,
                    });
                }
                target_iface
            }
            Ty::Iface {
                iface,
                args,
                assoc_bindings,
            } => {
                let target_iface = Ty::App(TyCon::Named(iface.clone()), args.clone());
                if !self.type_implements_interface_type(&src_inner, &target_iface) {
                    return Err(TypeError {
                        message: format!("type `{src_inner}` does not implement `{target_iface}`"),
                        span,
                    });
                }

                // Verify the impl's associated types satisfy the requested bindings.
                let src_inner_stripped = strip_readonly(&src_inner).clone();
                match &src_inner_stripped {
                    Ty::App(TyCon::Named(type_name), _type_args)
                        if self.env.interfaces.contains_key(type_name) =>
                    {
                        return Err(TypeError {
                            message: format!(
                                "cannot upcast an interface value `{src_inner_stripped}` to `{iface}{{...}}`; upcast requires a concrete receiver type"
                            ),
                            span,
                        });
                    }
                    Ty::App(TyCon::Named(_type_name), _type_args) => {
                        for (aname, expected) in &assoc_bindings {
                            let actual = self.normalize_assoc_projs_in_ty(Ty::AssocProj {
                                iface: iface.clone(),
                                iface_args: args.clone(),
                                assoc: aname.clone(),
                                self_ty: Box::new(src_inner_stripped.clone()),
                            });
                            let expected = self.normalize_assoc_projs_in_ty(expected.clone());
                            if actual != expected {
                                return Err(TypeError {
                                    message: format!(
                                        "cannot upcast to `{iface}{{{aname} = {expected}}}`; implementation defines `{aname}` as `{actual}`"
                                    ),
                                    span,
                                });
                            }
                        }
                    }
                    Ty::Iface {
                        assoc_bindings: src_bindings,
                        ..
                    } => {
                        for (aname, expected) in &assoc_bindings {
                            let Some(actual) = src_bindings.get(aname) else {
                                return Err(TypeError {
                                    message: format!(
                                        "cannot upcast to `{iface}{{...}}`; missing associated type binding `{aname}`"
                                    ),
                                    span,
                                });
                            };
                            let actual = self.normalize_assoc_projs_in_ty(actual.clone());
                            let expected = self.normalize_assoc_projs_in_ty(expected.clone());
                            if actual != expected {
                                return Err(TypeError {
                                    message: format!(
                                        "cannot upcast to `{iface}{{{aname} = {expected}}}`; got `{aname} = {actual}`"
                                    ),
                                    span,
                                });
                            }
                        }
                    }
                    Ty::Gen(id) => {
                        let Some(gp) = self.sig.generics.get(*id) else {
                            return Err(TypeError {
                                message: "internal error: unknown generic parameter id".to_string(),
                                span,
                            });
                        };
                        let mut ok = false;
                        'bounds: for bound in &gp.bounds {
                            let Ty::Iface {
                                iface: bound_iface,
                                args: bound_args,
                                assoc_bindings: bound_bindings,
                            } = bound
                            else {
                                continue;
                            };
                            let Some(mapped_args) = infer_super_interface_args(
                                self.env,
                                bound_iface,
                                bound_args,
                                &iface,
                            ) else {
                                continue;
                            };
                            if mapped_args != args {
                                continue;
                            }
                            for (aname, expected) in &assoc_bindings {
                                let Some(actual) = bound_bindings.get(aname) else {
                                    continue 'bounds;
                                };
                                let actual = self.normalize_assoc_projs_in_ty(actual.clone());
                                let expected = self.normalize_assoc_projs_in_ty(expected.clone());
                                if actual != expected {
                                    continue 'bounds;
                                }
                            }
                            ok = true;
                            break;
                        }
                        if !ok {
                            return Err(TypeError {
                                message: format!(
                                    "cannot upcast type parameter `{}` to `{iface}{{...}}`; add a bound `{}`: `{iface}{{...}}`",
                                    gp.name, gp.name
                                ),
                                span,
                            });
                        }
                    }
                    other => {
                        return Err(TypeError {
                            message: format!(
                                "cannot upcast type `{other}` to `{iface}{{...}}`; upcast requires a nominal type or a bounded type parameter"
                            ),
                            span,
                        });
                    }
                }

                Ty::Iface {
                    iface,
                    args,
                    assoc_bindings,
                }
            }
            other => {
                return Err(TypeError {
                    message: format!("`as` target must be an interface type, got `{other}`"),
                    span,
                });
            }
        };
        Ok(if src_is_readonly {
            Ty::Readonly(Box::new(out))
        } else {
            out
        })
    }

    fn typecheck_is(&mut self, expr: &Expr, ty: &TypeExpr, span: Span) -> Result<Ty, TypeError> {
        // Evaluate/typecheck the LHS for side effects and to populate `expr_types`.
        let _ = self.typecheck_expr(expr, ExprUse::Value)?;

        // Validate the target type is runtime-checkable.
        let _ = self.lower_runtime_checkable_type(ty, ty.span(), "`is`")?;
        let _ = span;
        Ok(Ty::Bool)
    }

    fn typecheck_as_question(
        &mut self,
        expr: &Expr,
        ty: &TypeExpr,
        span: Span,
    ) -> Result<Ty, TypeError> {
        let src_ty = self.typecheck_expr(expr, ExprUse::Value)?;
        let src_ty = self.infer.resolve_ty(src_ty);
        let src_is_readonly = matches!(src_ty, Ty::Readonly(_));

        let target = self.lower_runtime_checkable_type(ty, ty.span(), "`as?`")?;
        let target = if src_is_readonly {
            target.as_readonly_view()
        } else {
            target
        };
        let _ = span;

        // Built-in enum `Option<T> { Some(T), None }`.
        Ok(Ty::App(TyCon::Named("Option".to_string()), vec![target]))
    }

    fn lower_runtime_checkable_type(
        &self,
        ty: &TypeExpr,
        span: Span,
        op: &str,
    ) -> Result<Ty, TypeError> {
        let target_ty = self.lower_type_expr_in_fn(ty)?;

        let target_ty = match target_ty {
            Ty::Readonly(inner) => {
                return Err(TypeError {
                    message: format!(
                        "{op} target type must not be `readonly`; got `readonly {inner}`"
                    ),
                    span,
                });
            }
            other => other,
        };

        let (name, args) = match target_ty {
            Ty::App(TyCon::Named(name), args) => (name, args),
            Ty::Iface { iface, .. } => {
                return Err(TypeError {
                    message: format!(
                        "{op} target does not support interface types with associated type bindings yet; use `{iface}` without `{{...}}`"
                    ),
                    span,
                });
            }
            other => {
                return Err(TypeError {
                    message: format!(
                        "{op} target must be a nominal type (`struct`/`enum`/`interface`), got `{other}`"
                    ),
                    span,
                });
            }
        };

        if !self.env.structs.contains_key(&name)
            && !self.env.enums.contains_key(&name)
            && !self.env.interfaces.contains_key(&name)
        {
            return Err(TypeError {
                message: format!(
                    "{op} target must be a nominal type (`struct`/`enum`/`interface`), got `{name}`"
                ),
                span,
            });
        }

        Ok(Ty::App(TyCon::Named(name), args))
    }

    fn infer_interface_args_for_receiver(&self, recv_ty: &Ty, iface: &str) -> Option<Vec<Ty>> {
        let recv_ty = strip_readonly(recv_ty);
        let iface_arity = self.env.interfaces.get(iface)?.generics.len();

        match recv_ty {
            // Bounded generic `T`: infer from the bounds list, possibly via subinterface mapping.
            Ty::Gen(id) => {
                let bounds = self.sig.generics.get(*id)?.bounds.clone();
                let mut found: Option<Vec<Ty>> = None;
                for bound in bounds {
                    let (bound_iface, bound_args) = match bound {
                        Ty::App(TyCon::Named(bound_iface), bound_args) => (bound_iface, bound_args),
                        Ty::Iface { iface, args, .. } => (iface, args),
                        _ => continue,
                    };
                    let Some(args) =
                        infer_super_interface_args(self.env, &bound_iface, &bound_args, iface)
                    else {
                        continue;
                    };
                    if args.len() != iface_arity {
                        continue;
                    }
                    match &found {
                        None => found = Some(args),
                        Some(prev) if prev == &args => {}
                        Some(_) => return None,
                    }
                }
                found
            }

            // Interface-typed value `J{...}`: infer from inheritance.
            Ty::Iface {
                iface: recv_iface,
                args,
                ..
            } => infer_super_interface_args(self.env, recv_iface, args, iface),

            // Interface-typed value `J<...>`: infer from inheritance.
            Ty::App(TyCon::Named(type_name), type_args)
                if self.env.interfaces.contains_key(type_name) =>
            {
                infer_super_interface_args(self.env, type_name, type_args, iface)
            }

            // Concrete type `S<...>`: infer by the initial-stage coherence rule (interface args
            // are a prefix of the concrete type args).
            Ty::App(TyCon::Named(type_name), type_args) => {
                if !self
                    .env
                    .interface_impls
                    .contains(&(type_name.clone(), iface.to_string()))
                {
                    return None;
                }
                if iface_arity > type_args.len() {
                    return None;
                }
                Some(type_args[..iface_arity].to_vec())
            }

            // Primitive types may have built-in impls for arity-0 interfaces.
            Ty::Unit
            | Ty::Bool
            | Ty::Int
            | Ty::Float
            | Ty::Byte
            | Ty::Char
            | Ty::String
            | Ty::Bytes => {
                if iface_arity != 0 {
                    return None;
                }
                let type_name = match recv_ty {
                    Ty::Unit => "unit",
                    Ty::Bool => "bool",
                    Ty::Int => "int",
                    Ty::Float => "float",
                    Ty::Byte => "byte",
                    Ty::Char => "char",
                    Ty::String => "string",
                    Ty::Bytes => "bytes",
                    _ => unreachable!("matched primitive types"),
                };
                if !self
                    .env
                    .interface_impls
                    .contains(&(type_name.to_string(), iface.to_string()))
                {
                    return None;
                }
                Some(Vec::new())
            }

            // Arrays are not nominal types, but can have built-in impls (arity-0 only for now).
            Ty::Array(_elem) => {
                if iface_arity != 0 {
                    return None;
                }
                if !self
                    .env
                    .interface_impls
                    .contains(&("array".to_string(), iface.to_string()))
                {
                    return None;
                }
                Some(Vec::new())
            }

            _ => None,
        }
    }

    fn ensure_implements_interface_type(
        &self,
        ty: &Ty,
        iface_ty: &Ty,
        span: Span,
    ) -> Result<(), TypeError> {
        if self.type_implements_interface_type(ty, iface_ty) {
            Ok(())
        } else {
            Err(TypeError {
                message: format!("type `{ty}` does not implement `{iface_ty}`"),
                span,
            })
        }
    }

    fn type_implements_interface_type(&self, ty: &Ty, iface_ty: &Ty) -> bool {
        let iface_ty = strip_readonly(iface_ty);
        let Ty::App(TyCon::Named(iface_name), iface_args) = iface_ty else {
            return false;
        };
        if !self.env.interfaces.contains_key(iface_name) {
            return false;
        }

        let ty = strip_readonly(ty);
        match ty {
            // Generic `T` implements `I<...>` iff one of its bounds implies it.
            Ty::Gen(id) => {
                let Some(gp) = self.sig.generics.get(*id) else {
                    return false;
                };
                for bound in &gp.bounds {
                    let (bound_iface, bound_args) = match bound {
                        Ty::App(TyCon::Named(bound_iface), bound_args) => (bound_iface, bound_args),
                        Ty::Iface { iface, args, .. } => (iface, args),
                        _ => continue,
                    };
                    let Some(args) =
                        infer_super_interface_args(self.env, bound_iface, bound_args, iface_name)
                    else {
                        continue;
                    };
                    if args == *iface_args {
                        return true;
                    }
                }
                false
            }

            // Interface value `J{...}` implements `I<...>` iff `J<...>` implies that instantiation.
            Ty::Iface {
                iface: type_name,
                args: type_args,
                ..
            } => infer_super_interface_args(self.env, type_name, type_args, iface_name)
                .is_some_and(|args| args == *iface_args),

            // Interface value `J<...>` implements `I<...>` iff `J<...>` implies that instantiation.
            Ty::App(TyCon::Named(type_name), type_args)
                if self.env.interfaces.contains_key(type_name) =>
            {
                infer_super_interface_args(self.env, type_name, type_args, iface_name)
                    .is_some_and(|args| args == *iface_args)
            }

            // Concrete nominal type: initial-stage coherence => interface args are a prefix of
            // the concrete type args.
            Ty::App(TyCon::Named(type_name), type_args) => {
                if !self
                    .env
                    .interface_impls
                    .contains(&(type_name.clone(), iface_name.to_string()))
                {
                    return false;
                }
                if iface_args.len() > type_args.len() {
                    return false;
                }
                type_args[..iface_args.len()] == *iface_args
            }

            // Primitive types may have built-in impls for arity-0 interfaces.
            Ty::Unit
            | Ty::Bool
            | Ty::Int
            | Ty::Float
            | Ty::Byte
            | Ty::Char
            | Ty::String
            | Ty::Bytes => {
                if !iface_args.is_empty() {
                    return false;
                }
                let type_name = match ty {
                    Ty::Unit => "unit",
                    Ty::Bool => "bool",
                    Ty::Int => "int",
                    Ty::Float => "float",
                    Ty::Byte => "byte",
                    Ty::Char => "char",
                    Ty::String => "string",
                    Ty::Bytes => "bytes",
                    _ => return false,
                };
                self.env
                    .interface_impls
                    .contains(&(type_name.to_string(), iface_name.to_string()))
            }

            // Arrays are not nominal types, but can have built-in impls (arity-0 only for now).
            Ty::Array(_elem) => {
                if !iface_args.is_empty() {
                    return false;
                }
                self.env
                    .interface_impls
                    .contains(&("array".to_string(), iface_name.to_string()))
            }

            _ => false,
        }
    }
}

fn contains_infer_vars(ty: &Ty) -> bool {
    match ty {
        Ty::Var(_) => true,
        Ty::Array(elem) | Ty::Readonly(elem) => contains_infer_vars(elem),
        Ty::Tuple(items) => items.iter().any(contains_infer_vars),
        Ty::Fn { params, ret } => {
            params.iter().any(contains_infer_vars) || contains_infer_vars(ret)
        }
        Ty::Cont { param, ret } => contains_infer_vars(param) || contains_infer_vars(ret),
        Ty::App(con, args) => matches!(con, TyCon::Var(_)) || args.iter().any(contains_infer_vars),
        Ty::Iface {
            args,
            assoc_bindings,
            ..
        } => {
            args.iter().any(contains_infer_vars) || assoc_bindings.values().any(contains_infer_vars)
        }
        Ty::AssocProj {
            iface_args,
            self_ty,
            ..
        } => iface_args.iter().any(contains_infer_vars) || contains_infer_vars(self_ty),
        _ => false,
    }
}
