use crate::ast::{Item, ModItem, ModKind, Program, UseItem, Visibility};
use crate::parser::{ParseError, Parser};
use crate::source::Span;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct LoadError {
    pub(crate) message: String,
    pub(crate) span: Span,
}

impl fmt::Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for LoadError {}

impl From<ParseError> for LoadError {
    fn from(err: ParseError) -> Self {
        Self {
            message: err.message,
            span: err.span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ResolveError {
    pub(crate) message: String,
    pub(crate) span: Span,
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for ResolveError {}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct ModulePath {
    segments: Vec<String>,
}

impl ModulePath {
    pub(crate) fn root() -> Self {
        Self {
            segments: Vec::new(),
        }
    }

    pub(crate) fn child(&self, name: &str) -> Self {
        let mut segments = self.segments.clone();
        segments.push(name.to_string());
        Self { segments }
    }

    pub(crate) fn parent(&self) -> Option<Self> {
        if self.segments.is_empty() {
            None
        } else {
            let mut segments = self.segments.clone();
            segments.pop();
            Some(Self { segments })
        }
    }

    pub(crate) fn is_descendant_of(&self, ancestor: &Self) -> bool {
        if ancestor.segments.len() > self.segments.len() {
            return false;
        }
        self.segments
            .iter()
            .zip(ancestor.segments.iter())
            .all(|(a, b)| a == b)
    }

    pub(crate) fn qualify(&self, name: &str) -> String {
        if self.segments.is_empty() {
            name.to_string()
        } else {
            format!("{}::{name}", self.segments.join("::"))
        }
    }

    pub(crate) fn fqn(&self) -> String {
        self.segments.join("::")
    }

    pub(crate) fn display(&self) -> String {
        if self.segments.is_empty() {
            "crate".to_string()
        } else {
            self.segments.join("::")
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DefKind {
    Struct,
    Enum,
    Interface,
}

#[derive(Clone, Debug)]
pub(crate) struct DefDecl {
    pub(crate) vis: Visibility,
    pub(crate) defining_module: ModulePath,
}

#[derive(Clone, Debug)]
pub(crate) enum BindingTarget {
    Module(ModulePath),
    Function(String),
    Struct(String),
    Enum(String),
    Interface(String),
}

#[derive(Clone, Debug)]
pub(crate) struct Binding {
    pub(crate) vis: Visibility,
    /// The module where this binding is introduced (for privacy checking).
    pub(crate) defining_module: ModulePath,
    pub(crate) span: Span,
    pub(crate) target: BindingTarget,
}

impl Binding {
    fn is_accessible_from(&self, from: &ModulePath) -> bool {
        self.vis.is_public() || from.is_descendant_of(&self.defining_module)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ModuleScope {
    pub(crate) path: ModulePath,

    pub(crate) modules: BTreeMap<String, Binding>,
    pub(crate) types: BTreeMap<String, Binding>,
    pub(crate) values: BTreeMap<String, Binding>,

    pending_uses: Vec<UseItem>,
}

type UseBindings = Vec<(Namespace, String, Binding)>;

#[derive(Clone, Debug, Default)]
pub(crate) struct ModuleResolver {
    scopes: BTreeMap<ModulePath, ModuleScope>,
    defs: BTreeMap<String, DefDecl>,
}

impl ModuleResolver {
    pub(crate) fn build(program: &Program) -> Result<Self, ResolveError> {
        let mut resolver = Self::default();
        resolver.collect_module(&ModulePath::root(), &program.items)?;

        resolver.inject_builtin_option()?;
        resolver.inject_builtin_core()?;
        resolver.resolve_all_uses()?;
        Ok(resolver)
    }

    pub(crate) fn def(&self, fqn: &str) -> Option<&DefDecl> {
        self.defs.get(fqn)
    }

    pub(crate) fn resolve_value_fqn(
        &self,
        from: &ModulePath,
        segments: &[String],
        span: Span,
    ) -> Result<String, ResolveError> {
        let scope = self.scopes.get(from).ok_or_else(|| ResolveError {
            message: format!("internal error: unknown module `{}`", from.display()),
            span,
        })?;
        let Some(item) =
            self.try_resolve_item_path_with_scope(scope, segments, Namespace::Value, span)?
        else {
            return Err(ResolveError {
                message: format!(
                    "unknown value `{}`",
                    segments
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join("::")
                ),
                span,
            });
        };
        match item {
            ResolvedItem::Function { fqn } => Ok(fqn),
            _ => Err(ResolveError {
                message: "expected a value, found a type".to_string(),
                span,
            }),
        }
    }

    pub(crate) fn try_resolve_type_fqn(
        &self,
        from: &ModulePath,
        segments: &[String],
        span: Span,
    ) -> Result<Option<(DefKind, String)>, ResolveError> {
        let scope = self.scopes.get(from).ok_or_else(|| ResolveError {
            message: format!("internal error: unknown module `{}`", from.display()),
            span,
        })?;
        let Some(item) =
            self.try_resolve_item_path_with_scope(scope, segments, Namespace::Type, span)?
        else {
            return Ok(None);
        };
        let out = match item {
            ResolvedItem::Struct { fqn } => Some((DefKind::Struct, fqn)),
            ResolvedItem::Enum { fqn } => Some((DefKind::Enum, fqn)),
            ResolvedItem::Interface { fqn } => Some((DefKind::Interface, fqn)),
            ResolvedItem::Function { .. } => None,
        };
        Ok(out)
    }

    pub(crate) fn resolve_type_fqn(
        &self,
        from: &ModulePath,
        segments: &[String],
        span: Span,
    ) -> Result<(DefKind, String), ResolveError> {
        match self.try_resolve_type_fqn(from, segments, span)? {
            Some(out) => Ok(out),
            None => Err(ResolveError {
                message: format!(
                    "unknown type `{}`",
                    segments
                        .iter()
                        .map(|s| s.as_str())
                        .collect::<Vec<_>>()
                        .join("::")
                ),
                span,
            }),
        }
    }

    fn collect_module(&mut self, path: &ModulePath, items: &[Item]) -> Result<(), ResolveError> {
        if self.scopes.contains_key(path) {
            return Err(ResolveError {
                message: format!("duplicate module `{}`", path.display()),
                span: Span::new(0, 0),
            });
        }

        let mut scope = ModuleScope {
            path: path.clone(),
            modules: BTreeMap::new(),
            types: BTreeMap::new(),
            values: BTreeMap::new(),
            pending_uses: Vec::new(),
        };

        for item in items {
            match item {
                Item::Mod(m) => self.collect_mod_item(&mut scope, path, m)?,
                Item::Use(u) => scope.pending_uses.push(u.clone()),
                Item::Function(f) => {
                    let local_name = f.name.name.clone();
                    let full_name = path.qualify(&local_name);
                    if scope.values.contains_key(&local_name) {
                        return Err(ResolveError {
                            message: format!(
                                "duplicate function `{local_name}` in {}",
                                path.display()
                            ),
                            span: f.name.span,
                        });
                    }
                    scope.values.insert(
                        local_name,
                        Binding {
                            vis: f.vis,
                            defining_module: path.clone(),
                            span: f.name.span,
                            target: BindingTarget::Function(full_name.clone()),
                        },
                    );
                    if self
                        .defs
                        .insert(
                            full_name.clone(),
                            DefDecl {
                                vis: f.vis,
                                defining_module: path.clone(),
                            },
                        )
                        .is_some()
                    {
                        return Err(ResolveError {
                            message: format!("duplicate function `{full_name}`"),
                            span: f.name.span,
                        });
                    }
                }
                Item::Struct(s) => self.collect_type_def(
                    &mut scope,
                    path,
                    &s.name.name,
                    s.vis,
                    s.span,
                    DefKind::Struct,
                )?,
                Item::Enum(e) => self.collect_type_def(
                    &mut scope,
                    path,
                    &e.name.name,
                    e.vis,
                    e.span,
                    DefKind::Enum,
                )?,
                Item::Interface(i) => {
                    self.collect_type_def(
                        &mut scope,
                        path,
                        &i.name.name,
                        i.vis,
                        i.span,
                        DefKind::Interface,
                    )?;
                }
                Item::Impl(_) => {}
            }
        }

        self.scopes.insert(path.clone(), scope);
        Ok(())
    }

    fn collect_type_def(
        &mut self,
        scope: &mut ModuleScope,
        module_path: &ModulePath,
        local_name: &str,
        vis: Visibility,
        span: Span,
        kind: DefKind,
    ) -> Result<(), ResolveError> {
        if scope.types.contains_key(local_name) {
            return Err(ResolveError {
                message: format!("duplicate type `{local_name}` in {}", module_path.display()),
                span,
            });
        }

        let full_name = module_path.qualify(local_name);
        let target = match kind {
            DefKind::Struct => BindingTarget::Struct(full_name.clone()),
            DefKind::Enum => BindingTarget::Enum(full_name.clone()),
            DefKind::Interface => BindingTarget::Interface(full_name.clone()),
        };

        scope.types.insert(
            local_name.to_string(),
            Binding {
                vis,
                defining_module: module_path.clone(),
                span,
                target,
            },
        );

        if self
            .defs
            .insert(
                full_name.clone(),
                DefDecl {
                    vis,
                    defining_module: module_path.clone(),
                },
            )
            .is_some()
        {
            return Err(ResolveError {
                message: format!("duplicate type `{full_name}`"),
                span,
            });
        }

        Ok(())
    }

    fn collect_mod_item(
        &mut self,
        scope: &mut ModuleScope,
        module_path: &ModulePath,
        item: &ModItem,
    ) -> Result<(), ResolveError> {
        let local_name = item.name.name.clone();
        if local_name == "core" || local_name == "std" {
            return Err(ResolveError {
                message: format!("module name `{local_name}` is reserved"),
                span: item.name.span,
            });
        }
        if scope.modules.contains_key(&local_name) {
            return Err(ResolveError {
                message: format!(
                    "duplicate module `{local_name}` in {}",
                    module_path.display()
                ),
                span: item.name.span,
            });
        }

        let child_path = module_path.child(&local_name);
        let child_fqn = child_path.fqn();
        scope.modules.insert(
            local_name,
            Binding {
                vis: item.vis,
                defining_module: module_path.clone(),
                span: item.span,
                target: BindingTarget::Module(child_path.clone()),
            },
        );
        if self
            .defs
            .insert(
                child_fqn.clone(),
                DefDecl {
                    vis: item.vis,
                    defining_module: module_path.clone(),
                },
            )
            .is_some()
        {
            return Err(ResolveError {
                message: format!("duplicate module `{child_fqn}`"),
                span: item.span,
            });
        }

        match &item.kind {
            ModKind::Inline { items } => self.collect_module(&child_path, items),
            ModKind::File => Err(ResolveError {
                message: "file modules must be loaded before name resolution".to_string(),
                span: item.span,
            }),
        }
    }

    fn inject_builtin_option(&mut self) -> Result<(), ResolveError> {
        // `Option<T>` is treated as a type visible in every module (prelude-like).
        let option_def = DefDecl {
            vis: Visibility::Public {
                span: Span::new(0, 0),
            },
            defining_module: ModulePath::root(),
        };
        self.defs.insert("Option".to_string(), option_def);

        let binding = Binding {
            vis: Visibility::Public {
                span: Span::new(0, 0),
            },
            defining_module: ModulePath::root(),
            span: Span::new(0, 0),
            target: BindingTarget::Enum("Option".to_string()),
        };

        for scope in self.scopes.values_mut() {
            if scope.types.contains_key("Option") {
                return Err(ResolveError {
                    message: "cannot redefine built-in type `Option`".to_string(),
                    span: Span::new(0, 0),
                });
            }
            scope.types.insert("Option".to_string(), binding.clone());
        }
        Ok(())
    }

    fn inject_builtin_core(&mut self) -> Result<(), ResolveError> {
        // Inject a built-in `core` module, available from every module (extern-prelude-like).
        let core_path = ModulePath::root().child("core");
        if self.scopes.contains_key(&core_path) {
            return Err(ResolveError {
                message: "module path `core` is reserved".to_string(),
                span: Span::new(0, 0),
            });
        }

        self.defs.insert(
            core_path.fqn(),
            DefDecl {
                vis: Visibility::Public {
                    span: Span::new(0, 0),
                },
                defining_module: ModulePath::root(),
            },
        );

        let core_binding = Binding {
            vis: Visibility::Public {
                span: Span::new(0, 0),
            },
            defining_module: ModulePath::root(),
            span: Span::new(0, 0),
            target: BindingTarget::Module(core_path.clone()),
        };
        for scope in self.scopes.values_mut() {
            if scope.modules.contains_key("core") {
                return Err(ResolveError {
                    message: "module name `core` is reserved".to_string(),
                    span: Span::new(0, 0),
                });
            }
            scope
                .modules
                .insert("core".to_string(), core_binding.clone());
        }

        let intrinsics_path = core_path.child("intrinsics");
        let prelude_path = core_path.child("prelude");

        // Module defs for `core::intrinsics` and `core::prelude`.
        for module_path in [&intrinsics_path, &prelude_path] {
            self.defs.insert(
                module_path.fqn(),
                DefDecl {
                    vis: Visibility::Public {
                        span: Span::new(0, 0),
                    },
                    defining_module: core_path.clone(),
                },
            );
        }

        // `core` scope.
        let mut core_scope = ModuleScope {
            path: core_path.clone(),
            modules: BTreeMap::new(),
            types: BTreeMap::new(),
            values: BTreeMap::new(),
            pending_uses: Vec::new(),
        };

        // Submodules.
        let core_child_binding = |target: &ModulePath| Binding {
            vis: Visibility::Public {
                span: Span::new(0, 0),
            },
            defining_module: core_path.clone(),
            span: Span::new(0, 0),
            target: BindingTarget::Module(target.clone()),
        };
        core_scope.modules.insert(
            "intrinsics".to_string(),
            core_child_binding(&intrinsics_path),
        );
        core_scope
            .modules
            .insert("prelude".to_string(), core_child_binding(&prelude_path));

        // Re-exports from `core::intrinsics` into `core` root.
        core_scope.types.insert(
            "ArrayIter".to_string(),
            Binding {
                vis: Visibility::Public {
                    span: Span::new(0, 0),
                },
                defining_module: core_path.clone(),
                span: Span::new(0, 0),
                target: BindingTarget::Struct("core::intrinsics::ArrayIter".to_string()),
            },
        );
        for (local, target) in [
            ("panic", "core::intrinsics::panic"),
            ("into_iter", "core::intrinsics::into_iter"),
            ("next", "core::intrinsics::next"),
        ] {
            core_scope.values.insert(
                local.to_string(),
                Binding {
                    vis: Visibility::Public {
                        span: Span::new(0, 0),
                    },
                    defining_module: core_path.clone(),
                    span: Span::new(0, 0),
                    target: BindingTarget::Function(target.to_string()),
                },
            );
        }

        // `core::intrinsics` scope.
        let mut intrinsics_scope = ModuleScope {
            path: intrinsics_path.clone(),
            modules: BTreeMap::new(),
            types: BTreeMap::new(),
            values: BTreeMap::new(),
            pending_uses: Vec::new(),
        };

        self.inject_builtin_type(
            &mut intrinsics_scope,
            &intrinsics_path,
            "ArrayIter",
            DefKind::Struct,
            "core::intrinsics::ArrayIter",
        )?;

        for name in [
            // f-string helpers.
            "core::intrinsics::string_concat",
            "core::intrinsics::to_string",
            // Panic.
            "core::intrinsics::panic",
            // Boolean.
            "core::intrinsics::bool_not",
            "core::intrinsics::bool_eq",
            "core::intrinsics::bool_ne",
            // Integer arithmetic & comparisons.
            "core::intrinsics::int_add",
            "core::intrinsics::int_sub",
            "core::intrinsics::int_mul",
            "core::intrinsics::int_div",
            "core::intrinsics::int_mod",
            "core::intrinsics::int_eq",
            "core::intrinsics::int_ne",
            "core::intrinsics::int_lt",
            "core::intrinsics::int_le",
            "core::intrinsics::int_gt",
            "core::intrinsics::int_ge",
            // Float arithmetic & comparisons.
            "core::intrinsics::float_add",
            "core::intrinsics::float_sub",
            "core::intrinsics::float_mul",
            "core::intrinsics::float_div",
            "core::intrinsics::float_mod",
            "core::intrinsics::float_eq",
            "core::intrinsics::float_ne",
            "core::intrinsics::float_lt",
            "core::intrinsics::float_le",
            "core::intrinsics::float_gt",
            "core::intrinsics::float_ge",
            // Primitive equality helpers.
            "core::intrinsics::string_eq",
            "core::intrinsics::string_ne",
            "core::intrinsics::bytes_eq",
            "core::intrinsics::bytes_ne",
            "core::intrinsics::unit_eq",
            "core::intrinsics::unit_ne",
            // Iterator protocol.
            "core::intrinsics::into_iter",
            "core::intrinsics::next",
        ] {
            let local = name
                .strip_prefix("core::intrinsics::")
                .unwrap_or(name)
                .to_string();
            if intrinsics_scope.values.contains_key(&local) {
                continue;
            }
            intrinsics_scope.values.insert(
                local,
                Binding {
                    vis: Visibility::Public {
                        span: Span::new(0, 0),
                    },
                    defining_module: intrinsics_path.clone(),
                    span: Span::new(0, 0),
                    target: BindingTarget::Function(name.to_string()),
                },
            );
            self.defs.insert(
                name.to_string(),
                DefDecl {
                    vis: Visibility::Public {
                        span: Span::new(0, 0),
                    },
                    defining_module: intrinsics_path.clone(),
                },
            );
        }

        // `core::prelude` scope.
        let mut prelude_scope = ModuleScope {
            path: prelude_path.clone(),
            modules: BTreeMap::new(),
            types: BTreeMap::new(),
            values: BTreeMap::new(),
            pending_uses: Vec::new(),
        };
        prelude_scope.values.insert(
            "panic".to_string(),
            Binding {
                vis: Visibility::Public {
                    span: Span::new(0, 0),
                },
                defining_module: prelude_path.clone(),
                span: Span::new(0, 0),
                target: BindingTarget::Function("core::intrinsics::panic".to_string()),
            },
        );

        // Install built-in scopes.
        self.scopes.insert(intrinsics_path, intrinsics_scope);
        self.scopes.insert(prelude_path, prelude_scope);
        self.scopes.insert(core_path, core_scope);

        // Auto-import `core::prelude` (currently: `panic`) into every module.
        for scope in self.scopes.values_mut() {
            if scope.values.contains_key("panic") {
                continue;
            }
            scope.values.insert(
                "panic".to_string(),
                Binding {
                    vis: Visibility::Private,
                    defining_module: scope.path.clone(),
                    span: Span::new(0, 0),
                    target: BindingTarget::Function("core::intrinsics::panic".to_string()),
                },
            );
        }

        Ok(())
    }

    fn inject_builtin_type(
        &mut self,
        scope: &mut ModuleScope,
        defining_module: &ModulePath,
        local_name: &str,
        kind: DefKind,
        fqn: &str,
    ) -> Result<(), ResolveError> {
        if scope.types.contains_key(local_name) {
            return Err(ResolveError {
                message: format!("duplicate builtin type `{local_name}`"),
                span: Span::new(0, 0),
            });
        }
        let target = match kind {
            DefKind::Struct => BindingTarget::Struct(fqn.to_string()),
            DefKind::Enum => BindingTarget::Enum(fqn.to_string()),
            DefKind::Interface => BindingTarget::Interface(fqn.to_string()),
        };
        scope.types.insert(
            local_name.to_string(),
            Binding {
                vis: Visibility::Public {
                    span: Span::new(0, 0),
                },
                defining_module: defining_module.clone(),
                span: Span::new(0, 0),
                target,
            },
        );
        self.defs.insert(
            fqn.to_string(),
            DefDecl {
                vis: Visibility::Public {
                    span: Span::new(0, 0),
                },
                defining_module: defining_module.clone(),
            },
        );
        Ok(())
    }

    fn resolve_all_uses(&mut self) -> Result<(), ResolveError> {
        let module_paths: Vec<ModulePath> = self.scopes.keys().cloned().collect();

        loop {
            let mut progress = false;
            for module in &module_paths {
                let resolved = self.resolve_uses_in_module(module)?;
                progress |= resolved;
            }
            if !progress {
                break;
            }
        }

        for module in module_paths {
            if let Some(scope) = self.scopes.get(&module)
                && let Some(first) = scope.pending_uses.first()
            {
                let span = first.span;
                return Err(ResolveError {
                    message: format!(
                        "unresolved `use` in {}: `{}`",
                        module.display(),
                        first
                            .path
                            .segments
                            .iter()
                            .map(|s| s.name.as_str())
                            .collect::<Vec<_>>()
                            .join("::")
                    ),
                    span,
                });
            }
        }

        Ok(())
    }

    fn resolve_uses_in_module(&mut self, module: &ModulePath) -> Result<bool, ResolveError> {
        let Some(mut scope) = self.scopes.remove(module) else {
            return Ok(false);
        };

        if scope.pending_uses.is_empty() {
            self.scopes.insert(module.clone(), scope);
            return Ok(false);
        }

        let mut progress = false;
        let mut remaining = Vec::new();

        let pending_uses = std::mem::take(&mut scope.pending_uses);
        for use_item in pending_uses {
            match self.try_resolve_use(&scope, &use_item)? {
                Some(bindings) => {
                    for (ns, name, binding) in bindings {
                        match ns {
                            Namespace::Module => {
                                if scope.modules.contains_key(&name) {
                                    return Err(ResolveError {
                                        message: format!(
                                            "duplicate name `{name}` in module namespace of {}",
                                            scope.path.display()
                                        ),
                                        span: use_item.span,
                                    });
                                }
                                scope.modules.insert(name, binding);
                            }
                            Namespace::Type => {
                                if scope.types.contains_key(&name) {
                                    return Err(ResolveError {
                                        message: format!(
                                            "duplicate name `{name}` in type namespace of {}",
                                            scope.path.display()
                                        ),
                                        span: use_item.span,
                                    });
                                }
                                scope.types.insert(name, binding);
                            }
                            Namespace::Value => {
                                if scope.values.contains_key(&name) {
                                    return Err(ResolveError {
                                        message: format!(
                                            "duplicate name `{name}` in value namespace of {}",
                                            scope.path.display()
                                        ),
                                        span: use_item.span,
                                    });
                                }
                                scope.values.insert(name, binding);
                            }
                        }
                    }
                    progress = true;
                }
                None => remaining.push(use_item),
            }
        }

        scope.pending_uses = remaining;
        self.scopes.insert(module.clone(), scope);
        Ok(progress)
    }

    fn try_resolve_use(
        &self,
        scope: &ModuleScope,
        use_item: &UseItem,
    ) -> Result<Option<UseBindings>, ResolveError> {
        if use_item.path.segments.is_empty() {
            return Err(ResolveError {
                message: "empty `use` path".to_string(),
                span: use_item.span,
            });
        }

        let alias = use_item
            .alias
            .as_ref()
            .map(|i| i.name.as_str())
            .unwrap_or_else(|| {
                use_item
                    .path
                    .segments
                    .last()
                    .expect("non-empty")
                    .name
                    .as_str()
            })
            .to_string();

        let segments: Vec<String> = use_item
            .path
            .segments
            .iter()
            .map(|s| s.name.clone())
            .collect();

        let mut out: UseBindings = Vec::new();
        if let Some(target_module) =
            self.try_resolve_module_path_with_scope(scope, &segments, use_item.span)?
        {
            if use_item.vis.is_public() {
                let fqn = target_module.fqn();
                if let Some(def) = self.defs.get(&fqn)
                    && !def.vis.is_public()
                {
                    return Err(ResolveError {
                        message: format!(
                            "cannot re-export private module `{}`",
                            target_module.display()
                        ),
                        span: use_item.span,
                    });
                }
            }
            out.push((
                Namespace::Module,
                alias.clone(),
                Binding {
                    vis: use_item.vis,
                    defining_module: scope.path.clone(),
                    span: use_item.span,
                    target: BindingTarget::Module(target_module),
                },
            ));
        }

        if let Some(target) =
            self.try_resolve_item_path_with_scope(scope, &segments, Namespace::Type, use_item.span)?
        {
            if use_item.vis.is_public() && !self.is_target_public(&target)? {
                return Err(ResolveError {
                    message: format!("cannot re-export private type `{}`", target.display_name()),
                    span: use_item.span,
                });
            }
            out.push((
                Namespace::Type,
                alias.clone(),
                Binding {
                    vis: use_item.vis,
                    defining_module: scope.path.clone(),
                    span: use_item.span,
                    target: target.into_binding_target(),
                },
            ));
        }

        if let Some(target) = self.try_resolve_item_path_with_scope(
            scope,
            &segments,
            Namespace::Value,
            use_item.span,
        )? {
            if use_item.vis.is_public() && !self.is_target_public(&target)? {
                return Err(ResolveError {
                    message: format!("cannot re-export private value `{}`", target.display_name()),
                    span: use_item.span,
                });
            }
            out.push((
                Namespace::Value,
                alias.clone(),
                Binding {
                    vis: use_item.vis,
                    defining_module: scope.path.clone(),
                    span: use_item.span,
                    target: target.into_binding_target(),
                },
            ));
        }

        if out.is_empty() {
            return Ok(None);
        }

        Ok(Some(out))
    }

    fn is_target_public(&self, target: &ResolvedItem) -> Result<bool, ResolveError> {
        match target {
            ResolvedItem::Struct { fqn }
            | ResolvedItem::Enum { fqn }
            | ResolvedItem::Interface { fqn }
            | ResolvedItem::Function { fqn } => {
                Ok(self.defs.get(fqn).is_some_and(|d| d.vis.is_public()))
            }
        }
    }

    fn try_resolve_module_path_with_scope(
        &self,
        current: &ModuleScope,
        segments: &[String],
        span: Span,
    ) -> Result<Option<ModulePath>, ResolveError> {
        let (mut module, mut idx) = self.resolve_base_module(current, segments, span)?;
        while idx < segments.len() {
            let name = segments[idx].as_str();
            let scope = self
                .lookup_scope(current, &module)
                .ok_or_else(|| ResolveError {
                    message: format!("unknown module `{}`", module.display()),
                    span,
                })?;
            let Some(binding) = scope.modules.get(name) else {
                return Ok(None);
            };
            if !binding.is_accessible_from(&current.path) {
                return Err(ResolveError {
                    message: format!("module `{name}` is private"),
                    span: binding.span,
                });
            }
            let BindingTarget::Module(next) = &binding.target else {
                return Err(ResolveError {
                    message: format!("`{name}` is not a module"),
                    span: binding.span,
                });
            };
            module = next.clone();
            idx += 1;
        }
        Ok(Some(module))
    }

    fn try_resolve_item_path_with_scope(
        &self,
        current: &ModuleScope,
        segments: &[String],
        ns: Namespace,
        span: Span,
    ) -> Result<Option<ResolvedItem>, ResolveError> {
        if segments.is_empty() {
            return Err(ResolveError {
                message: "empty path".to_string(),
                span,
            });
        }

        let (mut module, mut idx) = self.resolve_base_module(current, segments, span)?;
        while idx + 1 < segments.len() {
            let name = segments[idx].as_str();
            let scope = self
                .lookup_scope(current, &module)
                .ok_or_else(|| ResolveError {
                    message: format!("unknown module `{}`", module.display()),
                    span,
                })?;
            let Some(binding) = scope.modules.get(name) else {
                return Ok(None);
            };
            if !binding.is_accessible_from(&current.path) {
                return Err(ResolveError {
                    message: format!("module `{name}` is private"),
                    span: binding.span,
                });
            }
            let BindingTarget::Module(next) = &binding.target else {
                return Err(ResolveError {
                    message: format!("`{name}` is not a module"),
                    span: binding.span,
                });
            };
            module = next.clone();
            idx += 1;
        }

        let last = segments.last().expect("non-empty").as_str();
        let scope = self
            .lookup_scope(current, &module)
            .ok_or_else(|| ResolveError {
                message: format!("unknown module `{}`", module.display()),
                span,
            })?;

        let binding = match ns {
            Namespace::Type => scope.types.get(last),
            Namespace::Value => scope.values.get(last),
            Namespace::Module => scope.modules.get(last),
        };
        let Some(binding) = binding else {
            return Ok(None);
        };

        if !binding.is_accessible_from(&current.path) {
            return Err(ResolveError {
                message: format!("`{last}` is private"),
                span: binding.span,
            });
        }

        match ResolvedItem::from_binding_target(&binding.target, span) {
            Ok(item) => Ok(Some(item)),
            Err(_) => Ok(None),
        }
    }

    fn resolve_base_module(
        &self,
        current: &ModuleScope,
        segments: &[String],
        span: Span,
    ) -> Result<(ModulePath, usize), ResolveError> {
        if segments.is_empty() {
            return Ok((current.path.clone(), 0));
        }

        let head = segments[0].as_str();
        if head == "crate" {
            return Ok((ModulePath::root(), 1));
        }
        if head == "self" {
            return Ok((current.path.clone(), 1));
        }
        if head == "super" {
            let mut base = current.path.parent().ok_or_else(|| ResolveError {
                message: "`super` at crate root".to_string(),
                span,
            })?;
            let mut idx = 1;
            while idx < segments.len() && segments[idx].as_str() == "super" {
                base = base.parent().ok_or_else(|| ResolveError {
                    message: "`super` at crate root".to_string(),
                    span,
                })?;
                idx += 1;
            }
            return Ok((base, idx));
        }

        Ok((current.path.clone(), 0))
    }

    fn lookup_scope<'a>(
        &'a self,
        current: &'a ModuleScope,
        module: &ModulePath,
    ) -> Option<&'a ModuleScope> {
        if module == &current.path {
            Some(current)
        } else {
            self.scopes.get(module)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Namespace {
    Module,
    Type,
    Value,
}

#[derive(Clone, Debug)]
enum ResolvedItem {
    Struct { fqn: String },
    Enum { fqn: String },
    Interface { fqn: String },
    Function { fqn: String },
}

impl ResolvedItem {
    fn from_binding_target(target: &BindingTarget, span: Span) -> Result<Self, ResolveError> {
        match target {
            BindingTarget::Struct(fqn) => Ok(Self::Struct { fqn: fqn.clone() }),
            BindingTarget::Enum(fqn) => Ok(Self::Enum { fqn: fqn.clone() }),
            BindingTarget::Interface(fqn) => Ok(Self::Interface { fqn: fqn.clone() }),
            BindingTarget::Function(fqn) => Ok(Self::Function { fqn: fqn.clone() }),
            BindingTarget::Module(_) => Err(ResolveError {
                message: "expected an item, found a module".to_string(),
                span,
            }),
        }
    }

    fn into_binding_target(self) -> BindingTarget {
        match self {
            ResolvedItem::Struct { fqn } => BindingTarget::Struct(fqn),
            ResolvedItem::Enum { fqn } => BindingTarget::Enum(fqn),
            ResolvedItem::Interface { fqn } => BindingTarget::Interface(fqn),
            ResolvedItem::Function { fqn } => BindingTarget::Function(fqn),
        }
    }

    fn display_name(&self) -> &str {
        match self {
            ResolvedItem::Struct { fqn }
            | ResolvedItem::Enum { fqn }
            | ResolvedItem::Interface { fqn }
            | ResolvedItem::Function { fqn } => fqn.as_str(),
        }
    }
}

pub(crate) struct ModuleLoader {
    next_base_offset: usize,
    loaded_files: BTreeSet<PathBuf>,
}

impl ModuleLoader {
    pub(crate) fn new() -> Self {
        Self {
            next_base_offset: 0,
            loaded_files: BTreeSet::new(),
        }
    }

    pub(crate) fn load_program_from_file(
        &mut self,
        entry_path: &Path,
    ) -> Result<Program, LoadError> {
        let source = fs::read_to_string(entry_path).map_err(|e| LoadError {
            message: format!("failed to read `{}`: {e}", entry_path.display()),
            span: Span::new(0, 0),
        })?;

        let entry_path = entry_path
            .canonicalize()
            .unwrap_or_else(|_| entry_path.to_path_buf());
        self.loaded_files.insert(entry_path.clone());

        let base_offset = self.alloc_base_offset(&source);
        let mut parser = Parser::with_base_offset(&source, base_offset)?;
        let mut program = parser.parse_program()?;

        let module_dir = entry_path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));
        program.items = self.inline_modules(program.items, &module_dir)?;
        Ok(program)
    }

    fn alloc_base_offset(&mut self, src: &str) -> usize {
        let base = self.next_base_offset;
        self.next_base_offset = self.next_base_offset.saturating_add(src.len() + 1);
        base
    }

    fn inline_modules(
        &mut self,
        items: Vec<Item>,
        module_dir: &Path,
    ) -> Result<Vec<Item>, LoadError> {
        let mut out = Vec::with_capacity(items.len());
        for item in items {
            match item {
                Item::Mod(m) => out.push(Item::Mod(self.inline_mod_item(m, module_dir)?)),
                other => out.push(other),
            }
        }
        Ok(out)
    }

    fn inline_mod_item(&mut self, item: ModItem, module_dir: &Path) -> Result<ModItem, LoadError> {
        let name = item.name.name.clone();
        if name == "core" || name == "std" {
            return Err(LoadError {
                message: format!("module name `{name}` is reserved"),
                span: item.name.span,
            });
        }
        let child_dir = module_dir.join(&name);

        match item.kind {
            ModKind::Inline { items } => {
                let items = self.inline_modules(items, &child_dir)?;
                Ok(ModItem {
                    kind: ModKind::Inline { items },
                    ..item
                })
            }
            ModKind::File => {
                let file_path = module_dir.join(format!("{name}.rusk"));
                let dir_path = child_dir.join("mod.rusk");

                let file_exists = file_path.exists();
                let dir_exists = dir_path.exists();
                if file_exists && dir_exists {
                    return Err(LoadError {
                        message: format!(
                            "ambiguous module `{name}`: both `{}` and `{}` exist",
                            file_path.display(),
                            dir_path.display()
                        ),
                        span: item.span,
                    });
                }

                let path = if file_exists {
                    file_path
                } else if dir_exists {
                    dir_path
                } else {
                    return Err(LoadError {
                        message: format!(
                            "module `{name}` not found (expected `{}` or `{}`)",
                            file_path.display(),
                            dir_path.display()
                        ),
                        span: item.span,
                    });
                };

                let canon = path.canonicalize().unwrap_or_else(|_| path.clone());
                if !self.loaded_files.insert(canon.clone()) {
                    return Err(LoadError {
                        message: format!("module file `{}` loaded multiple times", canon.display()),
                        span: item.span,
                    });
                }

                let source = fs::read_to_string(&canon).map_err(|e| LoadError {
                    message: format!("failed to read `{}`: {e}", canon.display()),
                    span: item.span,
                })?;
                let base_offset = self.alloc_base_offset(&source);
                let mut parser = Parser::with_base_offset(&source, base_offset)?;
                let mut program = parser.parse_program()?;
                program.items = self.inline_modules(program.items, &child_dir)?;

                Ok(ModItem {
                    kind: ModKind::Inline {
                        items: program.items,
                    },
                    ..item
                })
            }
        }
    }
}
