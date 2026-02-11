use crate::ast::{
    BinaryOp, BindingKind, Block, EnumItem, Expr, FieldName, FnItem, GenericParam, Ident,
    ImplHeader, ImplItem, InterfaceItem, Item, MatchArm, MatchPat, PatLiteral, Pattern, PrimType,
    Program, StructItem, TypeExpr, UnaryOp, Visibility,
};
use crate::modules::{ModulePath, ModuleResolver};
use crate::source::Span;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TypeError {
    pub(crate) message: String,
    pub(crate) span: Span,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for TypeError {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum Ty {
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
    Array(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn {
        params: Vec<Ty>,
        ret: Box<Ty>,
    },
    Cont {
        param: Box<Ty>,
        ret: Box<Ty>,
    },
    Readonly(Box<Ty>),
    App(TyCon, Vec<Ty>),

    /// Rigid reference to a generic parameter of arity 0.
    Gen(GenId),
    /// Inference type variable (kind `Type`).
    Var(TypeVarId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum TyCon {
    Named(String),
    /// Rigid reference to a generic parameter of arity > 0.
    Gen(GenId),
    /// Inference variable for a type constructor (kind `Type^n -> Type`).
    Var(ConVarId),
}

pub(crate) type GenId = usize;
pub(crate) type TypeVarId = u32;
pub(crate) type ConVarId = u32;

impl Ty {
    pub(crate) fn is_ref_like(&self) -> bool {
        matches!(
            self,
            Ty::Array(_) | Ty::Tuple(_) | Ty::App(..) | Ty::Gen(_) | Ty::Var(_) | Ty::Readonly(_)
        )
    }

    pub(crate) fn as_readonly_view(&self) -> Ty {
        match self {
            Ty::Readonly(_) => self.clone(),
            _ if self.is_ref_like() => Ty::Readonly(Box::new(self.clone())),
            _ => self.clone(),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Unit => write!(f, "unit"),
            Ty::Bool => write!(f, "bool"),
            Ty::Int => write!(f, "int"),
            Ty::Float => write!(f, "float"),
            Ty::String => write!(f, "string"),
            Ty::Bytes => write!(f, "bytes"),
            Ty::Array(elem) => write!(f, "[{elem}]"),
            Ty::Tuple(items) => {
                write!(f, "(")?;
                for (idx, item) in items.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                if items.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            Ty::Fn { params, ret } => {
                write!(f, "fn(")?;
                for (idx, p) in params.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{p}")?;
                }
                write!(f, ") -> {ret}")
            }
            Ty::Cont { param, ret } => write!(f, "cont({param}) -> {ret}"),
            Ty::Readonly(inner) => write!(f, "readonly {inner}"),
            Ty::App(con, args) => {
                write!(f, "{con}")?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    for (idx, a) in args.iter().enumerate() {
                        if idx != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{a}")?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Ty::Gen(id) => write!(f, "<gen#{id}>"),
            Ty::Var(id) => write!(f, "<t{id}>"),
        }
    }
}

impl fmt::Display for TyCon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyCon::Named(name) => write!(f, "{name}"),
            TyCon::Gen(id) => write!(f, "<gen#{id}>"),
            TyCon::Var(id) => write!(f, "<F{id}>"),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct GenericParamInfo {
    pub(crate) name: String,
    pub(crate) arity: usize,
    /// Optional interface constraint name (currently must be a simple path with no args).
    pub(crate) constraint: Option<String>,
    pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub(crate) struct StructDef {
    pub(crate) name: String,
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) fields: Vec<(String, Ty)>,
}

#[derive(Clone, Debug)]
pub(crate) struct EnumDef {
    pub(crate) name: String,
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) variants: BTreeMap<String, Vec<Ty>>,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceMethodSig {
    pub(crate) params: Vec<Ty>,
    pub(crate) ret: Ty,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceMethod {
    pub(crate) origin: String,
    pub(crate) sig: InterfaceMethodSig,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceDef {
    pub(crate) name: String,
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) supers: Vec<String>,
    /// Methods declared directly in this interface (not including inherited ones).
    pub(crate) methods: BTreeMap<String, InterfaceMethodSig>,
    /// Full method set including inherited ones, with canonical origin interface IDs.
    pub(crate) all_methods: BTreeMap<String, InterfaceMethod>,
    pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub(crate) struct FnSig {
    pub(crate) name: String,
    pub(crate) vis: Visibility,
    pub(crate) defining_module: ModulePath,
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) params: Vec<Ty>,
    pub(crate) ret: Ty,
    pub(crate) span: Span,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct ProgramEnv {
    pub(crate) modules: ModuleResolver,
    pub(crate) structs: BTreeMap<String, StructDef>,
    pub(crate) enums: BTreeMap<String, EnumDef>,
    pub(crate) interfaces: BTreeMap<String, InterfaceDef>,

    /// All callable functions by resolved name.
    ///
    /// Keys include:
    /// - top-level functions: `main`
    /// - inherent methods: `Type::method`
    /// - core intrinsics: `core::intrinsics::int_add`
    pub(crate) functions: BTreeMap<String, FnSig>,

    /// Interface impl method table keyed by canonical interface method id:
    ///
    /// `(dynamic_type_name, origin_interface_name, method_name) -> function_name`.
    ///
    /// `function_name` must exist in `functions`.
    pub(crate) interface_methods: BTreeMap<(String, String, String), String>,

    /// Nominal interface implementation table:
    ///
    /// `(dynamic_type_name, interface_name)` is present iff the type implements the interface.
    ///
    /// Entries include transitive super-interfaces, so if `J: I` and `T` implements `J`, then
    /// `(T, I)` is also present.
    pub(crate) interface_impls: BTreeSet<(String, String)>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct TypeInfo {
    pub(crate) expr_types: HashMap<Span, Ty>,
}

pub(crate) fn build_env(program: &Program) -> Result<ProgramEnv, TypeError> {
    let modules = ModuleResolver::build(program).map_err(|e| TypeError {
        message: e.message,
        span: e.span,
    })?;

    let mut env = ProgramEnv {
        modules,
        ..Default::default()
    };
    add_prelude(&mut env);

    // First pass: declare nominal types.
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Struct(s) => declare_struct(&mut env, module, s),
            Item::Enum(e) => declare_enum(&mut env, module, e),
            Item::Interface(i) => declare_interface(&mut env, module, i),
            _ => Ok(()),
        },
    )?;

    // Second pass: declare function signatures.
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Function(f) => {
                let full_name = module.qualify(&f.name.name);
                declare_function_sig(&mut env, module, f, Some(full_name))
            }
            _ => Ok(()),
        },
    )?;

    // Third pass: fill type members (fields/variants/methods).
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Struct(s) => fill_struct(&mut env, module, s),
            Item::Enum(e) => fill_enum(&mut env, module, e),
            Item::Interface(i) => fill_interface(&mut env, module, i),
            _ => Ok(()),
        },
    )?;

    // Interface inheritance pass: validate graph + compute full method sets.
    compute_interface_inheritance(&mut env)?;

    // Fourth pass: process impl items (declare method signatures + interface method table).
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Impl(imp) => process_impl_item(&mut env, module, imp),
            _ => Ok(()),
        },
    )?;

    Ok(env)
}

fn walk_module_items(
    items: &[Item],
    module: &ModulePath,
    visit: &mut impl FnMut(&ModulePath, &Item) -> Result<(), TypeError>,
) -> Result<(), TypeError> {
    for item in items {
        visit(module, item)?;
        if let Item::Mod(m) = item {
            let child = module.child(&m.name.name);
            match &m.kind {
                crate::ast::ModKind::Inline { items } => {
                    walk_module_items(items, &child, visit)?;
                }
                crate::ast::ModKind::File => {
                    return Err(TypeError {
                        message: "file modules must be loaded before typechecking".to_string(),
                        span: m.span,
                    });
                }
            }
        }
    }
    Ok(())
}

fn add_prelude(env: &mut ProgramEnv) {
    // Built-in enum Option<T> { Some(T), None }
    env.enums.insert(
        "Option".to_string(),
        EnumDef {
            name: "Option".to_string(),
            generics: vec![GenericParamInfo {
                name: "T".to_string(),
                arity: 0,
                constraint: None,
                span: Span::new(0, 0),
            }],
            variants: BTreeMap::from([
                ("Some".to_string(), vec![Ty::Gen(0)]),
                ("None".to_string(), vec![]),
            ]),
        },
    );

    // Built-in iterator struct used by `for` desugaring.
    env.structs.insert(
        "core::intrinsics::ArrayIter".to_string(),
        StructDef {
            name: "core::intrinsics::ArrayIter".to_string(),
            generics: vec![GenericParamInfo {
                name: "T".to_string(),
                arity: 0,
                constraint: None,
                span: Span::new(0, 0),
            }],
            fields: vec![
                ("arr".to_string(), Ty::Array(Box::new(Ty::Gen(0)))),
                ("idx".to_string(), Ty::Int),
            ],
        },
    );

    let mut add_fn = |name: &str, generics: Vec<GenericParamInfo>, params: Vec<Ty>, ret: Ty| {
        env.functions.insert(
            name.to_string(),
            FnSig {
                name: name.to_string(),
                vis: Visibility::Public {
                    span: Span::new(0, 0),
                },
                defining_module: ModulePath::root(),
                generics,
                params,
                ret,
                span: Span::new(0, 0),
            },
        );
    };

    // f"..." desugaring helpers.
    add_fn(
        "core::intrinsics::string_concat",
        Vec::new(),
        vec![Ty::String, Ty::String],
        Ty::String,
    );
    add_fn(
        "core::intrinsics::to_string",
        vec![GenericParamInfo {
            name: "T".to_string(),
            arity: 0,
            constraint: None,
            span: Span::new(0, 0),
        }],
        vec![Ty::Gen(0)],
        Ty::String,
    );

    // Panic (diverging) intrinsic.
    add_fn(
        "core::intrinsics::panic",
        vec![GenericParamInfo {
            name: "T".to_string(),
            arity: 0,
            constraint: None,
            span: Span::new(0, 0),
        }],
        vec![Ty::String],
        Ty::Gen(0),
    );

    // Boolean.
    add_fn(
        "core::intrinsics::bool_not",
        Vec::new(),
        vec![Ty::Bool],
        Ty::Bool,
    );
    add_fn(
        "core::intrinsics::bool_eq",
        Vec::new(),
        vec![Ty::Bool, Ty::Bool],
        Ty::Bool,
    );
    add_fn(
        "core::intrinsics::bool_ne",
        Vec::new(),
        vec![Ty::Bool, Ty::Bool],
        Ty::Bool,
    );

    // Integer arithmetic & comparisons.
    for (name, ret) in [
        ("core::intrinsics::int_add", Ty::Int),
        ("core::intrinsics::int_sub", Ty::Int),
        ("core::intrinsics::int_mul", Ty::Int),
        ("core::intrinsics::int_div", Ty::Int),
        ("core::intrinsics::int_mod", Ty::Int),
        ("core::intrinsics::int_eq", Ty::Bool),
        ("core::intrinsics::int_ne", Ty::Bool),
        ("core::intrinsics::int_lt", Ty::Bool),
        ("core::intrinsics::int_le", Ty::Bool),
        ("core::intrinsics::int_gt", Ty::Bool),
        ("core::intrinsics::int_ge", Ty::Bool),
    ] {
        add_fn(name, Vec::new(), vec![Ty::Int, Ty::Int], ret);
    }

    // Float arithmetic & comparisons.
    for (name, ret) in [
        ("core::intrinsics::float_add", Ty::Float),
        ("core::intrinsics::float_sub", Ty::Float),
        ("core::intrinsics::float_mul", Ty::Float),
        ("core::intrinsics::float_div", Ty::Float),
        ("core::intrinsics::float_mod", Ty::Float),
        ("core::intrinsics::float_eq", Ty::Bool),
        ("core::intrinsics::float_ne", Ty::Bool),
        ("core::intrinsics::float_lt", Ty::Bool),
        ("core::intrinsics::float_le", Ty::Bool),
        ("core::intrinsics::float_gt", Ty::Bool),
        ("core::intrinsics::float_ge", Ty::Bool),
    ] {
        add_fn(name, Vec::new(), vec![Ty::Float, Ty::Float], ret);
    }

    // Primitive equality helpers.
    for (name, ty) in [
        ("core::intrinsics::string_eq", Ty::String),
        ("core::intrinsics::string_ne", Ty::String),
        ("core::intrinsics::bytes_eq", Ty::Bytes),
        ("core::intrinsics::bytes_ne", Ty::Bytes),
        ("core::intrinsics::unit_eq", Ty::Unit),
        ("core::intrinsics::unit_ne", Ty::Unit),
    ] {
        add_fn(name, Vec::new(), vec![ty.clone(), ty.clone()], Ty::Bool);
    }

    // Iterator protocol (currently for dynamic arrays only).
    let iter_generics = vec![GenericParamInfo {
        name: "T".to_string(),
        arity: 0,
        constraint: None,
        span: Span::new(0, 0),
    }];
    add_fn(
        "core::intrinsics::into_iter",
        iter_generics.clone(),
        vec![Ty::Array(Box::new(Ty::Gen(0)))],
        Ty::App(
            TyCon::Named("core::intrinsics::ArrayIter".to_string()),
            vec![Ty::Gen(0)],
        ),
    );
    add_fn(
        "core::intrinsics::next",
        iter_generics.clone(),
        vec![Ty::App(
            TyCon::Named("core::intrinsics::ArrayIter".to_string()),
            vec![Ty::Gen(0)],
        )],
        Ty::App(TyCon::Named("Option".to_string()), vec![Ty::Gen(0)]),
    );
}

fn declare_struct(
    env: &mut ProgramEnv,
    module: &ModulePath,
    item: &StructItem,
) -> Result<(), TypeError> {
    let name = module.qualify(&item.name.name);
    if env.structs.contains_key(&name)
        || env.enums.contains_key(&name)
        || env.interfaces.contains_key(&name)
    {
        return Err(TypeError {
            message: format!("duplicate type name `{name}`"),
            span: item.name.span,
        });
    }
    if item.name.name == "Option" {
        return Err(TypeError {
            message: "cannot redefine built-in type `Option`".to_string(),
            span: item.name.span,
        });
    }
    let generics = lower_generic_params(env, module, &item.generics)?;
    if generics.iter().any(|g| g.arity != 0) {
        return Err(TypeError {
            message: "higher-kinded generics on structs are not supported in v0.4".to_string(),
            span: item.name.span,
        });
    }
    env.structs.insert(
        name.clone(),
        StructDef {
            name,
            generics,
            fields: Vec::new(),
        },
    );
    Ok(())
}

fn declare_enum(
    env: &mut ProgramEnv,
    module: &ModulePath,
    item: &EnumItem,
) -> Result<(), TypeError> {
    let name = module.qualify(&item.name.name);
    if env.structs.contains_key(&name)
        || env.enums.contains_key(&name)
        || env.interfaces.contains_key(&name)
    {
        return Err(TypeError {
            message: format!("duplicate type name `{name}`"),
            span: item.name.span,
        });
    }
    if item.name.name == "Option" {
        return Err(TypeError {
            message: "cannot redefine built-in type `Option`".to_string(),
            span: item.name.span,
        });
    }
    let generics = lower_generic_params(env, module, &item.generics)?;
    if generics.iter().any(|g| g.arity != 0) {
        return Err(TypeError {
            message: "higher-kinded generics on enums are not supported in v0.4".to_string(),
            span: item.name.span,
        });
    }
    env.enums.insert(
        name.clone(),
        EnumDef {
            name,
            generics,
            variants: BTreeMap::new(),
        },
    );
    Ok(())
}

fn declare_interface(
    env: &mut ProgramEnv,
    module: &ModulePath,
    item: &InterfaceItem,
) -> Result<(), TypeError> {
    let name = module.qualify(&item.name.name);
    if env.structs.contains_key(&name)
        || env.enums.contains_key(&name)
        || env.interfaces.contains_key(&name)
    {
        return Err(TypeError {
            message: format!("duplicate type name `{name}`"),
            span: item.name.span,
        });
    }
    env.interfaces.insert(
        name.clone(),
        InterfaceDef {
            name,
            generics: lower_generic_params(env, module, &item.generics)?,
            supers: Vec::new(),
            methods: BTreeMap::new(),
            all_methods: BTreeMap::new(),
            span: item.span,
        },
    );
    Ok(())
}

fn declare_function_sig(
    env: &mut ProgramEnv,
    module: &ModulePath,
    func: &FnItem,
    name_override: Option<String>,
) -> Result<(), TypeError> {
    let name = name_override.unwrap_or_else(|| func.name.name.clone());
    if env.functions.contains_key(&name) {
        return Err(TypeError {
            message: format!("duplicate function `{name}`"),
            span: func.name.span,
        });
    }

    let generics = lower_generic_params(env, module, &func.generics)?;
    let scope = GenericScope::new(&generics)?;
    let params = func
        .params
        .iter()
        .map(|p| lower_type_expr(env, module, &scope, &p.ty))
        .collect::<Result<Vec<_>, _>>()?;
    let ret = lower_type_expr(env, module, &scope, &func.ret)?;

    env.functions.insert(
        name.clone(),
        FnSig {
            name,
            vis: func.vis,
            defining_module: module.clone(),
            generics,
            params,
            ret,
            span: func.span,
        },
    );
    Ok(())
}

fn fill_struct(
    env: &mut ProgramEnv,
    module: &ModulePath,
    item: &StructItem,
) -> Result<(), TypeError> {
    let full_name = module.qualify(&item.name.name);
    let generics = env
        .structs
        .get(&full_name)
        .expect("declared in first pass")
        .generics
        .clone();
    let struct_name = full_name.clone();
    let scope = GenericScope::new(&generics)?;
    let mut seen = BTreeSet::new();
    let mut fields = Vec::with_capacity(item.fields.len());
    for field in &item.fields {
        let field_name = field.name.name.clone();
        if !seen.insert(field_name.clone()) {
            return Err(TypeError {
                message: format!(
                    "duplicate field `{}` in struct `{}`",
                    field_name, struct_name
                ),
                span: field.name.span,
            });
        }
        let ty = lower_type_expr(env, module, &scope, &field.ty)?;
        fields.push((field_name, ty));
    }
    env.structs
        .get_mut(&full_name)
        .expect("declared in first pass")
        .fields = fields;
    Ok(())
}

fn fill_enum(env: &mut ProgramEnv, module: &ModulePath, item: &EnumItem) -> Result<(), TypeError> {
    let full_name = module.qualify(&item.name.name);
    let generics = env
        .enums
        .get(&full_name)
        .expect("declared in first pass")
        .generics
        .clone();
    let enum_name = full_name.clone();
    let scope = GenericScope::new(&generics)?;

    let mut variants = BTreeMap::new();
    for variant in &item.variants {
        let vname = variant.name.name.clone();
        if variants.contains_key(&vname) {
            return Err(TypeError {
                message: format!("duplicate variant `{vname}` in enum `{enum_name}`"),
                span: variant.name.span,
            });
        }
        let fields = variant
            .fields
            .iter()
            .map(|t| lower_type_expr(env, module, &scope, t))
            .collect::<Result<Vec<_>, _>>()?;
        variants.insert(vname, fields);
    }
    env.enums
        .get_mut(&full_name)
        .expect("declared in first pass")
        .variants = variants;
    Ok(())
}

fn fill_interface(
    env: &mut ProgramEnv,
    module: &ModulePath,
    item: &InterfaceItem,
) -> Result<(), TypeError> {
    let full_name = module.qualify(&item.name.name);
    let generics = env
        .interfaces
        .get(&full_name)
        .expect("declared in first pass")
        .generics
        .clone();

    // v0.4 restriction: interfaces used for effects must be monomorphic.
    if !generics.is_empty() {
        return Err(TypeError {
            message: "interfaces cannot be generic in v0.4".to_string(),
            span: item.name.span,
        });
    }

    // Resolve and validate super-interfaces.
    let mut supers = Vec::new();
    let mut seen = BTreeSet::new();
    for sup in &item.supers {
        if !sup.segments.iter().all(|seg| seg.args.is_empty()) {
            return Err(TypeError {
                message: "super-interface references cannot have type arguments in v0.4"
                    .to_string(),
                span: sup.span,
            });
        }
        let segments: Vec<String> = sup
            .segments
            .iter()
            .map(|seg| seg.name.name.clone())
            .collect();
        let (kind, fqn) = env
            .modules
            .resolve_type_fqn(module, &segments, sup.span)
            .map_err(|e| TypeError {
                message: e.message,
                span: e.span,
            })?;
        if kind != crate::modules::DefKind::Interface {
            return Err(TypeError {
                message: format!("super-interface must be an interface, got `{fqn}`"),
                span: sup.span,
            });
        }
        if seen.insert(fqn.clone()) {
            supers.push(fqn);
        }
    }

    let scope = GenericScope::new(&generics)?;
    let mut methods = BTreeMap::new();
    for member in &item.members {
        if !member.generics.is_empty() {
            return Err(TypeError {
                message: "interface methods cannot be generic in v0.4".to_string(),
                span: member.name.span,
            });
        }
        let mname = member.name.name.clone();
        if methods.contains_key(&mname) {
            return Err(TypeError {
                message: format!("duplicate interface method `{mname}`"),
                span: member.name.span,
            });
        }
        let params = member
            .params
            .iter()
            .map(|p| lower_type_expr(env, module, &scope, &p.ty))
            .collect::<Result<Vec<_>, _>>()?;
        let ret = lower_type_expr(env, module, &scope, &member.ret)?;
        methods.insert(mname.clone(), InterfaceMethodSig { params, ret });
    }
    env.interfaces
        .get_mut(&full_name)
        .expect("declared in first pass")
        .methods = methods;
    env.interfaces
        .get_mut(&full_name)
        .expect("declared in first pass")
        .supers = supers;
    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InterfaceVisitState {
    Visiting,
    Done,
}

fn compute_interface_inheritance(env: &mut ProgramEnv) -> Result<(), TypeError> {
    let names: Vec<String> = env.interfaces.keys().cloned().collect();
    let mut state: HashMap<String, InterfaceVisitState> = HashMap::new();
    let mut stack: Vec<String> = Vec::new();

    for name in names {
        compute_interface_all_methods(env, &name, &mut state, &mut stack)?;
    }
    Ok(())
}

fn compute_interface_all_methods(
    env: &mut ProgramEnv,
    iface: &str,
    state: &mut HashMap<String, InterfaceVisitState>,
    stack: &mut Vec<String>,
) -> Result<(), TypeError> {
    match state.get(iface).copied() {
        Some(InterfaceVisitState::Done) => return Ok(()),
        Some(InterfaceVisitState::Visiting) => {
            let span = env
                .interfaces
                .get(iface)
                .map(|d| d.span)
                .unwrap_or_else(|| Span::new(0, 0));

            // Try to show a useful cycle path.
            let mut cycle = Vec::new();
            if let Some(pos) = stack.iter().position(|s| s == iface) {
                cycle.extend_from_slice(&stack[pos..]);
            } else {
                cycle.extend_from_slice(stack);
            }
            cycle.push(iface.to_string());

            return Err(TypeError {
                message: format!(
                    "interface inheritance cycle detected: {}",
                    cycle.join(" -> ")
                ),
                span,
            });
        }
        None => {}
    }

    state.insert(iface.to_string(), InterfaceVisitState::Visiting);
    stack.push(iface.to_string());

    let (supers, own_methods, span) = match env.interfaces.get(iface) {
        Some(def) => (def.supers.clone(), def.methods.clone(), def.span),
        None => {
            return Err(TypeError {
                message: format!("internal error: missing interface `{iface}`"),
                span: Span::new(0, 0),
            });
        }
    };

    for sup in &supers {
        compute_interface_all_methods(env, sup, state, stack)?;
    }

    let mut all: BTreeMap<String, InterfaceMethod> = BTreeMap::new();
    for (name, sig) in own_methods {
        all.insert(
            name,
            InterfaceMethod {
                origin: iface.to_string(),
                sig,
            },
        );
    }

    for sup in supers {
        let super_methods = env
            .interfaces
            .get(&sup)
            .expect("super-interfaces resolved in fill_interface")
            .all_methods
            .clone();
        for (name, meth) in super_methods {
            match all.get(&name) {
                None => {
                    all.insert(name, meth);
                }
                Some(existing) => {
                    if existing.origin == meth.origin {
                        // Diamond duplication: same canonical method, ok.
                        continue;
                    }
                    return Err(TypeError {
                        message: format!(
                            "conflicting inherited method `{name}` in interface `{iface}` (from `{}` and `{}`)",
                            existing.origin, meth.origin
                        ),
                        span,
                    });
                }
            }
        }
    }

    env.interfaces.get_mut(iface).expect("exists").all_methods = all;

    stack.pop();
    state.insert(iface.to_string(), InterfaceVisitState::Done);
    Ok(())
}

fn process_impl_item(
    env: &mut ProgramEnv,
    module: &ModulePath,
    item: &ImplItem,
) -> Result<(), TypeError> {
    let impl_generics = lower_generic_params(env, module, &item.generics)?;
    let impl_scope = GenericScope::new(&impl_generics)?;

    match &item.header {
        ImplHeader::Inherent { ty, span: _ } => {
            let ty = lower_path_type(env, module, &impl_scope, ty)?;
            let (type_name, type_args) = match ty {
                Ty::App(TyCon::Named(name), args) => (name, args),
                other => {
                    return Err(TypeError {
                        message: format!(
                            "inherent impl target must be a nominal type, got `{other}`"
                        ),
                        span: item.span,
                    });
                }
            };
            if !env.structs.contains_key(&type_name) && !env.enums.contains_key(&type_name) {
                return Err(TypeError {
                    message: format!("unknown type `{type_name}` in impl"),
                    span: item.span,
                });
            }
            enforce_erased_impl_args(&impl_generics, &type_name, &type_args, item.span)?;

            for method in &item.members {
                let qual_name = format!("{type_name}::{}", method.name.name);
                declare_function_sig(env, module, method, Some(qual_name))?;
            }
        }
        ImplHeader::InterfaceForType {
            interface,
            ty,
            span: _,
        } => {
            let iface_ty = lower_path_type(env, module, &impl_scope, interface)?;
            let iface_name = match iface_ty {
                Ty::App(TyCon::Named(name), args) => {
                    if !args.is_empty() {
                        return Err(TypeError {
                            message: "generic interfaces are not supported in v0.4".to_string(),
                            span: item.span,
                        });
                    }
                    name
                }
                other => {
                    return Err(TypeError {
                        message: format!(
                            "impl interface must be a nominal interface type, got `{other}`"
                        ),
                        span: item.span,
                    });
                }
            };
            let Some(iface_def) = env.interfaces.get(&iface_name) else {
                return Err(TypeError {
                    message: format!("unknown interface `{iface_name}`"),
                    span: item.span,
                });
            };
            if !iface_def.generics.is_empty() {
                return Err(TypeError {
                    message: "generic interfaces are not supported in v0.4".to_string(),
                    span: item.span,
                });
            }
            let iface_methods = iface_def.all_methods.clone();
            let iface_def_name = iface_def.name.clone();

            let ty = lower_path_type(env, module, &impl_scope, ty)?;
            let (type_name, type_args) = match ty {
                Ty::App(TyCon::Named(name), args) => (name, args),
                other => {
                    return Err(TypeError {
                        message: format!("impl target must be a nominal type, got `{other}`"),
                        span: item.span,
                    });
                }
            };
            if !env.structs.contains_key(&type_name) && !env.enums.contains_key(&type_name) {
                return Err(TypeError {
                    message: format!("unknown type `{type_name}` in impl"),
                    span: item.span,
                });
            }
            enforce_erased_impl_args(&impl_generics, &type_name, &type_args, item.span)?;

            // Validate method set matches the interface definition.
            let mut impl_methods_by_name: BTreeMap<String, &FnItem> = BTreeMap::new();
            for method in &item.members {
                let mname = method.name.name.clone();
                if impl_methods_by_name.insert(mname.clone(), method).is_some() {
                    return Err(TypeError {
                        message: format!("duplicate method `{mname}` in impl"),
                        span: method.name.span,
                    });
                }
            }
            for (mname, info) in &iface_methods {
                let Some(method) = impl_methods_by_name.get(mname) else {
                    return Err(TypeError {
                        message: format!(
                            "missing method `{mname}` required by interface `{}`",
                            iface_def_name
                        ),
                        span: item.span,
                    });
                };
                if !method.generics.is_empty() {
                    return Err(TypeError {
                        message: "impl methods cannot be generic in v0.4 interface impls"
                            .to_string(),
                        span: method.name.span,
                    });
                }
                let expected_params = info.sig.params.len() + 1;
                if method.params.len() != expected_params {
                    return Err(TypeError {
                        message: format!(
                            "method `{}` must take {} parameter(s): receiver + {} arg(s)",
                            mname,
                            expected_params,
                            info.sig.params.len()
                        ),
                        span: method.name.span,
                    });
                }

                // Declare the implementing function with a stable internal name.
                let impl_fn_name = format!("impl::{iface_name}::for::{type_name}::{mname}");
                declare_function_sig(env, module, method, Some(impl_fn_name.clone()))?;
                let key = (type_name.clone(), info.origin.clone(), mname.clone());
                if env.interface_methods.contains_key(&key) {
                    return Err(TypeError {
                        message: format!(
                            "overlapping impls for `{type_name}`: duplicate implementation of `{origin}::{mname}`",
                            origin = info.origin
                        ),
                        span: item.span,
                    });
                }
                env.interface_methods.insert(key, impl_fn_name);
            }

            // Disallow extra methods (strictness).
            for extra in impl_methods_by_name.keys() {
                if !iface_methods.contains_key(extra) {
                    return Err(TypeError {
                        message: format!(
                            "extra method `{extra}` not declared in interface `{}`",
                            iface_def_name
                        ),
                        span: item.span,
                    });
                }
            }

            // Record nominal interface implementations, including transitive supers.
            //
            // This is used for:
            // - checked casts (`as?`) / type tests (`is`) against interfaces, including marker
            //   interfaces with zero methods
            // - overlap detection for marker interfaces (no methods => no interface_methods keys)
            let mut all_ifaces = BTreeSet::<String>::new();
            collect_interface_and_supers(env, &iface_def_name, &mut all_ifaces);
            for iface in all_ifaces {
                let key = (type_name.clone(), iface.clone());
                if !env.interface_impls.insert(key) {
                    return Err(TypeError {
                        message: format!(
                            "overlapping impls for `{type_name}`: duplicate implementation of interface `{iface}`"
                        ),
                        span: item.span,
                    });
                }
            }
        }
    }

    Ok(())
}

fn enforce_erased_impl_args(
    generics: &[GenericParamInfo],
    name: &str,
    args: &[Ty],
    span: Span,
) -> Result<(), TypeError> {
    if generics.is_empty() {
        // Monomorphic impls are only valid for arity-0 nominal types.
        if !args.is_empty() {
            return Err(TypeError {
                message: format!(
                    "impl for `{name}` must not specify concrete type arguments (type args are erased at runtime)"
                ),
                span,
            });
        }
        return Ok(());
    }

    // For erased generics, we only support "for all instantiations" impls:
    // the header must use exactly the impl's type parameters as arguments (in order).
    if args.len() != generics.len() {
        return Err(TypeError {
            message: format!(
                "impl for `{name}` must apply all {} type parameter(s)",
                generics.len()
            ),
            span,
        });
    }
    for (idx, (arg, gp)) in args.iter().zip(generics.iter()).enumerate() {
        let expected = Ty::Gen(idx);
        if arg != &expected {
            return Err(TypeError {
                message: format!(
                    "impl for `{name}` must use type parameter `{}` at position {}",
                    gp.name, idx
                ),
                span,
            });
        }
    }
    Ok(())
}

fn lower_generic_params(
    env: &ProgramEnv,
    module: &ModulePath,
    params: &[GenericParam],
) -> Result<Vec<GenericParamInfo>, TypeError> {
    let mut out = Vec::with_capacity(params.len());
    let mut seen = BTreeSet::new();
    for p in params {
        let name = p.name.name.clone();
        if !seen.insert(name.clone()) {
            return Err(TypeError {
                message: format!("duplicate generic parameter `{name}`"),
                span: p.name.span,
            });
        }

        let constraint = match &p.constraint {
            None => None,
            Some(path) => {
                if !path.segments.iter().all(|seg| seg.args.is_empty()) {
                    return Err(TypeError {
                        message: "generic constraints cannot have type arguments in v0.4"
                            .to_string(),
                        span: path.span,
                    });
                }
                let segments: Vec<String> =
                    path.segments.iter().map(|s| s.name.name.clone()).collect();
                let (kind, fqn) = env
                    .modules
                    .resolve_type_fqn(module, &segments, path.span)
                    .map_err(|e| TypeError {
                        message: e.message,
                        span: e.span,
                    })?;
                if kind != crate::modules::DefKind::Interface {
                    return Err(TypeError {
                        message: format!("generic constraint must be an interface, got `{fqn}`"),
                        span: path.span,
                    });
                }
                Some(fqn)
            }
        };

        out.push(GenericParamInfo {
            name,
            arity: p.arity,
            constraint,
            span: p.span,
        });
    }
    Ok(out)
}

#[derive(Clone, Debug)]
struct GenericScope {
    names: HashMap<String, GenId>,
    arities: Vec<usize>,
}

impl GenericScope {
    fn new(params: &[GenericParamInfo]) -> Result<Self, TypeError> {
        let mut names = HashMap::new();
        let mut arities = Vec::with_capacity(params.len());
        for (idx, p) in params.iter().enumerate() {
            if names.insert(p.name.clone(), idx).is_some() {
                return Err(TypeError {
                    message: format!("duplicate generic parameter `{}`", p.name),
                    span: p.span,
                });
            }
            arities.push(p.arity);
        }
        Ok(Self { names, arities })
    }

    fn lookup(&self, name: &str) -> Option<(GenId, usize)> {
        let id = *self.names.get(name)?;
        Some((id, self.arities[id]))
    }
}

fn lower_type_expr(
    env: &ProgramEnv,
    module: &ModulePath,
    scope: &GenericScope,
    ty: &TypeExpr,
) -> Result<Ty, TypeError> {
    match ty {
        TypeExpr::Readonly { inner, .. } => Ok(Ty::Readonly(Box::new(lower_type_expr(
            env, module, scope, inner,
        )?))),
        TypeExpr::Prim { prim, .. } => Ok(match prim {
            PrimType::Unit => Ty::Unit,
            PrimType::Bool => Ty::Bool,
            PrimType::Int => Ty::Int,
            PrimType::Float => Ty::Float,
            PrimType::String => Ty::String,
            PrimType::Bytes => Ty::Bytes,
        }),
        TypeExpr::Array { elem, .. } => Ok(Ty::Array(Box::new(lower_type_expr(
            env, module, scope, elem,
        )?))),
        TypeExpr::Tuple { items, .. } => {
            let items = items
                .iter()
                .map(|t| lower_type_expr(env, module, scope, t))
                .collect::<Result<Vec<_>, _>>()?;
            if items.is_empty() {
                Ok(Ty::Unit)
            } else {
                Ok(Ty::Tuple(items))
            }
        }
        TypeExpr::Fn { params, ret, .. } => {
            let params = params
                .iter()
                .map(|p| lower_type_expr(env, module, scope, p))
                .collect::<Result<Vec<_>, _>>()?;
            let ret = lower_type_expr(env, module, scope, ret)?;
            Ok(Ty::Fn {
                params,
                ret: Box::new(ret),
            })
        }
        TypeExpr::Cont { param, ret, .. } => Ok(Ty::Cont {
            param: Box::new(lower_type_expr(env, module, scope, param)?),
            ret: Box::new(lower_type_expr(env, module, scope, ret)?),
        }),
        TypeExpr::Path(path) => lower_path_type(env, module, scope, path),
    }
}

fn lower_path_type(
    env: &ProgramEnv,
    module: &ModulePath,
    scope: &GenericScope,
    path: &crate::ast::PathType,
) -> Result<Ty, TypeError> {
    if path.segments.is_empty() {
        return Err(TypeError {
            message: "empty type path".to_string(),
            span: path.span,
        });
    }

    // v0.4 restriction: only the last segment may have type arguments.
    for seg in &path.segments[..path.segments.len() - 1] {
        if !seg.args.is_empty() {
            return Err(TypeError {
                message: "type arguments are only supported on the last path segment".to_string(),
                span: seg.span,
            });
        }
    }

    let last = path.segments.last().expect("non-empty");
    let raw_segments: Vec<String> = path.segments.iter().map(|s| s.name.name.clone()).collect();
    let args = last
        .args
        .iter()
        .map(|a| lower_type_expr(env, module, scope, a))
        .collect::<Result<Vec<_>, _>>()?;

    if raw_segments.len() == 1
        && let Some((gen_id, arity)) = scope.lookup(&raw_segments[0])
    {
        if arity == 0 {
            if !args.is_empty() {
                return Err(TypeError {
                    message: format!(
                        "type parameter `{}` does not take type arguments",
                        raw_segments[0]
                    ),
                    span: path.span,
                });
            }
            return Ok(Ty::Gen(gen_id));
        }
        if args.len() != arity {
            return Err(TypeError {
                message: format!(
                    "type constructor `{}` expects {arity} type argument(s), got {}",
                    raw_segments[0],
                    args.len()
                ),
                span: path.span,
            });
        }
        return Ok(Ty::App(TyCon::Gen(gen_id), args));
    }

    let (kind, name) = env
        .modules
        .resolve_type_fqn(module, &raw_segments, path.span)
        .map_err(|e| TypeError {
            message: e.message,
            span: e.span,
        })?;

    let arity = match kind {
        crate::modules::DefKind::Struct => env.structs.get(&name).expect("declared").generics.len(),
        crate::modules::DefKind::Enum => env.enums.get(&name).expect("declared").generics.len(),
        crate::modules::DefKind::Interface => {
            env.interfaces.get(&name).expect("declared").generics.len()
        }
    };

    if args.len() != arity {
        return Err(TypeError {
            message: format!(
                "type `{name}` expects {arity} type argument(s), got {}",
                args.len()
            ),
            span: path.span,
        });
    }

    Ok(Ty::App(TyCon::Named(name), args))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ExprUse {
    Value,
    Stmt,
}

#[derive(Clone, Debug)]
struct InferCtx {
    next_type_var: TypeVarId,
    next_con_var: ConVarId,
    type_bindings: HashMap<TypeVarId, Ty>,
    con_bindings: HashMap<ConVarId, TyCon>,
    con_arities: HashMap<ConVarId, usize>,
    type_constraints: HashMap<TypeVarId, Vec<String>>,
}

impl InferCtx {
    fn new() -> Self {
        Self {
            next_type_var: 0,
            next_con_var: 0,
            type_bindings: HashMap::new(),
            con_bindings: HashMap::new(),
            con_arities: HashMap::new(),
            type_constraints: HashMap::new(),
        }
    }

    fn fresh_type_var(&mut self) -> Ty {
        let id = self.next_type_var;
        self.next_type_var = self.next_type_var.wrapping_add(1);
        Ty::Var(id)
    }

    fn fresh_con_var(&mut self, arity: usize) -> TyCon {
        let id = self.next_con_var;
        self.next_con_var = self.next_con_var.wrapping_add(1);
        self.con_arities.insert(id, arity);
        TyCon::Var(id)
    }

    fn add_constraint(&mut self, var: TypeVarId, iface: String) {
        self.type_constraints.entry(var).or_default().push(iface);
    }

    fn resolve_ty(&mut self, ty: Ty) -> Ty {
        match ty {
            Ty::Var(id) => {
                if let Some(bound) = self.type_bindings.get(&id).cloned() {
                    let resolved = self.resolve_ty(bound);
                    self.type_bindings.insert(id, resolved.clone());
                    resolved
                } else {
                    Ty::Var(id)
                }
            }
            Ty::Array(elem) => Ty::Array(Box::new(self.resolve_ty(*elem))),
            Ty::Tuple(items) => Ty::Tuple(items.into_iter().map(|t| self.resolve_ty(t)).collect()),
            Ty::Fn { params, ret } => Ty::Fn {
                params: params.into_iter().map(|p| self.resolve_ty(p)).collect(),
                ret: Box::new(self.resolve_ty(*ret)),
            },
            Ty::Cont { param, ret } => Ty::Cont {
                param: Box::new(self.resolve_ty(*param)),
                ret: Box::new(self.resolve_ty(*ret)),
            },
            Ty::Readonly(inner) => {
                let inner = self.resolve_ty(*inner);
                if inner.is_ref_like() {
                    inner.as_readonly_view()
                } else {
                    inner
                }
            }
            Ty::App(con, args) => Ty::App(
                self.resolve_con(con),
                args.into_iter().map(|a| self.resolve_ty(a)).collect(),
            ),
            other => other,
        }
    }

    fn resolve_con(&mut self, con: TyCon) -> TyCon {
        match con {
            TyCon::Var(id) => {
                if let Some(bound) = self.con_bindings.get(&id).cloned() {
                    let resolved = self.resolve_con(bound);
                    self.con_bindings.insert(id, resolved.clone());
                    resolved
                } else {
                    TyCon::Var(id)
                }
            }
            other => other,
        }
    }

    fn occurs_in(&mut self, var: TypeVarId, ty: &Ty) -> bool {
        match self.resolve_ty(ty.clone()) {
            Ty::Var(id) => id == var,
            Ty::Array(elem) | Ty::Readonly(elem) => self.occurs_in(var, elem.as_ref()),
            Ty::Tuple(items) => items.iter().any(|item| self.occurs_in(var, item)),
            Ty::Fn { params, ret } => {
                params.iter().any(|p| self.occurs_in(var, p)) || self.occurs_in(var, ret.as_ref())
            }
            Ty::Cont { param, ret } => {
                self.occurs_in(var, param.as_ref()) || self.occurs_in(var, ret.as_ref())
            }
            Ty::App(_, args) => args.iter().any(|a| self.occurs_in(var, a)),
            _ => false,
        }
    }

    fn unify(&mut self, a: Ty, b: Ty, span: Span) -> Result<Ty, TypeError> {
        let a = self.resolve_ty(a);
        let b = self.resolve_ty(b);
        if a == b {
            return Ok(a);
        }

        match (a, b) {
            (Ty::Var(id), other) | (other, Ty::Var(id)) => {
                if self.occurs_in(id, &other) {
                    return Err(TypeError {
                        message: "occurs check failed in type inference".to_string(),
                        span,
                    });
                }
                self.type_bindings.insert(id, other.clone());
                Ok(other)
            }
            (Ty::Readonly(a_inner), Ty::Readonly(b_inner)) => {
                let inner = self.unify(*a_inner, *b_inner, span)?;
                Ok(Ty::Readonly(Box::new(inner)))
            }
            (Ty::Array(ae), Ty::Array(be)) => {
                let elem = self.unify(*ae, *be, span)?;
                Ok(Ty::Array(Box::new(elem)))
            }
            (Ty::Tuple(a_items), Ty::Tuple(b_items)) => {
                if a_items.len() != b_items.len() {
                    return Err(TypeError {
                        message: format!(
                            "tuple arity mismatch: expected {}, got {}",
                            a_items.len(),
                            b_items.len()
                        ),
                        span,
                    });
                }
                let mut items = Vec::with_capacity(a_items.len());
                for (a, b) in a_items.into_iter().zip(b_items.into_iter()) {
                    items.push(self.unify(a, b, span)?);
                }
                Ok(Ty::Tuple(items))
            }
            (
                Ty::Fn {
                    params: ap,
                    ret: ar,
                },
                Ty::Fn {
                    params: bp,
                    ret: br,
                },
            ) => {
                if ap.len() != bp.len() {
                    return Err(TypeError {
                        message: format!(
                            "function arity mismatch: expected {}, got {}",
                            ap.len(),
                            bp.len()
                        ),
                        span,
                    });
                }
                let mut params = Vec::with_capacity(ap.len());
                for (a, b) in ap.into_iter().zip(bp.into_iter()) {
                    params.push(self.unify(a, b, span)?);
                }
                let ret = self.unify(*ar, *br, span)?;
                Ok(Ty::Fn {
                    params,
                    ret: Box::new(ret),
                })
            }
            (Ty::Cont { param: ap, ret: ar }, Ty::Cont { param: bp, ret: br }) => {
                let param = self.unify(*ap, *bp, span)?;
                let ret = self.unify(*ar, *br, span)?;
                Ok(Ty::Cont {
                    param: Box::new(param),
                    ret: Box::new(ret),
                })
            }
            (Ty::App(ac, aa), Ty::App(bc, ba)) => {
                if aa.len() != ba.len() {
                    return Err(TypeError {
                        message: format!(
                            "type application arity mismatch: expected {}, got {}",
                            aa.len(),
                            ba.len()
                        ),
                        span,
                    });
                }
                let con = self.unify_con(ac, bc, span)?;
                let mut args = Vec::with_capacity(aa.len());
                for (a, b) in aa.into_iter().zip(ba.into_iter()) {
                    args.push(self.unify(a, b, span)?);
                }
                Ok(Ty::App(con, args))
            }
            (a, b) => Err(TypeError {
                message: format!("type mismatch: expected `{a}`, got `{b}`"),
                span,
            }),
        }
    }

    fn unify_con(&mut self, a: TyCon, b: TyCon, span: Span) -> Result<TyCon, TypeError> {
        let a = self.resolve_con(a);
        let b = self.resolve_con(b);
        if a == b {
            return Ok(a);
        }

        match (a, b) {
            (TyCon::Var(id), other) | (other, TyCon::Var(id)) => {
                self.con_bindings.insert(id, other.clone());
                Ok(other)
            }
            (a, b) => Err(TypeError {
                message: format!("type constructor mismatch: `{a}` vs `{b}`"),
                span,
            }),
        }
    }
}

#[derive(Clone, Debug)]
struct InstFn {
    params: Vec<Ty>,
    ret: Ty,
}

fn instantiate_fn(sig: &FnSig, infer: &mut InferCtx) -> InstFn {
    let mut ty_subst: HashMap<GenId, Ty> = HashMap::new();
    let mut con_subst: HashMap<GenId, TyCon> = HashMap::new();

    for (idx, gp) in sig.generics.iter().enumerate() {
        let gen_id: GenId = idx;
        if gp.arity == 0 {
            let ty = infer.fresh_type_var();
            if let Ty::Var(var_id) = ty
                && let Some(iface) = gp.constraint.clone()
            {
                infer.add_constraint(var_id, iface);
            }
            ty_subst.insert(gen_id, ty);
        } else {
            let con = infer.fresh_con_var(gp.arity);
            con_subst.insert(gen_id, con);
        }
    }

    InstFn {
        params: sig
            .params
            .iter()
            .cloned()
            .map(|t| subst_ty(t, &ty_subst, &con_subst))
            .collect(),
        ret: subst_ty(sig.ret.clone(), &ty_subst, &con_subst),
    }
}

fn subst_ty(ty: Ty, ty_subst: &HashMap<GenId, Ty>, con_subst: &HashMap<GenId, TyCon>) -> Ty {
    match ty {
        Ty::Gen(id) => ty_subst.get(&id).cloned().unwrap_or(Ty::Gen(id)),
        Ty::Var(id) => Ty::Var(id),
        Ty::Array(elem) => Ty::Array(Box::new(subst_ty(*elem, ty_subst, con_subst))),
        Ty::Fn { params, ret } => Ty::Fn {
            params: params
                .into_iter()
                .map(|p| subst_ty(p, ty_subst, con_subst))
                .collect(),
            ret: Box::new(subst_ty(*ret, ty_subst, con_subst)),
        },
        Ty::Cont { param, ret } => Ty::Cont {
            param: Box::new(subst_ty(*param, ty_subst, con_subst)),
            ret: Box::new(subst_ty(*ret, ty_subst, con_subst)),
        },
        Ty::Readonly(inner) => Ty::Readonly(Box::new(subst_ty(*inner, ty_subst, con_subst))),
        Ty::App(con, args) => {
            let con = match con {
                TyCon::Gen(id) => con_subst.get(&id).cloned().unwrap_or(TyCon::Gen(id)),
                other => other,
            };
            Ty::App(
                con,
                args.into_iter()
                    .map(|a| subst_ty(a, ty_subst, con_subst))
                    .collect(),
            )
        }
        other => other,
    }
}

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
                info.expr_types.extend(fn_info.expr_types);
                Ok(())
            }
            Item::Impl(imp) => {
                let fn_info = typecheck_impl_item(env, module, imp)?;
                info.expr_types.extend(fn_info.expr_types);
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
    let impl_generics = lower_generic_params(env, module, &item.generics)?;
    let impl_scope = GenericScope::new(&impl_generics)?;
    match &item.header {
        ImplHeader::Inherent { ty, .. } => {
            let ty = lower_path_type(env, module, &impl_scope, ty)?;
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
            for method in &item.members {
                let fn_name = format!("{type_name}::{}", method.name.name);
                let sig = env.functions.get(&fn_name).ok_or(TypeError {
                    message: format!("internal error: missing method signature for `{fn_name}`"),
                    span: method.name.span,
                })?;
                let fn_info = typecheck_function_body(env, module, sig, method)?;
                info.expr_types.extend(fn_info.expr_types);
            }
        }
        ImplHeader::InterfaceForType { interface, ty, .. } => {
            let iface_ty = lower_path_type(env, module, &impl_scope, interface)?;
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
            let type_name = match ty {
                Ty::App(TyCon::Named(name), _args) => name,
                other => {
                    return Err(TypeError {
                        message: format!("impl target must be a nominal type, got `{other}`"),
                        span: item.span,
                    });
                }
            };

            for method in &item.members {
                let mname = &method.name.name;
                let fn_name = format!("impl::{iface_name}::for::{type_name}::{mname}");
                let sig = env.functions.get(&fn_name).ok_or(TypeError {
                    message: format!("internal error: missing method signature for `{fn_name}`"),
                    span: method.name.span,
                })?;
                let fn_info = typecheck_function_body(env, module, sig, method)?;
                info.expr_types.extend(fn_info.expr_types);
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
) -> Result<TypeInfo, TypeError> {
    let mut tc = FnTypechecker::new(env, module.clone(), sig);

    // Parameters are immutable bindings.
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

    let body_ty = tc.typecheck_block(&func.body, ExprUse::Value)?;
    tc.infer
        .unify(sig.ret.clone(), body_ty, func.body.span)
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
    Ok(TypeInfo { expr_types })
}

struct FnTypechecker<'a> {
    env: &'a ProgramEnv,
    module: ModulePath,
    sig: &'a FnSig,
    return_ty: Ty,
    infer: InferCtx,
    scopes: Vec<HashMap<String, LocalInfo>>,
    all_bindings: Vec<(String, Span, Ty)>,
    expr_types: HashMap<Span, Ty>,
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
            infer: InferCtx::new(),
            scopes: vec![HashMap::new()],
            all_bindings: Vec::new(),
            expr_types: HashMap::new(),
            loop_depth: 0,
            reserved_names: BTreeSet::from(["resume".to_string()]),
        }
    }

    fn finish(&mut self) -> Result<(), TypeError> {
        // Ensure constraints are satisfied.
        for (&var, ifaces) in self.infer.type_constraints.clone().iter() {
            let resolved = self.infer.resolve_ty(Ty::Var(var));
            if matches!(resolved, Ty::Var(_)) {
                return Err(TypeError {
                    message: "cannot infer a constrained type".to_string(),
                    span: self.sig.span,
                });
            }
            for iface in ifaces {
                if !self.type_implements_interface(&resolved, iface) {
                    return Err(TypeError {
                        message: format!(
                            "type `{resolved}` does not implement interface `{iface}`"
                        ),
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

        Ok(())
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
                name,
                ty,
                init,
                span,
            } => {
                let declared = if let Some(ty_expr) = ty {
                    let scope = GenericScope::new(&self.sig.generics)?;
                    Some(lower_type_expr(self.env, &self.module, &scope, ty_expr)?)
                } else {
                    None
                };

                let init_ty = if let Some(init_expr) = init {
                    Some(self.typecheck_expr(init_expr, ExprUse::Value)?)
                } else {
                    None
                };

                let local_ty =
                    match (declared, init_ty) {
                        (Some(decl), Some(init_ty)) => self.infer.unify(decl, init_ty, *span)?,
                        (Some(decl), None) => decl,
                        (None, Some(init_ty)) => init_ty,
                        (None, None) => return Err(TypeError {
                            message:
                                "cannot infer type for uninitialized `let` binding; add `: Type`"
                                    .to_string(),
                            span: *span,
                        }),
                    };

                if matches!(kind, BindingKind::Readonly) {
                    self.bind_local(
                        name,
                        LocalInfo {
                            ty: local_ty.as_readonly_view(),
                            kind: *kind,
                            span: *span,
                        },
                    )?;
                } else {
                    self.bind_local(
                        name,
                        LocalInfo {
                            ty: local_ty,
                            kind: *kind,
                            span: *span,
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
                self.infer.unify(got, self.return_ty.clone(), *span)?;
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
            Expr::Int { .. } => Ty::Int,
            Expr::Float { .. } => Ty::Float,
            Expr::String { .. } => Ty::String,
            Expr::Bytes { .. } => Ty::Bytes,

            Expr::Path { path, span } => self.typecheck_path_expr(path, *span)?,
            Expr::Array { items, span: _ } => {
                let elem_ty = self.infer.fresh_type_var();
                for item in items {
                    let t = self.typecheck_expr(item, ExprUse::Value)?;
                    let _ = self.infer.unify(elem_ty.clone(), t, item.span())?;
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
                self.infer.unify(cond_ty, Ty::Bool, *span)?;
                self.loop_depth += 1;
                let _ = self.typecheck_block(body, ExprUse::Stmt)?;
                self.loop_depth -= 1;
                Ty::Unit
            }
            Expr::For {
                binding,
                iter,
                body,
                span,
            } => self.typecheck_for(binding, iter, body, *span)?,
            Expr::Block { block, .. } => self.typecheck_block(block, use_kind)?,

            Expr::Call { callee, args, span } => self.typecheck_call(callee, args, *span)?,
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
        if def.generics.iter().any(|g| g.arity != 0) {
            return Err(TypeError {
                message: "higher-kinded generics on structs are not supported in v0.4".to_string(),
                span,
            });
        }

        let mut type_args: Vec<Ty> = Vec::with_capacity(def.generics.len());
        for _ in &def.generics {
            type_args.push(self.infer.fresh_type_var());
        }
        let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
        let con_subst: HashMap<GenId, TyCon> = HashMap::new();

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
            self.infer.unify(expected.clone(), got, value_expr.span())?;
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
        for _ in &def.generics {
            type_args.push(self.infer.fresh_type_var());
        }
        let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
        let con_subst: HashMap<GenId, TyCon> = HashMap::new();

        for (expr, expected) in fields.iter().zip(variant_fields.iter()) {
            let expected = subst_ty(expected.clone(), &ty_subst, &con_subst);
            let got = self.typecheck_expr(expr, ExprUse::Value)?;
            self.infer.unify(expected, got, expr.span())?;
        }

        Ok(Ty::App(TyCon::Named(def.name.clone()), type_args))
    }

    fn typecheck_effect_call(
        &mut self,
        interface: &crate::ast::Path,
        method: &Ident,
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let segments: Vec<String> = interface.segments.iter().map(|s| s.name.clone()).collect();
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

        let Some(iface) = self.env.interfaces.get(&iface_name) else {
            return Err(TypeError {
                message: format!("unknown interface `{iface_name}`"),
                span: interface.span,
            });
        };
        let Some(method_info) = iface.all_methods.get(&method.name) else {
            return Err(TypeError {
                message: format!("unknown effect/method `{}.{}`", iface_name, method.name),
                span: method.span,
            });
        };
        let sig = &method_info.sig;
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
            self.infer.unify(expected.clone(), got, arg.span())?;
        }
        Ok(sig.ret.clone())
    }

    fn typecheck_lambda(
        &mut self,
        params: &[crate::ast::LambdaParam],
        body: &Block,
        _span: Span,
    ) -> Result<Ty, TypeError> {
        let scope = GenericScope::new(&self.sig.generics)?;
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
                lower_type_expr(self.env, &self.module, &scope, ty_expr)?
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
        self.infer.unify(cond_ty, Ty::Bool, cond.span())?;
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
        self.infer.unify(then_ty, else_ty, span)
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
                    self.infer.unify(result_ty.clone(), body_ty, arm.span)?;
                    self.pop_scope();
                }
                MatchPat::Effect(effect_pat) => {
                    let iface_segments: Vec<String> = effect_pat
                        .interface
                        .segments
                        .iter()
                        .map(|s| s.name.clone())
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
                    let Some(iface) = self.env.interfaces.get(&iface_name) else {
                        return Err(TypeError {
                            message: format!("unknown interface `{iface_name}`"),
                            span: effect_pat.interface.span,
                        });
                    };
                    let Some(method_info) = iface.all_methods.get(&effect_pat.method.name) else {
                        return Err(TypeError {
                            message: format!(
                                "unknown effect/method `{}.{}`",
                                iface_name, effect_pat.method.name
                            ),
                            span: effect_pat.method.span,
                        });
                    };
                    let msig = &method_info.sig;
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
                    self.infer.unify(result_ty.clone(), body_ty, arm.span)?;
                    self.pop_scope();
                }
            }
        }

        if !saw_value_arm {
            return Err(TypeError {
                message: "`match` must have at least one value arm".to_string(),
                span,
            });
        }

        Ok(self.infer.resolve_ty(result_ty))
    }

    fn typecheck_for(
        &mut self,
        binding: &Ident,
        iter: &Expr,
        body: &Block,
        span: Span,
    ) -> Result<Ty, TypeError> {
        let iter_ty = self.typecheck_expr(iter, ExprUse::Value)?;
        let iter_ty = self.infer.resolve_ty(iter_ty);
        let (is_readonly, inner) = match iter_ty {
            Ty::Readonly(inner) => (true, *inner),
            other => (false, other),
        };
        let Ty::Array(elem) = inner else {
            return Err(TypeError {
                message: format!("`for` expects an array iterable, got `{inner}`"),
                span,
            });
        };
        let elem_ty = if is_readonly {
            elem.as_ref().as_readonly_view()
        } else {
            *elem
        };

        self.push_scope();
        self.bind_local(
            binding,
            LocalInfo {
                ty: elem_ty,
                kind: BindingKind::Const,
                span: binding.span,
            },
        )?;
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
        args: &[Expr],
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
                    let local_ty = local.ty.clone();
                    self.expr_types.insert(*callee_span, local_ty.clone());
                    return self.typecheck_call_via_fn_value(local_ty, args, span);
                }
            }

            let segments: Vec<String> = path.segments.iter().map(|s| s.name.clone()).collect();
            let mut func_name: Option<String> = None;

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
                            }
                        }
                        crate::modules::DefKind::Interface => {
                            return self.typecheck_interface_method_call(
                                &type_fqn, last_ident, args, span,
                            );
                        }
                        crate::modules::DefKind::Struct => {
                            let candidate = format!("{type_fqn}::{last}");
                            if self.env.functions.contains_key(&candidate) {
                                func_name = Some(candidate);
                            }
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
                self.infer.unify(expected.clone(), got, arg.span())?;
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
            return self.typecheck_method_call(base, name, args, span);
        }

        let callee_ty = self.typecheck_expr(callee, ExprUse::Value)?;
        self.typecheck_call_via_fn_value(callee_ty, args, span)
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
                    self.infer.unify(expected.clone(), got, arg.span())?;
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
                self.infer.unify(*param, got, args[0].span())?;
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
        let msig = &method_info.sig;
        if args.is_empty() {
            return Err(TypeError {
                message: format!(
                    "interface method call `{iface_name}::{method_name}` requires an explicit receiver argument"
                ),
                span,
            });
        }
        if args.len() != msig.params.len() + 1 {
            return Err(TypeError {
                message: format!(
                    "arity mismatch for `{iface_name}::{method_name}`: expected {}, got {}",
                    msig.params.len() + 1,
                    args.len()
                ),
                span,
            });
        }

        let recv_ty = self.typecheck_expr(&args[0], ExprUse::Value)?;
        self.ensure_implements_interface(&recv_ty, iface_name, args[0].span())?;

        for (arg, expected) in args[1..].iter().zip(msig.params.iter()) {
            let got = self.typecheck_expr(arg, ExprUse::Value)?;
            self.infer.unify(expected.clone(), got, arg.span())?;
        }
        Ok(msig.ret.clone())
    }

    fn typecheck_method_call(
        &mut self,
        receiver: &Expr,
        method: &Ident,
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let recv_ty = self.typecheck_expr(receiver, ExprUse::Value)?;
        let recv_ty_resolved = self.infer.resolve_ty(recv_ty.clone());

        // Inherent method wins whenever the receiver has a nominal name.
        if let Some(type_name) = nominal_type_name(&recv_ty_resolved) {
            let inherent_name = format!("{type_name}::{}", method.name);
            if let Some(sig) = self.env.functions.get(&inherent_name) {
                if !sig.vis.is_public() && !self.module.is_descendant_of(&sig.defining_module) {
                    return Err(TypeError {
                        message: format!("method `{inherent_name}` is private"),
                        span: method.span,
                    });
                }
                let inst = instantiate_fn(sig, &mut self.infer);
                if inst.params.is_empty() {
                    return Err(TypeError {
                        message: format!(
                            "inherent method `{inherent_name}` is not an instance method"
                        ),
                        span,
                    });
                }
                let receiver_expected = inst.params[0].clone();
                self.infer
                    .unify(receiver_expected, recv_ty, receiver.span())?;
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
                    self.infer.unify(expected.clone(), got, arg.span())?;
                }
                return Ok(inst.ret);
            }
        }

        // Interface method resolution depends on the *static* receiver type.
        let recv_for_dispatch = match &recv_ty_resolved {
            Ty::Readonly(inner) => inner.as_ref(),
            other => other,
        };

        // Constrained generic receiver: `T: I` => resolve within `I`.
        if let Ty::Gen(id) = recv_for_dispatch {
            let Some(iface_name) = self
                .sig
                .generics
                .get(*id)
                .and_then(|g| g.constraint.as_deref())
            else {
                return Err(TypeError {
                    message: format!("unknown method `{}` on `{recv_ty_resolved}`", method.name),
                    span: method.span,
                });
            };
            let Some(iface_def) = self.env.interfaces.get(iface_name) else {
                return Err(TypeError {
                    message: format!("unknown interface `{iface_name}`"),
                    span: method.span,
                });
            };
            let Some(info) = iface_def.all_methods.get(&method.name) else {
                return Err(TypeError {
                    message: format!("unknown method `{}` on `{recv_ty_resolved}`", method.name),
                    span: method.span,
                });
            };
            let msig = &info.sig;
            if args.len() != msig.params.len() {
                return Err(TypeError {
                    message: format!(
                        "arity mismatch for `{iface_name}::{}`: expected {}, got {}",
                        method.name,
                        msig.params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(msig.params.iter()) {
                let got = self.typecheck_expr(arg, ExprUse::Value)?;
                self.infer.unify(expected.clone(), got, arg.span())?;
            }
            return Ok(msig.ret.clone());
        }

        // Interface-typed receiver: resolve within that interface (including inherited methods).
        if let Ty::App(TyCon::Named(iface_name), _args) = recv_for_dispatch
            && let Some(iface_def) = self.env.interfaces.get(iface_name)
        {
            let Some(info) = iface_def.all_methods.get(&method.name) else {
                return Err(TypeError {
                    message: format!("unknown method `{}` on `{iface_name}`", method.name),
                    span: method.span,
                });
            };
            let msig = &info.sig;
            if args.len() != msig.params.len() {
                return Err(TypeError {
                    message: format!(
                        "arity mismatch for `{iface_name}::{}`: expected {}, got {}",
                        method.name,
                        msig.params.len(),
                        args.len()
                    ),
                    span,
                });
            }
            for (arg, expected) in args.iter().zip(msig.params.iter()) {
                let got = self.typecheck_expr(arg, ExprUse::Value)?;
                self.infer.unify(expected.clone(), got, arg.span())?;
            }
            return Ok(msig.ret.clone());
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

        let mut origins: BTreeMap<String, String> = BTreeMap::new();
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
            if self.type_implements_interface(&recv_ty_resolved, iface_name) {
                origins
                    .entry(method_info.origin.clone())
                    .or_insert_with(|| iface_name.clone());
            }
        }

        if origins.is_empty() {
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

        let iface = origins.into_values().next().expect("origins.len()==1");
        let iface_def = self.env.interfaces.get(&iface).expect("exists");
        let msig = &iface_def
            .all_methods
            .get(&method.name)
            .expect("method exists for chosen interface")
            .sig;
        if args.len() != msig.params.len() {
            return Err(TypeError {
                message: format!(
                    "arity mismatch for `{iface}::{}`: expected {}, got {}",
                    method.name,
                    msig.params.len(),
                    args.len()
                ),
                span,
            });
        }
        for (arg, expected) in args.iter().zip(msig.params.iter()) {
            let got = self.typecheck_expr(arg, ExprUse::Value)?;
            self.infer.unify(expected.clone(), got, arg.span())?;
        }
        Ok(msig.ret.clone())
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
        self.infer.unify(idx_ty, Ty::Int, index.span())?;

        let base_ty = self.infer.resolve_ty(base_ty);
        let (is_readonly, inner) = match base_ty {
            Ty::Readonly(inner) => (true, *inner),
            other => (false, other),
        };
        let Ty::Array(elem) = inner else {
            return Err(TypeError {
                message: format!("indexing requires an array, got `{inner}`"),
                span,
            });
        };
        let elem_ty = if is_readonly {
            elem.as_readonly_view()
        } else {
            *elem
        };
        Ok(elem_ty)
    }

    fn typecheck_unary(&mut self, op: UnaryOp, expr: &Expr, span: Span) -> Result<Ty, TypeError> {
        let t = self.typecheck_expr(expr, ExprUse::Value)?;
        match op {
            UnaryOp::Not => {
                self.infer.unify(t, Ty::Bool, span)?;
                Ok(Ty::Bool)
            }
            UnaryOp::Neg => {
                let t = self.infer.resolve_ty(t);
                match t {
                    Ty::Int => Ok(Ty::Int),
                    Ty::Float => Ok(Ty::Float),
                    other => Err(TypeError {
                        message: format!("unary `-` requires `int` or `float`, got `{other}`"),
                        span,
                    }),
                }
            }
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
                self.infer.unify(lt, Ty::Bool, left.span())?;
                self.infer.unify(rt, Ty::Bool, right.span())?;
                Ok(Ty::Bool)
            }
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                let lt = self.typecheck_expr(left, ExprUse::Value)?;
                let rt = self.typecheck_expr(right, ExprUse::Value)?;
                let t = self.infer.unify(lt, rt, span)?;
                let t = self.infer.resolve_ty(t);
                match t {
                    Ty::Int | Ty::Float => Ok(t),
                    other => Err(TypeError {
                        message: format!(
                            "arithmetic operator requires `int` or `float`, got `{other}`"
                        ),
                        span,
                    }),
                }
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge => {
                let lt = self.typecheck_expr(left, ExprUse::Value)?;
                let rt = self.typecheck_expr(right, ExprUse::Value)?;
                let t = self.infer.unify(lt, rt, span)?;
                let t = self.infer.resolve_ty(t);
                match op {
                    BinaryOp::Eq | BinaryOp::Ne => match t {
                        Ty::Unit | Ty::Bool | Ty::Int | Ty::Float | Ty::String | Ty::Bytes => {
                            Ok(Ty::Bool)
                        }
                        other => Err(TypeError {
                            message: format!("`==`/`!=` not supported for type `{other}`"),
                            span,
                        }),
                    },
                    _ => match t {
                        Ty::Int | Ty::Float => Ok(Ty::Bool),
                        other => Err(TypeError {
                            message: format!("comparison not supported for type `{other}`"),
                            span,
                        }),
                    },
                }
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
                self.infer.unify(dst_ty, rhs, value.span())?;
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
                        self.infer.unify(expected, rhs, value.span())?;
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
                        self.infer.unify(expected, rhs, value.span())?;
                        Ok(Ty::Unit)
                    }
                }
            }
            Expr::Index { base, index, .. } => {
                let base_ty = self.typecheck_expr(base, ExprUse::Value)?;
                let idx_ty = self.typecheck_expr(index, ExprUse::Value)?;
                self.infer.unify(idx_ty, Ty::Int, index.span())?;
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
                self.infer.unify(*elem, rhs, value.span())?;
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

        let scope = GenericScope::new(&self.sig.generics)?;
        let target_ty = lower_type_expr(self.env, &self.module, &scope, ty)?;
        let (iface_name, args) = match target_ty {
            Ty::App(TyCon::Named(name), args) => (name, args),
            other => {
                return Err(TypeError {
                    message: format!("`as` target must be an interface type, got `{other}`"),
                    span,
                });
            }
        };
        if !args.is_empty() {
            return Err(TypeError {
                message: "generic interfaces are not supported in v0.4".to_string(),
                span,
            });
        }
        if !self.env.interfaces.contains_key(&iface_name) {
            return Err(TypeError {
                message: format!("`as` target must be an interface type, got `{iface_name}`"),
                span,
            });
        }
        if !self.type_implements_interface(&src_inner, &iface_name) {
            return Err(TypeError {
                message: format!(
                    "type `{}` does not implement interface `{iface_name}`",
                    src_inner
                ),
                span,
            });
        }

        let out = Ty::App(TyCon::Named(iface_name), Vec::new());
        Ok(if src_is_readonly {
            Ty::Readonly(Box::new(out))
        } else {
            out
        })
    }

    fn typecheck_is(&mut self, expr: &Expr, ty: &TypeExpr, span: Span) -> Result<Ty, TypeError> {
        // Evaluate/typecheck the LHS for side effects and to populate `expr_types`.
        let _ = self.typecheck_expr(expr, ExprUse::Value)?;

        // Validate the target type is runtime-checkable in v0.4.
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
        let scope = GenericScope::new(&self.sig.generics)?;
        let target_ty = lower_type_expr(self.env, &self.module, &scope, ty)?;

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
            other => {
                return Err(TypeError {
                    message: format!(
                        "{op} target must be a nominal type (`struct`/`enum`/`interface`), got `{other}`"
                    ),
                    span,
                });
            }
        };

        if !args.is_empty() {
            return Err(TypeError {
                message: format!(
                    "{op} target type must be monomorphic in v0.4 (type args are erased at runtime)"
                ),
                span,
            });
        }

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

        Ok(Ty::App(TyCon::Named(name), Vec::new()))
    }

    fn ensure_implements_interface(
        &self,
        ty: &Ty,
        iface: &str,
        span: Span,
    ) -> Result<(), TypeError> {
        if self.type_implements_interface(ty, iface) {
            Ok(())
        } else {
            Err(TypeError {
                message: format!("type `{ty}` does not implement interface `{iface}`"),
                span,
            })
        }
    }

    fn type_implements_interface(&self, ty: &Ty, iface: &str) -> bool {
        if !self.env.interfaces.contains_key(iface) {
            return false;
        }

        let ty = match ty {
            Ty::Readonly(inner) => inner.as_ref(),
            other => other,
        };

        match ty {
            // Generic constraint: `T: J` implements `I` iff `J: I`.
            Ty::Gen(id) => self
                .sig
                .generics
                .get(*id)
                .and_then(|g| g.constraint.as_deref())
                .is_some_and(|bound| is_subinterface(self.env, bound, iface)),

            // Interface values: `J` implements `I` iff `J: I`.
            Ty::App(TyCon::Named(type_name), _args)
                if self.env.interfaces.contains_key(type_name) =>
            {
                is_subinterface(self.env, type_name, iface)
            }

            // Concrete nominal type: must provide implementations for every canonical method in
            // the target interface, including super-interfaces.
            Ty::App(TyCon::Named(type_name), _args) => self
                .env
                .interface_impls
                .contains(&(type_name.clone(), iface.to_string())),

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
        _ => false,
    }
}

fn nominal_type_name(ty: &Ty) -> Option<&str> {
    match ty {
        Ty::Readonly(inner) => nominal_type_name(inner),
        Ty::App(TyCon::Named(name), _) => Some(name.as_str()),
        _ => None,
    }
}

fn collect_interface_and_supers(env: &ProgramEnv, iface: &str, out: &mut BTreeSet<String>) {
    if !out.insert(iface.to_string()) {
        return;
    }
    let Some(def) = env.interfaces.get(iface) else {
        return;
    };
    for sup in &def.supers {
        collect_interface_and_supers(env, sup, out);
    }
}

fn is_subinterface(env: &ProgramEnv, sub: &str, sup: &str) -> bool {
    if sub == sup {
        return true;
    }

    let mut stack = vec![sub.to_string()];
    let mut seen = BTreeSet::<String>::new();
    while let Some(cur) = stack.pop() {
        if !seen.insert(cur.clone()) {
            continue;
        }
        if cur == sup {
            return true;
        }
        let Some(def) = env.interfaces.get(&cur) else {
            continue;
        };
        for parent in &def.supers {
            stack.push(parent.clone());
        }
    }
    false
}
