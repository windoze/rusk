use crate::ast::{
    BinaryOp, BindingKind, Block, EnumItem, Expr, FieldName, FnItem, FnItemKind, GenericParam,
    Ident, ImplHeader, ImplItem, ImplMember, InterfaceItem, InterfaceMember, IntrinsicFnItem, Item,
    MatchArm, MatchPat, MethodReceiverKind, Param, PatLiteral, Pattern, PrimType, Program,
    StructItem, TypeExpr, UnaryOp, Visibility,
};
use crate::host::{CompileOptions, HostVisibility};
use crate::modules::{ModulePath, ModuleResolver};
use crate::source::Span;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
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
    Never,
    Bool,
    Int,
    Float,
    Byte,
    Char,
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

    /// Interface value type with associated type bindings.
    ///
    /// Surface syntax: `I{Item = int}` or `I<T>{Item = int}`.
    Iface {
        iface: String,
        args: Vec<Ty>,
        assoc_bindings: BTreeMap<String, Ty>,
    },

    /// Associated type projection.
    ///
    /// Surface syntax:
    /// - `Self::Item` inside `interface I { ... }` / `impl I for T { ... }`
    /// - `I<...>::Item<T>` in generic code
    AssocProj {
        iface: String,
        iface_args: Vec<Ty>,
        assoc: String,
        self_ty: Box<Ty>,
    },

    /// `Self` placeholder type (only meaningful inside interface + impl method contexts).
    SelfType,

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
            Ty::Array(_)
                | Ty::Tuple(_)
                | Ty::App(..)
                | Ty::Iface { .. }
                | Ty::AssocProj { .. }
                | Ty::Gen(_)
                | Ty::Var(_)
                | Ty::Readonly(_)
                | Ty::SelfType
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
            Ty::Never => write!(f, "!"),
            Ty::Bool => write!(f, "bool"),
            Ty::Int => write!(f, "int"),
            Ty::Float => write!(f, "float"),
            Ty::Byte => write!(f, "byte"),
            Ty::Char => write!(f, "char"),
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
            Ty::Iface {
                iface,
                args,
                assoc_bindings,
            } => {
                write!(f, "{iface}")?;
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
                write!(f, "{{")?;
                for (idx, (name, ty)) in assoc_bindings.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{name} = {ty}")?;
                }
                write!(f, "}}")
            }
            Ty::AssocProj {
                iface,
                iface_args,
                assoc,
                self_ty,
            } => {
                write!(f, "{iface}")?;
                if !iface_args.is_empty() {
                    write!(f, "<")?;
                    for (idx, a) in iface_args.iter().enumerate() {
                        if idx != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{a}")?;
                    }
                    write!(f, ">")?;
                }
                write!(f, "::{assoc}<{self_ty}>")
            }
            Ty::SelfType => write!(f, "Self"),
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
    /// Interface bounds (`T: I + J<K> + ...`), stored as instantiated interface types.
    ///
    /// In the initial generics-rework stage, bounds are only allowed on arity-0 type parameters
    /// for `fn`/method generics (not on `impl`/`struct`/`enum`/`interface` generics).
    pub(crate) bounds: Vec<Ty>,
    pub(crate) span: Span,
}

#[derive(Clone, Debug)]
pub(crate) struct StructDef {
    pub(crate) name: String,
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) fields: Vec<(String, Ty)>,
    pub(crate) is_newtype: bool,
}

#[derive(Clone, Debug)]
pub(crate) struct EnumDef {
    pub(crate) name: String,
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) variants: BTreeMap<String, Vec<Ty>>,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceMethodSig {
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) params: Vec<Ty>,
    pub(crate) ret: Ty,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceDefaultTemplate {
    pub(crate) name: Ident,
    pub(crate) generics: Vec<GenericParam>,
    pub(crate) params: Vec<Param>,
    pub(crate) ret: TypeExpr,
    pub(crate) body: Block,
    pub(crate) span: Span,
}

pub(crate) fn synthesize_default_method_fn_item(
    receiver_readonly: bool,
    template: &InterfaceDefaultTemplate,
) -> FnItem {
    FnItem {
        vis: Visibility::Private,
        kind: FnItemKind::Method {
            receiver: MethodReceiverKind::Instance {
                readonly: receiver_readonly,
            },
        },
        name: template.name.clone(),
        generics: template.generics.clone(),
        params: template.params.clone(),
        ret: template.ret.clone(),
        body: template.body.clone(),
        span: template.span,
    }
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceMethodDecl {
    pub(crate) receiver_readonly: bool,
    pub(crate) has_default: bool,
    pub(crate) default_template: Option<InterfaceDefaultTemplate>,
    pub(crate) sig: InterfaceMethodSig,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceAssocTypeDecl {}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceAssocType {
    pub(crate) origin: String,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceMethod {
    pub(crate) origin: String,
    pub(crate) receiver_readonly: bool,
    pub(crate) has_default: bool,
    pub(crate) default_template: Option<InterfaceDefaultTemplate>,
    pub(crate) sig: InterfaceMethodSig,
}

#[derive(Clone, Debug)]
pub(crate) struct InterfaceDef {
    pub(crate) name: String,
    pub(crate) generics: Vec<GenericParamInfo>,
    pub(crate) supers: Vec<Ty>,
    /// Associated types declared directly in this interface (not including inherited ones).
    pub(crate) assoc_types: BTreeMap<String, InterfaceAssocTypeDecl>,
    /// Full associated type set including inherited ones, with canonical origin interface IDs.
    pub(crate) all_assoc_types: BTreeMap<String, InterfaceAssocType>,
    /// Methods declared directly in this interface (not including inherited ones).
    pub(crate) methods: BTreeMap<String, InterfaceMethodDecl>,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum InherentMethodKind {
    Instance { readonly: bool },
    Static,
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

    /// Metadata for inherent methods declared in `impl Type { ... }`, keyed by the resolved
    /// function name `Type::method`.
    pub(crate) inherent_method_kinds: BTreeMap<String, InherentMethodKind>,

    /// Interface impl method table keyed by canonical interface method id:
    ///
    /// `(dynamic_type_name, origin_interface_name, method_name) -> function_name`.
    ///
    /// `function_name` must exist in `functions`.
    pub(crate) interface_methods: BTreeMap<(String, String, String), String>,

    /// Associated type definitions for interface impls.
    ///
    /// Keyed by:
    /// `(dynamic_type_name, interface_name, assoc_name) -> assoc_type_template`.
    ///
    /// The stored type is in the impl's generic environment (using `Ty::Gen` indices).
    pub(crate) interface_assoc_types: BTreeMap<(String, String, String), Ty>,

    /// Nominal interface implementation table:
    ///
    /// `(dynamic_type_name, interface_name)` is present iff the type implements the interface.
    ///
    /// Entries include transitive super-interfaces, so if `J: I` and `T` implements `J`, then
    /// `(T, I)` is also present.
    pub(crate) interface_impls: BTreeSet<(String, String)>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct FnTypeInfo {
    pub(crate) expr_types: HashMap<Span, Ty>,
    /// Reified type arguments (kind `Type`) for direct (named) calls.
    ///
    /// Keyed by the call expression span.
    pub(crate) call_type_args: HashMap<(Span, String), Vec<Ty>>,
    /// Reified type arguments (kind `Type`) for interface method generics at a call site.
    ///
    /// This is used for both:
    /// - fully-qualified calls: `I::m(recv, ...)`
    /// - method sugar calls that resolve to an interface method: `recv.m(...)`
    ///
    /// Keyed by the call expression span.
    pub(crate) method_type_args: HashMap<(Span, String), Vec<Ty>>,
    /// Instantiated interface type arguments for effects (perform + handler patterns).
    ///
    /// Effect identity includes:
    /// - canonical origin interface (FQN)
    /// - instantiated interface type arguments
    /// - method name
    ///
    /// We record the instantiated interface arguments for the *canonical origin interface* at each
    /// effect call / pattern site so the compiler can reify them as runtime `TypeRep` operands.
    pub(crate) effect_interface_args: HashMap<(Span, String), Vec<Ty>>,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct TypeInfo {
    pub(crate) functions: HashMap<String, FnTypeInfo>,
}

impl TypeInfo {
    pub(crate) fn for_fn(&self, name: &str) -> Option<&FnTypeInfo> {
        self.functions.get(name)
    }
}

pub(crate) fn build_env(
    program: &Program,
    options: &CompileOptions,
) -> Result<ProgramEnv, TypeError> {
    let modules = ModuleResolver::build(program, &options.host_modules).map_err(|e| TypeError {
        message: e.message,
        span: e.span,
    })?;

    let mut env = ProgramEnv {
        modules,
        ..Default::default()
    };
    add_prelude(&mut env);
    add_host_modules(&mut env, options)?;

    // First pass: declare interfaces (so later passes can validate bounds against them).
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Interface(i) => declare_interface(&mut env, module, i),
            _ => Ok(()),
        },
    )?;

    // Second pass: declare structs/enums (including their generic bounds).
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Struct(s) => declare_struct(&mut env, module, s),
            Item::Enum(e) => declare_enum(&mut env, module, e),
            _ => Ok(()),
        },
    )?;

    // Third pass: fill interface members (methods + associated types).
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Interface(i) => fill_interface(&mut env, module, i),
            _ => Ok(()),
        },
    )?;

    // Interface inheritance pass: validate graph + compute full method + assoc type sets.
    compute_interface_inheritance(&mut env)?;
    validate_interfaces_after_inheritance(&env)?;

    // Fourth pass: declare top-level function signatures.
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Function(f) => {
                let full_name = module.qualify(&f.name.name);
                if expected_core_intrinsic_sig(&full_name).is_some() {
                    return Err(TypeError {
                        message: format!(
                            "intrinsic `{full_name}` must be declared with `intrinsic fn`"
                        ),
                        span: f.name.span,
                    });
                }
                declare_function_sig(&mut env, module, &[], f, Some(full_name))
            }
            Item::IntrinsicFn(f) => declare_intrinsic_fn_sig(&mut env, module, f),
            _ => Ok(()),
        },
    )?;

    // Fifth pass: fill struct/enum members (fields/variants).
    walk_module_items(
        &program.items,
        &ModulePath::root(),
        &mut |module, item| match item {
            Item::Struct(s) => fill_struct(&mut env, module, s),
            Item::Enum(e) => fill_enum(&mut env, module, e),
            _ => Ok(()),
        },
    )?;

    // Sixth pass: process impl items (declare method signatures + interface method table).
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

fn add_host_modules(env: &mut ProgramEnv, options: &CompileOptions) -> Result<(), TypeError> {
    for (module_name, module) in &options.host_modules {
        let defining_module = ModulePath::root().child(module_name);
        for func in &module.functions {
            let full_name = format!("{module_name}::{}", func.name);
            if env.functions.contains_key(&full_name) {
                return Err(TypeError {
                    message: format!("duplicate function `{full_name}`"),
                    span: Span::new(0, 0),
                });
            }

            let vis = match func.visibility {
                HostVisibility::Private => Visibility::Private,
                HostVisibility::Public => Visibility::Public {
                    span: Span::new(0, 0),
                },
            };

            let params = func
                .sig
                .params
                .iter()
                .map(ty_from_host_type)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|message| TypeError {
                    message: format!("host function `{full_name}`: {message}"),
                    span: Span::new(0, 0),
                })?;
            let ret = ty_from_host_type(&func.sig.ret).map_err(|message| TypeError {
                message: format!("host function `{full_name}`: {message}"),
                span: Span::new(0, 0),
            })?;

            env.functions.insert(
                full_name.clone(),
                FnSig {
                    name: full_name,
                    vis,
                    defining_module: defining_module.clone(),
                    generics: Vec::new(),
                    params,
                    ret,
                    span: Span::new(0, 0),
                },
            );
        }
    }

    Ok(())
}

fn ty_from_host_type(ty: &crate::host::HostType) -> Result<Ty, String> {
    match ty {
        crate::host::HostType::Any => Err("unsupported host type: `any`".to_string()),
        crate::host::HostType::Unit => Ok(Ty::Unit),
        crate::host::HostType::Bool => Ok(Ty::Bool),
        crate::host::HostType::Int => Ok(Ty::Int),
        crate::host::HostType::Float => Ok(Ty::Float),
        crate::host::HostType::String => Ok(Ty::String),
        crate::host::HostType::Bytes => Ok(Ty::Bytes),
        crate::host::HostType::TypeRep => Err("unsupported host type: `typerep`".to_string()),
        crate::host::HostType::Array(elem) => Ok(Ty::Array(Box::new(ty_from_host_type(elem)?))),
        crate::host::HostType::Tuple(items) => Ok(Ty::Tuple(
            items
                .iter()
                .map(ty_from_host_type)
                .collect::<Result<Vec<_>, _>>()?,
        )),
    }
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
                bounds: Vec::new(),
                span: Span::new(0, 0),
            }],
            variants: BTreeMap::from([
                ("Some".to_string(), vec![Ty::Gen(0)]),
                ("None".to_string(), vec![]),
            ]),
        },
    );
    let span0 = Span::new(0, 0);

    let mut add_fn = |name: &str, generics: Vec<GenericParamInfo>, params: Vec<Ty>, ret: Ty| {
        env.functions.insert(
            name.to_string(),
            FnSig {
                name: name.to_string(),
                vis: Visibility::Public { span: span0 },
                defining_module: ModulePath::root(),
                generics,
                params,
                ret,
                span: span0,
            },
        );
    };

    // Built-in inherent methods on primitive types (method-call sugar support).
    //
    // These are thin wrappers around `core::intrinsics::*` and are intentionally explicit
    // (no implicit conversions between `int` and `byte`/`char`).
    // `int` ↔ `byte` / `char`.
    add_fn(
        "int::to_byte",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Int))],
        Ty::Byte,
    );
    env.inherent_method_kinds.insert(
        "int::to_byte".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "int::try_byte",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Int))],
        Ty::App(TyCon::Named("Option".to_string()), vec![Ty::Byte]),
    );
    env.inherent_method_kinds.insert(
        "int::try_byte".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "int::to_char",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Int))],
        Ty::Char,
    );
    env.inherent_method_kinds.insert(
        "int::to_char".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "int::try_char",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Int))],
        Ty::App(TyCon::Named("Option".to_string()), vec![Ty::Char]),
    );
    env.inherent_method_kinds.insert(
        "int::try_char".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "byte::to_int",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Byte))],
        Ty::Int,
    );
    env.inherent_method_kinds.insert(
        "byte::to_int".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "char::to_int",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Char))],
        Ty::Int,
    );
    env.inherent_method_kinds.insert(
        "char::to_int".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    // `bytes`.
    add_fn(
        "bytes::get",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Bytes)), Ty::Int],
        Ty::App(TyCon::Named("Option".to_string()), vec![Ty::Byte]),
    );
    env.inherent_method_kinds.insert(
        "bytes::get".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "bytes::slice",
        Vec::new(),
        vec![
            Ty::Readonly(Box::new(Ty::Bytes)),
            Ty::Int,
            Ty::App(TyCon::Named("Option".to_string()), vec![Ty::Int]),
        ],
        Ty::Bytes,
    );
    env.inherent_method_kinds.insert(
        "bytes::slice".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "bytes::to_array",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Bytes))],
        Ty::Array(Box::new(Ty::Byte)),
    );
    env.inherent_method_kinds.insert(
        "bytes::to_array".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    add_fn(
        "bytes::from_array",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Array(Box::new(Ty::Byte))))],
        Ty::Bytes,
    );
    env.inherent_method_kinds
        .insert("bytes::from_array".to_string(), InherentMethodKind::Static);

    // `string`.
    add_fn(
        "string::slice",
        Vec::new(),
        vec![
            Ty::Readonly(Box::new(Ty::String)),
            Ty::Int,
            Ty::App(TyCon::Named("Option".to_string()), vec![Ty::Int]),
        ],
        Ty::String,
    );
    env.inherent_method_kinds.insert(
        "string::slice".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    // `string` iteration helpers.
    add_fn(
        "string::chars",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::String))],
        Ty::App(
            TyCon::Named("core::intrinsics::StringIter".to_string()),
            Vec::new(),
        ),
    );
    env.inherent_method_kinds.insert(
        "string::chars".to_string(),
        InherentMethodKind::Instance { readonly: true },
    );

    // `string` construction/decoding helpers (static methods).
    add_fn(
        "string::from_chars",
        Vec::new(),
        vec![Ty::Readonly(Box::new(Ty::Array(Box::new(Ty::Char))))],
        Ty::String,
    );
    env.inherent_method_kinds
        .insert("string::from_chars".to_string(), InherentMethodKind::Static);

    add_fn("string::from_utf8", Vec::new(), vec![Ty::Bytes], Ty::String);
    env.inherent_method_kinds
        .insert("string::from_utf8".to_string(), InherentMethodKind::Static);

    add_fn(
        "string::from_utf8_strict",
        Vec::new(),
        vec![Ty::Bytes],
        Ty::App(TyCon::Named("Option".to_string()), vec![Ty::String]),
    );
    env.inherent_method_kinds.insert(
        "string::from_utf8_strict".to_string(),
        InherentMethodKind::Static,
    );

    for (name, ret) in [
        ("string::from_utf16_le", Ty::String),
        (
            "string::from_utf16_le_strict",
            Ty::App(TyCon::Named("Option".to_string()), vec![Ty::String]),
        ),
        ("string::from_utf16_be", Ty::String),
        (
            "string::from_utf16_be_strict",
            Ty::App(TyCon::Named("Option".to_string()), vec![Ty::String]),
        ),
    ] {
        add_fn(
            name,
            Vec::new(),
            vec![Ty::Readonly(Box::new(Ty::Array(Box::new(Ty::Int))))],
            ret,
        );
        env.inherent_method_kinds
            .insert(name.to_string(), InherentMethodKind::Static);
    }

    // Array methods: ergonomic wrappers over `core::intrinsics::array_*`.
    let array_generics = || {
        vec![GenericParamInfo {
            name: "T".to_string(),
            arity: 0,
            bounds: Vec::new(),
            span: span0,
        }]
    };
    let array = |t: Ty| Ty::Array(Box::new(t));
    let ro = |t: Ty| Ty::Readonly(Box::new(t));
    let option = |t: Ty| Ty::App(TyCon::Named("Option".to_string()), vec![t]);

    for (name, params, ret, readonly_receiver) in [
        // Mutating operations.
        (
            "array::push",
            vec![array(Ty::Gen(0)), Ty::Gen(0)],
            Ty::Unit,
            false,
        ),
        (
            "array::pop",
            vec![array(Ty::Gen(0))],
            option(Ty::Gen(0)),
            false,
        ),
        ("array::clear", vec![array(Ty::Gen(0))], Ty::Unit, false),
        (
            "array::insert",
            vec![array(Ty::Gen(0)), Ty::Int, Ty::Gen(0)],
            Ty::Unit,
            false,
        ),
        (
            "array::remove",
            vec![array(Ty::Gen(0)), Ty::Int],
            Ty::Gen(0),
            false,
        ),
        (
            "array::resize",
            vec![array(Ty::Gen(0)), Ty::Int, Ty::Gen(0)],
            Ty::Unit,
            false,
        ),
        (
            "array::extend",
            vec![array(Ty::Gen(0)), array(Ty::Gen(0))],
            Ty::Unit,
            false,
        ),
        // Non-mutating operations.
        (
            "array::slice",
            vec![array(Ty::Gen(0)), Ty::Int, Ty::Int],
            array(Ty::Gen(0)),
            false,
        ),
        (
            "array::slice_ro",
            vec![ro(array(Ty::Gen(0))), Ty::Int, Ty::Int],
            array(ro(Ty::Gen(0))),
            true,
        ),
        (
            "array::concat",
            vec![array(Ty::Gen(0)), array(Ty::Gen(0))],
            array(Ty::Gen(0)),
            false,
        ),
        (
            "array::concat_ro",
            vec![ro(array(Ty::Gen(0))), ro(array(Ty::Gen(0)))],
            array(ro(Ty::Gen(0))),
            true,
        ),
        // Copying helpers.
        (
            "array::copy",
            vec![array(Ty::Gen(0))],
            array(Ty::Gen(0)),
            false,
        ),
        (
            "array::copy_ro",
            vec![ro(array(Ty::Gen(0)))],
            array(ro(Ty::Gen(0))),
            true,
        ),
    ] {
        add_fn(name, array_generics(), params, ret);
        env.inherent_method_kinds.insert(
            name.to_string(),
            InherentMethodKind::Instance {
                readonly: readonly_receiver,
            },
        );
    }

    // Built-in interface impls (used by desugarings).
    let to_string_iface = "core::fmt::ToString";
    for prim in [
        "unit", "bool", "int", "float", "byte", "char", "string", "bytes",
    ] {
        env.interface_impls
            .insert((prim.to_string(), to_string_iface.to_string()));
        env.interface_methods.insert(
            (
                prim.to_string(),
                to_string_iface.to_string(),
                "to_string".to_string(),
            ),
            format!("impl::{to_string_iface}::for::{prim}::to_string"),
        );
    }

    // Built-in `core::ops::*` interface impls for primitives.
    //
    // The language currently does not allow source-authored `impl ... for int`/`bool`/etc, but we
    // still want generic code to be able to use the operator interfaces from `core::ops`.
    let add_ops_impl = |env: &mut ProgramEnv, prim: &str, iface: &str, method: &str| {
        env.interface_impls
            .insert((prim.to_string(), iface.to_string()));
        env.interface_methods.insert(
            (prim.to_string(), iface.to_string(), method.to_string()),
            format!("impl::{iface}::for::{prim}::{method}"),
        );
    };

    // Arithmetic.
    for prim in ["int", "float"] {
        add_ops_impl(env, prim, "core::ops::Add", "add");
        add_ops_impl(env, prim, "core::ops::Sub", "sub");
        add_ops_impl(env, prim, "core::ops::Mul", "mul");
        add_ops_impl(env, prim, "core::ops::Div", "div");
        add_ops_impl(env, prim, "core::ops::Rem", "rem");
        add_ops_impl(env, prim, "core::ops::Neg", "neg");
    }
    // String concatenation.
    add_ops_impl(env, "string", "core::ops::Add", "add");

    // Boolean ops.
    add_ops_impl(env, "bool", "core::ops::Not", "not");

    // Equality.
    for prim in [
        "unit", "bool", "int", "float", "byte", "char", "string", "bytes",
    ] {
        add_ops_impl(env, prim, "core::ops::Eq", "eq");
        add_ops_impl(env, prim, "core::ops::Ne", "ne");
    }

    // Ordering.
    for prim in ["int", "float"] {
        add_ops_impl(env, prim, "core::ops::Lt", "lt");
        add_ops_impl(env, prim, "core::ops::Le", "le");
        add_ops_impl(env, prim, "core::ops::Gt", "gt");
        add_ops_impl(env, prim, "core::ops::Ge", "ge");
    }

    // `core::len::Len` for built-in container types.
    let len_iface = "core::len::Len";
    for type_name in ["bytes", "array"] {
        env.interface_impls
            .insert((type_name.to_string(), len_iface.to_string()));
        env.interface_methods.insert(
            (
                type_name.to_string(),
                len_iface.to_string(),
                "len".to_string(),
            ),
            format!("impl::{len_iface}::for::{type_name}::len"),
        );
    }

    // `core::hash::Hash` for built-in primitive types.
    let hash_iface = "core::hash::Hash";
    for prim in ["unit", "bool", "int", "byte", "char", "string", "bytes"] {
        env.interface_impls
            .insert((prim.to_string(), hash_iface.to_string()));
        env.interface_methods.insert(
            (prim.to_string(), hash_iface.to_string(), "hash".to_string()),
            format!("impl::{hash_iface}::for::{prim}::hash"),
        );
    }
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
    let generics = lower_generic_params(env, module, &item.generics, true)?;
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
            is_newtype: matches!(item.body, crate::ast::StructBody::NewType { .. }),
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
    let generics = lower_generic_params(env, module, &item.generics, true)?;
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
            generics: lower_generic_params(env, module, &item.generics, false)?,
            supers: Vec::new(),
            assoc_types: BTreeMap::new(),
            all_assoc_types: BTreeMap::new(),
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
    generic_prefix: &[GenericParamInfo],
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

    let method_generics =
        lower_generic_params_in_scope(env, module, generic_prefix, &func.generics, true)?;
    let mut generics: Vec<GenericParamInfo> = generic_prefix.to_vec();
    generics.extend(method_generics);
    let scope = GenericScope::new(&generics)?;
    for gp in &generics {
        for bound in &gp.bounds {
            if contains_self_type(bound) {
                return Err(TypeError {
                    message: "`Self` can only be used in interface or instance-method contexts"
                        .to_string(),
                    span: gp.span,
                });
            }
        }
    }

    let mut params = Vec::with_capacity(func.params.len());
    for p in &func.params {
        let ty = lower_type_expr(env, module, &scope, &p.ty)?;
        if contains_self_type(&ty) {
            return Err(TypeError {
                message: "`Self` can only be used in interface or instance-method contexts"
                    .to_string(),
                span: p.ty.span(),
            });
        }
        validate_assoc_projs_in_ty(env, &ty, p.ty.span())?;
        validate_assoc_projs_well_formed_in_ty(env, &generics, &ty, p.ty.span())?;
        validate_nominal_bounds_well_formed_in_ty(env, &generics, &ty, p.ty.span())?;
        validate_value_ty(env, &ty, p.ty.span())?;
        params.push(ty);
    }
    let ret = lower_type_expr(env, module, &scope, &func.ret)?;
    if contains_self_type(&ret) {
        return Err(TypeError {
            message: "`Self` can only be used in interface or instance-method contexts".to_string(),
            span: func.ret.span(),
        });
    }
    validate_assoc_projs_in_ty(env, &ret, func.ret.span())?;
    validate_assoc_projs_well_formed_in_ty(env, &generics, &ret, func.ret.span())?;
    validate_nominal_bounds_well_formed_in_ty(env, &generics, &ret, func.ret.span())?;
    validate_value_ty(env, &ret, func.ret.span())?;

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

#[derive(Clone, Debug)]
struct ExpectedIntrinsicSig {
    generic_count: usize,
    params: Vec<Ty>,
    ret: Ty,
}

fn expected_core_intrinsic_sig(name: &str) -> Option<ExpectedIntrinsicSig> {
    use TyCon::Named;

    let option = |t: Ty| Ty::App(Named("Option".to_string()), vec![t]);
    let array = |t: Ty| Ty::Array(Box::new(t));
    let ro = |t: Ty| Ty::Readonly(Box::new(t));
    let array_iter = |t: Ty| Ty::App(Named("core::intrinsics::ArrayIter".to_string()), vec![t]);
    let string_iter = || Ty::App(Named("core::intrinsics::StringIter".to_string()), vec![]);
    let bytes_iter = || Ty::App(Named("core::intrinsics::BytesIter".to_string()), vec![]);

    Some(match name {
        // f-string helpers.
        "core::intrinsics::string_concat" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::String, Ty::String],
            ret: Ty::String,
        },
        "core::intrinsics::to_string" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![Ty::Gen(0)],
            ret: Ty::String,
        },

        // Panic.
        "core::intrinsics::panic" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::String],
            ret: Ty::Never,
        },

        // Boolean.
        "core::intrinsics::bool_not" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bool],
            ret: Ty::Bool,
        },
        "core::intrinsics::bool_eq" | "core::intrinsics::bool_ne" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bool, Ty::Bool],
            ret: Ty::Bool,
        },

        // Integer arithmetic & comparisons.
        "core::intrinsics::int_add"
        | "core::intrinsics::int_sub"
        | "core::intrinsics::int_mul"
        | "core::intrinsics::int_div"
        | "core::intrinsics::int_mod" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int, Ty::Int],
            ret: Ty::Int,
        },
        "core::intrinsics::int_eq"
        | "core::intrinsics::int_ne"
        | "core::intrinsics::int_lt"
        | "core::intrinsics::int_le"
        | "core::intrinsics::int_gt"
        | "core::intrinsics::int_ge" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int, Ty::Int],
            ret: Ty::Bool,
        },

        // Float arithmetic & comparisons.
        "core::intrinsics::float_add"
        | "core::intrinsics::float_sub"
        | "core::intrinsics::float_mul"
        | "core::intrinsics::float_div"
        | "core::intrinsics::float_mod" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Float, Ty::Float],
            ret: Ty::Float,
        },
        "core::intrinsics::float_eq"
        | "core::intrinsics::float_ne"
        | "core::intrinsics::float_lt"
        | "core::intrinsics::float_le"
        | "core::intrinsics::float_gt"
        | "core::intrinsics::float_ge" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Float, Ty::Float],
            ret: Ty::Bool,
        },

        // Primitive equality helpers.
        "core::intrinsics::string_eq" | "core::intrinsics::string_ne" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::String, Ty::String],
            ret: Ty::Bool,
        },
        "core::intrinsics::bytes_eq" | "core::intrinsics::bytes_ne" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes, Ty::Bytes],
            ret: Ty::Bool,
        },
        "core::intrinsics::unit_eq" | "core::intrinsics::unit_ne" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Unit, Ty::Unit],
            ret: Ty::Bool,
        },

        // Primitive conversions: `int` ↔ `byte` / `char`.
        "core::intrinsics::int_to_byte" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int],
            ret: Ty::Byte,
        },
        "core::intrinsics::int_try_byte" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int],
            ret: option(Ty::Byte),
        },
        "core::intrinsics::byte_to_int" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Byte],
            ret: Ty::Int,
        },
        "core::intrinsics::int_to_char" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int],
            ret: Ty::Char,
        },
        "core::intrinsics::int_try_char" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int],
            ret: option(Ty::Char),
        },
        "core::intrinsics::char_to_int" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Char],
            ret: Ty::Int,
        },

        // `bytes` operations.
        "core::intrinsics::bytes_len" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes],
            ret: Ty::Int,
        },
        "core::intrinsics::bytes_get" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes, Ty::Int],
            ret: option(Ty::Byte),
        },
        "core::intrinsics::bytes_slice" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes, Ty::Int, option(Ty::Int)],
            ret: Ty::Bytes,
        },
        "core::intrinsics::bytes_to_array" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes],
            ret: array(Ty::Byte),
        },
        "core::intrinsics::bytes_from_array" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![ro(array(Ty::Byte))],
            ret: Ty::Bytes,
        },

        // `string` operations.
        "core::intrinsics::string_slice" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::String, Ty::Int, option(Ty::Int)],
            ret: Ty::String,
        },
        "core::intrinsics::string_from_chars" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![ro(array(Ty::Char))],
            ret: Ty::String,
        },
        "core::intrinsics::string_from_utf8" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes],
            ret: Ty::String,
        },
        "core::intrinsics::string_from_utf8_strict" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes],
            ret: option(Ty::String),
        },
        "core::intrinsics::string_from_utf16_le" | "core::intrinsics::string_from_utf16_be" => {
            ExpectedIntrinsicSig {
                generic_count: 0,
                params: vec![ro(array(Ty::Int))],
                ret: Ty::String,
            }
        }
        "core::intrinsics::string_from_utf16_le_strict"
        | "core::intrinsics::string_from_utf16_be_strict" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![ro(array(Ty::Int))],
            ret: option(Ty::String),
        },

        // Hashing.
        "core::intrinsics::hash_int" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int],
            ret: Ty::Int,
        },
        "core::intrinsics::hash_string" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::String],
            ret: Ty::Int,
        },
        "core::intrinsics::hash_bytes" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes],
            ret: Ty::Int,
        },
        "core::intrinsics::hash_combine" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Int, Ty::Int],
            ret: Ty::Int,
        },

        // Iterator protocol.
        "core::intrinsics::into_iter" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0))],
            ret: array_iter(Ty::Gen(0)),
        },
        "core::intrinsics::next" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array_iter(Ty::Gen(0))],
            ret: option(Ty::Gen(0)),
        },
        "core::intrinsics::string_into_iter" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::String],
            ret: string_iter(),
        },
        "core::intrinsics::string_next" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![string_iter()],
            ret: option(Ty::Char),
        },
        "core::intrinsics::bytes_into_iter" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![Ty::Bytes],
            ret: bytes_iter(),
        },
        "core::intrinsics::bytes_next" => ExpectedIntrinsicSig {
            generic_count: 0,
            params: vec![bytes_iter()],
            ret: option(Ty::Byte),
        },

        // Array operations.
        "core::intrinsics::array_len" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0))],
            ret: Ty::Int,
        },
        "core::intrinsics::array_len_ro" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![ro(array(Ty::Gen(0)))],
            ret: Ty::Int,
        },
        "core::intrinsics::array_push" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0)), Ty::Gen(0)],
            ret: Ty::Unit,
        },
        "core::intrinsics::array_pop" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0))],
            ret: option(Ty::Gen(0)),
        },
        "core::intrinsics::array_clear" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0))],
            ret: Ty::Unit,
        },
        "core::intrinsics::array_resize" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0)), Ty::Int, Ty::Gen(0)],
            ret: Ty::Unit,
        },
        "core::intrinsics::array_insert" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0)), Ty::Int, Ty::Gen(0)],
            ret: Ty::Unit,
        },
        "core::intrinsics::array_remove" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0)), Ty::Int],
            ret: Ty::Gen(0),
        },
        "core::intrinsics::array_extend" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0)), array(Ty::Gen(0))],
            ret: Ty::Unit,
        },
        "core::intrinsics::array_concat" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0)), array(Ty::Gen(0))],
            ret: array(Ty::Gen(0)),
        },
        "core::intrinsics::array_concat_ro" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![ro(array(Ty::Gen(0))), ro(array(Ty::Gen(0)))],
            ret: array(ro(Ty::Gen(0))),
        },
        "core::intrinsics::array_slice" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![array(Ty::Gen(0)), Ty::Int, Ty::Int],
            ret: array(Ty::Gen(0)),
        },
        "core::intrinsics::array_slice_ro" => ExpectedIntrinsicSig {
            generic_count: 1,
            params: vec![ro(array(Ty::Gen(0))), Ty::Int, Ty::Int],
            ret: array(ro(Ty::Gen(0))),
        },

        _ => return None,
    })
}

fn declare_intrinsic_fn_sig(
    env: &mut ProgramEnv,
    module: &ModulePath,
    func: &IntrinsicFnItem,
) -> Result<(), TypeError> {
    let name = module.qualify(&func.name.name);
    if env.functions.contains_key(&name) {
        return Err(TypeError {
            message: format!("duplicate function `{name}`"),
            span: func.name.span,
        });
    }

    let Some(expected) = expected_core_intrinsic_sig(&name) else {
        return Err(TypeError {
            message: format!("unknown intrinsic `{name}`"),
            span: func.name.span,
        });
    };

    let generics = lower_generic_params_in_scope(env, module, &[], &func.generics, true)?;
    if generics.len() != expected.generic_count {
        return Err(TypeError {
            message: format!(
                "intrinsic `{name}` generic arity mismatch: expected {}, got {}",
                expected.generic_count,
                generics.len()
            ),
            span: func.span,
        });
    }

    let scope = GenericScope::new(&generics)?;
    for gp in &generics {
        for bound in &gp.bounds {
            if contains_self_type(bound) {
                return Err(TypeError {
                    message: "`Self` can only be used in interface or instance-method contexts"
                        .to_string(),
                    span: gp.span,
                });
            }
        }
    }

    let mut params = Vec::with_capacity(func.params.len());
    for p in &func.params {
        let ty = lower_type_expr(env, module, &scope, &p.ty)?;
        if contains_self_type(&ty) {
            return Err(TypeError {
                message: "`Self` can only be used in interface or instance-method contexts"
                    .to_string(),
                span: p.ty.span(),
            });
        }
        validate_assoc_projs_in_ty(env, &ty, p.ty.span())?;
        validate_assoc_projs_well_formed_in_ty(env, &generics, &ty, p.ty.span())?;
        validate_value_ty(env, &ty, p.ty.span())?;
        params.push(ty);
    }
    let ret = lower_type_expr(env, module, &scope, &func.ret)?;
    if contains_self_type(&ret) {
        return Err(TypeError {
            message: "`Self` can only be used in interface or instance-method contexts".to_string(),
            span: func.ret.span(),
        });
    }
    validate_assoc_projs_in_ty(env, &ret, func.ret.span())?;
    validate_assoc_projs_well_formed_in_ty(env, &generics, &ret, func.ret.span())?;
    validate_value_ty(env, &ret, func.ret.span())?;

    if params != expected.params || ret != expected.ret {
        return Err(TypeError {
            message: format!("invalid signature for intrinsic `{name}`"),
            span: func.span,
        });
    }

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

fn declare_method_sig_with_receiver(
    env: &mut ProgramEnv,
    module: &ModulePath,
    generic_prefix: &[GenericParamInfo],
    method: &FnItem,
    name: String,
    receiver: Ty,
    self_iface: Option<&SelfIfaceCtx>,
) -> Result<(), TypeError> {
    if env.functions.contains_key(&name) {
        return Err(TypeError {
            message: format!("duplicate function `{name}`"),
            span: method.name.span,
        });
    }

    let self_ty = strip_readonly(&receiver).clone();

    let mut method_generics =
        lower_generic_params_in_scope(env, module, generic_prefix, &method.generics, true)?;
    for g in &mut method_generics {
        for bound in &mut g.bounds {
            if contains_self_type(bound) {
                *bound = subst_self_type(bound.clone(), &self_ty);
            }
        }
    }
    let mut generics: Vec<GenericParamInfo> = generic_prefix.to_vec();
    generics.extend(method_generics);
    let scope = GenericScope::new(&generics)?;

    let mut params = Vec::with_capacity(method.params.len() + 1);
    validate_assoc_projs_in_ty(env, &receiver, method.span)?;
    validate_assoc_projs_well_formed_in_ty(env, &generics, &receiver, method.span)?;
    validate_value_ty(env, &receiver, method.span)?;
    params.push(receiver);
    for p in &method.params {
        let mut ty = lower_type_expr_with_self_iface(env, module, &scope, &p.ty, self_iface)?;
        if contains_self_type(&ty) {
            ty = subst_self_type(ty, &self_ty);
        }
        validate_assoc_projs_in_ty(env, &ty, p.ty.span())?;
        validate_assoc_projs_well_formed_in_ty(env, &generics, &ty, p.ty.span())?;
        validate_value_ty(env, &ty, p.ty.span())?;
        params.push(ty);
    }
    let mut ret = lower_type_expr_with_self_iface(env, module, &scope, &method.ret, self_iface)?;
    if contains_self_type(&ret) {
        ret = subst_self_type(ret, &self_ty);
    }
    validate_assoc_projs_in_ty(env, &ret, method.ret.span())?;
    validate_assoc_projs_well_formed_in_ty(env, &generics, &ret, method.ret.span())?;
    validate_value_ty(env, &ret, method.ret.span())?;

    env.functions.insert(
        name.clone(),
        FnSig {
            name,
            vis: method.vis,
            defining_module: module.clone(),
            generics,
            params,
            ret,
            span: method.span,
        },
    );
    Ok(())
}

struct InterfaceDefaultWrapperSigDecl<'a> {
    iface_arity: usize,
    receiver_ty: Ty,
    method_info: &'a InterfaceMethod,
    name: String,
    span: Span,
}

fn declare_synthetic_interface_default_wrapper_sig(
    env: &mut ProgramEnv,
    module: &ModulePath,
    impl_generics: &[GenericParamInfo],
    decl: InterfaceDefaultWrapperSigDecl<'_>,
) -> Result<(), TypeError> {
    let InterfaceDefaultWrapperSigDecl {
        iface_arity,
        receiver_ty,
        method_info,
        name,
        span,
    } = decl;
    if env.functions.contains_key(&name) {
        return Err(TypeError {
            message: format!("duplicate function `{name}`"),
            span,
        });
    }

    let impl_arity = impl_generics.len();
    if impl_arity < iface_arity {
        return Err(TypeError {
            message: "internal error: interface arity exceeds impl arity".to_string(),
            span,
        });
    }
    let shift = impl_arity - iface_arity;
    let self_ty = strip_readonly(&receiver_ty).clone();

    let mut generics: Vec<GenericParamInfo> = impl_generics.to_vec();
    for g in &method_info.sig.generics {
        let bounds = g
            .bounds
            .iter()
            .cloned()
            .map(|b| {
                let b = shift_method_generics(b, iface_arity, shift);
                if contains_self_type(&b) {
                    subst_self_type(b, &self_ty)
                } else {
                    b
                }
            })
            .collect();
        generics.push(GenericParamInfo {
            name: g.name.clone(),
            arity: g.arity,
            bounds,
            span: g.span,
        });
    }

    let mut params = Vec::with_capacity(method_info.sig.params.len() + 1);
    params.push(receiver_ty);
    params.extend(method_info.sig.params.iter().cloned().map(|t| {
        let t = shift_method_generics(t, iface_arity, shift);
        if contains_self_type(&t) {
            subst_self_type(t, &self_ty)
        } else {
            t
        }
    }));
    let mut ret = shift_method_generics(method_info.sig.ret.clone(), iface_arity, shift);
    if contains_self_type(&ret) {
        ret = subst_self_type(ret, &self_ty);
    }

    for ty in &params {
        validate_value_ty(env, ty, span)?;
    }
    validate_value_ty(env, &ret, span)?;

    env.functions.insert(
        name.clone(),
        FnSig {
            name,
            vis: Visibility::Private,
            defining_module: module.clone(),
            generics,
            params,
            ret,
            span,
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
    let mut fields = Vec::new();
    let is_newtype = match &item.body {
        crate::ast::StructBody::Named { fields: decls } => {
            fields.reserve(decls.len());
            for field in decls {
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
                if contains_self_type(&ty) {
                    return Err(TypeError {
                        message: "`Self` is not allowed in struct field types".to_string(),
                        span: field.ty.span(),
                    });
                }
                validate_assoc_projs_in_ty(env, &ty, field.ty.span())?;
                validate_assoc_projs_well_formed_in_ty(env, &generics, &ty, field.ty.span())?;
                validate_nominal_bounds_well_formed_in_ty(env, &generics, &ty, field.ty.span())?;
                validate_value_ty(env, &ty, field.ty.span())?;
                fields.push((field_name, ty));
            }
            false
        }
        crate::ast::StructBody::NewType { inner } => {
            let ty = lower_type_expr(env, module, &scope, inner)?;
            if contains_self_type(&ty) {
                return Err(TypeError {
                    message: "`Self` is not allowed in struct field types".to_string(),
                    span: inner.span(),
                });
            }
            validate_assoc_projs_in_ty(env, &ty, inner.span())?;
            validate_assoc_projs_well_formed_in_ty(env, &generics, &ty, inner.span())?;
            validate_nominal_bounds_well_formed_in_ty(env, &generics, &ty, inner.span())?;
            validate_value_ty(env, &ty, inner.span())?;
            fields.push((".0".to_string(), ty));
            true
        }
    };
    env.structs
        .get_mut(&full_name)
        .expect("declared in first pass")
        .fields = fields;
    env.structs
        .get_mut(&full_name)
        .expect("declared in first pass")
        .is_newtype = is_newtype;
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
            .map(|t| {
                let ty = lower_type_expr(env, module, &scope, t)?;
                if contains_self_type(&ty) {
                    return Err(TypeError {
                        message: "`Self` is not allowed in enum variant field types".to_string(),
                        span: t.span(),
                    });
                }
                validate_assoc_projs_in_ty(env, &ty, t.span())?;
                validate_assoc_projs_well_formed_in_ty(env, &generics, &ty, t.span())?;
                validate_nominal_bounds_well_formed_in_ty(env, &generics, &ty, t.span())?;
                validate_value_ty(env, &ty, t.span())?;
                Ok(ty)
            })
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

    if generics.iter().any(|g| g.arity != 0) {
        return Err(TypeError {
            message: "higher-kinded generics on interfaces are not supported".to_string(),
            span: item.name.span,
        });
    }

    // Resolve and validate super-interfaces.
    let mut supers = Vec::new();
    let mut seen = Vec::<Ty>::new();
    let iface_scope = GenericScope::new(&generics)?;
    for sup in &item.supers {
        let sup_ty = lower_path_type(env, module, &iface_scope, sup)?;
        if contains_self_type(&sup_ty) {
            return Err(TypeError {
                message: "`Self` is not allowed in super-interface types".to_string(),
                span: sup.span,
            });
        }
        let Ty::App(TyCon::Named(name), args) = &sup_ty else {
            return Err(TypeError {
                message: format!("super-interface must be an interface type, got `{sup_ty}`"),
                span: sup.span,
            });
        };
        if !env.interfaces.contains_key(name) {
            return Err(TypeError {
                message: format!("super-interface must be an interface, got `{name}`"),
                span: sup.span,
            });
        }

        // Initial-stage restriction: super-interface type arguments must be a prefix of this
        // interface's own type parameters, in order (avoids type-level specialization).
        for (idx, arg) in args.iter().enumerate() {
            if arg != &Ty::Gen(idx) {
                return Err(TypeError {
                    message: "super-interface type arguments must be the interface's own type parameters (in order)"
                        .to_string(),
                    span: sup.span,
                });
            }
        }

        if !seen.contains(&sup_ty) {
            seen.push(sup_ty.clone());
            supers.push(sup_ty);
        }
    }

    let self_iface_ctx = SelfIfaceCtx {
        iface: full_name.clone(),
        iface_args: (0..generics.len()).map(Ty::Gen).collect(),
    };

    let mut assoc_types = BTreeMap::new();
    let mut methods = BTreeMap::new();
    for member in &item.members {
        match member {
            InterfaceMember::AssocType(decl) => {
                let name = decl.name.name.clone();
                if name == "Self" {
                    return Err(TypeError {
                        message: "`Self` is a reserved type name and cannot be used as an associated type name"
                            .to_string(),
                        span: decl.name.span,
                    });
                }
                if assoc_types
                    .insert(name.clone(), InterfaceAssocTypeDecl {})
                    .is_some()
                {
                    return Err(TypeError {
                        message: format!(
                            "duplicate associated type `{name}` in interface `{full_name}`"
                        ),
                        span: decl.name.span,
                    });
                }
            }
            InterfaceMember::Method(member) => {
                let method_generics =
                    lower_generic_params_in_scope(env, module, &generics, &member.generics, true)?;
                if method_generics.iter().any(|g| g.arity != 0) {
                    return Err(TypeError {
                        message: "higher-kinded generics on interface methods are not supported"
                            .to_string(),
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
                let mut combined = generics.clone();
                combined.extend(method_generics.clone());
                let scope = GenericScope::new(&combined)?;
                let params = member
                    .params
                    .iter()
                    .map(|p| {
                        lower_type_expr_with_self_iface(
                            env,
                            module,
                            &scope,
                            &p.ty,
                            Some(&self_iface_ctx),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let ret = lower_type_expr_with_self_iface(
                    env,
                    module,
                    &scope,
                    &member.ret,
                    Some(&self_iface_ctx),
                )?;
                let default_template = member.body.clone().map(|body| InterfaceDefaultTemplate {
                    name: member.name.clone(),
                    generics: member.generics.clone(),
                    params: member.params.clone(),
                    ret: member.ret.clone(),
                    body,
                    span: member.span,
                });
                methods.insert(
                    mname.clone(),
                    InterfaceMethodDecl {
                        receiver_readonly: member.readonly,
                        has_default: member.body.is_some(),
                        default_template,
                        sig: InterfaceMethodSig {
                            generics: method_generics,
                            params: params.clone(),
                            ret: ret.clone(),
                        },
                    },
                );
            }
        }
    }
    env.interfaces
        .get_mut(&full_name)
        .expect("declared in first pass")
        .assoc_types = assoc_types;
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

    let (supers, own_assoc_types, own_methods, span) = match env.interfaces.get(iface) {
        Some(def) => (
            def.supers.clone(),
            def.assoc_types.clone(),
            def.methods.clone(),
            def.span,
        ),
        None => {
            return Err(TypeError {
                message: format!("internal error: missing interface `{iface}`"),
                span: Span::new(0, 0),
            });
        }
    };

    let iface_arity = env
        .interfaces
        .get(iface)
        .map(|d| d.generics.len())
        .unwrap_or(0);

    for sup in &supers {
        let Ty::App(TyCon::Named(sup_name), _args) = sup else {
            return Err(TypeError {
                message: "internal error: non-nominal super-interface type".to_string(),
                span,
            });
        };
        compute_interface_all_methods(env, sup_name, state, stack)?;
    }

    let mut all: BTreeMap<String, InterfaceMethod> = BTreeMap::new();
    for (name, decl) in own_methods {
        all.insert(
            name,
            InterfaceMethod {
                origin: iface.to_string(),
                receiver_readonly: decl.receiver_readonly,
                has_default: decl.has_default,
                default_template: decl.default_template,
                sig: decl.sig,
            },
        );
    }

    let mut all_assoc: BTreeMap<String, InterfaceAssocType> = BTreeMap::new();
    for (name, _decl) in own_assoc_types {
        all_assoc.insert(
            name,
            InterfaceAssocType {
                origin: iface.to_string(),
            },
        );
    }

    for sup in supers {
        let Ty::App(TyCon::Named(sup_name), sup_args) = sup else {
            return Err(TypeError {
                message: "internal error: non-nominal super-interface type".to_string(),
                span,
            });
        };
        let sup_def = env
            .interfaces
            .get(&sup_name)
            .expect("super-interfaces resolved in fill_interface");
        let sup_arity = sup_def.generics.len();
        if sup_args.len() != sup_arity {
            return Err(TypeError {
                message: "internal error: super-interface arity mismatch".to_string(),
                span,
            });
        }
        let iface_subst: HashMap<GenId, Ty> = sup_args.into_iter().enumerate().collect();

        let super_methods = sup_def.all_methods.clone();
        for (name, meth) in super_methods {
            let rewritten_sig =
                rewrite_interface_method_sig(&meth.sig, &iface_subst, sup_arity, iface_arity);
            let rewritten = InterfaceMethod {
                origin: meth.origin,
                receiver_readonly: meth.receiver_readonly,
                has_default: meth.has_default,
                default_template: meth.default_template,
                sig: rewritten_sig,
            };

            match all.get(&name) {
                None => {
                    all.insert(name, rewritten);
                }
                Some(existing) => {
                    if existing.origin == rewritten.origin {
                        // Diamond duplication: same canonical method, ok.
                        continue;
                    }
                    return Err(TypeError {
                        message: format!(
                            "conflicting inherited method `{name}` in interface `{iface}` (from `{}` and `{}`)",
                            existing.origin, rewritten.origin
                        ),
                        span,
                    });
                }
            }
        }

        let super_assoc_types = sup_def.all_assoc_types.clone();
        for (name, assoc) in super_assoc_types {
            match all_assoc.get(&name) {
                None => {
                    all_assoc.insert(name, assoc);
                }
                Some(existing) => {
                    if existing.origin == assoc.origin {
                        // Diamond duplication: same canonical associated type, ok.
                        continue;
                    }
                    return Err(TypeError {
                        message: format!(
                            "conflicting inherited associated type `{name}` in interface `{iface}` (from `{}` and `{}`)",
                            existing.origin, assoc.origin
                        ),
                        span,
                    });
                }
            }
        }
    }

    env.interfaces.get_mut(iface).expect("exists").all_methods = all;
    env.interfaces
        .get_mut(iface)
        .expect("exists")
        .all_assoc_types = all_assoc;

    stack.pop();
    state.insert(iface.to_string(), InterfaceVisitState::Done);
    Ok(())
}

fn rewrite_interface_method_sig(
    sig: &InterfaceMethodSig,
    iface_subst: &HashMap<GenId, Ty>,
    old_iface_arity: usize,
    new_iface_arity: usize,
) -> InterfaceMethodSig {
    let mut rewritten_generics = Vec::with_capacity(sig.generics.len());
    for g in &sig.generics {
        let bounds = g
            .bounds
            .iter()
            .cloned()
            .map(|b| {
                rewrite_ty_for_inherited_method(b, iface_subst, old_iface_arity, new_iface_arity)
            })
            .collect();
        rewritten_generics.push(GenericParamInfo {
            name: g.name.clone(),
            arity: g.arity,
            bounds,
            span: g.span,
        });
    }

    InterfaceMethodSig {
        generics: rewritten_generics,
        params: sig
            .params
            .iter()
            .cloned()
            .map(|t| {
                rewrite_ty_for_inherited_method(t, iface_subst, old_iface_arity, new_iface_arity)
            })
            .collect(),
        ret: rewrite_ty_for_inherited_method(
            sig.ret.clone(),
            iface_subst,
            old_iface_arity,
            new_iface_arity,
        ),
    }
}

fn rewrite_ty_for_inherited_method(
    ty: Ty,
    iface_subst: &HashMap<GenId, Ty>,
    old_iface_arity: usize,
    new_iface_arity: usize,
) -> Ty {
    match ty {
        Ty::Gen(id) => {
            if id < old_iface_arity {
                iface_subst.get(&id).cloned().unwrap_or(Ty::Gen(id))
            } else {
                let method_id = id - old_iface_arity;
                Ty::Gen(new_iface_arity + method_id)
            }
        }
        Ty::Array(elem) => Ty::Array(Box::new(rewrite_ty_for_inherited_method(
            *elem,
            iface_subst,
            old_iface_arity,
            new_iface_arity,
        ))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .into_iter()
                .map(|t| {
                    rewrite_ty_for_inherited_method(
                        t,
                        iface_subst,
                        old_iface_arity,
                        new_iface_arity,
                    )
                })
                .collect(),
        ),
        Ty::Fn { params, ret } => Ty::Fn {
            params: params
                .into_iter()
                .map(|p| {
                    rewrite_ty_for_inherited_method(
                        p,
                        iface_subst,
                        old_iface_arity,
                        new_iface_arity,
                    )
                })
                .collect(),
            ret: Box::new(rewrite_ty_for_inherited_method(
                *ret,
                iface_subst,
                old_iface_arity,
                new_iface_arity,
            )),
        },
        Ty::Cont { param, ret } => Ty::Cont {
            param: Box::new(rewrite_ty_for_inherited_method(
                *param,
                iface_subst,
                old_iface_arity,
                new_iface_arity,
            )),
            ret: Box::new(rewrite_ty_for_inherited_method(
                *ret,
                iface_subst,
                old_iface_arity,
                new_iface_arity,
            )),
        },
        Ty::Readonly(inner) => Ty::Readonly(Box::new(rewrite_ty_for_inherited_method(
            *inner,
            iface_subst,
            old_iface_arity,
            new_iface_arity,
        ))),
        Ty::App(con, args) => Ty::App(
            rewrite_con_for_inherited_method(con, iface_subst, old_iface_arity, new_iface_arity),
            args.into_iter()
                .map(|a| {
                    rewrite_ty_for_inherited_method(
                        a,
                        iface_subst,
                        old_iface_arity,
                        new_iface_arity,
                    )
                })
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
                .map(|a| {
                    rewrite_ty_for_inherited_method(
                        a,
                        iface_subst,
                        old_iface_arity,
                        new_iface_arity,
                    )
                })
                .collect(),
            assoc_bindings: assoc_bindings
                .into_iter()
                .map(|(name, ty)| {
                    (
                        name,
                        rewrite_ty_for_inherited_method(
                            ty,
                            iface_subst,
                            old_iface_arity,
                            new_iface_arity,
                        ),
                    )
                })
                .collect(),
        },
        Ty::AssocProj {
            iface,
            iface_args,
            assoc,
            self_ty,
        } => Ty::AssocProj {
            iface,
            iface_args: iface_args
                .into_iter()
                .map(|a| {
                    rewrite_ty_for_inherited_method(
                        a,
                        iface_subst,
                        old_iface_arity,
                        new_iface_arity,
                    )
                })
                .collect(),
            assoc,
            self_ty: Box::new(rewrite_ty_for_inherited_method(
                *self_ty,
                iface_subst,
                old_iface_arity,
                new_iface_arity,
            )),
        },
        other => other,
    }
}

fn rewrite_con_for_inherited_method(
    con: TyCon,
    _iface_subst: &HashMap<GenId, Ty>,
    _old_iface_arity: usize,
    _new_iface_arity: usize,
) -> TyCon {
    // Higher-kinded generics are not supported for interfaces in this stage, so there are no
    // interface-level type constructor parameters to substitute/shift.
    con
}

fn pattern_binds_name(pat: &Pattern, name: &str) -> bool {
    match pat {
        Pattern::Wildcard { .. } => false,
        Pattern::Bind { name: ident, .. } => ident.name == name,
        Pattern::Literal { .. } => false,
        Pattern::Tuple {
            prefix,
            rest,
            suffix,
            ..
        } => {
            prefix.iter().any(|p| pattern_binds_name(p, name))
                || suffix.iter().any(|p| pattern_binds_name(p, name))
                || rest
                    .as_ref()
                    .is_some_and(|r| r.binding.as_ref().is_some_and(|ident| ident.name == name))
        }
        Pattern::Enum { fields, .. } => fields.iter().any(|p| pattern_binds_name(p, name)),
        Pattern::Struct { fields, .. } => {
            fields.iter().any(|(_field, p)| pattern_binds_name(p, name))
        }
        Pattern::Ctor { args, .. } => args.iter().any(|p| pattern_binds_name(p, name)),
        Pattern::Array {
            prefix,
            rest,
            suffix,
            ..
        } => {
            prefix.iter().any(|p| pattern_binds_name(p, name))
                || suffix.iter().any(|p| pattern_binds_name(p, name))
                || rest
                    .as_ref()
                    .is_some_and(|r| r.binding.as_ref().is_some_and(|ident| ident.name == name))
        }
    }
}

fn process_impl_item(
    env: &mut ProgramEnv,
    module: &ModulePath,
    item: &ImplItem,
) -> Result<(), TypeError> {
    let impl_generics = lower_generic_params(env, module, &item.generics, true)?;
    let impl_scope = GenericScope::new(&impl_generics)?;

    match &item.header {
        ImplHeader::Inherent { ty, span: _ } => {
            let ty = lower_path_type(env, module, &impl_scope, ty)?;
            validate_nominal_bounds_well_formed_in_ty(env, &impl_generics, &ty, item.span)?;
            let (type_name, type_args) = match ty {
                Ty::App(TyCon::Named(name), args) => (name, args),
                Ty::Iface { iface, .. } => {
                    return Err(TypeError {
                        message: format!(
                            "inherent impl target must be a nominal type, got interface `{iface}{{...}}`"
                        ),
                        span: item.span,
                    });
                }
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

            for member in &item.members {
                let ImplMember::Method(method) = member else {
                    continue;
                };
                let qual_name = format!("{type_name}::{}", method.name.name);
                if method
                    .params
                    .iter()
                    .any(|p| pattern_binds_name(&p.pat, "self"))
                {
                    return Err(TypeError {
                        message: "explicit `self` parameters are not allowed in methods; use the implicit receiver"
                            .to_string(),
                        span: method.span,
                    });
                }

                let FnItemKind::Method { receiver } = method.kind else {
                    return Err(TypeError {
                        message: "internal error: expected method item in impl".to_string(),
                        span: method.span,
                    });
                };

                match receiver {
                    MethodReceiverKind::Static => {
                        declare_function_sig(
                            env,
                            module,
                            &impl_generics,
                            method,
                            Some(qual_name.clone()),
                        )?;
                        env.inherent_method_kinds
                            .insert(qual_name, InherentMethodKind::Static);
                    }
                    MethodReceiverKind::Instance { readonly } => {
                        let mut recv = Ty::App(TyCon::Named(type_name.clone()), type_args.clone());
                        if readonly {
                            recv = Ty::Readonly(Box::new(recv));
                        }
                        declare_method_sig_with_receiver(
                            env,
                            module,
                            &impl_generics,
                            method,
                            qual_name.clone(),
                            recv,
                            None,
                        )?;
                        env.inherent_method_kinds
                            .insert(qual_name, InherentMethodKind::Instance { readonly });
                    }
                }
            }
        }
        ImplHeader::InterfaceForType {
            interface,
            ty,
            span: _,
        } => {
            let iface_ty = lower_path_type(env, module, &impl_scope, interface)?;
            validate_nominal_bounds_well_formed_in_ty(env, &impl_generics, &iface_ty, item.span)?;
            let (iface_name, iface_args) = match iface_ty {
                Ty::App(TyCon::Named(name), args) => (name, args),
                Ty::Iface { iface, .. } => {
                    return Err(TypeError {
                        message: format!(
                            "impl interface type must not specify associated type bindings; write `impl {iface} for ... {{ type ...; }}`"
                        ),
                        span: interface.span,
                    });
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
            let iface_arity = iface_def.generics.len();
            if iface_args.len() != iface_arity {
                return Err(TypeError {
                    message: format!(
                        "interface `{iface_name}` expects {iface_arity} type argument(s), got {}",
                        iface_args.len()
                    ),
                    span: item.span,
                });
            }
            for (idx, arg) in iface_args.iter().enumerate() {
                if arg != &Ty::Gen(idx) {
                    return Err(TypeError {
                        message: "impl interface type arguments must be the impl's type parameters (in order); specialization is not supported yet"
                            .to_string(),
                        span: interface.span,
                    });
                }
            }
            let iface_methods = iface_def.all_methods.clone();
            let iface_def_name = iface_def.name.clone();

            let ty = lower_path_type(env, module, &impl_scope, ty)?;
            validate_nominal_bounds_well_formed_in_ty(env, &impl_generics, &ty, item.span)?;
            let (type_name, type_args) = match ty {
                Ty::App(TyCon::Named(name), args) => (name, args),
                Ty::Iface { iface, .. } => {
                    return Err(TypeError {
                        message: format!(
                            "impl target must be a nominal type, got interface `{iface}{{...}}`"
                        ),
                        span: item.span,
                    });
                }
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

            let self_iface_ctx = SelfIfaceCtx {
                iface: iface_name.clone(),
                iface_args: iface_args.clone(),
            };
            let expected_recv_base = Ty::App(TyCon::Named(type_name.clone()), type_args.clone());

            // Collect associated type definitions + methods.
            let iface_assoc_types = iface_def.all_assoc_types.clone();
            let mut impl_assoc_by_name: BTreeMap<String, (Ty, Span)> = BTreeMap::new();
            let mut impl_methods_by_name: BTreeMap<String, &FnItem> = BTreeMap::new();
            for member in &item.members {
                match member {
                    ImplMember::AssocType(def) => {
                        let aname = def.name.name.clone();
                        if aname == "Self" {
                            return Err(TypeError {
                                message: "`Self` is a reserved type name and cannot be used as an associated type name"
                                    .to_string(),
                                span: def.name.span,
                            });
                        }
                        if impl_assoc_by_name.contains_key(&aname) {
                            return Err(TypeError {
                                message: format!(
                                    "duplicate associated type definition `{aname}` in impl"
                                ),
                                span: def.name.span,
                            });
                        }
                        let mut aty = lower_type_expr_with_self_iface(
                            env,
                            module,
                            &impl_scope,
                            &def.ty,
                            Some(&self_iface_ctx),
                        )?;
                        if contains_self_type(&aty) {
                            aty = subst_self_type(aty, &expected_recv_base);
                        }
                        if contains_assoc_proj(&aty) {
                            return Err(TypeError {
                                message: "associated type definitions must not reference associated types in this stage"
                                    .to_string(),
                                span: def.ty.span(),
                            });
                        }
                        validate_value_ty(env, &aty, def.ty.span())?;
                        validate_nominal_bounds_well_formed_in_ty(
                            env,
                            &impl_generics,
                            &aty,
                            def.ty.span(),
                        )?;
                        impl_assoc_by_name.insert(aname, (aty, def.span));
                    }
                    ImplMember::Method(method) => {
                        let mname = method.name.name.clone();
                        if method
                            .params
                            .iter()
                            .any(|p| pattern_binds_name(&p.pat, "self"))
                        {
                            return Err(TypeError {
                                message: "explicit `self` parameters are not allowed in methods; use the implicit receiver"
                                    .to_string(),
                                span: method.span,
                            });
                        }
                        if let FnItemKind::Method { receiver } = method.kind
                            && matches!(receiver, MethodReceiverKind::Static)
                        {
                            return Err(TypeError {
                                message: "`static fn` is not allowed in interface impl blocks"
                                    .to_string(),
                                span: method.span,
                            });
                        }
                        if impl_methods_by_name.insert(mname.clone(), method).is_some() {
                            return Err(TypeError {
                                message: format!("duplicate method `{mname}` in impl"),
                                span: method.name.span,
                            });
                        }
                    }
                }
            }

            // Validate associated type definitions are complete and contain no extras.
            for aname in iface_assoc_types.keys() {
                if !impl_assoc_by_name.contains_key(aname) {
                    return Err(TypeError {
                        message: format!(
                            "missing associated type `{aname}` required by interface `{iface_def_name}`"
                        ),
                        span: item.span,
                    });
                }
            }
            for (aname, (_aty, aspan)) in &impl_assoc_by_name {
                if !iface_assoc_types.contains_key(aname) {
                    return Err(TypeError {
                        message: format!(
                            "unknown associated type `{aname}` in impl of interface `{iface_def_name}`"
                        ),
                        span: *aspan,
                    });
                }
            }
            let assoc_defs: BTreeMap<String, Ty> = impl_assoc_by_name
                .iter()
                .map(|(name, (ty, _span))| (name.clone(), ty.clone()))
                .collect();
            for (mname, info) in &iface_methods {
                let method = impl_methods_by_name.get(mname).copied();

                let impl_fn_name = format!("impl::{iface_name}::for::{type_name}::{mname}");

                let receiver_ty = if info.receiver_readonly {
                    Ty::Readonly(Box::new(expected_recv_base.clone()))
                } else {
                    expected_recv_base.clone()
                };

                let key = (type_name.clone(), info.origin.clone(), mname.clone());

                let Some(method) = method else {
                    if !info.has_default {
                        return Err(TypeError {
                            message: format!(
                                "missing method `{mname}` required by interface `{}`",
                                iface_def_name
                            ),
                            span: item.span,
                        });
                    }

                    declare_synthetic_interface_default_wrapper_sig(
                        env,
                        module,
                        &impl_generics,
                        InterfaceDefaultWrapperSigDecl {
                            iface_arity,
                            receiver_ty,
                            method_info: info,
                            name: impl_fn_name.clone(),
                            span: item.span,
                        },
                    )?;
                    if let Some(sig) = env.functions.get_mut(&impl_fn_name) {
                        sig.params = sig
                            .params
                            .clone()
                            .into_iter()
                            .map(|t| {
                                subst_assoc_projs_for_impl(t, &expected_recv_base, &assoc_defs)
                            })
                            .collect();
                        sig.ret = subst_assoc_projs_for_impl(
                            sig.ret.clone(),
                            &expected_recv_base,
                            &assoc_defs,
                        );
                        for g in &mut sig.generics {
                            g.bounds = g
                                .bounds
                                .iter()
                                .cloned()
                                .map(|b| {
                                    subst_assoc_projs_for_impl(b, &expected_recv_base, &assoc_defs)
                                })
                                .collect();
                        }
                    }

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
                    continue;
                };

                if method.generics.len() != info.sig.generics.len() {
                    return Err(TypeError {
                        message: format!(
                            "method `{mname}` must declare {} generic parameter(s) to match the interface",
                            info.sig.generics.len()
                        ),
                        span: method.name.span,
                    });
                }
                let FnItemKind::Method { receiver } = method.kind else {
                    return Err(TypeError {
                        message: "internal error: expected method item in impl".to_string(),
                        span: method.span,
                    });
                };
                let MethodReceiverKind::Instance { readonly } = receiver else {
                    return Err(TypeError {
                        message: "internal error: interface impl methods must be instance methods"
                            .to_string(),
                        span: method.span,
                    });
                };
                if readonly != info.receiver_readonly {
                    return Err(TypeError {
                        message: format!(
                            "method `{mname}` receiver mutability does not match interface `{iface_def_name}`"
                        ),
                        span: method.name.span,
                    });
                }

                let expected_params = info.sig.params.len();
                if method.params.len() != expected_params {
                    return Err(TypeError {
                        message: format!(
                            "method `{}` must take {} parameter(s)",
                            mname, expected_params
                        ),
                        span: method.name.span,
                    });
                }

                // Declare the implementing function with a stable internal name.
                declare_method_sig_with_receiver(
                    env,
                    module,
                    &impl_generics,
                    method,
                    impl_fn_name.clone(),
                    receiver_ty,
                    Some(&self_iface_ctx),
                )?;
                if let Some(sig) = env.functions.get_mut(&impl_fn_name) {
                    sig.params = sig
                        .params
                        .clone()
                        .into_iter()
                        .map(|t| subst_assoc_projs_for_impl(t, &expected_recv_base, &assoc_defs))
                        .collect();
                    sig.ret = subst_assoc_projs_for_impl(
                        sig.ret.clone(),
                        &expected_recv_base,
                        &assoc_defs,
                    );
                    for g in &mut sig.generics {
                        g.bounds = g
                            .bounds
                            .iter()
                            .cloned()
                            .map(|b| {
                                subst_assoc_projs_for_impl(b, &expected_recv_base, &assoc_defs)
                            })
                            .collect();
                    }
                }

                // Validate the declared signature matches the interface contract.
                let impl_sig = env
                    .functions
                    .get(&impl_fn_name)
                    .expect("declared just above")
                    .clone();
                let impl_arity = impl_generics.len();
                let expected_iface_arity = iface_arity;
                if impl_arity < expected_iface_arity {
                    return Err(TypeError {
                        message: "internal error: interface arity exceeds impl arity".to_string(),
                        span: item.span,
                    });
                }
                // Receiver type must match the impl target type.
                if let Some(recv_ty) = impl_sig.params.first() {
                    let expected_recv = Ty::App(TyCon::Named(type_name.clone()), type_args.clone());
                    if strip_readonly(recv_ty) != strip_readonly(&expected_recv) {
                        return Err(TypeError {
                            message: format!(
                                "method `{mname}` receiver type mismatch: expected `{expected_recv}`, got `{recv_ty}`"
                            ),
                            span: method.span,
                        });
                    }
                }
                // Non-receiver params + return must match the interface signature, with method
                // generic indices shifted into the impl's generic environment.
                let shift = impl_arity - expected_iface_arity;

                // Method-generic parameters (arity + bounds) must match the interface contract.
                let got_method_generics = impl_sig.generics.get(impl_arity..).unwrap_or(&[]);
                if got_method_generics.len() != info.sig.generics.len() {
                    return Err(TypeError {
                        message: "internal error: impl method generic arity mismatch".to_string(),
                        span: method.name.span,
                    });
                }
                let fmt_bounds = |bounds: &[Ty]| {
                    if bounds.is_empty() {
                        "no bounds".to_string()
                    } else {
                        bounds
                            .iter()
                            .map(|b| b.to_string())
                            .collect::<Vec<_>>()
                            .join(" + ")
                    }
                };
                for (idx, (got, expected)) in got_method_generics
                    .iter()
                    .zip(info.sig.generics.iter())
                    .enumerate()
                {
                    if got.arity != expected.arity {
                        return Err(TypeError {
                            message: format!(
                                "method `{mname}` generic parameter {} arity does not match interface `{iface_def_name}`",
                                idx + 1
                            ),
                            span: method
                                .generics
                                .get(idx)
                                .map(|g| g.span)
                                .unwrap_or(method.name.span),
                        });
                    }

                    let expected_bounds = expected
                        .bounds
                        .iter()
                        .cloned()
                        .map(|b| {
                            let b = shift_method_generics(b, expected_iface_arity, shift);
                            if contains_self_type(&b) {
                                subst_assoc_projs_for_impl(
                                    subst_self_type(b, &expected_recv_base),
                                    &expected_recv_base,
                                    &assoc_defs,
                                )
                            } else {
                                subst_assoc_projs_for_impl(b, &expected_recv_base, &assoc_defs)
                            }
                        })
                        .collect::<Vec<_>>();
                    let bounds_match = expected_bounds.len() == got.bounds.len()
                        && expected_bounds.iter().all(|b| got.bounds.contains(b));
                    if !bounds_match {
                        return Err(TypeError {
                            message: format!(
                                "method `{mname}` generic parameter {} bounds do not match interface `{iface_def_name}`: expected {}, got {}",
                                idx + 1,
                                fmt_bounds(&expected_bounds),
                                fmt_bounds(&got.bounds)
                            ),
                            span: method
                                .generics
                                .get(idx)
                                .map(|g| g.span)
                                .unwrap_or(method.name.span),
                        });
                    }
                }

                let expected_params = info
                    .sig
                    .params
                    .iter()
                    .cloned()
                    .map(|t| {
                        let t = shift_method_generics(t, expected_iface_arity, shift);
                        if contains_self_type(&t) {
                            subst_assoc_projs_for_impl(
                                subst_self_type(t, &expected_recv_base),
                                &expected_recv_base,
                                &assoc_defs,
                            )
                        } else {
                            subst_assoc_projs_for_impl(t, &expected_recv_base, &assoc_defs)
                        }
                    })
                    .collect::<Vec<_>>();
                let got_params = impl_sig.params.iter().skip(1).cloned().collect::<Vec<_>>();
                if expected_params != got_params {
                    return Err(TypeError {
                        message: format!(
                            "method `{mname}` parameter types do not match interface `{iface_def_name}`"
                        ),
                        span: method.name.span,
                    });
                }
                let mut expected_ret =
                    shift_method_generics(info.sig.ret.clone(), expected_iface_arity, shift);
                if contains_self_type(&expected_ret) {
                    expected_ret = subst_assoc_projs_for_impl(
                        subst_self_type(expected_ret, &expected_recv_base),
                        &expected_recv_base,
                        &assoc_defs,
                    );
                } else {
                    expected_ret =
                        subst_assoc_projs_for_impl(expected_ret, &expected_recv_base, &assoc_defs);
                }
                if expected_ret != impl_sig.ret {
                    return Err(TypeError {
                        message: format!(
                            "method `{mname}` return type does not match interface `{iface_def_name}`"
                        ),
                        span: method.name.span,
                    });
                }

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

            // Record associated type definitions for this impl, including transitive supers.
            let mut all_ifaces = BTreeSet::<String>::new();
            collect_interface_and_supers(env, &iface_def_name, &mut all_ifaces);
            for iface in all_ifaces {
                let Some(def) = env.interfaces.get(&iface) else {
                    continue;
                };
                for aname in def.all_assoc_types.keys() {
                    let Some(aty) = assoc_defs.get(aname) else {
                        continue;
                    };
                    let key = (type_name.clone(), iface.clone(), aname.clone());
                    if env.interface_assoc_types.insert(key, aty.clone()).is_some() {
                        return Err(TypeError {
                            message: format!(
                                "overlapping impls for `{type_name}`: duplicate associated type `{iface}::{aname}`"
                            ),
                            span: item.span,
                        });
                    }
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
    allow_bounds: bool,
) -> Result<Vec<GenericParamInfo>, TypeError> {
    lower_generic_params_in_scope(env, module, &[], params, allow_bounds)
}

fn lower_generic_params_in_scope(
    env: &ProgramEnv,
    module: &ModulePath,
    prefix: &[GenericParamInfo],
    params: &[GenericParam],
    allow_bounds: bool,
) -> Result<Vec<GenericParamInfo>, TypeError> {
    let mut combined: Vec<GenericParamInfo> = prefix.to_vec();
    let mut out = Vec::with_capacity(params.len());

    let mut seen = BTreeSet::<String>::new();
    for p in prefix {
        seen.insert(p.name.clone());
    }

    for p in params {
        let name = p.name.name.clone();
        if name == "Self" {
            return Err(TypeError {
                message: "`Self` is a reserved type name and cannot be used as a generic parameter"
                    .to_string(),
                span: p.name.span,
            });
        }
        if !seen.insert(name.clone()) {
            return Err(TypeError {
                message: format!("duplicate generic parameter `{name}`"),
                span: p.name.span,
            });
        }

        if !allow_bounds && !p.bounds.is_empty() {
            return Err(TypeError {
                message: "generic bounds are not supported on `interface` generics yet".to_string(),
                span: p.span,
            });
        }
        if p.arity != 0 && !p.bounds.is_empty() {
            return Err(TypeError {
                message: "bounds on higher-kinded type parameters are not supported".to_string(),
                span: p.span,
            });
        }

        let mut bounds = Vec::<Ty>::new();
        if allow_bounds {
            let scope = GenericScope::new(&combined)?;
            for bound in &p.bounds {
                let bty = lower_path_type(env, module, &scope, bound)?;
                let iface_name = match &bty {
                    Ty::App(TyCon::Named(iface_name), _args) => iface_name.as_str(),
                    Ty::Iface { iface, .. } => iface.as_str(),
                    other => {
                        return Err(TypeError {
                            message: format!(
                                "generic bound must be an interface type, got `{other}`"
                            ),
                            span: bound.span,
                        });
                    }
                };
                if !env.interfaces.contains_key(iface_name) {
                    return Err(TypeError {
                        message: format!(
                            "generic bound must be an interface type, got `{iface_name}`"
                        ),
                        span: bound.span,
                    });
                }
                if !bounds.contains(&bty) {
                    bounds.push(bty);
                }
            }
        }

        let info = GenericParamInfo {
            name,
            arity: p.arity,
            bounds,
            span: p.span,
        };
        combined.push(info.clone());
        out.push(info);
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
    lower_type_expr_with_self_iface(env, module, scope, ty, None)
}

#[derive(Clone, Debug)]
struct SelfIfaceCtx {
    iface: String,
    iface_args: Vec<Ty>,
}

fn lower_type_expr_with_self_iface(
    env: &ProgramEnv,
    module: &ModulePath,
    scope: &GenericScope,
    ty: &TypeExpr,
    self_iface: Option<&SelfIfaceCtx>,
) -> Result<Ty, TypeError> {
    match ty {
        TypeExpr::Readonly { inner, .. } => Ok(Ty::Readonly(Box::new(
            lower_type_expr_with_self_iface(env, module, scope, inner, self_iface)?,
        ))),
        TypeExpr::Prim { prim, .. } => Ok(match prim {
            PrimType::Unit => Ty::Unit,
            PrimType::Never => Ty::Never,
            PrimType::Bool => Ty::Bool,
            PrimType::Int => Ty::Int,
            PrimType::Float => Ty::Float,
            PrimType::Byte => Ty::Byte,
            PrimType::Char => Ty::Char,
            PrimType::String => Ty::String,
            PrimType::Bytes => Ty::Bytes,
        }),
        TypeExpr::Array { elem, .. } => Ok(Ty::Array(Box::new(lower_type_expr_with_self_iface(
            env, module, scope, elem, self_iface,
        )?))),
        TypeExpr::Tuple { items, .. } => {
            let items = items
                .iter()
                .map(|t| lower_type_expr_with_self_iface(env, module, scope, t, self_iface))
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
                .map(|p| lower_type_expr_with_self_iface(env, module, scope, p, self_iface))
                .collect::<Result<Vec<_>, _>>()?;
            let ret = lower_type_expr_with_self_iface(env, module, scope, ret, self_iface)?;
            Ok(Ty::Fn {
                params,
                ret: Box::new(ret),
            })
        }
        TypeExpr::Cont { param, ret, .. } => Ok(Ty::Cont {
            param: Box::new(lower_type_expr_with_self_iface(
                env, module, scope, param, self_iface,
            )?),
            ret: Box::new(lower_type_expr_with_self_iface(
                env, module, scope, ret, self_iface,
            )?),
        }),
        TypeExpr::Path(path) => {
            lower_path_type_with_self_iface(env, module, scope, path, self_iface)
        }
    }
}

fn lower_path_type(
    env: &ProgramEnv,
    module: &ModulePath,
    scope: &GenericScope,
    path: &crate::ast::PathType,
) -> Result<Ty, TypeError> {
    lower_path_type_with_self_iface(env, module, scope, path, None)
}

fn lower_path_type_with_self_iface(
    env: &ProgramEnv,
    module: &ModulePath,
    scope: &GenericScope,
    path: &crate::ast::PathType,
    self_iface: Option<&SelfIfaceCtx>,
) -> Result<Ty, TypeError> {
    if path.segments.is_empty() {
        return Err(TypeError {
            message: "empty type path".to_string(),
            span: path.span,
        });
    }

    // `Self` is a special placeholder type in interface / impl contexts.
    if path.segments.len() == 1 && path.segments[0].name.name == "Self" {
        if !path.assoc_bindings.is_empty() {
            return Err(TypeError {
                message: "`Self` type must not have associated type bindings".to_string(),
                span: path.span,
            });
        }
        if !path.segments[0].args.is_empty() {
            return Err(TypeError {
                message: "`Self` does not take type arguments".to_string(),
                span: path.span,
            });
        }
        return Ok(Ty::SelfType);
    }

    // `Self::Assoc` inside interfaces / their impls.
    if path.segments.len() == 2 && path.segments[0].name.name == "Self" {
        if !path.assoc_bindings.is_empty() {
            return Err(TypeError {
                message: "`Self::...` type paths must not have associated type bindings"
                    .to_string(),
                span: path.span,
            });
        }
        if !path.segments[0].args.is_empty() || !path.segments[1].args.is_empty() {
            return Err(TypeError {
                message: "`Self::Assoc` does not take type arguments".to_string(),
                span: path.span,
            });
        }
        let Some(self_iface) = self_iface else {
            return Err(TypeError {
                message: "`Self::Assoc` can only be used inside interfaces and their impls"
                    .to_string(),
                span: path.span,
            });
        };
        let assoc_name = path.segments[1].name.name.clone();
        return Ok(Ty::AssocProj {
            iface: self_iface.iface.clone(),
            iface_args: self_iface.iface_args.clone(),
            assoc: assoc_name,
            self_ty: Box::new(Ty::SelfType),
        });
    }
    if path.segments.len() > 2 && path.segments[0].name.name == "Self" {
        return Err(TypeError {
            message: "only `Self` and `Self::Assoc` type paths are supported in this stage"
                .to_string(),
            span: path.span,
        });
    }

    // Qualified associated type projection: `Iface<...>::Assoc<T>`.
    if path.segments.len() >= 2 && path.assoc_bindings.is_empty() {
        let assoc_seg = path.segments.last().expect("len >= 2");
        if assoc_seg.args.len() == 1 {
            let iface_segs = &path.segments[..path.segments.len() - 1];
            let iface_raw_segments: Vec<String> =
                iface_segs.iter().map(|s| s.name.name.clone()).collect();
            let iface_arg_count: usize = iface_segs.iter().map(|seg| seg.args.len()).sum();
            let mut iface_args = Vec::with_capacity(iface_arg_count);
            for seg in iface_segs {
                for a in &seg.args {
                    iface_args.push(lower_type_expr_with_self_iface(
                        env, module, scope, a, self_iface,
                    )?);
                }
            }
            if let Some((kind, iface_name)) = env
                .modules
                .try_resolve_type_fqn(module, &iface_raw_segments, path.span)
                .map_err(|e| TypeError {
                    message: e.message,
                    span: e.span,
                })?
                && kind == crate::modules::DefKind::Interface
            {
                let arity = env
                    .interfaces
                    .get(&iface_name)
                    .expect("declared")
                    .generics
                    .len();
                if iface_args.len() != arity {
                    return Err(TypeError {
                        message: format!(
                            "interface `{iface_name}` expects {arity} type argument(s), got {}",
                            iface_args.len()
                        ),
                        span: path.span,
                    });
                }
                let assoc_name = assoc_seg.name.name.clone();
                let self_ty = lower_type_expr_with_self_iface(
                    env,
                    module,
                    scope,
                    &assoc_seg.args[0],
                    self_iface,
                )?;
                return Ok(Ty::AssocProj {
                    iface: iface_name,
                    iface_args,
                    assoc: assoc_name,
                    self_ty: Box::new(self_ty),
                });
            }
        }
    }

    let raw_segments: Vec<String> = path.segments.iter().map(|s| s.name.name.clone()).collect();
    let arg_count: usize = path.segments.iter().map(|seg| seg.args.len()).sum();
    let mut args = Vec::with_capacity(arg_count);
    for seg in &path.segments {
        for a in &seg.args {
            args.push(lower_type_expr_with_self_iface(
                env, module, scope, a, self_iface,
            )?);
        }
    }

    if raw_segments.len() == 1
        && let Some((gen_id, arity)) = scope.lookup(&raw_segments[0])
    {
        if !path.assoc_bindings.is_empty() {
            return Err(TypeError {
                message: "associated type bindings are only allowed on interface types".to_string(),
                span: path.span,
            });
        }
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

    if path.assoc_bindings.is_empty() {
        Ok(Ty::App(TyCon::Named(name), args))
    } else {
        if kind != crate::modules::DefKind::Interface {
            return Err(TypeError {
                message: "associated type bindings are only allowed on interface types".to_string(),
                span: path.span,
            });
        }
        let mut assoc_bindings = BTreeMap::<String, Ty>::new();
        for binding in &path.assoc_bindings {
            let bname = binding.name.name.clone();
            if assoc_bindings.contains_key(&bname) {
                return Err(TypeError {
                    message: format!("duplicate associated type binding `{bname}`"),
                    span: binding.name.span,
                });
            }
            let bty = lower_type_expr_with_self_iface(env, module, scope, &binding.ty, self_iface)?;
            assoc_bindings.insert(bname, bty);
        }
        Ok(Ty::Iface {
            iface: name,
            args,
            assoc_bindings,
        })
    }
}

fn validate_interface_assoc_bindings(
    env: &ProgramEnv,
    iface: &str,
    assoc_bindings: &BTreeMap<String, Ty>,
    span: Span,
) -> Result<(), TypeError> {
    let Some(def) = env.interfaces.get(iface) else {
        return Err(TypeError {
            message: format!("unknown interface `{iface}`"),
            span,
        });
    };

    if def.all_assoc_types.is_empty() {
        return Err(TypeError {
            message: format!(
                "interface `{iface}` does not declare any associated types, so it must not use `{{...}}` bindings"
            ),
            span,
        });
    }

    let mut missing = Vec::new();
    for name in def.all_assoc_types.keys() {
        if !assoc_bindings.contains_key(name) {
            missing.push(name.clone());
        }
    }
    if !missing.is_empty() {
        return Err(TypeError {
            message: format!(
                "interface `{iface}` requires associated type bindings for: {}",
                missing.join(", ")
            ),
            span,
        });
    }

    let mut extra = Vec::new();
    for name in assoc_bindings.keys() {
        if !def.all_assoc_types.contains_key(name) {
            extra.push(name.clone());
        }
    }
    if !extra.is_empty() {
        return Err(TypeError {
            message: format!(
                "unknown associated type binding(s) for interface `{iface}`: {}",
                extra.join(", ")
            ),
            span,
        });
    }

    Ok(())
}

fn validate_value_ty(env: &ProgramEnv, ty: &Ty, span: Span) -> Result<(), TypeError> {
    match ty {
        Ty::Readonly(inner) => validate_value_ty(env, inner, span),
        Ty::Array(elem) => validate_value_ty(env, elem, span),
        Ty::Tuple(items) => {
            for item in items {
                validate_value_ty(env, item, span)?;
            }
            Ok(())
        }
        Ty::Fn { params, ret } => {
            for p in params {
                validate_value_ty(env, p, span)?;
            }
            validate_value_ty(env, ret, span)
        }
        Ty::Cont { param, ret } => {
            validate_value_ty(env, param, span)?;
            validate_value_ty(env, ret, span)
        }
        Ty::App(TyCon::Named(name), args) => {
            for a in args {
                validate_value_ty(env, a, span)?;
            }
            if let Some(def) = env.interfaces.get(name)
                && !def.all_assoc_types.is_empty()
            {
                let required = def
                    .all_assoc_types
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>()
                    .join(", ");
                return Err(TypeError {
                    message: format!(
                        "interface `{name}` has associated types and must be used with bindings: `{name}{{{required} = ...}}`"
                    ),
                    span,
                });
            }
            Ok(())
        }
        Ty::App(_con, args) => {
            for a in args {
                validate_value_ty(env, a, span)?;
            }
            Ok(())
        }
        Ty::Iface {
            iface,
            args,
            assoc_bindings,
        } => {
            for a in args {
                validate_value_ty(env, a, span)?;
            }
            for ty in assoc_bindings.values() {
                validate_value_ty(env, ty, span)?;
            }
            validate_interface_assoc_bindings(env, iface, assoc_bindings, span)
        }
        Ty::AssocProj {
            iface_args,
            self_ty,
            ..
        } => {
            for a in iface_args {
                validate_value_ty(env, a, span)?;
            }
            validate_value_ty(env, self_ty, span)
        }
        Ty::Unit
        | Ty::Never
        | Ty::Bool
        | Ty::Int
        | Ty::Float
        | Ty::Byte
        | Ty::Char
        | Ty::String
        | Ty::Bytes
        | Ty::SelfType
        | Ty::Gen(_)
        | Ty::Var(_) => Ok(()),
    }
}

fn validate_interfaces_after_inheritance(env: &ProgramEnv) -> Result<(), TypeError> {
    for (iface_name, iface_def) in &env.interfaces {
        for (mname, method) in &iface_def.all_methods {
            let mut combined_generics = iface_def.generics.clone();
            combined_generics.extend(method.sig.generics.clone());

            for p in &method.sig.params {
                validate_value_ty(env, p, iface_def.span)?;
                validate_assoc_projs_in_ty(env, p, iface_def.span)?;
                validate_assoc_projs_well_formed_in_ty(env, &combined_generics, p, iface_def.span)?;
                validate_nominal_bounds_well_formed_in_ty(
                    env,
                    &combined_generics,
                    p,
                    iface_def.span,
                )?;
            }
            validate_value_ty(env, &method.sig.ret, iface_def.span)?;
            validate_assoc_projs_in_ty(env, &method.sig.ret, iface_def.span)?;
            validate_assoc_projs_well_formed_in_ty(
                env,
                &combined_generics,
                &method.sig.ret,
                iface_def.span,
            )?;
            validate_nominal_bounds_well_formed_in_ty(
                env,
                &combined_generics,
                &method.sig.ret,
                iface_def.span,
            )?;
            for gp in &method.sig.generics {
                for bound in &gp.bounds {
                    // Bounds are not value types, but bindings (if present) must still be complete.
                    validate_assoc_bindings_in_bound(env, bound, iface_def.span)?;
                    validate_assoc_projs_in_ty(env, bound, iface_def.span)?;
                    validate_assoc_projs_well_formed_in_ty(
                        env,
                        &combined_generics,
                        bound,
                        iface_def.span,
                    )?;
                    validate_nominal_bounds_well_formed_in_ty(
                        env,
                        &combined_generics,
                        bound,
                        iface_def.span,
                    )?;
                }
            }

            let _ = (iface_name, mname);
        }
    }
    Ok(())
}

fn validate_assoc_bindings_in_bound(
    env: &ProgramEnv,
    bound: &Ty,
    span: Span,
) -> Result<(), TypeError> {
    match bound {
        Ty::Iface {
            iface,
            assoc_bindings,
            ..
        } => validate_interface_assoc_bindings(env, iface, assoc_bindings, span),
        Ty::App(..) => Ok(()),
        other => Err(TypeError {
            message: format!("generic bound must be an interface type, got `{other}`"),
            span,
        }),
    }
}

fn validate_assoc_projs_in_ty(env: &ProgramEnv, ty: &Ty, span: Span) -> Result<(), TypeError> {
    match ty {
        Ty::AssocProj {
            iface,
            assoc,
            iface_args,
            self_ty,
        } => {
            let Some(iface_def) = env.interfaces.get(iface) else {
                return Err(TypeError {
                    message: format!("unknown interface `{iface}` in associated type projection"),
                    span,
                });
            };
            if !iface_def.all_assoc_types.contains_key(assoc) {
                return Err(TypeError {
                    message: format!("unknown associated type `{assoc}` in interface `{iface}`"),
                    span,
                });
            }
            for a in iface_args {
                validate_assoc_projs_in_ty(env, a, span)?;
            }
            validate_assoc_projs_in_ty(env, self_ty, span)?;
            Ok(())
        }
        Ty::Iface {
            args,
            assoc_bindings,
            ..
        } => {
            for a in args {
                validate_assoc_projs_in_ty(env, a, span)?;
            }
            for t in assoc_bindings.values() {
                validate_assoc_projs_in_ty(env, t, span)?;
            }
            Ok(())
        }
        Ty::Array(elem) | Ty::Readonly(elem) => validate_assoc_projs_in_ty(env, elem, span),
        Ty::Tuple(items) => {
            for t in items {
                validate_assoc_projs_in_ty(env, t, span)?;
            }
            Ok(())
        }
        Ty::Fn { params, ret } => {
            for p in params {
                validate_assoc_projs_in_ty(env, p, span)?;
            }
            validate_assoc_projs_in_ty(env, ret, span)
        }
        Ty::Cont { param, ret } => {
            validate_assoc_projs_in_ty(env, param, span)?;
            validate_assoc_projs_in_ty(env, ret, span)
        }
        Ty::App(_con, args) => {
            for a in args {
                validate_assoc_projs_in_ty(env, a, span)?;
            }
            Ok(())
        }
        Ty::Unit
        | Ty::Never
        | Ty::Bool
        | Ty::Int
        | Ty::Float
        | Ty::Byte
        | Ty::Char
        | Ty::String
        | Ty::Bytes
        | Ty::SelfType
        | Ty::Gen(_)
        | Ty::Var(_) => Ok(()),
    }
}

fn validate_assoc_projs_well_formed_in_ty(
    env: &ProgramEnv,
    generics: &[GenericParamInfo],
    ty: &Ty,
    span: Span,
) -> Result<(), TypeError> {
    match ty {
        Ty::AssocProj {
            iface,
            iface_args,
            assoc,
            self_ty,
        } => {
            for a in iface_args {
                validate_assoc_projs_well_formed_in_ty(env, generics, a, span)?;
            }
            validate_assoc_projs_well_formed_in_ty(env, generics, self_ty, span)?;

            let iface_ty = Ty::App(TyCon::Named(iface.clone()), iface_args.clone());
            let self_ty_stripped = strip_readonly(self_ty);

            match self_ty_stripped {
                Ty::SelfType => Ok(()),
                Ty::Gen(id) => {
                    let gp = generics.get(*id).ok_or(TypeError {
                        message: "internal error: unknown generic parameter id in assoc projection"
                            .to_string(),
                        span,
                    })?;

                    let mut ok = false;
                    for bound in &gp.bounds {
                        let (bound_iface, bound_args) = match bound {
                            Ty::App(TyCon::Named(bound_iface), bound_args) => {
                                (bound_iface, bound_args)
                            }
                            Ty::Iface { iface, args, .. } => (iface, args),
                            _ => continue,
                        };
                        let Some(args) =
                            infer_super_interface_args(env, bound_iface, bound_args, iface)
                        else {
                            continue;
                        };
                        if args == *iface_args {
                            ok = true;
                            break;
                        }
                    }

                    if ok {
                        Ok(())
                    } else {
                        Err(TypeError {
                            message: format!(
                                "associated type projection `{iface}::{assoc}<...>` requires a bound `{}`: `{iface_ty}`",
                                gp.name
                            ),
                            span,
                        })
                    }
                }
                Ty::App(TyCon::Named(type_name), type_args) => {
                    if env.interfaces.contains_key(type_name) {
                        return Err(TypeError {
                            message: format!(
                                "associated type projection `{iface}::{assoc}<...>` requires a concrete nominal type or a type parameter; got interface value type `{type_name}`"
                            ),
                            span,
                        });
                    }

                    if !env
                        .interface_impls
                        .contains(&(type_name.clone(), iface.clone()))
                    {
                        return Err(TypeError {
                            message: format!("type `{type_name}` does not implement `{iface_ty}`"),
                            span,
                        });
                    }

                    // Initial-stage coherence rule: interface args are a prefix of the concrete type args.
                    if iface_args.len() > type_args.len()
                        || type_args[..iface_args.len()] != *iface_args
                    {
                        return Err(TypeError {
                            message: format!("type `{type_name}` does not implement `{iface_ty}`"),
                            span,
                        });
                    }
                    Ok(())
                }
                other => Err(TypeError {
                    message: format!(
                        "associated type projection `{iface}::{assoc}<...>` requires a concrete nominal type or a type parameter; got `{other}`"
                    ),
                    span,
                }),
            }?;

            Ok(())
        }
        Ty::Iface {
            args,
            assoc_bindings,
            ..
        } => {
            for a in args {
                validate_assoc_projs_well_formed_in_ty(env, generics, a, span)?;
            }
            for t in assoc_bindings.values() {
                validate_assoc_projs_well_formed_in_ty(env, generics, t, span)?;
            }
            Ok(())
        }
        Ty::Array(elem) | Ty::Readonly(elem) => {
            validate_assoc_projs_well_formed_in_ty(env, generics, elem, span)
        }
        Ty::Tuple(items) => {
            for t in items {
                validate_assoc_projs_well_formed_in_ty(env, generics, t, span)?;
            }
            Ok(())
        }
        Ty::Fn { params, ret } => {
            for p in params {
                validate_assoc_projs_well_formed_in_ty(env, generics, p, span)?;
            }
            validate_assoc_projs_well_formed_in_ty(env, generics, ret, span)
        }
        Ty::Cont { param, ret } => {
            validate_assoc_projs_well_formed_in_ty(env, generics, param, span)?;
            validate_assoc_projs_well_formed_in_ty(env, generics, ret, span)
        }
        Ty::App(_con, args) => {
            for a in args {
                validate_assoc_projs_well_formed_in_ty(env, generics, a, span)?;
            }
            Ok(())
        }
        Ty::Unit
        | Ty::Never
        | Ty::Bool
        | Ty::Int
        | Ty::Float
        | Ty::Byte
        | Ty::Char
        | Ty::String
        | Ty::Bytes
        | Ty::SelfType
        | Ty::Gen(_)
        | Ty::Var(_) => Ok(()),
    }
}

fn type_implements_interface_type_in_scope(
    env: &ProgramEnv,
    generics: &[GenericParamInfo],
    ty: &Ty,
    iface_ty: &Ty,
) -> bool {
    let iface_ty = strip_readonly(iface_ty);
    let Ty::App(TyCon::Named(iface_name), iface_args) = iface_ty else {
        return false;
    };
    if !env.interfaces.contains_key(iface_name) {
        return false;
    }

    let ty = strip_readonly(ty);
    match ty {
        // Generic `T` implements `I<...>` iff one of its bounds implies it.
        Ty::Gen(id) => {
            let Some(gp) = generics.get(*id) else {
                return false;
            };
            for bound in &gp.bounds {
                let (bound_iface, bound_args) = match bound {
                    Ty::App(TyCon::Named(bound_iface), bound_args) => (bound_iface, bound_args),
                    Ty::Iface { iface, args, .. } => (iface, args),
                    _ => continue,
                };
                let Some(args) =
                    infer_super_interface_args(env, bound_iface, bound_args, iface_name)
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
        } => infer_super_interface_args(env, type_name, type_args, iface_name)
            .is_some_and(|args| args == *iface_args),

        // Interface value `J<...>` implements `I<...>` iff `J<...>` implies that instantiation.
        Ty::App(TyCon::Named(type_name), type_args) if env.interfaces.contains_key(type_name) => {
            infer_super_interface_args(env, type_name, type_args, iface_name)
                .is_some_and(|args| args == *iface_args)
        }

        // Concrete nominal type: initial-stage coherence => interface args are a prefix of the
        // concrete type args.
        Ty::App(TyCon::Named(type_name), type_args) => {
            if !env
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
            env.interface_impls
                .contains(&(type_name.to_string(), iface_name.to_string()))
        }

        // Arrays are not nominal types, but can have built-in impls (arity-0 only for now).
        Ty::Array(_elem) => {
            if !iface_args.is_empty() {
                return false;
            }
            env.interface_impls
                .contains(&("array".to_string(), iface_name.to_string()))
        }

        _ => false,
    }
}

fn validate_nominal_bounds_well_formed_in_ty(
    env: &ProgramEnv,
    generics: &[GenericParamInfo],
    ty: &Ty,
    span: Span,
) -> Result<(), TypeError> {
    match ty {
        Ty::Array(elem) | Ty::Readonly(elem) => {
            validate_nominal_bounds_well_formed_in_ty(env, generics, elem, span)
        }
        Ty::Tuple(items) => {
            for t in items {
                validate_nominal_bounds_well_formed_in_ty(env, generics, t, span)?;
            }
            Ok(())
        }
        Ty::Fn { params, ret } => {
            for p in params {
                validate_nominal_bounds_well_formed_in_ty(env, generics, p, span)?;
            }
            validate_nominal_bounds_well_formed_in_ty(env, generics, ret, span)
        }
        Ty::Cont { param, ret } => {
            validate_nominal_bounds_well_formed_in_ty(env, generics, param, span)?;
            validate_nominal_bounds_well_formed_in_ty(env, generics, ret, span)
        }
        Ty::Iface {
            args,
            assoc_bindings,
            ..
        } => {
            for a in args {
                validate_nominal_bounds_well_formed_in_ty(env, generics, a, span)?;
            }
            for t in assoc_bindings.values() {
                validate_nominal_bounds_well_formed_in_ty(env, generics, t, span)?;
            }
            Ok(())
        }
        Ty::AssocProj {
            iface_args,
            self_ty,
            ..
        } => {
            for a in iface_args {
                validate_nominal_bounds_well_formed_in_ty(env, generics, a, span)?;
            }
            validate_nominal_bounds_well_formed_in_ty(env, generics, self_ty, span)
        }
        Ty::App(TyCon::Named(type_name), type_args) => {
            for a in type_args {
                validate_nominal_bounds_well_formed_in_ty(env, generics, a, span)?;
            }

            let def_generics = if let Some(def) = env.structs.get(type_name) {
                Some(&def.generics)
            } else if let Some(def) = env.enums.get(type_name) {
                Some(&def.generics)
            } else {
                env.interfaces.get(type_name).map(|def| &def.generics)
            };

            let Some(def_generics) = def_generics else {
                return Ok(());
            };

            // Bounds are expressed in terms of the nominal type's own type parameters (`GenId`),
            // so substitute those with the actual type arguments before checking.
            let ty_subst: HashMap<GenId, Ty> = type_args.iter().cloned().enumerate().collect();
            let con_subst: HashMap<GenId, TyCon> = HashMap::new();

            for (idx, gp) in def_generics.iter().enumerate() {
                if gp.bounds.is_empty() {
                    continue;
                }
                let Some(arg) = type_args.get(idx) else {
                    // Arity is checked elsewhere; this is a defensive fallback.
                    continue;
                };

                for bound in &gp.bounds {
                    let bound = subst_ty(bound.clone(), &ty_subst, &con_subst);
                    if !type_implements_interface_type_in_scope(env, generics, arg, &bound) {
                        return Err(TypeError {
                            message: format!(
                                "type `{arg}` does not satisfy bound `{bound}` for type parameter `{}` of `{type_name}`",
                                gp.name
                            ),
                            span,
                        });
                    }
                }
            }

            Ok(())
        }
        Ty::App(_con, args) => {
            // Type constructor application (`F<T>`) doesn't participate in interface bounds yet.
            for a in args {
                validate_nominal_bounds_well_formed_in_ty(env, generics, a, span)?;
            }
            Ok(())
        }
        Ty::Unit
        | Ty::Never
        | Ty::Bool
        | Ty::Int
        | Ty::Float
        | Ty::Byte
        | Ty::Char
        | Ty::String
        | Ty::Bytes
        | Ty::SelfType
        | Ty::Gen(_)
        | Ty::Var(_) => Ok(()),
    }
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
    type_constraints: HashMap<TypeVarId, Vec<Ty>>,
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

    fn add_constraint(&mut self, var: TypeVarId, bound: Ty) {
        self.type_constraints.entry(var).or_default().push(bound);
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
            Ty::Iface {
                iface,
                args,
                assoc_bindings,
            } => Ty::Iface {
                iface,
                args: args.into_iter().map(|a| self.resolve_ty(a)).collect(),
                assoc_bindings: assoc_bindings
                    .into_iter()
                    .map(|(name, ty)| (name, self.resolve_ty(ty)))
                    .collect(),
            },
            Ty::AssocProj {
                iface,
                iface_args,
                assoc,
                self_ty,
            } => Ty::AssocProj {
                iface,
                iface_args: iface_args.into_iter().map(|a| self.resolve_ty(a)).collect(),
                assoc,
                self_ty: Box::new(self.resolve_ty(*self_ty)),
            },
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
            Ty::Iface {
                args,
                assoc_bindings,
                ..
            } => {
                args.iter().any(|a| self.occurs_in(var, a))
                    || assoc_bindings.values().any(|t| self.occurs_in(var, t))
            }
            Ty::AssocProj {
                iface_args,
                self_ty,
                ..
            } => {
                iface_args.iter().any(|a| self.occurs_in(var, a))
                    || self.occurs_in(var, self_ty.as_ref())
            }
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
            (
                Ty::Iface {
                    iface: a_iface,
                    args: a_args,
                    assoc_bindings: a_bindings,
                },
                Ty::Iface {
                    iface: b_iface,
                    args: b_args,
                    assoc_bindings: b_bindings,
                },
            ) => {
                if a_iface != b_iface {
                    return Err(TypeError {
                        message: format!("type mismatch: expected `{a_iface}`, got `{b_iface}`"),
                        span,
                    });
                }
                if a_args.len() != b_args.len() {
                    return Err(TypeError {
                        message: format!(
                            "type application arity mismatch: expected {}, got {}",
                            a_args.len(),
                            b_args.len()
                        ),
                        span,
                    });
                }
                if a_bindings.len() != b_bindings.len() || a_bindings.keys().ne(b_bindings.keys()) {
                    return Err(TypeError {
                        message: format!("type mismatch: expected `{a_iface}`, got `{b_iface}`"),
                        span,
                    });
                }
                let mut args = Vec::with_capacity(a_args.len());
                for (a, b) in a_args.into_iter().zip(b_args.into_iter()) {
                    args.push(self.unify(a, b, span)?);
                }
                let mut assoc_bindings = BTreeMap::new();
                for (name, a_ty) in a_bindings {
                    let b_ty = b_bindings.get(&name).expect("keys checked");
                    assoc_bindings.insert(name, self.unify(a_ty, b_ty.clone(), span)?);
                }
                Ok(Ty::Iface {
                    iface: a_iface,
                    args,
                    assoc_bindings,
                })
            }
            (
                Ty::AssocProj {
                    iface: a_iface,
                    iface_args: a_iface_args,
                    assoc: a_assoc,
                    self_ty: a_self_ty,
                },
                Ty::AssocProj {
                    iface: b_iface,
                    iface_args: b_iface_args,
                    assoc: b_assoc,
                    self_ty: b_self_ty,
                },
            ) => {
                if a_iface != b_iface || a_assoc != b_assoc {
                    return Err(TypeError {
                        message: format!(
                            "type mismatch: expected `{a_iface}::{a_assoc}<...>`, got `{b_iface}::{b_assoc}<...>`"
                        ),
                        span,
                    });
                }
                if a_iface_args.len() != b_iface_args.len() {
                    return Err(TypeError {
                        message: format!(
                            "type application arity mismatch: expected {}, got {}",
                            a_iface_args.len(),
                            b_iface_args.len()
                        ),
                        span,
                    });
                }
                let mut iface_args = Vec::with_capacity(a_iface_args.len());
                for (a, b) in a_iface_args.into_iter().zip(b_iface_args.into_iter()) {
                    iface_args.push(self.unify(a, b, span)?);
                }
                let self_ty = self.unify(*a_self_ty, *b_self_ty, span)?;
                Ok(Ty::AssocProj {
                    iface: a_iface,
                    iface_args,
                    assoc: a_assoc,
                    self_ty: Box::new(self_ty),
                })
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
    reified_type_args: Vec<Ty>,
}

fn instantiate_fn(sig: &FnSig, infer: &mut InferCtx) -> InstFn {
    let mut ty_subst: HashMap<GenId, Ty> = HashMap::new();
    let mut con_subst: HashMap<GenId, TyCon> = HashMap::new();
    let mut reified_type_args: Vec<Ty> = Vec::new();

    for (idx, gp) in sig.generics.iter().enumerate() {
        let gen_id: GenId = idx;
        if gp.arity == 0 {
            let ty = infer.fresh_type_var();
            let ty_for_args = ty.clone();
            if let Ty::Var(var_id) = ty {
                for bound in &gp.bounds {
                    let bound = subst_ty(bound.clone(), &ty_subst, &con_subst);
                    infer.add_constraint(var_id, bound);
                }
            }
            reified_type_args.push(ty_for_args.clone());
            ty_subst.insert(gen_id, ty_for_args);
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
        reified_type_args,
    }
}

fn instantiate_interface_method_sig(
    sig: &InterfaceMethodSig,
    iface_args: &[Ty],
    iface_arity: usize,
    self_ty: Option<&Ty>,
    infer: &mut InferCtx,
) -> InstFn {
    let mut ty_subst: HashMap<GenId, Ty> = HashMap::new();
    let mut con_subst: HashMap<GenId, TyCon> = HashMap::new();
    let mut reified_type_args: Vec<Ty> = Vec::new();

    for (idx, arg) in iface_args.iter().cloned().enumerate() {
        ty_subst.insert(idx, arg);
    }
    debug_assert_eq!(iface_args.len(), iface_arity);

    // Method generics are appended after the interface generics.
    for (idx, gp) in sig.generics.iter().enumerate() {
        let gen_id: GenId = iface_arity + idx;
        if gp.arity == 0 {
            let ty = infer.fresh_type_var();
            let ty_for_args = ty.clone();
            if let Ty::Var(var_id) = ty {
                // Insert before processing bounds so bounds may mention earlier method generics.
                ty_subst.insert(gen_id, ty_for_args.clone());
                for bound in &gp.bounds {
                    let bound = subst_ty(bound.clone(), &ty_subst, &con_subst);
                    infer.add_constraint(var_id, bound);
                }
            } else {
                ty_subst.insert(gen_id, ty_for_args.clone());
            }
            reified_type_args.push(ty_for_args);
        } else {
            let con = infer.fresh_con_var(gp.arity);
            con_subst.insert(gen_id, con);
        }
    }

    let mut inst = InstFn {
        params: sig
            .params
            .iter()
            .cloned()
            .map(|t| subst_ty(t, &ty_subst, &con_subst))
            .collect(),
        ret: subst_ty(sig.ret.clone(), &ty_subst, &con_subst),
        reified_type_args,
    };

    if let Some(self_ty) = self_ty {
        if inst.params.iter().any(contains_self_type) {
            inst.params = inst
                .params
                .into_iter()
                .map(|t| subst_self_type(t, self_ty))
                .collect();
        }
        if contains_self_type(&inst.ret) {
            inst.ret = subst_self_type(inst.ret, self_ty);
        }
    }

    inst
}

fn subst_ty(ty: Ty, ty_subst: &HashMap<GenId, Ty>, con_subst: &HashMap<GenId, TyCon>) -> Ty {
    match ty {
        Ty::Gen(id) => ty_subst.get(&id).cloned().unwrap_or(Ty::Gen(id)),
        Ty::Var(id) => Ty::Var(id),
        Ty::Array(elem) => Ty::Array(Box::new(subst_ty(*elem, ty_subst, con_subst))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .into_iter()
                .map(|t| subst_ty(t, ty_subst, con_subst))
                .collect(),
        ),
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
        Ty::Iface {
            iface,
            args,
            assoc_bindings,
        } => Ty::Iface {
            iface,
            args: args
                .into_iter()
                .map(|a| subst_ty(a, ty_subst, con_subst))
                .collect(),
            assoc_bindings: assoc_bindings
                .into_iter()
                .map(|(name, ty)| (name, subst_ty(ty, ty_subst, con_subst)))
                .collect(),
        },
        Ty::AssocProj {
            iface,
            iface_args,
            assoc,
            self_ty,
        } => Ty::AssocProj {
            iface,
            iface_args: iface_args
                .into_iter()
                .map(|a| subst_ty(a, ty_subst, con_subst))
                .collect(),
            assoc,
            self_ty: Box::new(subst_ty(*self_ty, ty_subst, con_subst)),
        },
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

                let synthesized =
                    synthesize_default_method_fn_item(method_info.receiver_readonly, template);
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
    Ok(FnTypeInfo {
        expr_types,
        call_type_args,
        method_type_args,
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
            effect_interface_args: HashMap::new(),
            stmt_exprs: HashSet::new(),
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
                if let Ty::App(TyCon::Named(type_name), type_args) = self_ty_stripped
                    && let Some(template) = self
                        .env
                        .interface_assoc_types
                        .get(&(type_name.clone(), iface.clone(), assoc.clone()))
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
            Expr::Int { .. } => Ty::Int,
            Expr::Float { .. } => Ty::Float,
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
                binding,
                iter,
                body,
                span,
            } => self.typecheck_for(binding, iter, body, *span)?,
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
                if contains_infer_vars(arg) {
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
        binding: &Ident,
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
        let recv_ty_resolved = self.infer.resolve_ty(recv_ty.clone());
        if !method_info.receiver_readonly && matches!(recv_ty_resolved, Ty::Readonly(_)) {
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
        if recv_is_iface_object && !interface_method_sig_is_dyn_dispatchable(&method_info.sig) {
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
            Ty::App(TyCon::Named(name), _) if self.env.interfaces.contains_key(name) => None,
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
        self.ensure_implements_interface_type(&recv_ty_resolved, &target_iface, args[0].span())?;

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

    fn typecheck_method_call(
        &mut self,
        receiver: &Expr,
        method: &Ident,
        explicit_type_args: &[TypeExpr],
        args: &[Expr],
        span: Span,
    ) -> Result<Ty, TypeError> {
        let recv_ty = self.typecheck_expr(receiver, ExprUse::Value)?;
        let recv_ty_resolved = self.infer.resolve_ty(recv_ty.clone());
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
        {
            if receiver_is_readonly && !info.receiver_readonly {
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
                if receiver_is_readonly && !info.receiver_readonly {
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
            if !interface_method_sig_is_dyn_dispatchable(&info.sig) {
                let method_id = format!("{}::{}", info.origin, method.name);
                return Err(TypeError {
                    message: format!(
                        "method `{method_id}` is not dynamically dispatchable because it mentions `Self` in its signature; call requires a concrete receiver type"
                    ),
                    span: method.span,
                });
            }
            if receiver_is_readonly && !info.receiver_readonly {
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
            if !interface_method_sig_is_dyn_dispatchable(&info.sig) {
                let method_id = format!("{}::{}", info.origin, method.name);
                return Err(TypeError {
                    message: format!(
                        "method `{method_id}` is not dynamically dispatchable because it mentions `Self` in its signature; call requires a concrete receiver type"
                    ),
                    span: method.span,
                });
            }
            if receiver_is_readonly && !info.receiver_readonly {
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
            if receiver_is_readonly && !method_info.receiver_readonly {
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
        match op {
            UnaryOp::Not => {
                let t_resolved = self.infer.resolve_ty(t.clone());
                if matches!(strip_readonly(&t_resolved), Ty::Bool | Ty::Never) {
                    let _ = self.unify_expected(Ty::Bool, t, span)?;
                    return Ok(Ty::Bool);
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
            UnaryOp::Neg => {
                let t_resolved = self.infer.resolve_ty(t);
                match strip_readonly(&t_resolved) {
                    Ty::Int => Ok(Ty::Int),
                    Ty::Float => Ok(Ty::Float),
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

pub(crate) fn contains_self_type(ty: &Ty) -> bool {
    match ty {
        Ty::SelfType => true,
        Ty::Array(elem) | Ty::Readonly(elem) => contains_self_type(elem),
        Ty::Tuple(items) => items.iter().any(contains_self_type),
        Ty::Fn { params, ret } => params.iter().any(contains_self_type) || contains_self_type(ret),
        Ty::Cont { param, ret } => contains_self_type(param) || contains_self_type(ret),
        Ty::App(_con, args) => args.iter().any(contains_self_type),
        Ty::Iface {
            args,
            assoc_bindings,
            ..
        } => args.iter().any(contains_self_type) || assoc_bindings.values().any(contains_self_type),
        Ty::AssocProj {
            iface_args,
            self_ty,
            ..
        } => iface_args.iter().any(contains_self_type) || contains_self_type(self_ty),
        Ty::Gen(_)
        | Ty::Var(_)
        | Ty::Unit
        | Ty::Never
        | Ty::Bool
        | Ty::Int
        | Ty::Float
        | Ty::Byte
        | Ty::Char
        | Ty::String
        | Ty::Bytes => false,
    }
}

fn contains_naked_self_type(ty: &Ty) -> bool {
    match ty {
        Ty::SelfType => true,
        Ty::Array(elem) | Ty::Readonly(elem) => contains_naked_self_type(elem),
        Ty::Tuple(items) => items.iter().any(contains_naked_self_type),
        Ty::Fn { params, ret } => {
            params.iter().any(contains_naked_self_type) || contains_naked_self_type(ret)
        }
        Ty::Cont { param, ret } => contains_naked_self_type(param) || contains_naked_self_type(ret),
        Ty::App(_con, args) => args.iter().any(contains_naked_self_type),
        Ty::Iface {
            args,
            assoc_bindings,
            ..
        } => {
            args.iter().any(contains_naked_self_type)
                || assoc_bindings.values().any(contains_naked_self_type)
        }
        Ty::AssocProj {
            iface_args,
            self_ty,
            ..
        } => {
            iface_args.iter().any(contains_naked_self_type)
                || match self_ty.as_ref() {
                    // `Self::Assoc` is allowed for dyn dispatch; it becomes concrete once
                    // associated types are bound on the interface value type.
                    Ty::SelfType => false,
                    other => contains_naked_self_type(other),
                }
        }
        Ty::Gen(_)
        | Ty::Var(_)
        | Ty::Unit
        | Ty::Never
        | Ty::Bool
        | Ty::Int
        | Ty::Float
        | Ty::Byte
        | Ty::Char
        | Ty::String
        | Ty::Bytes => false,
    }
}

fn contains_assoc_proj(ty: &Ty) -> bool {
    match ty {
        Ty::AssocProj { .. } => true,
        Ty::Array(elem) | Ty::Readonly(elem) => contains_assoc_proj(elem),
        Ty::Tuple(items) => items.iter().any(contains_assoc_proj),
        Ty::Fn { params, ret } => {
            params.iter().any(contains_assoc_proj) || contains_assoc_proj(ret)
        }
        Ty::Cont { param, ret } => contains_assoc_proj(param) || contains_assoc_proj(ret),
        Ty::App(_con, args) => args.iter().any(contains_assoc_proj),
        Ty::Iface {
            args,
            assoc_bindings,
            ..
        } => {
            args.iter().any(contains_assoc_proj) || assoc_bindings.values().any(contains_assoc_proj)
        }
        Ty::SelfType
        | Ty::Gen(_)
        | Ty::Var(_)
        | Ty::Unit
        | Ty::Never
        | Ty::Bool
        | Ty::Int
        | Ty::Float
        | Ty::Byte
        | Ty::Char
        | Ty::String
        | Ty::Bytes => false,
    }
}

fn subst_assoc_projs_for_impl(ty: Ty, self_ty: &Ty, assoc_defs: &BTreeMap<String, Ty>) -> Ty {
    match ty {
        Ty::AssocProj {
            iface,
            iface_args,
            assoc,
            self_ty: proj_self_ty,
        } => {
            if proj_self_ty.as_ref() == self_ty {
                assoc_defs.get(&assoc).cloned().unwrap_or(Ty::AssocProj {
                    iface,
                    iface_args,
                    assoc,
                    self_ty: proj_self_ty,
                })
            } else {
                Ty::AssocProj {
                    iface,
                    iface_args: iface_args
                        .into_iter()
                        .map(|a| subst_assoc_projs_for_impl(a, self_ty, assoc_defs))
                        .collect(),
                    assoc,
                    self_ty: Box::new(subst_assoc_projs_for_impl(
                        *proj_self_ty,
                        self_ty,
                        assoc_defs,
                    )),
                }
            }
        }
        Ty::Array(elem) => Ty::Array(Box::new(subst_assoc_projs_for_impl(
            *elem, self_ty, assoc_defs,
        ))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .into_iter()
                .map(|t| subst_assoc_projs_for_impl(t, self_ty, assoc_defs))
                .collect(),
        ),
        Ty::Fn { params, ret } => Ty::Fn {
            params: params
                .into_iter()
                .map(|p| subst_assoc_projs_for_impl(p, self_ty, assoc_defs))
                .collect(),
            ret: Box::new(subst_assoc_projs_for_impl(*ret, self_ty, assoc_defs)),
        },
        Ty::Cont { param, ret } => Ty::Cont {
            param: Box::new(subst_assoc_projs_for_impl(*param, self_ty, assoc_defs)),
            ret: Box::new(subst_assoc_projs_for_impl(*ret, self_ty, assoc_defs)),
        },
        Ty::Readonly(inner) => Ty::Readonly(Box::new(subst_assoc_projs_for_impl(
            *inner, self_ty, assoc_defs,
        ))),
        Ty::App(con, args) => Ty::App(
            con,
            args.into_iter()
                .map(|a| subst_assoc_projs_for_impl(a, self_ty, assoc_defs))
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
                .map(|a| subst_assoc_projs_for_impl(a, self_ty, assoc_defs))
                .collect(),
            assoc_bindings: assoc_bindings
                .into_iter()
                .map(|(name, ty)| (name, subst_assoc_projs_for_impl(ty, self_ty, assoc_defs)))
                .collect(),
        },
        other => other,
    }
}

pub(crate) fn subst_self_type(ty: Ty, self_ty: &Ty) -> Ty {
    match ty {
        Ty::SelfType => self_ty.clone(),
        Ty::Array(elem) => Ty::Array(Box::new(subst_self_type(*elem, self_ty))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .into_iter()
                .map(|t| subst_self_type(t, self_ty))
                .collect(),
        ),
        Ty::Fn { params, ret } => Ty::Fn {
            params: params
                .into_iter()
                .map(|p| subst_self_type(p, self_ty))
                .collect(),
            ret: Box::new(subst_self_type(*ret, self_ty)),
        },
        Ty::Cont { param, ret } => Ty::Cont {
            param: Box::new(subst_self_type(*param, self_ty)),
            ret: Box::new(subst_self_type(*ret, self_ty)),
        },
        Ty::Readonly(inner) => Ty::Readonly(Box::new(subst_self_type(*inner, self_ty))),
        Ty::App(con, args) => Ty::App(
            con,
            args.into_iter()
                .map(|a| subst_self_type(a, self_ty))
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
                .map(|a| subst_self_type(a, self_ty))
                .collect(),
            assoc_bindings: assoc_bindings
                .into_iter()
                .map(|(name, ty)| (name, subst_self_type(ty, self_ty)))
                .collect(),
        },
        Ty::AssocProj {
            iface,
            iface_args,
            assoc,
            self_ty: proj_self_ty,
        } => Ty::AssocProj {
            iface,
            iface_args: iface_args
                .into_iter()
                .map(|a| subst_self_type(a, self_ty))
                .collect(),
            assoc,
            self_ty: Box::new(subst_self_type(*proj_self_ty, self_ty)),
        },
        other => other,
    }
}

pub(crate) fn interface_method_sig_is_dyn_dispatchable(sig: &InterfaceMethodSig) -> bool {
    if contains_naked_self_type(&sig.ret) {
        return false;
    }
    if sig.params.iter().any(contains_naked_self_type) {
        return false;
    }
    for g in &sig.generics {
        if g.bounds.iter().any(contains_naked_self_type) {
            return false;
        }
    }
    true
}

fn interface_method_sig_mentions_self_or_assoc(sig: &InterfaceMethodSig) -> bool {
    if contains_naked_self_type(&sig.ret) {
        return true;
    }
    if sig.params.iter().any(contains_naked_self_type) {
        return true;
    }
    for g in &sig.generics {
        if g.bounds.iter().any(contains_naked_self_type) {
            return true;
        }
    }
    if contains_assoc_proj(&sig.ret) {
        return true;
    }
    if sig.params.iter().any(contains_assoc_proj) {
        return true;
    }
    for g in &sig.generics {
        if g.bounds.iter().any(contains_assoc_proj) {
            return true;
        }
    }
    false
}

fn nominal_type_name(ty: &Ty) -> Option<&str> {
    match ty {
        Ty::Readonly(inner) => nominal_type_name(inner),
        Ty::Array(_) => Some("array"),
        Ty::Unit => Some("unit"),
        Ty::Bool => Some("bool"),
        Ty::Int => Some("int"),
        Ty::Float => Some("float"),
        Ty::Byte => Some("byte"),
        Ty::Char => Some("char"),
        Ty::String => Some("string"),
        Ty::Bytes => Some("bytes"),
        Ty::App(TyCon::Named(name), _) => Some(name.as_str()),
        _ => None,
    }
}

fn strip_readonly(ty: &Ty) -> &Ty {
    match ty {
        Ty::Readonly(inner) => inner.as_ref(),
        other => other,
    }
}

fn infer_super_interface_args(
    env: &ProgramEnv,
    sub_iface: &str,
    sub_args: &[Ty],
    target_iface: &str,
) -> Option<Vec<Ty>> {
    if sub_iface == target_iface {
        return Some(sub_args.to_vec());
    }

    let sub_def = env.interfaces.get(sub_iface)?;
    if sub_args.len() != sub_def.generics.len() {
        return None;
    }

    let ty_subst: HashMap<GenId, Ty> = sub_args.iter().cloned().enumerate().collect();
    let con_subst: HashMap<GenId, TyCon> = HashMap::new();

    for sup in &sub_def.supers {
        let instantiated = subst_ty(sup.clone(), &ty_subst, &con_subst);
        let Ty::App(TyCon::Named(name), args) = instantiated else {
            continue;
        };
        if let Some(found) = infer_super_interface_args(env, &name, &args, target_iface) {
            return Some(found);
        }
    }

    None
}

fn shift_method_generics(ty: Ty, iface_arity: usize, shift: usize) -> Ty {
    if shift == 0 {
        return ty;
    }

    match ty {
        Ty::Gen(id) if id >= iface_arity => Ty::Gen(id + shift),
        Ty::Array(elem) => Ty::Array(Box::new(shift_method_generics(*elem, iface_arity, shift))),
        Ty::Tuple(items) => Ty::Tuple(
            items
                .into_iter()
                .map(|t| shift_method_generics(t, iface_arity, shift))
                .collect(),
        ),
        Ty::Fn { params, ret } => Ty::Fn {
            params: params
                .into_iter()
                .map(|p| shift_method_generics(p, iface_arity, shift))
                .collect(),
            ret: Box::new(shift_method_generics(*ret, iface_arity, shift)),
        },
        Ty::Cont { param, ret } => Ty::Cont {
            param: Box::new(shift_method_generics(*param, iface_arity, shift)),
            ret: Box::new(shift_method_generics(*ret, iface_arity, shift)),
        },
        Ty::Readonly(inner) => {
            Ty::Readonly(Box::new(shift_method_generics(*inner, iface_arity, shift)))
        }
        Ty::App(con, args) => Ty::App(
            con,
            args.into_iter()
                .map(|a| shift_method_generics(a, iface_arity, shift))
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
                .map(|a| shift_method_generics(a, iface_arity, shift))
                .collect(),
            assoc_bindings: assoc_bindings
                .into_iter()
                .map(|(name, ty)| (name, shift_method_generics(ty, iface_arity, shift)))
                .collect(),
        },
        Ty::AssocProj {
            iface,
            iface_args,
            assoc,
            self_ty,
        } => Ty::AssocProj {
            iface,
            iface_args: iface_args
                .into_iter()
                .map(|a| shift_method_generics(a, iface_arity, shift))
                .collect(),
            assoc,
            self_ty: Box::new(shift_method_generics(*self_ty, iface_arity, shift)),
        },
        other => other,
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
        let Ty::App(TyCon::Named(name), _args) = sup else {
            continue;
        };
        collect_interface_and_supers(env, name, out);
    }
}
