use std::path::PathBuf;

use core::marker::PhantomData;

/// A host function signature used by the compiler front-end.
///
/// Host signatures are used for:
/// - validating declared host imports in user code and sysroot modules
/// - lowering host calls into MIR/bytecode with an ABI-safe boundary
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostFnSig {
    /// Parameter types.
    pub params: Vec<HostType>,
    /// Return type.
    pub ret: HostType,
}

/// A typed delimited continuation marker (`cont(P) -> R`) for host signatures.
///
/// This is a compile-time-only helper used by [`HostFnSig::of`] and the host module builder APIs.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Cont<P, R>(PhantomData<fn(P) -> R>);

/// A value type available at the VM/host boundary.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HostType {
    /// A dynamically typed value. Used when a more precise type is not available.
    Any,
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
    /// A one-shot delimited continuation value (`cont(P) -> R`).
    ///
    /// This is typechecked at compile time, but lowers to an opaque ABI handle at the VM/host
    /// boundary.
    Cont {
        param: Box<HostType>,
        ret: Box<HostType>,
    },
    /// A runtime type representation (`typerep`), used by some core intrinsics.
    TypeRep,
    Array(Box<HostType>),
    Tuple(Vec<HostType>),
}

/// A Rust type that can be used at the bytecode v0 host ABI boundary.
///
/// This is intentionally restricted to ABI-safe primitives plus typed continuations.
pub trait HostTypeOf {
    fn host_type() -> HostType;
}

impl HostTypeOf for () {
    fn host_type() -> HostType {
        HostType::Unit
    }
}

impl HostTypeOf for bool {
    fn host_type() -> HostType {
        HostType::Bool
    }
}

impl HostTypeOf for i64 {
    fn host_type() -> HostType {
        HostType::Int
    }
}

impl HostTypeOf for f64 {
    fn host_type() -> HostType {
        HostType::Float
    }
}

impl HostTypeOf for String {
    fn host_type() -> HostType {
        HostType::String
    }
}

impl HostTypeOf for Vec<u8> {
    fn host_type() -> HostType {
        HostType::Bytes
    }
}

impl<P, R> HostTypeOf for Cont<P, R>
where
    P: HostTypeOf,
    R: HostTypeOf,
{
    fn host_type() -> HostType {
        HostType::Cont {
            param: Box::new(P::host_type()),
            ret: Box::new(R::host_type()),
        }
    }
}

/// A Rust type that maps to a host parameter list.
///
/// Implemented for tuples up to a fixed arity (currently 8).
pub trait HostParamTypes {
    fn host_param_types() -> Vec<HostType>;
}

impl HostParamTypes for () {
    fn host_param_types() -> Vec<HostType> {
        Vec::new()
    }
}

macro_rules! impl_host_param_types_tuple {
    ($( $t:ident ),+ $(,)?) => {
        impl<$( $t ),+> HostParamTypes for ( $( $t, )+ )
        where
            $( $t: HostTypeOf ),+
        {
            fn host_param_types() -> Vec<HostType> {
                vec![ $( $t::host_type(), )+ ]
            }
        }
    };
}

impl_host_param_types_tuple!(T0);
impl_host_param_types_tuple!(T0, T1);
impl_host_param_types_tuple!(T0, T1, T2);
impl_host_param_types_tuple!(T0, T1, T2, T3);
impl_host_param_types_tuple!(T0, T1, T2, T3, T4);
impl_host_param_types_tuple!(T0, T1, T2, T3, T4, T5);
impl_host_param_types_tuple!(T0, T1, T2, T3, T4, T5, T6);
impl_host_param_types_tuple!(T0, T1, T2, T3, T4, T5, T6, T7);

/// A Rust type that maps to a host return type.
pub trait HostReturnType {
    fn host_return_type() -> HostType;
}

impl<T> HostReturnType for T
where
    T: HostTypeOf,
{
    fn host_return_type() -> HostType {
        T::host_type()
    }
}

/// Visibility of a host-declared item (function/module).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostVisibility {
    /// Not exported; only usable within the declaring module.
    Private,
    /// Exported; usable from other modules.
    Public,
}

/// A declared host function in a host module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostFunctionDecl {
    /// Function visibility.
    pub visibility: HostVisibility,
    /// Module-local function name (e.g. `"println"`).
    pub name: String,
    /// Function signature.
    pub sig: HostFnSig,
}

/// A host module declaration made available to the compiler during compilation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostModuleDecl {
    /// Module visibility.
    pub visibility: HostVisibility,
    /// Functions declared by this module.
    pub functions: Vec<HostFunctionDecl>,
}

/// Builder for [`HostModuleDecl`].
#[derive(Clone, Debug)]
pub struct HostModuleBuilder {
    visibility: HostVisibility,
    functions: Vec<HostFunctionDecl>,
}

impl HostFnSig {
    /// Constructs a host signature from Rust types.
    ///
    /// This is intentionally restricted to ABI-safe types (bytecode v0).
    pub fn of<Args, Ret>() -> Self
    where
        Args: HostParamTypes,
        Ret: HostReturnType,
    {
        Self {
            params: Args::host_param_types(),
            ret: Ret::host_return_type(),
        }
    }
}

impl HostModuleDecl {
    /// Starts building a public host module declaration.
    pub fn public() -> HostModuleBuilder {
        HostModuleBuilder {
            visibility: HostVisibility::Public,
            functions: Vec::new(),
        }
    }

    /// Starts building a private host module declaration.
    pub fn private() -> HostModuleBuilder {
        HostModuleBuilder {
            visibility: HostVisibility::Private,
            functions: Vec::new(),
        }
    }
}

impl HostModuleBuilder {
    /// Adds a function declaration with visibility defaulting to the module visibility.
    pub fn function<Args, Ret>(self, name: impl Into<String>) -> Self
    where
        Args: HostParamTypes,
        Ret: HostReturnType,
    {
        let visibility = self.visibility;
        self.function_with_visibility::<Args, Ret>(visibility, name)
    }

    /// Adds a function declaration with explicit visibility.
    pub fn function_with_visibility<Args, Ret>(
        mut self,
        visibility: HostVisibility,
        name: impl Into<String>,
    ) -> Self
    where
        Args: HostParamTypes,
        Ret: HostReturnType,
    {
        self.functions.push(HostFunctionDecl {
            visibility,
            name: name.into(),
            sig: HostFnSig::of::<Args, Ret>(),
        });
        self
    }

    /// Completes the builder.
    pub fn build(self) -> HostModuleDecl {
        HostModuleDecl {
            visibility: self.visibility,
            functions: self.functions,
        }
    }
}

/// A declaration of an externalized effect operation (for bytecode v0).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalEffectDecl {
    /// Effect interface name (e.g. `"console::IO"`).
    pub interface: String,
    /// Effect method name (e.g. `"print"`).
    pub method: String,
    /// Operation signature.
    pub sig: HostFnSig,
}

/// Compilation options for the script front-end.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompileOptions {
    /// Peephole optimization level to apply to generated bytecode.
    pub opt_level: rusk_bytecode::OptLevel,
    /// Optional sysroot directory. When not set, the compiler tries:
    /// - `$RUSK_SYSROOT`
    /// - `./sysroot` (current working directory)
    /// - `../../sysroot` relative to the `rusk-compiler` crate directory
    pub sysroot: Option<PathBuf>,
    /// Whether to load `sysroot/std` (if present).
    pub load_std: bool,
    /// Declared host modules available for import (name -> declaration).
    pub host_modules: Vec<(String, HostModuleDecl)>,
    /// Declared external effects available for `perform`/`resume` at runtime.
    pub external_effects: Vec<ExternalEffectDecl>,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            opt_level: rusk_bytecode::OptLevel::default(),
            sysroot: None,
            load_std: true,
            host_modules: Vec::new(),
            external_effects: Vec::new(),
        }
    }
}

impl CompileOptions {
    /// Registers a host module declaration under `module_name`.
    ///
    /// Host modules are used to back sysroot-provided functionality and/or embedder-provided APIs.
    pub fn register_host_module(
        &mut self,
        module_name: impl Into<String>,
        module: HostModuleDecl,
    ) -> Result<(), String> {
        let module_name = module_name.into();
        if module_name.is_empty() {
            return Err("host module name cannot be empty".to_string());
        }
        if module_name.contains("::") {
            return Err(format!(
                "nested host modules are not supported: `{module_name}` contains `::`"
            ));
        }
        if self
            .host_modules
            .iter()
            .any(|(name, _)| name == &module_name)
        {
            return Err(format!("duplicate host module `{module_name}`"));
        }
        self.host_modules.push((module_name, module));
        Ok(())
    }

    /// Registers an external effect operation declaration.
    ///
    /// The compiler uses this to assign stable `EffectId`s and to validate ABI safety for the
    /// bytecode v0 boundary.
    pub fn register_external_effect(
        &mut self,
        interface: impl Into<String>,
        method: impl Into<String>,
        sig: HostFnSig,
    ) -> Result<(), String> {
        let interface = interface.into();
        let method = method.into();
        if interface.is_empty() {
            return Err("external effect interface name cannot be empty".to_string());
        }
        if method.is_empty() {
            return Err("external effect method name cannot be empty".to_string());
        }
        if self
            .external_effects
            .iter()
            .any(|e| e.interface == interface && e.method == method)
        {
            return Err(format!(
                "duplicate external effect declaration `{interface}.{method}`"
            ));
        }
        self.external_effects.push(ExternalEffectDecl {
            interface,
            method,
            sig,
        });
        Ok(())
    }

    /// Registers an external effect operation declaration from Rust types.
    pub fn register_external_effect_typed<Args, Ret>(
        &mut self,
        interface: impl Into<String>,
        method: impl Into<String>,
    ) -> Result<(), String>
    where
        Args: HostParamTypes,
        Ret: HostReturnType,
    {
        self.register_external_effect(interface, method, HostFnSig::of::<Args, Ret>())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn host_fn_sig_of_primitives() {
        let sig = HostFnSig::of::<(i64, bool), ()>();
        assert_eq!(
            sig,
            HostFnSig {
                params: vec![HostType::Int, HostType::Bool],
                ret: HostType::Unit
            }
        );
    }

    #[test]
    fn host_fn_sig_of_typed_continuation() {
        let sig = HostFnSig::of::<(Cont<i64, bool>,), ()>();
        assert_eq!(
            sig,
            HostFnSig {
                params: vec![HostType::Cont {
                    param: Box::new(HostType::Int),
                    ret: Box::new(HostType::Bool),
                }],
                ret: HostType::Unit,
            }
        );
    }

    #[test]
    fn host_module_builder_defaults_function_visibility() {
        let module = HostModuleDecl::public()
            .function::<(String,), ()>("print")
            .function::<(String,), ()>("println")
            .build();

        assert_eq!(module.visibility, HostVisibility::Public);
        assert_eq!(module.functions.len(), 2);
        assert_eq!(module.functions[0].visibility, HostVisibility::Public);
        assert_eq!(module.functions[0].name, "print".to_string());
        assert_eq!(
            module.functions[0].sig,
            HostFnSig {
                params: vec![HostType::String],
                ret: HostType::Unit
            }
        );
    }

    #[test]
    fn compile_options_register_external_effect_typed() {
        let mut options = CompileOptions::default();
        options
            .register_external_effect_typed::<(i64, i64), i64>("Test", "add")
            .unwrap();

        assert_eq!(options.external_effects.len(), 1);
        assert_eq!(options.external_effects[0].interface, "Test".to_string());
        assert_eq!(options.external_effects[0].method, "add".to_string());
        assert_eq!(
            options.external_effects[0].sig,
            HostFnSig {
                params: vec![HostType::Int, HostType::Int],
                ret: HostType::Int
            }
        );
    }
}
