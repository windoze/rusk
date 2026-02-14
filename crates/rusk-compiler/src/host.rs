use rusk_mir::HostFnSig;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HostVisibility {
    Private,
    Public,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostFunctionDecl {
    pub visibility: HostVisibility,
    /// Module-local function name (e.g. `"println"`).
    pub name: String,
    pub sig: HostFnSig,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostModuleDecl {
    pub visibility: HostVisibility,
    pub functions: Vec<HostFunctionDecl>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalEffectDecl {
    pub interface: String,
    pub method: String,
    pub sig: HostFnSig,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CompileOptions {
    pub host_modules: Vec<(String, HostModuleDecl)>,
    pub external_effects: Vec<ExternalEffectDecl>,
}

impl CompileOptions {
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
}
