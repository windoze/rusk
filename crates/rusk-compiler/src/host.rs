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

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct CompileOptions {
    pub host_modules: Vec<(String, HostModuleDecl)>,
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
}
