use serde::Deserialize;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct RuskLspConfig {
    pub entry_files: Vec<PathBuf>,
    pub sysroot: Option<PathBuf>,
    pub load_std: bool,
}

impl Default for RuskLspConfig {
    fn default() -> Self {
        Self {
            entry_files: Vec::new(),
            sysroot: None,
            load_std: true,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct InitOptions {
    pub entry_files: Vec<String>,
    pub sysroot: Option<String>,
    pub no_std: bool,
}

pub fn parse_cli_args(args: impl IntoIterator<Item = OsString>) -> Result<RuskLspConfig, String> {
    let mut args = args.into_iter().skip(1).peekable();
    let mut config = RuskLspConfig::default();

    while let Some(arg) = args.next() {
        let s = arg.to_string_lossy();
        match s.as_ref() {
            "--help" | "-h" => {
                return Err(help_text());
            }
            "--version" | "-V" => {
                return Err(format!("rusk-lsp {}", env!("CARGO_PKG_VERSION")));
            }
            "--entry" => {
                let Some(path) = args.next() else {
                    return Err("`--entry` 需要一个路径参数".to_string());
                };
                config.entry_files.push(PathBuf::from(path));
            }
            "--sysroot" => {
                let Some(path) = args.next() else {
                    return Err("`--sysroot` 需要一个路径参数".to_string());
                };
                config.sysroot = Some(PathBuf::from(path));
            }
            "--no-std" => config.load_std = false,
            _ if s.starts_with("--") => {
                return Err(format!("未知参数: {s}"));
            }
            _ => return Err(format!("不支持的位置参数: {s}")),
        }
    }

    Ok(config)
}

pub fn apply_init_options(
    config: &mut RuskLspConfig,
    root: Option<&Path>,
    init_options: Option<&serde_json::Value>,
) {
    let Some(value) = init_options else {
        return;
    };

    let Ok(opts) = serde_json::from_value::<InitOptions>(value.clone()) else {
        return;
    };

    if !opts.entry_files.is_empty() {
        config.entry_files = opts
            .entry_files
            .into_iter()
            .map(|p| resolve_path(root, PathBuf::from(p)))
            .collect();
    }

    if let Some(sysroot) = opts.sysroot {
        config.sysroot = Some(resolve_path(root, PathBuf::from(sysroot)));
    }

    if opts.no_std {
        config.load_std = false;
    }
}

pub fn resolve_path(root: Option<&Path>, path: PathBuf) -> PathBuf {
    let mut out = if path.is_absolute() {
        path
    } else if let Some(root) = root {
        root.join(path)
    } else {
        path
    };

    out = std::fs::canonicalize(&out).unwrap_or(out);
    out
}

fn help_text() -> String {
    [
        "usage: rusk-lsp [--entry <path>]... [--sysroot <path>] [--no-std]",
        "",
        "LSP 初始化参数 (initializationOptions, camelCase):",
        "  - entryFiles: [\"path/to/main.rusk\", ...]",
        "  - sysroot: \"path/to/sysroot\"",
        "  - noStd: true",
    ]
    .join("\n")
}
