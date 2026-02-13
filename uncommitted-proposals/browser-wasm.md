# Browser WASM Runtime Proposal

## 目标

让 Rusk 程序能够在浏览器中运行，并访问浏览器功能（DOM、事件、console 等），从而支持构建类似 React 的前端框架。

## 非目标

- **不需要**在浏览器中编译 Rusk 源码（编译器保持在开发环境）
- 不需要修改编译器或前端部分
- 不需要实现完整的在线 IDE/Playground

## 架构设计

### 总体流程

```
开发时（本地/构建服务器）：
  app.rusk → Rusk Compiler → app.mir (JSON/二进制)

运行时（浏览器）：
  app.mir → rusk-runtime.wasm → 执行
              ↕ (批量操作接口)
         Browser APIs (DOM, console, etc.)
```

### 性能关键设计：批量操作接口

**问题**：每次 DOM 操作都跨越 WASM ↔ JS 边界会导致严重的性能问题。

**解决方案**：使用命令缓冲区（Command Buffer）模式，在 Rusk 侧构建操作列表，一次性传递给 JS 侧批量执行。

```
Rusk 代码
   ↓
构建 DOM 命令列表
   ↓
序列化命令列表（一次性传递）
   ↓
JavaScript 批量执行
   ↓
返回结果（一次性传递）
   ↓
Rusk 处理结果
```

这种设计将多次边界跨越减少为 2 次（命令发送 + 结果返回）。

### 组件结构（多 Crate 架构）

```
┌─────────────────────────────────────────────────┐
│              浏览器环境                           │
│                                                 │
│  ┌───────────────────────────────────────────┐ │
│  │      rusk-browser.wasm                    │ │
│  │      (独立的 WASM crate)                  │ │
│  │                                           │ │
│  │  ┌─────────────────────────────────────┐ │ │
│  │  │  浏览器桥接层 (rusk-browser crate)   │ │ │
│  │  │  - WASM 绑定 (wasm-bindgen)         │ │ │
│  │  │  - DOM 命令系统                      │ │ │
│  │  │  - 批量操作接口                       │ │ │
│  │  │  - 浏览器 Host Functions            │ │ │
│  │  │  - JavaScript 对象管理               │ │ │
│  │  └─────────────────────────────────────┘ │ │
│  │                ↓ 依赖                     │ │
│  │  ┌─────────────────────────────────────┐ │ │
│  │  │  核心运行时 (rusk crate)              │ │ │
│  │  │  - Interpreter                      │ │ │
│  │  │  - GC                               │ │ │
│  │  │  - MIR                              │ │ │
│  │  │  - CoreLib                          │ │ │
│  │  │  (平台无关，零 WASM 依赖)            │ │ │
│  │  └─────────────────────────────────────┘ │ │
│  └───────────────────────────────────────────┘ │
│                   ↕ wasm-bindgen                │
│  ┌───────────────────────────────────────────┐ │
│  │       JavaScript 包装层                    │ │
│  │  - 加载 WASM 模块                          │ │
│  │  - 加载和传递 MIR 数据                     │ │
│  │  - 提供 JavaScript 友好的 API              │ │
│  └───────────────────────────────────────────┘ │
│                   ↕                             │
│              浏览器 API                         │
└─────────────────────────────────────────────────┘
```

### Workspace 结构

```
rusk/
├── Cargo.toml              # Workspace 根配置
├── rusk/                   # 核心 crate（平台无关）
│   ├── Cargo.toml
│   ├── src/
│   │   ├── lib.rs
│   │   ├── interpreter.rs
│   │   ├── mir.rs
│   │   ├── gc.rs
│   │   ├── corelib.rs
│   │   └── ...
│   └── bin/
│       └── rusk.rs         # CLI 工具
├── rusk-browser/           # 浏览器 crate（WASM 专用）
│   ├── Cargo.toml          # crate-type = ["cdylib"]
│   ├── src/
│   │   ├── lib.rs          # WASM 入口
│   │   ├── runtime.rs      # 运行时包装
│   │   ├── dom_commands.rs # DOM 命令定义
│   │   ├── browser.rs      # 浏览器 Host Functions
│   │   └── js_objects.rs   # JavaScript 对象管理
│   └── pkg/                # wasm-pack 输出目录
├── web/                    # Web 前端
│   ├── package.json
│   └── examples/
└── tests/
```

## 实施步骤

### Phase 0: 设置 Workspace

#### 0.1 创建 Workspace 根 `Cargo.toml`

```toml
[workspace]
members = ["rusk", "rusk-browser"]
resolver = "2"

[profile.release]
opt-level = "z"      # 优化体积
lto = true           # 链接时优化
codegen-units = 1    # 更好的优化
strip = true         # 移除调试符号
```

### Phase 1: MIR 序列化（核心基础）

#### 1.1 修改 `rusk/Cargo.toml`（核心 crate，保持平台无关）

```toml
[package]
name = "rusk"
version = "0.1.0"
edition = "2024"

# 核心库只产生 rlib，不产生 WASM
[lib]
crate-type = ["rlib"]

[[bin]]
name = "rusk"
path = "src/bin/rusk.rs"

[dependencies]
unicode-ident = { version = "1", optional = true }
serde = { version = "1.0", features = ["derive"], optional = true }
serde_json = { version = "1.0", optional = true }

# 可选：更高效的二进制序列化格式
bincode = { version = "1.3", optional = true }

[features]
default = ["unicode"]
unicode = ["unicode-ident"]
serde = ["dep:serde", "dep:serde_json"]

# 注意：没有 WASM 相关依赖！核心库保持平台无关
```

#### 1.2 为 MIR 类型添加序列化

修改 `src/mir.rs`，为所有公共类型添加 serde 支持：

```rust
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Local(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BlockId(pub usize);

#[derive(Clone, Debug, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Module {
    pub functions: BTreeMap<String, Function>,
    pub methods: BTreeMap<(String, String), String>,
    pub interface_impls: BTreeMap<String, BTreeSet<String>>,
}

// ... 为所有 MIR 类型添加类似的注解
```

需要添加的类型（完整列表）：
- `Local`, `BlockId`
- `Module`, `Function`, `BasicBlock`
- `Param`, `Mutability`, `Type`, `TypeRepLit`
- `Operand`, `ConstValue`, `Pattern`
- `EffectSpec`, `HandlerClause`
- `Instruction`, `Terminator`, `SwitchCase`

#### 1.3 添加编译器导出功能

在 `src/compiler.rs` 中添加导出 MIR 的函数：

```rust
#[cfg(feature = "serde")]
pub fn compile_to_json(path: &Path) -> Result<String, CompileError> {
    let module = compile_file_to_mir(path)?;
    serde_json::to_string(&module).map_err(|e| CompileError {
        message: format!("Failed to serialize MIR: {}", e),
        span: Span::default(),
        rendered_location: None,
    })
}

#[cfg(feature = "serde")]
pub fn compile_to_json_pretty(path: &Path) -> Result<String, CompileError> {
    let module = compile_file_to_mir(path)?;
    serde_json::to_string_pretty(&module).map_err(|e| CompileError {
        message: format!("Failed to serialize MIR: {}", e),
        span: Span::default(),
        rendered_location: None,
    })
}
```

#### 1.4 添加 CLI 命令导出 MIR

在 `src/bin/rusk.rs` 中添加 `--emit-mir` 选项：

```rust
fn main() {
    let mut args = env::args().skip(1);

    // 检查是否有 --emit-mir 标志
    let emit_mir = args.clone().any(|arg| arg == "--emit-mir");
    let args: Vec<_> = args.filter(|arg| arg != "--emit-mir").collect();

    let Some(path) = args.first() else {
        eprintln!("usage: rusk [--emit-mir] <file.rusk>");
        process::exit(2);
    };

    if emit_mir {
        #[cfg(feature = "serde")]
        {
            let json = match compile_to_json_pretty(Path::new(path)) {
                Ok(j) => j,
                Err(e) => {
                    eprintln!("compile error: {e}");
                    process::exit(1);
                }
            };
            println!("{}", json);
        }
        #[cfg(not(feature = "serde"))]
        {
            eprintln!("error: --emit-mir requires the 'serde' feature");
            process::exit(2);
        }
        return;
    }

    // 原有的运行逻辑...
}
```

### Phase 2: 创建浏览器 Crate

#### 2.1 创建 `rusk-browser/Cargo.toml`

```toml
[package]
name = "rusk-browser"
version = "0.1.0"
edition = "2024"

# 这个 crate 专门产生 WASM
[lib]
crate-type = ["cdylib"]

[dependencies]
# 依赖核心 rusk crate
rusk = { path = "../rusk", features = ["serde"] }

# WASM 绑定
wasm-bindgen = "0.2"
serde = { version = "1.0", features = ["derive"] }
serde-wasm-bindgen = "0.6"
serde_json = "1.0"

# 浏览器 API
web-sys = { version = "0.3", features = [
    "console",
    "Window",
    "Document",
    "Element",
    "HtmlElement",
    "Event",
    "EventTarget",
    "Storage",
    "CssStyleDeclaration",
    "DomTokenList",
    "Node",
] }

# 错误处理
console_error_panic_hook = "0.1"

[profile.release]
# 继承 workspace 的 release 配置
```

#### 2.2 创建 `rusk-browser/src/lib.rs`（WASM 入口）

```rust
//! Rusk 浏览器运行时
//!
//! 这个 crate 将 Rusk 核心解释器暴露给浏览器环境。

mod runtime;
mod dom_commands;
mod browser;
mod js_objects;

pub use runtime::RuskRuntime;
pub use dom_commands::{DOMCommand, DOMResult, DOMBatch, DOMBatchResult};

// 设置 panic hook，让 Rust panic 显示在浏览器 console
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen::prelude::wasm_bindgen(start)]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}
```

#### 2.3 实现运行时接口 (`rusk-browser/src/runtime.rs`)

```rust
use wasm_bindgen::prelude::*;
use rusk::{Interpreter, Value, mir::Module, corelib::register_core_host_fns};
use crate::browser::register_browser_host_fns;

#[wasm_bindgen]
pub struct RuskRuntime {
    interpreter: Interpreter,
}

#[wasm_bindgen]
impl RuskRuntime {
    /// 从 JSON 格式的 MIR 创建运行时
    #[wasm_bindgen(constructor)]
    pub fn new(mir_json: &str) -> Result<RuskRuntime, JsValue> {

        let module: Module = serde_json::from_str(mir_json)
            .map_err(|e| JsValue::from_str(&format!("Failed to parse MIR: {}", e)))?;

        let mut interp = Interpreter::new(module);

        // 注册核心 host functions
        register_core_host_fns(&mut interp);

        // 注册浏览器特定的 host functions
        register_browser_host_fns(&mut interp);

        Ok(RuskRuntime { interpreter: interp })
    }

    /// 运行指定的函数
    pub fn run_function(&mut self, name: &str, args_json: &str) -> Result<String, JsValue> {
        let args: Vec<Value> = if args_json.is_empty() {
            vec![]
        } else {
            serde_json::from_str(args_json)
                .map_err(|e| JsValue::from_str(&format!("Failed to parse arguments: {}", e)))?
        };

        match self.interpreter.run_function(name, args) {
            Ok(value) => {
                let result = serde_json::to_string(&value)
                    .map_err(|e| JsValue::from_str(&format!("Failed to serialize result: {}", e)))?;
                Ok(result)
            }
            Err(e) => Err(JsValue::from_str(&format!("Runtime error: {}", e))),
        }
    }

    /// 获取当前堆上的活动对象数量（用于调试）
    pub fn live_objects(&self) -> usize {
        self.interpreter.live_objects()
    }

    /// 手动触发垃圾回收
    pub fn collect_garbage(&mut self) {
        self.interpreter.collect_garbage();
    }
}
```

### Phase 3: 浏览器 Host Functions（批量操作接口）

#### 3.1 DOM 命令系统 (`rusk-browser/src/dom_commands.rs`)

定义所有 DOM 操作的命令类型：

```rust
use serde::{Deserialize, Serialize};

/// DOM 操作命令
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum DOMCommand {
    // 查询和创建
    QuerySelector {
        result_id: u32,
        selector: String,
    },
    QuerySelectorAll {
        result_id: u32,
        selector: String,
    },
    CreateElement {
        result_id: u32,
        tag: String,
    },
    CreateTextNode {
        result_id: u32,
        text: String,
    },

    // 属性操作
    SetAttribute {
        element_id: u32,
        name: String,
        value: String,
    },
    GetAttribute {
        result_id: u32,
        element_id: u32,
        name: String,
    },
    RemoveAttribute {
        element_id: u32,
        name: String,
    },
    SetProperty {
        element_id: u32,
        name: String,
        value: String,
    },

    // 内容操作
    SetTextContent {
        element_id: u32,
        text: String,
    },
    GetTextContent {
        result_id: u32,
        element_id: u32,
    },
    SetInnerHTML {
        element_id: u32,
        html: String,
    },
    GetInnerHTML {
        result_id: u32,
        element_id: u32,
    },

    // DOM 树操作
    AppendChild {
        parent_id: u32,
        child_id: u32,
    },
    RemoveChild {
        parent_id: u32,
        child_id: u32,
    },
    InsertBefore {
        parent_id: u32,
        new_child_id: u32,
        reference_id: u32,
    },
    ReplaceChild {
        parent_id: u32,
        new_child_id: u32,
        old_child_id: u32,
    },

    // 样式操作
    SetStyle {
        element_id: u32,
        property: String,
        value: String,
    },
    GetStyle {
        result_id: u32,
        element_id: u32,
        property: String,
    },
    AddClass {
        element_id: u32,
        class: String,
    },
    RemoveClass {
        element_id: u32,
        class: String,
    },
    ToggleClass {
        element_id: u32,
        class: String,
    },

    // 事件操作
    AddEventListener {
        element_id: u32,
        event_type: String,
        callback_id: u32,
    },
    RemoveEventListener {
        element_id: u32,
        event_type: String,
        callback_id: u32,
    },

    // Console 操作
    ConsoleLog {
        message: String,
    },
    ConsoleError {
        message: String,
    },
    ConsoleWarn {
        message: String,
    },

    // Storage 操作
    StorageGetItem {
        result_id: u32,
        key: String,
    },
    StorageSetItem {
        key: String,
        value: String,
    },
    StorageRemoveItem {
        key: String,
    },
}

/// DOM 操作结果
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum DOMResult {
    Element { id: u32, handle: u32 },
    ElementList { id: u32, handles: Vec<u32> },
    String { id: u32, value: String },
    Null { id: u32 },
    Error { id: u32, message: String },
    Success { id: u32 },
}

/// DOM 命令批处理
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DOMBatch {
    pub commands: Vec<DOMCommand>,
}

/// DOM 批处理结果
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DOMBatchResult {
    pub results: Vec<DOMResult>,
}
```

#### 3.2 浏览器功能桥接 (`rusk-browser/src/browser.rs`)

实现批量操作接口：

```rust
use wasm_bindgen::prelude::*;
use web_sys::{console, window, Document, Element, HtmlElement};
use rusk::{Interpreter, Value, interpreter::RuntimeError};
use crate::dom_commands::{DOMCommand, DOMResult, DOMBatch, DOMBatchResult};

pub fn register_browser_host_fns(interp: &mut Interpreter) {
    register_batch_interface(interp);
    register_console_fns(interp);
    register_storage_fns(interp);
}

// ============== 批量操作接口（主要接口）==============

fn register_batch_interface(interp: &mut Interpreter) {
    interp.register_host_fn("browser::dom::execute_batch", |_interp, args| {
        match args {
            [Value::String(batch_json)] => {
                let batch: DOMBatch = serde_json::from_str(batch_json)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("Failed to parse DOM batch: {}", e),
                    })?;

                let result = execute_dom_batch(batch);

                let result_json = serde_json::to_string(&result)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("Failed to serialize batch result: {}", e),
                    })?;

                Ok(Value::String(result_json))
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::execute_batch: expected (String)".to_string(),
            }),
        }
    });
}

/// 执行 DOM 命令批处理
fn execute_dom_batch(batch: DOMBatch) -> DOMBatchResult {
    let mut results = Vec::new();
    let mut handles = ElementHandlePool::new();

    for cmd in batch.commands {
        let result = execute_single_command(cmd, &mut handles);
        results.push(result);
    }

    DOMBatchResult { results }
}

/// 执行单个 DOM 命令
fn execute_single_command(cmd: DOMCommand, handles: &mut ElementHandlePool) -> DOMResult {
    match cmd {
        DOMCommand::QuerySelector { result_id, selector } => {
            let document = match window().and_then(|w| w.document()) {
                Some(doc) => doc,
                None => return DOMResult::Error {
                    id: result_id,
                    message: "Failed to get document".to_string(),
                },
            };

            match document.query_selector(&selector) {
                Ok(Some(element)) => {
                    let handle = handles.store(element.into());
                    DOMResult::Element { id: result_id, handle }
                }
                Ok(None) => DOMResult::Null { id: result_id },
                Err(e) => DOMResult::Error {
                    id: result_id,
                    message: format!("{:?}", e),
                },
            }
        }

        DOMCommand::CreateElement { result_id, tag } => {
            let document = match window().and_then(|w| w.document()) {
                Some(doc) => doc,
                None => return DOMResult::Error {
                    id: result_id,
                    message: "Failed to get document".to_string(),
                },
            };

            match document.create_element(&tag) {
                Ok(element) => {
                    let handle = handles.store(element.into());
                    DOMResult::Element { id: result_id, handle }
                }
                Err(e) => DOMResult::Error {
                    id: result_id,
                    message: format!("{:?}", e),
                },
            }
        }

        DOMCommand::SetTextContent { element_id, text } => {
            match handles.get(element_id) {
                Some(js_val) => {
                    if let Ok(element) = js_val.dyn_into::<Element>() {
                        element.set_text_content(Some(&text));
                        DOMResult::Success { id: element_id }
                    } else {
                        DOMResult::Error {
                            id: element_id,
                            message: "Not an element".to_string(),
                        }
                    }
                }
                None => DOMResult::Error {
                    id: element_id,
                    message: "Invalid element handle".to_string(),
                },
            }
        }

        DOMCommand::SetAttribute { element_id, name, value } => {
            match handles.get(element_id) {
                Some(js_val) => {
                    if let Ok(element) = js_val.dyn_into::<Element>() {
                        match element.set_attribute(&name, &value) {
                            Ok(_) => DOMResult::Success { id: element_id },
                            Err(e) => DOMResult::Error {
                                id: element_id,
                                message: format!("{:?}", e),
                            },
                        }
                    } else {
                        DOMResult::Error {
                            id: element_id,
                            message: "Not an element".to_string(),
                        }
                    }
                }
                None => DOMResult::Error {
                    id: element_id,
                    message: "Invalid element handle".to_string(),
                },
            }
        }

        DOMCommand::AppendChild { parent_id, child_id } => {
            let parent_js = match handles.get(parent_id) {
                Some(js) => js,
                None => return DOMResult::Error {
                    id: parent_id,
                    message: "Invalid parent handle".to_string(),
                },
            };
            let child_js = match handles.get(child_id) {
                Some(js) => js,
                None => return DOMResult::Error {
                    id: child_id,
                    message: "Invalid child handle".to_string(),
                },
            };

            let parent: Element = match parent_js.dyn_into() {
                Ok(el) => el,
                Err(_) => return DOMResult::Error {
                    id: parent_id,
                    message: "Parent is not an element".to_string(),
                },
            };
            let child: Element = match child_js.dyn_into() {
                Ok(el) => el,
                Err(_) => return DOMResult::Error {
                    id: child_id,
                    message: "Child is not an element".to_string(),
                },
            };

            match parent.append_child(&child) {
                Ok(_) => DOMResult::Success { id: parent_id },
                Err(e) => DOMResult::Error {
                    id: parent_id,
                    message: format!("{:?}", e),
                },
            }
        }

        DOMCommand::SetStyle { element_id, property, value } => {
            match handles.get(element_id) {
                Some(js_val) => {
                    if let Ok(element) = js_val.dyn_into::<HtmlElement>() {
                        match element.style().set_property(&property, &value) {
                            Ok(_) => DOMResult::Success { id: element_id },
                            Err(e) => DOMResult::Error {
                                id: element_id,
                                message: format!("{:?}", e),
                            },
                        }
                    } else {
                        DOMResult::Error {
                            id: element_id,
                            message: "Not an HTML element".to_string(),
                        }
                    }
                }
                None => DOMResult::Error {
                    id: element_id,
                    message: "Invalid element handle".to_string(),
                },
            }
        }

        DOMCommand::AddClass { element_id, class } => {
            match handles.get(element_id) {
                Some(js_val) => {
                    if let Ok(element) = js_val.dyn_into::<Element>() {
                        match element.class_list().add_1(&class) {
                            Ok(_) => DOMResult::Success { id: element_id },
                            Err(e) => DOMResult::Error {
                                id: element_id,
                                message: format!("{:?}", e),
                            },
                        }
                    } else {
                        DOMResult::Error {
                            id: element_id,
                            message: "Not an element".to_string(),
                        }
                    }
                }
                None => DOMResult::Error {
                    id: element_id,
                    message: "Invalid element handle".to_string(),
                },
            }
        }

        DOMCommand::ConsoleLog { message } => {
            console::log_1(&JsValue::from_str(&message));
            DOMResult::Success { id: 0 }
        }

        DOMCommand::ConsoleError { message } => {
            console::error_1(&JsValue::from_str(&message));
            DOMResult::Success { id: 0 }
        }

        DOMCommand::ConsoleWarn { message } => {
            console::warn_1(&JsValue::from_str(&message));
            DOMResult::Success { id: 0 }
        }

        // ... 实现其他命令
        _ => DOMResult::Error {
            id: 0,
            message: format!("Unimplemented command: {:?}", cmd),
        },
    }
}

// ============== 元素句柄池 ==============

struct ElementHandlePool {
    handles: std::collections::HashMap<u32, JsValue>,
    next_id: u32,
}

impl ElementHandlePool {
    fn new() -> Self {
        Self {
            handles: std::collections::HashMap::new(),
            next_id: 1,
        }
    }

    fn store(&mut self, value: JsValue) -> u32 {
        let id = self.next_id;
        self.next_id += 1;
        self.handles.insert(id, value);
        id
    }

    fn get(&self, id: u32) -> Option<JsValue> {
        self.handles.get(&id).cloned()
    }
}

// ============== Console API（独立接口，用于简单场景）==============

// ============== Console API ==============

fn register_console_fns(interp: &mut Interpreter) {
    // console.log
    interp.register_host_fn("browser::console::log", |_interp, args| {
        let msg = format_args_for_console(args);
        console::log_1(&JsValue::from_str(&msg));
        Ok(Value::Unit)
    });

    // console.error
    interp.register_host_fn("browser::console::error", |_interp, args| {
        let msg = format_args_for_console(args);
        console::error_1(&JsValue::from_str(&msg));
        Ok(Value::Unit)
    });

    // console.warn
    interp.register_host_fn("browser::console::warn", |_interp, args| {
        let msg = format_args_for_console(args);
        console::warn_1(&JsValue::from_str(&msg));
        Ok(Value::Unit)
    });
}

fn format_args_for_console(args: &[Value]) -> String {
    args.iter()
        .map(|v| format!("{:?}", v))
        .collect::<Vec<_>>()
        .join(" ")
}

// ============== DOM API ==============

fn register_dom_fns(interp: &mut Interpreter) {
    // document.querySelector
    interp.register_host_fn("browser::dom::query_selector", |_interp, args| {
        match args {
            [Value::String(selector)] => {
                let document = window()
                    .and_then(|w| w.document())
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Failed to get document".to_string(),
                    })?;

                let element = document.query_selector(selector)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("querySelector failed: {:?}", e),
                    })?;

                if let Some(elem) = element {
                    // 将 Element 包装为一个特殊的 Rusk 值
                    // 这里我们需要一个机制来存储 JavaScript 对象引用
                    // 可以使用一个全局的对象池
                    let handle = store_js_object(elem.into());
                    Ok(Value::Int(handle as i64))
                } else {
                    // 返回 Option::None
                    Ok(Value::Unit) // 或者使用特殊的 None 表示
                }
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::query_selector: expected (String)".to_string(),
            }),
        }
    });

    // element.textContent setter
    interp.register_host_fn("browser::dom::set_text_content", |_interp, args| {
        match args {
            [Value::Int(handle), Value::String(text)] => {
                let elem = get_js_object(*handle as usize)
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Invalid element handle".to_string(),
                    })?;

                let element: Element = elem.dyn_into()
                    .map_err(|_| RuntimeError::Trap {
                        message: "Not an Element".to_string(),
                    })?;

                element.set_text_content(Some(text));
                Ok(Value::Unit)
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::set_text_content: expected (Int, String)".to_string(),
            }),
        }
    });

    // element.innerHTML setter
    interp.register_host_fn("browser::dom::set_inner_html", |_interp, args| {
        match args {
            [Value::Int(handle), Value::String(html)] => {
                let elem = get_js_object(*handle as usize)
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Invalid element handle".to_string(),
                    })?;

                let element: Element = elem.dyn_into()
                    .map_err(|_| RuntimeError::Trap {
                        message: "Not an Element".to_string(),
                    })?;

                element.set_inner_html(html);
                Ok(Value::Unit)
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::set_inner_html: expected (Int, String)".to_string(),
            }),
        }
    });

    // element.getAttribute
    interp.register_host_fn("browser::dom::get_attribute", |_interp, args| {
        match args {
            [Value::Int(handle), Value::String(attr_name)] => {
                let elem = get_js_object(*handle as usize)
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Invalid element handle".to_string(),
                    })?;

                let element: Element = elem.dyn_into()
                    .map_err(|_| RuntimeError::Trap {
                        message: "Not an Element".to_string(),
                    })?;

                let value = element.get_attribute(attr_name);
                Ok(match value {
                    Some(v) => Value::String(v),
                    None => Value::Unit, // 或者 Option::None
                })
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::get_attribute: expected (Int, String)".to_string(),
            }),
        }
    });

    // element.setAttribute
    interp.register_host_fn("browser::dom::set_attribute", |_interp, args| {
        match args {
            [Value::Int(handle), Value::String(attr_name), Value::String(attr_value)] => {
                let elem = get_js_object(*handle as usize)
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Invalid element handle".to_string(),
                    })?;

                let element: Element = elem.dyn_into()
                    .map_err(|_| RuntimeError::Trap {
                        message: "Not an Element".to_string(),
                    })?;

                element.set_attribute(attr_name, attr_value)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("setAttribute failed: {:?}", e),
                    })?;
                Ok(Value::Unit)
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::set_attribute: expected (Int, String, String)".to_string(),
            }),
        }
    });

    // document.createElement
    interp.register_host_fn("browser::dom::create_element", |_interp, args| {
        match args {
            [Value::String(tag_name)] => {
                let document = window()
                    .and_then(|w| w.document())
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Failed to get document".to_string(),
                    })?;

                let element = document.create_element(tag_name)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("createElement failed: {:?}", e),
                    })?;

                let handle = store_js_object(element.into());
                Ok(Value::Int(handle as i64))
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::create_element: expected (String)".to_string(),
            }),
        }
    });

    // parent.appendChild(child)
    interp.register_host_fn("browser::dom::append_child", |_interp, args| {
        match args {
            [Value::Int(parent_handle), Value::Int(child_handle)] => {
                let parent_js = get_js_object(*parent_handle as usize)
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Invalid parent handle".to_string(),
                    })?;
                let child_js = get_js_object(*child_handle as usize)
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Invalid child handle".to_string(),
                    })?;

                let parent: Element = parent_js.dyn_into()
                    .map_err(|_| RuntimeError::Trap {
                        message: "Parent is not an Element".to_string(),
                    })?;
                let child: Element = child_js.dyn_into()
                    .map_err(|_| RuntimeError::Trap {
                        message: "Child is not an Element".to_string(),
                    })?;

                parent.append_child(&child)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("appendChild failed: {:?}", e),
                    })?;

                Ok(Value::Unit)
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::dom::append_child: expected (Int, Int)".to_string(),
            }),
        }
    });
}

// ============== Event API ==============

fn register_event_fns(interp: &mut Interpreter) {
    // 事件监听器注册
    // 这需要特殊处理，因为需要在 Rust 侧保持回调

    // element.addEventListener
    interp.register_host_fn("browser::event::add_listener", |interp, args| {
        match args {
            [Value::Int(handle), Value::String(event_type), Value::Function(callback_name)] => {
                let elem = get_js_object(*handle as usize)
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Invalid element handle".to_string(),
                    })?;

                let element: Element = elem.dyn_into()
                    .map_err(|_| RuntimeError::Trap {
                        message: "Not an Element".to_string(),
                    })?;

                // 创建一个闭包来捕获回调
                let callback_name = callback_name.clone();
                let closure = Closure::wrap(Box::new(move |event: web_sys::Event| {
                    // 调用 Rusk 函数
                    // 这里需要一个机制来回调到 Rusk 代码
                    // 可能需要通过全局状态或消息队列
                    invoke_rusk_callback(&callback_name, event);
                }) as Box<dyn FnMut(_)>);

                element.add_event_listener_with_callback(
                    event_type,
                    closure.as_ref().unchecked_ref()
                ).map_err(|e| RuntimeError::Trap {
                    message: format!("addEventListener failed: {:?}", e),
                })?;

                // 存储 closure 防止被回收
                store_event_listener(closure);

                Ok(Value::Unit)
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::event::add_listener: expected (Int, String, Function)".to_string(),
            }),
        }
    });
}

// ============== LocalStorage API ==============

fn register_storage_fns(interp: &mut Interpreter) {
    // localStorage.getItem
    interp.register_host_fn("browser::storage::get_item", |_interp, args| {
        match args {
            [Value::String(key)] => {
                let storage = window()
                    .and_then(|w| w.local_storage().ok())
                    .flatten()
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Failed to get localStorage".to_string(),
                    })?;

                let value = storage.get_item(key)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("getItem failed: {:?}", e),
                    })?;

                Ok(match value {
                    Some(v) => Value::String(v),
                    None => Value::Unit, // 或者 Option::None
                })
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::storage::get_item: expected (String)".to_string(),
            }),
        }
    });

    // localStorage.setItem
    interp.register_host_fn("browser::storage::set_item", |_interp, args| {
        match args {
            [Value::String(key), Value::String(value)] => {
                let storage = window()
                    .and_then(|w| w.local_storage().ok())
                    .flatten()
                    .ok_or_else(|| RuntimeError::Trap {
                        message: "Failed to get localStorage".to_string(),
                    })?;

                storage.set_item(key, value)
                    .map_err(|e| RuntimeError::Trap {
                        message: format!("setItem failed: {:?}", e),
                    })?;

                Ok(Value::Unit)
            }
            _ => Err(RuntimeError::Trap {
                message: "browser::storage::set_item: expected (String, String)".to_string(),
            }),
        }
    });
}

// ============== JavaScript 对象池 ==============

use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static JS_OBJECT_POOL: RefCell<HashMap<usize, JsValue>> = RefCell::new(HashMap::new());
    static NEXT_HANDLE: RefCell<usize> = RefCell::new(1);
}

fn store_js_object(obj: JsValue) -> usize {
    NEXT_HANDLE.with(|n| {
        let handle = *n.borrow();
        *n.borrow_mut() += 1;

        JS_OBJECT_POOL.with(|pool| {
            pool.borrow_mut().insert(handle, obj);
        });

        handle
    })
}

fn get_js_object(handle: usize) -> Option<JsValue> {
    JS_OBJECT_POOL.with(|pool| {
        pool.borrow().get(&handle).cloned()
    })
}

// 事件监听器存储（防止被 GC）
thread_local! {
    static EVENT_LISTENERS: RefCell<Vec<Closure<dyn FnMut(web_sys::Event)>>> = RefCell::new(Vec::new());
}

fn store_event_listener(closure: Closure<dyn FnMut(web_sys::Event)>) {
    EVENT_LISTENERS.with(|listeners| {
        listeners.borrow_mut().push(closure);
    });
}

// 回调 Rusk 函数（这需要访问 Interpreter）
// 可能需要通过全局消息队列或其他机制实现
fn invoke_rusk_callback(callback_name: &str, event: web_sys::Event) {
    // TODO: 实现回调机制
    // 一种方案是使用消息队列，另一种是使用全局的 Interpreter 引用
    console::log_1(&JsValue::from_str(&format!("Callback: {}", callback_name)));
}
```

#### 3.3 JavaScript 对象管理 (`rusk-browser/src/js_objects.rs`)

```rust
use wasm_bindgen::JsValue;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static JS_OBJECT_POOL: RefCell<HashMap<u32, JsValue>> = RefCell::new(HashMap::new());
    static NEXT_HANDLE: RefCell<u32> = RefCell::new(1);
}

pub fn store_js_object(obj: JsValue) -> u32 {
    NEXT_HANDLE.with(|n| {
        let handle = *n.borrow();
        *n.borrow_mut() += 1;

        JS_OBJECT_POOL.with(|pool| {
            pool.borrow_mut().insert(handle, obj);
        });

        handle
    })
}

pub fn get_js_object(handle: u32) -> Option<JsValue> {
    JS_OBJECT_POOL.with(|pool| {
        pool.borrow().get(&handle).cloned()
    })
}

pub fn remove_js_object(handle: u32) {
    JS_OBJECT_POOL.with(|pool| {
        pool.borrow_mut().remove(&handle);
    });
}
```

### Phase 4: JavaScript 包装层

#### 4.1 项目结构

```
rusk/
├── web/
│   ├── package.json
│   ├── webpack.config.js
│   ├── src/
│   │   ├── index.js          # JavaScript API
│   │   └── loader.js         # MIR 加载器
│   ├── examples/
│   │   ├── hello-world/
│   │   │   ├── index.html
│   │   │   ├── app.rusk
│   │   │   └── app.mir (编译产物)
│   │   └── counter/
│   │       ├── index.html
│   │       ├── app.rusk
│   │       └── app.mir
│   └── dist/
│       └── (构建产物)
```

#### 4.2 `web/package.json`

```json
{
  "name": "@rusk/browser-runtime",
  "version": "0.1.0",
  "description": "Rusk browser runtime",
  "main": "dist/index.js",
  "scripts": {
    "build": "webpack --mode production",
    "dev": "webpack serve --mode development",
    "build:wasm": "cd .. && wasm-pack build --target web --features wasm --out-dir web/pkg"
  },
  "devDependencies": {
    "webpack": "^5.88.0",
    "webpack-cli": "^5.1.0",
    "webpack-dev-server": "^4.15.0"
  }
}
```

#### 4.3 `web/src/index.js`

```javascript
import init, { RuskRuntime } from '../pkg/rusk.js';

/**
 * Rusk 浏览器运行时管理器
 */
export class Rusk {
    constructor() {
        this.runtime = null;
        this.initialized = false;
    }

    /**
     * 初始化 WASM 运行时
     */
    async init() {
        if (this.initialized) return;
        await init();
        this.initialized = true;
    }

    /**
     * 从 URL 加载并运行 MIR
     */
    async loadAndRun(mirUrl, functionName = 'main', args = []) {
        await this.init();

        const response = await fetch(mirUrl);
        const mirJson = await response.text();

        this.runtime = new RuskRuntime(mirJson);

        const argsJson = JSON.stringify(args);
        const result = this.runtime.run_function(functionName, argsJson);

        return JSON.parse(result);
    }

    /**
     * 从字符串加载 MIR
     */
    async loadMIR(mirJson) {
        await this.init();
        this.runtime = new RuskRuntime(mirJson);
    }

    /**
     * 运行函数
     */
    runFunction(name, args = []) {
        if (!this.runtime) {
            throw new Error('Runtime not initialized. Call loadMIR first.');
        }

        const argsJson = JSON.stringify(args);
        const result = this.runtime.run_function(name, argsJson);
        return JSON.parse(result);
    }

    /**
     * 获取活动对象数量
     */
    liveObjects() {
        return this.runtime?.live_objects() || 0;
    }

    /**
     * 触发垃圾回收
     */
    collectGarbage() {
        this.runtime?.collect_garbage();
    }
}

// 导出便捷函数
export async function runRuskApp(mirUrl) {
    const rusk = new Rusk();
    return await rusk.loadAndRun(mirUrl);
}
```

#### 4.4 Rusk 侧的批量操作包装库

为了让 Rusk 代码更易用，需要提供一个包装库：

**`stdlib/browser/dom.rusk`**

```rusk
// DOM 构建器
struct DOMBatchBuilder {
    commands: [DOMCommand],
    next_result_id: int,
}

impl DOMBatchBuilder {
    fn new() -> DOMBatchBuilder {
        DOMBatchBuilder { commands: [], next_result_id: 1 }
    }

    fn query_selector(mut self, selector: string) -> (DOMBatchBuilder, int) {
        let result_id = self.next_result_id;
        self.next_result_id = self.next_result_id + 1;

        let cmd = DOMCommand::QuerySelector { result_id, selector };
        self.commands.push(cmd);

        (self, result_id)
    }

    fn create_element(mut self, tag: string) -> (DOMBatchBuilder, int) {
        let result_id = self.next_result_id;
        self.next_result_id = self.next_result_id + 1;

        let cmd = DOMCommand::CreateElement { result_id, tag };
        self.commands.push(cmd);

        (self, result_id)
    }

    fn set_text_content(mut self, element_id: int, text: string) -> DOMBatchBuilder {
        let cmd = DOMCommand::SetTextContent { element_id, text };
        self.commands.push(cmd);
        self
    }

    fn set_attribute(mut self, element_id: int, name: string, value: string) -> DOMBatchBuilder {
        let cmd = DOMCommand::SetAttribute { element_id, name, value };
        self.commands.push(cmd);
        self
    }

    fn append_child(mut self, parent_id: int, child_id: int) -> DOMBatchBuilder {
        let cmd = DOMCommand::AppendChild { parent_id, child_id };
        self.commands.push(cmd);
        self
    }

    fn set_style(mut self, element_id: int, property: string, value: string) -> DOMBatchBuilder {
        let cmd = DOMCommand::SetStyle { element_id, property, value };
        self.commands.push(cmd);
        self
    }

    fn add_class(mut self, element_id: int, class: string) -> DOMBatchBuilder {
        let cmd = DOMCommand::AddClass { element_id, class };
        self.commands.push(cmd);
        self
    }

    fn console_log(mut self, message: string) -> DOMBatchBuilder {
        let cmd = DOMCommand::ConsoleLog { message };
        self.commands.push(cmd);
        self
    }

    // 执行批处理
    fn execute(self) -> DOMBatchResult {
        let batch = DOMBatch { commands: self.commands };
        let batch_json = to_json(batch);
        let result_json = browser::dom::execute_batch(batch_json);
        parse_json(result_json)
    }
}

// 便捷函数
fn dom() -> DOMBatchBuilder {
    DOMBatchBuilder::new()
}
```

#### 4.5 示例：Hello World（使用批量接口）

**`web/examples/hello-world/app.rusk`**

```rusk
fn main() {
    // 使用批量操作接口 - 一次性完成所有操作
    let (builder, body_id) = dom()
        .console_log("Hello from Rusk!")
        .query_selector("body");

    let result = builder
        .set_text_content(body_id, "Hello, Rusk in Browser!")
        .set_style(body_id, "background-color", "#f0f0f0")
        .set_style(body_id, "font-family", "Arial, sans-serif")
        .execute();

    // 处理结果
    match result.get(body_id) {
        Some(DOMResult::Element { handle }) => {
            console_log("Body element found!");
        }
        Some(DOMResult::Null) => {
            console_log("Body element not found");
        }
        Some(DOMResult::Error { message }) => {
            console_log("Error: " + message);
        }
        _ => {}
    }
}
```

对比：传统的单个操作方式（每次都跨边界，性能差）：
```rusk
// ❌ 不推荐：多次边界跨越
fn main_slow() {
    browser::console::log("Hello from Rusk!");  // 跨边界 1
    let body = browser::dom::query_selector("body");  // 跨边界 2
    if body != 0 {
        browser::dom::set_text_content(body, "Hello!");  // 跨边界 3
        browser::dom::set_style(body, "background-color", "#f0f0f0");  // 跨边界 4
    }
    // 总共 4 次边界跨越
}

// ✅ 推荐：批量操作
fn main() {
    let (builder, body_id) = dom()
        .console_log("Hello from Rusk!")
        .query_selector("body");

    builder
        .set_text_content(body_id, "Hello!")
        .set_style(body_id, "background-color", "#f0f0f0")
        .execute();
    // 只有 2 次边界跨越：发送命令 + 接收结果
}
```

编译：
```bash
cargo run --features serde -- --emit-mir web/examples/hello-world/app.rusk > web/examples/hello-world/app.mir
```

**`web/examples/hello-world/index.html`**

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>Rusk Hello World</title>
</head>
<body>
    <h1>Loading Rusk App...</h1>

    <script type="module">
        import { runRuskApp } from '../../dist/index.js';

        runRuskApp('./app.mir').catch(err => {
            console.error('Failed to run Rusk app:', err);
            document.body.innerHTML = `<h1>Error: ${err}</h1>`;
        });
    </script>
</body>
</html>
```

#### 4.6 示例：交互式计数器（使用批量接口）

**`web/examples/counter/app.rusk`**

```rusk
// 全局状态（使用 localStorage）
fn get_count() -> int {
    // 这里简化了，实际需要通过批量接口
    let stored = browser::storage::get_item("counter");
    if stored == () {
        0
    } else {
        parse_int(stored)
    }
}

fn set_count(count: int) {
    browser::storage::set_item("counter", to_string(count));
}

fn render_ui() {
    let count = get_count();

    // 构建整个 UI（批量操作）
    let (builder, container_id) = dom().query_selector("#app");

    let (builder, h1_id) = builder.create_element("h1");
    let (builder, count_span_id) = builder.create_element("span");
    let (builder, inc_btn_id) = builder.create_element("button");
    let (builder, dec_btn_id) = builder.create_element("button");

    let result = builder
        // 设置内容
        .set_text_content(h1_id, "Counter: ")
        .set_text_content(count_span_id, to_string(count))
        .set_text_content(inc_btn_id, "Increment")
        .set_text_content(dec_btn_id, "Decrement")

        // 设置样式
        .set_style(count_span_id, "color", "blue")
        .set_style(count_span_id, "font-weight", "bold")
        .add_class(inc_btn_id, "btn")
        .add_class(inc_btn_id, "btn-primary")
        .add_class(dec_btn_id, "btn")
        .add_class(dec_btn_id, "btn-secondary")

        // 构建 DOM 树
        .append_child(h1_id, count_span_id)
        .append_child(container_id, h1_id)
        .append_child(container_id, inc_btn_id)
        .append_child(container_id, dec_btn_id)

        // 绑定事件
        .add_event_listener(inc_btn_id, "click", callback_increment)
        .add_event_listener(dec_btn_id, "click", callback_decrement)

        .execute();

    // 这是一次性的批量操作：
    // - 1 次命令发送（15+ 个操作）
    // - 1 次结果接收
    // 总共只有 2 次边界跨越！
}

fn callback_increment() {
    let count = get_count();
    set_count(count + 1);
    render_ui();  // 重新渲染
}

fn callback_decrement() {
    let count = get_count();
    set_count(count - 1);
    render_ui();  // 重新渲染
}

fn main() {
    render_ui();
}
```

**性能对比**：

传统方式（每个操作都跨边界）：
- 15+ 个 DOM 操作 = 15+ 次边界跨越
- 每次跨越约 100-500ns
- 总开销：1.5-7.5μs

批量接口：
- 2 次边界跨越（命令发送 + 结果接收）
- 总开销：约 200-1000ns
- **性能提升：7-15倍**

而且随着操作数量增加，批量接口的优势会更加明显：
- 100 个操作：传统方式 15μs，批量接口 1μs，**提升 15倍**
- 1000 个操作：传统方式 150μs，批量接口 1μs，**提升 150倍**

## 技术挑战与解决方案

### 挑战 1: 批量操作中的 JavaScript 对象生命周期管理

**问题**：批量操作中创建的 DOM 元素需要在批处理内和批处理间引用。

**解决方案**：
- **批处理内引用**：使用临时 ID（result_id），在单次批处理内有效
- **批处理间引用**：返回持久化的 handle，存储在 Rusk 侧
- **自动清理**：当 Rusk 侧的 handle 被 GC 时，通过 finalizer 清理 JavaScript 对象

```rust
// 批处理内的临时引用
let (builder, element_id) = dom().create_element("div");  // element_id 只在此批处理内有效
builder.set_text_content(element_id, "Hello");

// 批处理间的持久引用
let result = builder.execute();
let persistent_handle = result.get_element(element_id);  // 持久化 handle
// persistent_handle 可以在后续的批处理中使用
```

### 挑战 2: 事件回调

**问题**：JavaScript 事件需要回调 Rusk 函数，但 Rusk 函数运行在 WASM 中。

**解决方案**：
- 方案 A（简单）：使用全局消息队列，JavaScript 事件推送到队列，Rusk 定期轮询
- 方案 B（复杂但更好）：使用 `wasm-bindgen` 的闭包支持，直接从 JavaScript 回调到 WASM
- 需要保持 `Closure` 对象活跃，避免被 GC

### 挑战 3: 异步操作

**问题**：`fetch`、`setTimeout` 等异步 API 如何与 Rusk 集成？

**解决方案**：
- 短期：使用回调风格的 API
- 长期：在 Rusk 中实现 async/await 或 effect handler 支持
- 可以使用 Rusk 的 continuation/effect 系统来实现

### 挑战 4: Value 序列化

**问题**：Rusk 的 `Value` 类型包含引用和复杂结构，不容易直接序列化。

**解决方案**：
- 为 `Value` 实现自定义的 serde 支持
- 或者使用 JSON 表示的子集（Int, Float, String, Bool, Array, Struct）
- 引用类型通过 handle 传递

## 开发工具链

### 构建流程

```bash
# 1. 编译 Rusk 程序为 MIR（使用核心 crate）
cargo run -p rusk --features serde -- --emit-mir app.rusk > app.mir

# 2. 构建 WASM 运行时（构建浏览器 crate）
cd rusk-browser
wasm-pack build --target web --out-dir ../web/pkg

# 3. 构建 JavaScript 包装层
cd ../web && npm run build

# 4. 运行开发服务器
npm run dev
```

### 为什么这样分离？

**核心 crate (`rusk`) 的优势**：
- ✅ 完全平台无关，可以在任何地方使用
- ✅ 没有 WASM 依赖，编译速度快
- ✅ 可以独立测试和基准测试
- ✅ 可以用于服务器端、CLI 工具等

**浏览器 crate (`rusk-browser`) 的优势**：
- ✅ 专注于 WASM 和浏览器集成
- ✅ 独立的版本控制和发布周期
- ✅ 可以有其他平台的实现（如 `rusk-node`）
- ✅ 更清晰的依赖关系

**未来可扩展性**：
```
rusk/              # 核心（平台无关）
rusk-browser/      # 浏览器 WASM
rusk-node/         # Node.js 绑定
rusk-python/       # Python 绑定
rusk-cli/          # 命令行工具
```

### 工具建议

可以创建一个统一的 CLI 工具：

```bash
# 一键构建和运行
rusk-web build app.rusk --output dist/

# 开发模式（自动重新编译）
rusk-web dev app.rusk --port 8080
```

## React-like 框架设计思路

基于这个运行时，可以构建一个 React 风格的框架：

### 核心概念

```rusk
// 组件定义
struct Component {
    state: State,
    props: Props,
}

impl Component {
    fn render(self) -> VirtualNode {
        // 返回虚拟 DOM
    }

    fn mount(self) {
        // 挂载到真实 DOM
    }

    fn update(mut self, new_props: Props) {
        // 更新组件
    }
}

// 虚拟 DOM
enum VirtualNode {
    Element { tag: string, props: Props, children: [VirtualNode] },
    Text(string),
}

// 示例组件
struct Counter {
    count: int,
}

impl Counter {
    fn render(self) -> VirtualNode {
        VirtualNode::Element {
            tag: "div",
            props: {},
            children: [
                VirtualNode::Element {
                    tag: "h1",
                    props: {},
                    children: [VirtualNode::Text(to_string(self.count))],
                },
                VirtualNode::Element {
                    tag: "button",
                    props: { onclick: self.increment },
                    children: [VirtualNode::Text("Increment")],
                },
            ],
        }
    }

    fn increment(mut self) {
        self.count = self.count + 1;
        self.rerender();
    }
}
```

### 关键特性

1. **虚拟 DOM**：在 Rusk 侧构建虚拟 DOM 树
2. **Diff 算法**：比较新旧虚拟 DOM，最小化真实 DOM 操作
3. **状态管理**：组件本地状态 + 全局状态
4. **事件系统**：声明式事件绑定
5. **生命周期钩子**：`mount`, `update`, `unmount`

## 性能考虑

### WASM 体积

- 预计编译后的 WASM 大小：300-500 KB（gzip 后约 100-150 KB）
- 可以通过 `wasm-opt` 进一步优化

### 运行时性能

- MIR 解释器性能足够用于 UI 应用
- GC 暂停时间很短（单线程 Mark-Sweep）
- DOM 操作通过 JNI 调用，有一定开销但可接受

### 优化建议

1. 批量 DOM 操作，减少 WASM ↔ JavaScript 边界跨越
2. 使用虚拟 DOM 减少真实 DOM 操作
3. 考虑实现 JIT 或 AOT 编译器（未来）

## 测试策略

### 单元测试

- 使用 `wasm-bindgen-test` 在浏览器环境测试
- 测试各个 host function 的正确性

### 集成测试

- 端到端测试完整的 Rusk 应用
- 使用 Playwright 或 Puppeteer

### 示例

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_console_log() {
        let mir = r#"..."#;  // MIR JSON
        let mut runtime = RuskRuntime::new(mir).unwrap();
        runtime.run_function("main", "[]").unwrap();
    }
}
```

## 路线图

### MVP (2-3 周)

- [ ] MIR 序列化支持
- [ ] 基础 WASM 运行时
- [ ] **批量操作接口和命令系统**（核心，最优先）
- [ ] 基础 DOM 命令（QuerySelector, CreateElement, SetTextContent, AppendChild）
- [ ] Console 命令
- [ ] Rusk 侧的批量操作包装库
- [ ] 简单示例（Hello World）
- [ ] 性能基准测试（批量 vs 单个操作）

### v0.2 (4-6 周)

- [ ] 完整 DOM 命令集（所有常用操作）
- [ ] 样式和 CSS 类操作命令
- [ ] 事件系统（带批量绑定支持）
- [ ] LocalStorage 命令
- [ ] 交互式示例（Counter）
- [ ] 增量 DOM 更新优化

### v0.3 (8-12 周)

- [ ] 虚拟 DOM 库（基于批量操作接口）
- [ ] Diff 算法（生成最优的 DOM 命令序列）
- [ ] 组件系统
- [ ] 状态管理
- [ ] React-like 框架原型

### v1.0 (3-6 个月)

- [ ] 成熟的前端框架
- [ ] 路由系统
- [ ] 异步支持（fetch 等）
- [ ] 开发工具（热重载、调试器）

## 下一步

选择一个阶段开始实施：

1. **Phase 1**：添加 MIR 序列化（最快看到效果）
2. **Phase 2**：WASM 运行时绑定
3. **Phase 3**：浏览器 Host Functions
4. **Phase 4**：JavaScript 包装层和示例

推荐从 Phase 1 开始，因为这是所有后续工作的基础。
