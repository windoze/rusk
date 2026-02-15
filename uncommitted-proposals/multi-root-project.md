# 多根项目（Multi-root Project）与外部 Tin（Extern Tins）支持

Date: 2026-02-15  
Status: Draft

本提案为 Rusk 引入“多根项目（crates-like）”的编译模型：一次编译可以同时加载多个 **tin**
（名称 TBD，本文沿用 *tin*），其中 **入口文件** 所在的 tin 作为“入口 tin”，其他 tin 作为
“外部 tin（extern tins）”通过编译器参数显式提供，并在源码中像 **顶层模块** 一样被引用。

同时，本提案扩展入口函数 `fn main` 以支持接收命令行参数与环境变量（`argv` / `env`），其中
`env` 的形式为 `[(string, string)]`，避免运行时再次 parse。

> 目标对齐：遵循 Rust 的惯例（`--extern` + 可重命名解决冲突），但保持 Rusk 的“脚本入口”体验
>（传给编译器的文件就是入口，**不区分** lib/bin tin）。

---

## 1. 背景与动机

当前 Rusk 仅支持单根模块树（一个入口文件 + `mod foo;` 递归加载）。当项目规模增长时，会出现：

- 需要把代码拆成多个可复用“包/组件”（类似 Rust crate），并在不同入口脚本间共享；
- 需要把“工具库/标准库扩展/内部基础设施”独立出来，与业务入口解耦；
- 需要提供更清晰的封装边界：跨 tin 只能访问 `pub` API。

此外，Rusk 的运行方式更接近 Python：入口文件被直接运行。为了把“工具式脚本”用得更顺手，
入口函数需要能拿到 `argv` 与 `env`。

---

## 2. 目标与非目标

### 2.1 目标（Goals）

- **多根加载**：编译器在编译入口文件时，额外接收若干外部 tin 的 root 文件路径。
- **像模块一样使用**：外部 tin 在源码中表现为“顶层模块名”，可在任何模块中直接使用
  `dep::foo::bar` 形式访问（extern-prelude-like）。
- **可重命名**：外部 tin 名称冲突由编译器参数显式重命名解决（对齐 Rust `--extern name=...`）。
- **tin 边界隐私**：跨 tin 只能访问对方 `pub` 项；对方私有项不可见。
- **入口 main 传参**：支持 `main(argv)` 与 `main(argv, env)` 的受限形式；`env` 为
  `[(string,string)]`。

### 2.2 非目标（Non-goals）

- **Workspace manifest**（例如 `Rusk.toml`）不在本阶段引入（后续作为工具层能力）。
- 不引入 Cargo 式的“独立编译 + 链接”流程：本阶段是“单次编译加载多个 tin”。
- 不引入新的源语言关键字/语法 item（例如 `extern tin`/`dep` 声明）：依赖由编译器参数决定。
- 不在本阶段支持“按模块运行”（`--run foo::bar`）的 CLI，但设计应为其预留空间。

---

## 3. 术语

- **tin**：一个独立的“根模块 + 子模块树”，拥有自己的 `crate::` 根与隐私边界（类似 Rust crate）。
- **入口 tin（entry tin）**：由编译器输入文件 `<entry>.rusk` 所确定的 tin。
- **外部 tin（extern tin）**：由编译器参数显式提供、并被注入到项目命名空间的 tin。
- **extern prelude**：一组从任意模块都可直接引用的顶层名字（类似 Rust 2018 的外部 crate 名）。

---

## 4. 设计概览

### 4.1 编译器参数（建议）

新增可重复参数：

```
--extern <alias>=<path/to/tin_root.rusk>
```

- `<alias>`：在源码中用作顶层模块名（例如 `math::Vec2`）。
- `<path>`：外部 tin 的根文件（一个 `.rusk` 文件）。

并保持入口文件为“最后一个位置参数”：

```
ruskc [--extern ...]* <entry.rusk>
```

> 说明：本提案不要求 CLI 立刻采用该语法，但建议作为标准接口，与库 API（`CompileOptions`）一致。

### 4.2 源码层使用方式

外部 tin 被当作顶层模块使用：

```rusk
use math::vec::Vec2;

fn main() {
  let v: Vec2 = Vec2 { x: 1, y: 2 };
}
```

无需新增语言语法；也不要求显式 `use` 后才能引用（`math::...` 直接可用）。

### 4.3 入口与执行模型

- **不区分** library/binary tin：入口文件就是入口 tin 的 crate root。
- 程序入口仍然是入口 tin 顶层的 `fn main`。
- 未来可以扩展“run a module”：例如 `--run foo::bar`，只要目标模块顶层存在可调用 `fn main`。

---

## 5. 名称解析与隐私语义

### 5.1 外部 tin 的可见性（extern-prelude-like）

对每个外部 tin alias `dep`：

- 在 **所有模块的 module namespace** 中注入一个名为 `dep` 的模块绑定（类似目前对 `core` 的注入）。
- 因此从任意模块都能写 `dep::...`，无需 `crate::dep::...`。

本提案建议的解析优先级与“像模块一样”一致：

1) 若当前模块作用域内存在 `mod dep`（或通过 `use` 引入的同名模块），则 `dep::...` 优先解析为该模块；
2) 否则回退到 extern prelude 的 `dep`（外部 tin）。

为避免不可达/无法消歧义的情况，强制：

- **crate root（入口 tin 根模块）不得声明与任何 extern alias 同名的模块**；
  冲突通过 `--extern <new_alias>=...` 重命名解决。

### 5.2 `crate::` 的语义：按 tin 绑定

单根时代 `crate::` 恒指向唯一根；多 tin 需要变为“指向当前所在 tin 的根”：

- 在入口 tin 内：`crate::...` 指向入口 tin 根模块（与现状一致）。
- 在外部 tin `dep` 内：`crate::...` 指向该外部 tin 的根模块（等价于 `dep::...`）。

> 这条规则是 tin 成为独立编译单元/封装边界的关键，否则外部 tin 内的 `crate::` 会错误指向入口 tin。

### 5.3 `self::` / `super::`

保持现有规则不变：

- `self::` 相对当前模块；
- `super::` 相对父模块（在 tin 根处使用 `super` 报错）。

### 5.4 `pub` 与跨 tin 访问

保持现有隐私模型，但 tin 的模块路径天然隔离：

- tin 内部：子模块可访问祖先模块的私有项（现有行为）。
- 跨 tin：由于模块路径不同，非 `pub` 项对其他 tin 不可达。

`pub use` re-export 的规则保持不变：被 re-export 的目标必须是 `pub`。

---

## 6. 模块加载模型（文件系统）

### 6.1 每个 tin 的 root 文件

- 入口 tin root：`<entry>.rusk`
- 外部 tin root：`--extern alias=/abs/or/rel/path/to/root.rusk`

### 6.2 `mod foo;` 的加载规则

保持 `RUSK_SPEC.md` 现有规则（相对于当前模块文件所在目录）：

- file module：`<dir>/<foo>.rusk`
- directory module：`<dir>/<foo>/mod.rusk`

关键点：外部 tin 的 `mod` 解析相对于 **外部 tin root 文件** 的目录，而不是入口文件目录。

### 6.3 文件唯一性与错误

为避免“同一文件在同一次编译中以不同路径/不同 tin 身份被加载”的歧义，建议：

- 一次编译中，每个 `.rusk` 文件（canonicalize 后）只能被加载一次；
  重复加载视为错误（包含“两个 extern 指向同一路径”的情况）。

---

## 7. `fn main` 参数支持（argv/env）

### 7.1 支持的入口签名（编译期强制）

入口 tin 顶层必须存在 `fn main`，且签名限定为以下三种之一（其他形式报错）：

1) `fn main() -> T?`
2) `fn main(argv: [string]) -> T?`
3) `fn main(argv: [string], env: [(string, string)]) -> T?`

其中：

- `argv`：命令行参数列表（`[string]`）
- `env`：环境变量列表（`[(string,string)]`）

> 说明：返回类型 `T` 目前由现有实现决定（通常是 `unit` 或任意可返回值）。

### 7.2 `argv` 约定（建议）

- `argv[0]` 为入口文件路径（与 Python/Rust 常见习惯一致，便于脚本自省）。
- `argv[1..]` 为用户传入参数。

建议 CLI 采用：

```
rusk <entry.rusk|entry.rbc> -- <arg1> <arg2> ...
```

`--` 后的参数进入 `argv[1..]`，避免与 `rusk` 自身 flags 冲突。

### 7.3 `env` 约定（建议）

- `env` 为 `[(key, value)]` 形式的列表，每项均为 `string`。
- 为了测试与跨平台一致性，建议 **按 key 字典序排序** 后传入（保证稳定顺序）。

编码与兼容性（建议）：

- 若宿主环境变量包含非 UTF-8 字节序列，具体处理策略暂定为“实现定义”；
  推荐实现采用 lossy 转换或跳过该项，并在文档中明确行为。

### 7.4 运行期行为

- 当入口 `main` 需要参数时，运行时在启动 VM 前构造对应的数组/元组对象并作为入口参数传入。
- 若用户直接运行 `.rbc` 且入口函数参数数量不是 0/1/2，运行时应给出明确错误（trap 或启动失败）。

---

## 8. 实现草案（供后续工程落地）

> 本节仅描述建议的实现方向，不在本提案中提交代码。

### 8.1 编译器前端：加载多个 root

建议在 `rusk_compiler::CompileOptions` 中新增：

- `extern_tins: Vec<ExternTinDecl>`，其中 `ExternTinDecl { alias: String, root: PathBuf }`

模块加载（概念）：

1) 加载入口文件，按现有规则 inline 其 `mod foo;`；
2) 对每个 extern tin root 执行同样的加载与 inline；
3) 将每个 extern tin 的顶层 items 包装为一个**合成的** `pub mod <alias> { ... }`，
   并注入到入口程序的根 items 中；
4) 在名称解析阶段，将 `<alias>` 注入到每个模块作用域（module namespace）以实现 extern prelude。

### 8.2 名称解析：按 tin 解析 `crate::`

实现上需要在 resolver/typeck 中区分“当前模块属于哪个 tin root”：

- 入口 tin：crate root 为全局根（空路径）
- 外部 tin `alias`：crate root 为 `alias` 模块路径

然后在解析 `crate::...` 时使用“当前模块的 crate root”而不是固定全局根。

### 8.3 运行时：入口参数注入

由于 `argv/env` 的类型包含数组/元组（非 `AbiValue`），建议：

- VM 提供一个“入口参数构造/注入”API（只覆盖本提案允许的形态）；
- CLI（`rusk`）负责解析 `--` 之后的参数并采集环境变量，传给 VM。

---

## 9. 未来工作

- 引入 workspace manifest（如 `Rusk.toml`）管理成员 tin、默认 alias、路径与工具链工作流。
- `rusk --run <module_path>`：运行某个模块的顶层 `fn main`（若存在）。
- 更通用的启动参数 ABI（例如支持 `main(argv, env, stdin)` 或结构化配置对象）。

---

## 10. 未决问题（Open Questions）

1) extern alias 与本地模块同名时的“遮蔽规则”是否需要进一步对齐 Rust（目前建议：允许子模块遮蔽，
   但禁止 crate root 冲突）。
2) `env` 非 UTF-8 的处理策略要不要在规范中定死（lossy vs skip vs trap）。
3) 是否允许同一外部 tin 以多个 alias 注入（当前建议：禁止，避免重复加载与名字歧义）。

