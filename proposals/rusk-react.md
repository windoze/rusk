# 提案：Rusk React（暂名）——基于 Algebraic Effects 的 Web UI 框架

日期：2026-02-21  
状态：Draft  
作者：Codex（设计草案）

---

## 0. 摘要

本文提出一个面向浏览器的 **React-like** Web UI 框架（暂名 `rusk-react`），用于在 **Rusk → Bytecode VM → WASM** 的运行形态下构建交互式界面。核心设计点：

1. **组件化**：以可复用组件为基础单位，通过组合构建复杂 UI。
2. **Effect-first（无 Hooks / 少 Hooks）**：用 Rusk 的原生代数效应（`@I.method(...)` + `match` handler）来表达 UI 能力边界、状态与生命周期，避免 React Hooks 的调用顺序规则、依赖数组（deps）陷阱与可组合性限制。
3. **Virtual DOM**：在 WASM 内构建/对比 VDOM，生成最小化 DOM Patch。
4. **WASM/JS 批处理**：Patch 以二进制 `bytes` 协议一次性提交给 JS（单次跨边界），事件也以 batch 方式回传，降低大量小调用的 overhead。

该提案是“框架 + 宿主集成协议”的设计文档，不包含实现。

---

## 1. 背景与动机

Rusk 目标之一是可嵌入（CLI / WASM / 设备端），并通过 **host imports** 与 **外部化 effects** 获得平台能力。浏览器 UI 属于典型“平台能力”：DOM、事件、计时器、网络等都在 JS 世界中。

如果直接让 Rusk/WASM 逐条调用 DOM API，会遇到两个核心问题：

- **跨 WASM/JS 边界开销**：大量细粒度 DOM 操作会导致频繁的跨边界调用，性能与功耗都差。
- **React Hooks 的复杂性**：Hooks 依赖“调用顺序稳定”与“deps 数组正确”，在条件分支、抽象封装、闭包捕获等场景里容易产生难排查问题。

Rusk 已经把 **代数效应 + 分界续延（delimited continuations）** 作为一等公民，因此我们可以把 UI 框架设计成：

- 对开发者：仍然是熟悉的**声明式组件**与**VDOM**体验；
- 对运行时：把“状态/生命周期/副作用/调度”建模为 **命名的 effect 操作**，由框架在边界处集中处理；
- 对宿主：通过“**一次 commit**”把所有 DOM 更新批量提交给 JS。

---

## 2. 目标（Goals）与非目标（Non-goals）

### 2.1 目标

- 提供**组件化** API：组件可复用、可组合、可嵌套，支持 `props`、`children`、`key`。
- 提供**Effect-first** 的状态/生命周期能力：
  - 不依赖“Hooks 必须按顺序调用”的规则；
  - 尽量避免“deps 数组”这一类易错接口；
  - 生命周期与资源释放（cleanup）语义明确。
- 实现 **VDOM → Diff → Patch** 管线，减少真实 DOM 操作数量。
- 设计一个 **bytes 级别** 的 Patch 协议，并强调：
  - **单次 commit**（每次渲染最多一次 WASM→JS 调用）；
  - 事件以 batch 回传（每轮交互最多一次 JS→WASM 调用）。

### 2.2 非目标

- 100% 兼容 React API/语义（JSX、Concurrent Features、Suspense 等）。
- 立即实现 SSR / Hydration（可作为后续扩展点）。
- 依赖大型 JS 运行时或 bundler：目标是小而可嵌入。
- 追求最极致的理论性能：先把架构与 ABI 设计清晰、可演进。

---

## 3. 总体架构（3 层）

### 3.1 分层

**(A) Rusk 应用层（用户代码）**

- 编写组件：`fn MyComp(props: Props) -> VNode`
- 使用框架提供的 `ui::*` 构建 VDOM、管理状态、注册副作用。

**(B) Rusk UI 运行时（WASM 内）**

- 执行渲染：从根组件开始生成 VDOM
- Diff：对比上次 VDOM，生成 Patch（DOMOps buffer）
- 调度：事件驱动、批处理 state 更新、合并渲染
- 管理 handler 表：事件回调与 effect cleanup

**(C) JS DOM Runtime（浏览器端）**

- 接收 Patch bytes，解析并执行真实 DOM 操作
- 管理 `node_id -> DOM Node` 映射
- 事件收集：把 DOM 事件编码为 bytes 批量回传到 WASM

### 3.2 数据流（单次交互）

1. JS 收到 DOM 事件 → 写入事件队列（bytes） → 调用 WASM `dispatch_events(events_bytes)`
2. WASM（UI runtime）处理事件：
   - 执行对应 handler（用户 Rusk 代码）
   - 多次 `set_state` 合并为一次“需要渲染”的标记
3. 如果需要渲染：
   - Render：生成新 VDOM
   - Diff：生成 Patch ops（bytes）
   - Commit：一次性 `dom_commit(ops_bytes)`（WASM→JS）
4. JS 应用 Patch，必要时安排下一帧（`requestAnimationFrame`）或 microtask

---

## 4. 组件模型（Component-based Architecture）

### 4.1 组件的形态

框架以“函数组件”为主：

```rusk
struct CounterProps { initial: int }

fn Counter(p: CounterProps) -> ui::VNode {
    // 见下文：state/effect 等通过 ui API 完成
    ui::text(f"init={p.initial}")
}
```

组件组合通过返回的 `VNode` 结构完成：

```rusk
fn App(_: unit) -> ui::VNode {
    ui::div([], [
        ui::comp(Counter, CounterProps { initial: 0 }, key="a"),
        ui::comp(Counter, CounterProps { initial: 10 }, key="b"),
    ])
}
```

> `key` 用于列表重排/插入/删除时维持组件身份（identity），与 React 类似。

### 4.2 Props 与类型擦除（Type Erasure）

为了在 VDOM 中统一存放不同组件的 `props`，框架采用“**Box + 空接口**”实现的类型擦除（基于 Rusk 的 `as`/`as?`）：

- 定义一个空接口 `Dyn`（框架内部）：
  - `interface Dyn {}`
  - `impl<T> Dyn for ui::Box<T> {}`
- 所有 `props` 以 `ui::Box<Props>` 的形式装箱并上转型为 `Dyn` 存入 VNode。
- 组件执行时再 `as? ui::Box<Props>` 下转，失败即为框架/用户错误（类型不匹配）。

该方案的优点：

- 不需要序列化为 `bytes`（在 WASM 内保持原生类型）
- 无需语言级 `Any`，只用现有 `interface` 与运行时类型测试

---

## 5. 用原生 Effects 取代 Hooks（Effect-first）

### 5.1 React Hooks 的典型痛点

- **调用顺序约束**：Hooks 不能在条件分支/循环中随意调用；抽象封装必须非常谨慎。
- **依赖数组（deps）易错**：漏依赖会产生陈旧闭包（stale closure），多依赖会导致过度执行。
- **组合能力受限**：把“状态 + 副作用 + 资源管理”组合成可复用抽象时容易踩规则。

### 5.2 Rusk 的机会：命名效应 + 动态作用域

在本框架中，UI 能力由一组明确命名的 effect 操作提供，例如：

- `@UiState.get(key)` / `@UiState.set(key, dyn_value)`：组件局部状态
- `@UiEffect.register(kind, key, callback)`：注册副作用（含 cleanup）
- `@UiScheduler.invalidate()`：请求重新渲染（自动批处理）
- `@UiContext.get(name)` / `@UiContext.provide(name, value)`：上下文

这些 effect 在渲染边界处由框架统一处理（通过 `match` 安装 handler）：

- 组件代码不需要持有全局 runtime 指针
- 不依赖“调用序号”，而是依赖显式的 **key（字符串或 intern id）**
- cleanup 由框架集中管理，避免资源泄露

### 5.3 核心 API（建议的用户层接口）

#### 5.3.1 `state`：keyed state（不依赖调用顺序）

```rusk
let count = ui::state<int>("count", 0);
let n = ui::get(count);
ui::set(count, n + 1);
```

语义：

- `ui::state<T>(key, init)`：若该 key 尚无值，则写入 `init`；返回一个 `State<T>` 句柄
- `ui::get/ ui::set`：在当前组件上下文中读写该 key 对应的值
- key 相同则同一个 state；key 不同则不同 state

因此在条件分支中也安全：

```rusk
if ui::get(show_advanced) {
    let adv = ui::state<int>("advanced_count", 0);
    ui::set(adv, ui::get(adv) + 1);
}
```

> 这解决了 React “Hooks 必须在顶层调用”的核心限制：这里的稳定性来自 **显式 key**，而不是“第几个 hook”。

#### 5.3.2 `effect` / `layout_effect`：显式生命周期副作用（无 deps 数组）

```rusk
ui::effect("log") {
    std::println(f"count={ui::get(count)}");
    || { () } // cleanup
}
```

设计要点：

- effect 以 key 标识（与 state 类似），不靠调用顺序
- 框架可提供两种执行时机：
  - `effect`：commit 后（类似 React `useEffect`）
  - `layout_effect`：DOM patch 后、浏览器 paint 前（类似 `useLayoutEffect`）
- deps 由框架处理（两条路线，二选一）：
  1) **自动依赖追踪（推荐）**：effect 执行时记录其读取过的 `UiState.get(key)`，当这些 key 变化才重新执行
  2) **显式 deps（备选）**：允许传 `deps: [string]` 作为优化/逃生通道

#### 5.3.3 `memo`：派生计算（避免手写缓存）

```rusk
let label = ui::memo("label") {
    f"count={ui::get(count)}"
};
```

同样推荐自动依赖追踪（读取了哪些 state，就依赖哪些 state）。

#### 5.3.4 `context`：跨层级注入（替代 React Context + hooks）

```rusk
ui::provide("theme", ui::box(theme)) {
    App(())
};

let theme = ui::context<Theme>("theme");
```

其中 `provide` 可通过 effect handler 的动态作用域实现“仅对子树生效”。

---

## 6. Virtual DOM 设计

### 6.1 `VNode`（概念模型）

```text
VNode =
  | Text(string)
  | Element { tag, key?, props, children }
  | Fragment { children }
  | Component { type_id, key?, props: Dyn }
```

说明：

- `Component` 节点在 render 阶段会被“展开”（调用组件函数得到子 VNode），但其 identity/state/effects 由 runtime 持久化管理。
- `key` 用于 children diff：当列表重排时保持节点身份，从而保持组件 state 与 DOM node 复用。

### 6.2 Props 与事件

Props 建议拆为三类（最终会编码到 Patch 中）：

- Attributes：`id`, `class`, `data-*`, `aria-*` 等
- Properties：如 `value`, `checked`（表单控件），更偏 DOM property 语义
- Events：如 `click`, `input`, `change`，通过 `handler_id` 连接 WASM 回调

为了让 Patch 协议紧凑，建议将属性/事件名做 **string interning**（见 §8）。

---

## 7. Diff 与 Patch（最小化 DOM 更新）

### 7.1 Diff 范围

Diff 的目标是把：

`old_host_tree` + `new_host_tree`

变成一串操作序列 `DomOps`，使得 JS 端真实 DOM 从旧状态变到新状态。

### 7.2 Patch 操作集合（建议）

以 `node_id` 作为 DOM Node 的稳定句柄，操作示例：

- `CreateElement(node_id, tag_atom)`
- `CreateText(node_id, text_atom)`
- `InsertBefore(parent_id, node_id, anchor_id_or_0)`
- `Remove(node_id)`
- `SetText(node_id, text_atom)`
- `SetAttr(node_id, name_atom, value_atom)`
- `RemoveAttr(node_id, name_atom)`
- `SetProp(node_id, name_atom, value)`（value 只支持少量基础类型：bool/int/float/string）
- `AddEvent(node_id, event_atom, handler_id, options)`
- `RemoveEvent(node_id, event_atom)`

> 该操作集合刻意偏“低级”，以便一次性批量执行，并且能与浏览器 DOM API 直连。

### 7.3 Children Diff（keyed + unkeyed）

推荐策略（与 React 类似但实现可简化）：

- 若 children 存在 `key`：使用 key 建索引（hash map），做 O(n) 的重排/复用/插入/删除
- 对无 key 的 children：按位置对齐 diff（fallback）

---

## 8. WASM/JS 边界批处理协议（核心性能点）

### 8.1 原则

- **每次渲染最多一次 WASM→JS 调用**：`dom_commit(ops_bytes)`
- **每轮事件最多一次 JS→WASM 调用**：`dispatch_events(events_bytes)`
- 所有结构化数据都以 `bytes` 编码（符合当前 bytecode v0 ABI 限制）

### 8.2 `dom_commit(ops_bytes)`：单次提交 DOMOps

在 JS 端维护：

- `nodes: Map<u32, Node>`
- `handlers: Map<u32, Function>`（事件回调代理）

WASM→JS 传输：

- 一个 `bytes` buffer（线性内存片段）包含：
  1) 可选：本轮新增的 intern 字符串表（atom id -> utf8）
  2) DOMOps 序列

### 8.3 二进制编码建议（示意）

建议采用“小端 + 变长 payload”的 TLV 风格：

```text
[magic:u32][version:u16][flags:u16]
[intern_section_len:u32][...]
[ops_section_len:u32][...]

intern_entry:
  [atom_id:u32][utf8_len:u32][utf8_bytes...]

op:
  [opcode:u8][argc:u8][flags:u16]
  [arg0:u32]...[argN:u32]
  [payload_len:u32][payload_bytes...]  // 可选，给 text/attr value 等
```

说明：

- `tag/attr/event/text` 尽量用 `atom_id` 表示，减少重复传输
- 对偶发的大字符串（例如文本节点）可直接走 payload，不必强制 intern

### 8.4 JS 端应用策略

- 将 Patch 应用视为一个事务：按顺序执行 ops
- 对于 `InsertBefore/Remove`，尽量使用原生 DOM API，不做额外抽象
- 事件监听建议使用 **事件委托（delegation）**（例如根容器一个 listener）：
  - Patch 中的 `AddEvent` 只更新 JS 侧 `node_id -> handler_id` 映射
  - 根 listener 收到事件后向上冒泡查找最近的 handler（或直接用 DOM node 上的 expando）
  - 这样可以显著减少 `addEventListener/removeEventListener` 次数

---

## 9. 事件系统（JS→WASM batch）

### 9.1 事件编码（建议）

事件回传用 `events_bytes`，包含若干条事件记录：

```text
event:
  [handler_id:u32]
  [event_atom:u32]
  [target_node_id:u32]
  [payload_len:u32][payload_bytes...]
```

payload 内容建议分层：

- 最小集：鼠标坐标、键盘码、输入值（字符串）等常用字段
- 扩展：可选 JSON（成本更高但开发更快）

### 9.2 事件处理的批处理语义

框架 runtime 在处理一个 `events_bytes` batch 时：

- 顺序执行对应 handler
- handler 内允许多次 `ui::set(...)`
- 在 batch 结束时最多触发一次渲染（如果有 state 变更）

这对应 React 的事件批处理（batched updates），但实现更直接。

### 9.3 Promise/异步的统一：把 Promise 当作“事件源”

浏览器里的 `fetch()`、`setTimeout()` 等异步 API 都以 `Promise` / 回调为核心抽象。要在本框架里“统一 Promise”，关键不是把 Promise 直接塞进 WASM，而是把它转成两件事：

1) **启动请求**：WASM→JS 的一次调用（host import）  
2) **完成通知**：JS→WASM 的一次批量事件回传（`events_bytes`）

这样 Promise 的 resolve/reject 会自然落在“事件 batch”里，从而：

- 不阻塞 VM：避免使用 VM 的 `StepResult::Request`（它会把整个 VM 置为 `Suspended`，只能单飞一个外部请求）。
- 支持并发：允许同时存在多条 in-flight 异步操作。
- 易于 batch：把多个 promise completion 与 DOM 事件合并成一次 JS→WASM 调用。

#### 9.3.1 Host API（建议）

新增一个浏览器宿主模块（名字可选：`web` / `js` / `dom`），提供 host imports：

- `web::fetch_start(url: string, init: bytes) -> int`
  - 启动 `fetch(url, parse(init))`，返回 `request_id`
  - `init` 可先用 JSON（开发快），后续再换紧凑二进制（性能更好）
- `web::fetch_abort(request_id: int) -> unit`
  - 使用 `AbortController` 取消请求（可选但建议）

#### 9.3.2 完成事件编码（建议）

在 `events_bytes` 里增加事件类型（tag），例如：

- `AsyncResolve { request_id, payload_bytes }`
- `AsyncReject  { request_id, error_string }`

对 `fetch` 的 `payload_bytes` 建议编码为（先求能用）：

- `status: int`
- `headers: bytes`（可用 JSON 或扁平化 `name\\0value\\0...`）
- `body: bytes`（一次性读取 `arrayBuffer()`；流式作为后续扩展）

#### 9.3.3 Rusk 侧：用 effect handler 实现 `await`（不依赖 Hooks）

框架在 WASM 内实现一个“协作式 async 调度器”，用 **Rusk 原生 effect + continuation** 来表达“挂起并等待”：

- 定义框架内部 effect：
  - `interface Async { fn await(request_id: int) -> Result<bytes, string>; }`
- 任务中写：
  - `let res = @Async.await(id);`
- 运行时 handler 遇到 `@Async.await` 时：
  - 捕获 continuation `k`
  - 把 `k` 存到 `pending: Map<int, cont(Result<bytes,string>) -> TaskResult>`（概念结构）
  - 立刻返回到调度器边界（暂停当前任务，不阻塞整个 VM）
- 当 JS 通过事件回传 `AsyncResolve/AsyncReject`：
  - 查表取出 `k`，执行 `k(result)` 继续任务
  - 任务里通常会 `ui::set(...)`，从而在 batch 结束触发一次重渲染

#### 9.3.4 取消与卸载（cleanup）

当组件卸载或某个 effect cleanup 触发时，框架应：

- 从 `pending` 表移除对应 `request_id` 的 continuation（使其不可恢复）
- 调用 `web::fetch_abort(request_id)`（避免浪费网络与内存）
- 若 JS 仍然回传了已取消请求的完成事件，WASM 侧忽略即可

---

## 10. 调度（Scheduler）与渲染节流

### 10.1 调度目标

- 多次 state 更新合并（同一 tick 内只渲染一次）
- 渲染与 commit 可以按需要对齐到：
  - microtask（更快响应）
  - `requestAnimationFrame`（更好与浏览器渲染管线对齐）

### 10.2 建议策略

默认策略：

- 事件 batch 结束后如果 `needs_render`：
  - 立即 render + commit（microtask）
  - 或者标记并在下一帧 `rAF` 执行（配置项）

高级策略（后续）：

- 对输入/滚动等高频事件优先 rAF
- 支持“优先级队列”（类似 React lanes，但先不做复杂）

---

## 11. 可扩展点（Roadmap）

按优先级建议：

1. **最小可用**：
   - `VNode`、Diff、Patch、dom_commit bytes 协议、事件 batch 回传
   - `ui::state/get/set`（keyed，无 hooks 顺序）
   - `ui::effect`（commit 后执行 + cleanup）
2. **开发体验**：
   - 更好的 element builder：`ui::div(...)`、`ui::button(...)` 等
   - JSX-like 预处理器（独立工具 `ruskx`）：把类 JSX 语法编译成 `ui::h(...)`
3. **性能优化**：
   - atom/string interning
   - 静态子树提升（hoist）
   - 细粒度 effect 依赖追踪（memo/effect 的自动订阅）
4. **高级能力**：
   - refs（拿到 node_id，提供 focus/scrollIntoView）
   - portals、suspense-like（可能借助 effects）
   - SSR/hydration

---

## 12. 开放问题（Open Questions）

1. **Props 的“最优形态”**：是以 `Dyn(Box<Props>)` 作为统一通道，还是引入更结构化的 `PropsMap`？
2. **事件委托 vs 逐节点监听**：委托更省监听器，但需要更复杂的 target 解析与映射。
3. **自动依赖追踪**：
   - 依赖追踪粒度是“state key”还是“更细粒度”（例如 path）？
   - 是否需要提供显式 deps 作为 escape hatch？
4. **文本与属性更新的编码格式**：atom interning 的阈值与策略如何选？
5. **与 Rusk effect 系统的边界**：哪些能力用 effect 表达（状态/调度/生命周期），哪些用普通库 API 更合适？
6. **异步/流式数据**：`fetch` 的 body 流（ReadableStream）是“多次产出”，而 continuation 是 one-shot；是否用“事件流订阅”而非 `await`？

---

## 13. 结语

`rusk-react` 的关键主张是：在 Rusk 的语言特性（代数效应 + 可嵌入 VM）背景下，UI 框架应当把“UI 能力”当作 **effect 能力边界**，把“DOM 变更”当作 **批处理事务**，从而同时获得：

- 比 Hooks 更稳定可组合的抽象方式（keyed + effect handler）
- 比逐条 DOM 调用更低的跨边界成本（single commit）
- React-like 的开发体验（组件 + VDOM）

后续如果该提案被采纳，可以进一步在 `completed-proposals/` 里固化为实施计划与里程碑。
