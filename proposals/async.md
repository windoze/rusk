# Rusk 的 Tokio 异步集成（单虚拟机调度器 + Tokio 宿主操作）

日期：2026-03-05（更新：2026-03-06）
状态：草案提案

## 摘要

本提案将 Rust **Tokio** 与 Rusk 的**效应（effects）+ 界定延续（delimited continuations）**集成，以支持直接风格（direct-style）的异步编程，**无需**引入着色（colored）的 `async fn` 或关键字 `await`。

关键的面向用户特性：

- `.await()` 是一个**普通的方法调用**，可能触发效应。
- Future 是**急切/热执行**的：构造 future 即启动宿主侧的异步操作。
- `.await()` 是**可共享的**：多个任务可以等待同一个 future；一旦完成，后续的等待将返回缓存的值。
- `spawn(...) -> JoinHandle` 启用**单虚拟机多任务**（单个虚拟机实例内的协作式绿色任务），并允许稍后 join。
- `yield()` **仅是调度提示**（无公平性保证）。
- 取消和失败表示为值：操作返回 `Future<Result<T, E>>` 而非陷入虚拟机（因此失败的 HTTP 请求不会杀死整个程序）。

目标人机工程学示例：

```rusk
// 从 Rusk 侧来看，同步和异步代码之间没有"颜色"差异。
// `.await()` 只是一个方法调用，可能触发效应并让出给调度器。
let res = std::http::request(
  "GET",
  "https://example.com",
  [],
  bytes::from_array([]),
).await();
match res {
  Ok(resp) => {
    // resp: std::http::Response
    // resp.status_code / resp.status_message / resp.headers / resp.body
  },
  Err(e) => { /* 处理错误 */ },
}
```

## 背景与约束

Rusk 已经具有：

- **代数效应 + 界定延续**（`@Iface.method(...)` 配合 `match` 效应分支；见 `RUSK_SPEC.md` §7）。
- 一个**宿主驱动的步进虚拟机 API** 与**外化效应**：
  `vm_step` → `StepResult::Request { effect_id, args, k }` → `vm_resume` / `vm_drop_continuation`
  （见 `BYTECODE_SPEC.md` §6.4 和 `completed-proposals/013-bytecode-ffi.md`）。

两个虚拟机/ABI 约束对异步设计很重要：

1. **ABI 边界受限但已支持结构化/泛型名义类型**：`extern fn` / 外化效应的签名必须是 ABI-eligible（`BYTECODE_SPEC.md` §3.1；亦见 `RUSK_SPEC.md` §3.2.1.1）。除了原语，还允许数组/元组/结构体/枚举，以及**闭合**的名义泛型实例（例如 `Option<bytes>`、`Result<std::http::Response, std::http::Error>`）。
2. **每个虚拟机只有一个未完成的外部请求**：在 `StepResult::Request` 之后，虚拟机会挂起直到被恢复或丢弃；同一时间只存在一个挂起（`BYTECODE_SPEC.md` §6.4）。

含义：如果我们将每个 `.await()` 实现为单独的外化效应（最简单的 Tokio 映射），那么*整个虚拟机会在第一个 await 处阻塞*，这使得**单虚拟机 `spawn` 多任务不可能**。

因此，本提案使用**虚拟机内调度器**（用效应/延续实现），并将宿主边界限制为：

- **同步宿主导入**，启动/取消/查询异步操作而不挂起虚拟机，以及
- 单个**外化**的"等待任何完成"效应（`wait_next`），仅在虚拟机没有可运行任务时使用。

## 目标

- 定义一个小的 `std::async` 表面：
  - 带有 `.await()` 作为方法调用的 `Future<T>`，
  - `spawn`（单虚拟机任务），
  - `yield()`（调度提示），
  - 取消为 `fut.cancel()`。
- 让常见的可失败操作返回 `Future<Result<T, E>>`（选择它而非陷阱）。
- 提供一个基于 Tokio 的宿主实现，该实现：
  - 在 Tokio 执行器上运行虚拟机（`!Send` 友好），
  - 急切地启动异步宿主操作，
  - 当任何操作完成时恢复虚拟机。
- （可选）提供基于 `reqwest` 的 `std::http` API。

## 非目标

- 将多虚拟机并发作为主要模型（虚拟机集群仍然可能，但不在此范围内）。
- 关键字级别的 `async fn` / `await` 语法。
- 单个虚拟机内的抢占式调度（这是协作式的）。
- 在 ABI 边界支持**未闭合**的泛型或运行时多态（`extern fn` / 外化效应仍必须是单态；名义泛型只允许闭合实例）。
- v1 中的完整异步生态系统（`select`、结构化并发、取消作用域、通道）。

## 设计概述

设计分为三层：

1. **Rusk 标准库 API（`sysroot/std`）**
   - `std::async`：future 抽象、spawn/yield 和调度器契约。
   - `std::time`、`std::http`（可选）：面向用户的 future。
2. **虚拟机内调度器（仍然是"Rusk 代码"）**
   - 实现为程序执行上的顶层 `match` 效应处理器。
   - 维护绿色任务的运行队列和待定 future 的等待者映射。
3. **Tokio 宿主**
   - 实现同步的"启动/取消/获取"宿主导入。
   - 实现一个外化效应 `wait_next() -> int`，它会阻塞直到*任何*宿主操作完成并返回其 `op_id`。

## Rusk 侧 API：`std::async`

### 1) Future 抽象

我们定义一个最小的 future 接口：

```rusk
pub interface Future<T> {
  fn await(self) -> T;

  // 默认：不可取消的 future 只返回 false。
  fn cancel(self) -> bool { false }
}
```

语义：

- `.await()` 可能会挂起*当前绿色任务*（不一定是整个虚拟机）。
- `.await()` 是**可共享的**：如果多个任务在同一个 future 值上调用 `.await()`，
  当 future 完成时所有任务都会被恢复，并且所有任务观察到相同的缓存完成值。
- 完成后的 `.await()` 立即返回缓存的值。

### 2) `spawn` 和可 join 性

我们添加单虚拟机多任务：

```rusk
pub struct JoinHandle { /* 不透明 */ }

pub enum TaskError {
  Cancelled,
}

impl Future<Result<unit, TaskError>> for JoinHandle {
  fn await(self) -> Result<unit, TaskError>;
  fn cancel(self) -> bool;
}

pub fn spawn(f: fn() -> unit) -> JoinHandle;
```

注意：

- `spawn` 是协作式且单虚拟机的：任务仅在虚拟机内调度器调度时运行。
- `JoinHandle` 上的 `.await()` 也是**可共享的**：多个任务可以 join 同一个句柄并观察到相同的缓存完成值。

### 3) `yield()` 作为提示

```rusk
pub fn yield() -> unit;
```

语义：

- `yield()` 是对虚拟机内调度器的**纯调度提示**。
- 不保证公平性、顺序或时机。
- 实际上，一个合理的实现是："将当前任务移到运行队列的末尾"。

### 4) 调度器钩子（内部）

上述公共 API 可以作为普通函数/方法实现，它们*内部*在私有调度器接口上执行效应（不外化到宿主）：

- （草图）
  ```rusk
  // 私有：宿主异步完成值（用于把不同 host op 的完成结果“擦到同一类型”）。
  //
  // 这使调度器可以维护单个 `waiters/cached: Map<int, ...>`，并且仍然是结构化值
  // （不再需要 bytes 标记编码）。
  enum HostOpResult {
    Sleep(Result<unit, std::time::Error>),
    Http(Result<std::http::Response, std::http::Error>),
  }

  // 私有：由调度器在语言内处理。
  interface _Scheduler {
    fn yield() -> unit;
    fn spawn(f: fn() -> unit) -> int; // 返回 `task_id`
    fn await_task(task_id: int) -> Result<unit, TaskError>;
    fn cancel_task(task_id: int) -> bool;
    fn await_host(op_id: int) -> HostOpResult;
    fn cancel_host(op_id: int) -> bool;
  }
  ```

- `std::async::yield()` 执行 `@std::async::_Scheduler.yield()`
- `std::async::spawn(f)` 执行 `@std::async::_Scheduler.spawn(f)` 并将 `task_id` 包装进 `JoinHandle`。
- 每个宿主支持的 `Future::await()` 执行 `@std::async::_Scheduler.await_host(op_id)`，并从 `HostOpResult` 中提取对应分支（例如 `Sleep(...)` / `Http(...)`）。
- 每个宿主支持的 `Future::cancel()` 执行 `@std::async::_Scheduler.cancel_host(op_id)`。
- `JoinHandle::await()` 执行 `@std::async::_Scheduler.await_task(task_id)`。
- `JoinHandle::cancel()` 执行 `@std::async::_Scheduler.cancel_task(task_id)`。

调度器本身通过用 `match` 包装程序入口点来安装，其中包含 `std::async::_Scheduler.*` 操作的效应分支。

## Rusk 侧 API：急切宿主 future

### 1) 定时器（`std::time`）

面向用户的 API（MVP）：

```rusk
pub struct Sleep { /* 不透明 */ }

impl std::async::Future<Result<unit, std::time::Error>> for Sleep {
  fn await(self) -> Result<unit, std::time::Error>;
  fn cancel(self) -> bool;
}

pub fn sleep_ms(ms: int) -> Sleep;
```

语义：

- `sleep_ms(ms)` 立即启动宿主定时器并返回 future 句柄。
- `.await()` 等待完成并返回 `Ok(())` 或 `Err(...)`（包括取消）。

### 2) HTTP（可选，`std::http`）

面向用户的 MVP：

```rusk
pub struct Request { /* 不透明 */ }

pub struct Response {
  status_code: int,
  status_message: Option<string>,
  headers: [(string, string)],
  body: bytes,
}

impl std::async::Future<Result<Response, std::http::Error>> for Request {
  fn await(self) -> Result<Response, std::http::Error>;
  fn cancel(self) -> bool;
}

pub fn request(method: string, url: string, headers: [(string, string)], body: bytes) -> Request;

// 便捷 API（可选；不作为设计核心）。
pub fn get(url: string) -> Request;
```

设计选择：

- 返回 `Future<Result<T, E>>`（已选择）而非在网络错误时陷阱。
- 在 ABI 支持结构化值之后，响应直接建模为 `Response { status_code, status_message, headers, body }`：
  - `headers` 保留顺序并允许重复（与 HTTP 语义对齐）。
  - `body` 仍然以 `bytes` 起步；流式传输可作为未来扩展。

## 虚拟机内调度器（单虚拟机、协作式）

### 高层模型

我们使用效应在虚拟机内完全实现绿色任务调度器：

- 每个绿色任务运行直到它：
  - 完成（返回），
  - 调用 `yield()`，或
  - 在待定 future 上调用 `.await()`（变为阻塞）。
- 当运行队列为空时，调度器通过执行单个外化效应 `@std::async::_HostAsync.wait_next()` 来阻塞虚拟机，以等待任何宿主操作完成。

这满足虚拟机约束"每个虚拟机只有一个未完成的外部请求"，同时仍支持许多进行中的操作和任务。

### 调度器状态（概念）

概念上调度器维护：

- `run_queue: [cont(unit) -> unit]`
- `waiters: Map<int, [cont(HostOpResult) -> unit]>` 由宿主 `op_id` 键入
- `cached: Map<int, HostOpResult>` 用于已完成的宿主操作（支持可共享的 `.await()`）
- `tasks: Map<int, TaskEntry>` 用于已 spawn 的任务

其中 `TaskEntry` 至少包含：

- 任务的入口函数 `fn() -> unit`（在它启动之前），
- 一个完成槽 `Option<Result<unit, TaskError>>`（支持可共享的 join），
- 一个 `join_waiters: [cont(Result<unit, TaskError>) -> unit]` 列表，
- 一个 `cancel_requested: bool` 标志（协作取消）。

缓存值与 `op_take(...)` 返回的结构化完成值一致（见"宿主结果返回类型"）。

### `.await()` 降级模式

每个具体的 future 类型通过与调度器交互来实现 `Future<T>::await()`。
概念上：

1. 如果 future 已经有缓存的完成值，立即返回它。
2. 否则，执行一个调度器效应，该效应：
   - 捕获当前任务的延续，
   - 将其注册为 future 底层 `op_id` 的等待者，
   - 切换到下一个可运行任务（如果没有则阻塞在 `wait_next` 上）。

因为延续是一次性的，每次 `.await()` 调用注册一个不同的延续；当宿主操作完成时，调度器恢复所有等待者各一次并清除等待者列表。

`JoinHandle::await()` 遵循相同的"缓存或停车"模式，除了完成是由调度器本身在任务完成时产生的。

### `yield()` 降级模式

`yield()` 是一个调度器效应，它：

- 捕获当前任务延续，
- 重新入队（实现定义的），
- 运行下一个可运行任务（如果没有则阻塞）。

### 取消（`fut.cancel()`）

取消是协作式且基于值的：

- 对于宿主支持的 future，`fut.cancel()` 调用调度器，调度器：
  - 从宿主请求取消（如果 future 由宿主操作支持），
  - 如果它仍然待定，将 future 标记为已完成并带有 `Err(Cancelled)`，
  - 用该错误值恢复所有等待的延续。

- 对于已 spawn 的任务，`handle.cancel()` 从调度器请求取消：
  - 如果任务仍然待定或阻塞，调度器可以将其完成为 `Err(TaskError::Cancelled)` 并唤醒所有 joiner，
  - 如果任务当前正在运行，取消是协作的，并在下一个调度器边界（`yield()` / 宿主 `.await()`）生效，除非任务显式检查取消标志（未来扩展）。

等待已取消的操作会产生 `Err(...)` 值（无陷阱）。

## 宿主 ABI 表面

### 核心思想：启动/取消/获取是同步的；只有 `wait_next` 挂起虚拟机

我们定义：

1. **同步宿主导入**（非让出）来创建和管理操作。
2. 一个**外化效应**来阻塞直到任何操作完成。

这使宿主边界兼容：

- "FFI 无让出"（`CALL_HOST` 是同步的，见 `completed-proposals/013-bytecode-ffi.md`），
- "每个虚拟机只有一个未完成请求"（`BYTECODE_SPEC.md` §6.4）。

### 外部效应：`wait_next`

```rusk
// 在 `std::async`（或内部子模块）中：
interface _HostAsync {
  fn wait_next() -> int; // 外化效应
}
```

宿主语义：

- 挂起虚拟机直到**任何**进行中的宿主操作完成。
- 用已完成操作的 `op_id`（一个 `int`）恢复。

注意：

- `wait_next` 必须声明为带有 ABI 签名 `() -> int` 的**外化**效应（`BYTECODE_SPEC.md` §6.4）。它应该保持单态（无运行时接口类型参数）。

### 同步宿主导入：操作管理

本提案故意通过使用整数 ID 保持 ABI 小且单态。

示例宿主导入（HTTP；确切的模块名称有待讨论，显示为 `loaf::_std_host_async`）：

- `loaf::_std_host_async::http_request_start(method: string, url: string, headers: [(string, string)], body: bytes) -> int`
- `loaf::_std_host_async::sleep_start_ms(ms: int) -> int`
- `loaf::_std_host_async::op_cancel(op_id: int) -> bool`
- `loaf::_std_host_async::op_take(op_id: int) -> Option<std::async::HostOpResult>`

注意：

- `op_take` 是**非阻塞的**；它必须立即返回。
- `op_take` 旨在是**消费性的**：一旦它报告终端状态（`Some(...)`），宿主可能会删除操作并释放资源。调度器在虚拟机堆中缓存完成值以启用可共享的等待。

相同的模式可用于定时器（`sleep_start_ms`）和任何其他支持异步的宿主操作。

### 宿主结果返回类型（结构化 ABI）

随着 ABI 已允许数组/元组/结构体/枚举以及**闭合**的名义泛型实例（`BYTECODE_SPEC.md` §3.1），
`op_take(op_id)` 不再需要返回 `bytes` 来做手工标记编码。

取而代之，`op_take` 使用结构化返回类型来表达“仍在等待 / 已终止（成功/失败/取消）”：

```rusk
// 返回 None 表示仍 Pending；返回 Some(...) 表示已终止（并且消费该 op 的结果）。
// `op_take` 位于 `loaf::_std_host_async` 模块中；此处仅展示其 ABI 形状。
extern fn op_take(op_id: int) -> Option<std::async::HostOpResult>;
```

其中 `HostOpResult` 是一个（私有）bridge enum（见上文调度器钩子草图）：

- 每个宿主操作对应一个变体（例如 `Sleep(...)` / `Http(...)`）。
- 每个变体内部携带一个 `Result<..>`，统一用 `Err(...)` 表示失败与取消（无陷阱）。

## Tokio 宿主实现（映射到 Tokio future）

### 运行时约束

虚拟机今天是 `!Send` 的（内部使用 `Rc`），所以 Tokio 集成应该基于：

- 一个当前线程运行时和/或 `tokio::task::LocalSet`，
- `spawn_local` 用于驱动虚拟机。

### 宿主操作表

宿主维护每个虚拟机的操作表：

- `op_id -> tokio::task::JoinHandle<OpResult>` 加上一个完成槽
- 一个完成队列（例如 `tokio::sync::mpsc`），在每个操作完成时发送 `op_id`

操作由同步宿主导入如 `http_request_start(method, url, headers, body)` 启动：

- spawn 一个 Tokio 任务（例如 `reqwest` 请求），
- 分配一个 `op_id`，
- 完成时，存储结构化完成值（`HostOpResult`）并将 `op_id` 入队到完成队列。

`wait_next()` 外部效应阻塞直到完成队列中有 `op_id` 可用，然后用该整数恢复虚拟机。

### 在 Tokio 下驱动虚拟机

Tokio 运行器在循环中驱动 `vm_step`：

- 如果虚拟机产生 `Done` 或 `Trap`，返回。
- 如果虚拟机产生 `Yield`，重新调度自己（正常执行器公平性）。
- 如果虚拟机为 `_HostAsync.wait_next` 产生 `Request`，停放虚拟机 future 直到操作完成，然后调用 `vm_resume(vm, k, AbiValue::Int(op_id))` 并继续步进。

值得注意的是，在这个设计中，宿主只需要外化**一个效应**（`wait_next`）而不是每个异步操作一个效应。

## GC / 宿主资源管理

本提案假设：

- 已完成操作的*有效载荷*（可能是结构化值，例如 `std::http::Response`）以 ABI-eligible 形式存在于虚拟机堆中并由虚拟机 GC 管理。
- 宿主侧资源（Tokio 任务、套接字、缓冲区）由宿主管理，并在以下情况下释放：
  - 操作完成且 `op_take` 消费它，或
  - Rusk 代码通过 `fut.cancel()` 显式取消，或
  - 整个虚拟机实例被删除（宿主清除每个虚拟机的操作表）。

我们在 v1 中**不**需要 GC 终结器或"drop 钩子"。（如果"从未等待的 future 泄漏宿主操作"成为实际问题，我们可能会重新审视这一点。）

## 后续工作（可选）

### 类型化任务结果（`spawn<T>`）

本提案使 `spawn` 可 join，但 MVP 签名故意是：

```rusk
pub fn spawn(f: fn() -> unit) -> JoinHandle;
```

这使虚拟机内调度器实现保持简单，因为所有任务入口点共享相同类型（`fn() -> unit`），所有 join 共享相同结果类型（`Result<unit, TaskError>`）。

长期来看，类型化变体是可取的（可能作为新类型以避免重载 `JoinHandle`）：

```rusk
pub struct Task<T> { /* 不透明 */ }

impl<T> Future<Result<T, TaskError>> for Task<T> {
  fn await(self) -> Result<T, TaskError>;
  fn cancel(self) -> bool;
}

pub fn spawn<T>(f: fn() -> T) -> Task<T>;
```

注意：

- 在单虚拟机调度器中执行此操作可能需要某种形式的**类型擦除**在调度器的任务表中（例如类似 `Any` 的运行时值容器）或比 `fn(...) -> ...` 更强大的闭包/函数值表示。
- 陷阱隔离仍未解决：在单个虚拟机中，陷阱是进程范围的，除非我们引入异常效应模型或虚拟机级别的故障隔离。

## 实现计划（建议阶段）

1. **阶段 1 — 调度器 + 定时器**
   - 添加带有调度器支持的 `.await()`、`spawn` 和 `yield()` 的 `std::async`。
   - 添加宿主操作表 + `wait_next` 效应。
   - 实现 `std::time::sleep_ms() -> Sleep`。
2. **阶段 2 — HTTP（reqwest 特性）**
   - 添加 `std::http::request(method, url, headers, body) -> Request` 与 `std::http::Response`。
   - （可选）保留 `std::http::get(url) -> Request` 作为对 `request("GET", ...)` 的薄封装。
   - 在 Cargo 特性后面映射到 Tokio 宿主中的 `reqwest`。
3. **阶段 3 — 人机工程学 + 类型化任务**
   - 添加类型化的 `spawn<T>(f: fn() -> T) -> Task<T>` API（可选）。
   - 在纯 Rusk 中尽可能添加基本组合器（`map`、`and_then`、`join_all`）。
4. **阶段 4 — 更好的结构化 I/O**
   - 考虑流式 body（避免一次性把大响应读入 `bytes`），以及更丰富的请求/响应建模（例如更强类型的 URL/方法、trailer）。

## 考虑的替代方案

### A) 外化每个 `.await()`（虚拟机作为 Tokio future）

优点：

- 非常简单的宿主映射：一个 `.await()` = 一个宿主请求。
- 与 `completed-proposals/013-bytecode-ffi.md` 中的"虚拟机集群"并发哲学一致。

缺点（阻止本提案的需求）：

- 与**单虚拟机 `spawn` 多任务**不兼容，因为"每个虚拟机只有一个未完成请求"。

### B) 仅多虚拟机（虚拟机集群）

优点：

- 已经完美适配虚拟机边界。

缺点：

- 明确超出范围：期望的模型是单虚拟机绿色任务。

### C) 添加更丰富的 ABI / 泛型外部效应

优点：

- 可以允许泛型 `await<T>` 效应和更丰富的宿主/标准库 API。

缺点：

- 显著更大的设计和实现工作量；推迟本提案的近期目标。

## 开放问题

1. **调度器钩子应该放在哪里？**
   我们是在标准 `std::async::run(...)` 中包装用户入口点，还是 Tokio 运行器调用专用包装器入口函数来安装调度器处理器？
2. **我们如何长期防止从未等待的 future 导致的宿主操作泄漏？**
   v1 依赖显式 `cancel()` 和宿主在完成/虚拟机删除时的清理。如果这还不够，我们可能需要 GC 终结器或引用计数的宿主句柄协议。
3. **任务陷阱隔离**
   在单个虚拟机中，陷阱是进程范围的。如果我们希望可 join 的任务报告失败而不陷阱整个程序，这可能需要异常效应模型或多虚拟机。
