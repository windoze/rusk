# Tokio Async Integration for Rusk (Single-VM Scheduler + Tokio Host Ops)

Date: 2026-03-05  
Status: Implemented

## Summary

This proposal integrates Rust **Tokio** with Rusk’s **effects + delimited continuations** to
support direct-style async **without** introducing a colored `async fn` / keyword `await`.

Key user-facing properties:

- `.await()` is a **normal method call** that may trigger an effect.
- Futures are **eager / hot**: constructing the future starts the host-side async operation.
- `.await()` is **sharable**: multiple tasks may await the same future; once completed it returns a
  cached value on subsequent awaits.
- `spawn(...) -> JoinHandle` enables **single-VM multitasking** (cooperative green tasks inside one
  VM instance) and allows joining later.
- `yield()` is a **scheduling hint only** (no fairness guarantees).
- Cancellation and failures are represented as values: operations return `Future<Result<T, E>>`
  rather than trapping the VM (so a failed HTTP request does not kill the whole program).

Example target ergonomics:

```rusk
// From the Rusk side, there is no “color” difference between sync and async code.
// `.await()` is just a method call that may trigger an effect and yield to the scheduler.
let res = std::http::get("https://example.com").await();
match res {
  Ok(body) => { /* use body */ },
  Err(e) => { /* handle error */ },
}
```

## Background & constraints

Rusk already has:

- **Algebraic effects + delimited continuations** (`@Iface.method(...)` with `match` effect arms;
  see `RUSK_SPEC.md` §7).
- A **host-driven stepping VM API** with **externalized effects**:
  `vm_step` → `StepResult::Request { effect_id, args, k }` → `vm_resume` / `vm_drop_continuation`
  (see `BYTECODE_SPEC.md` §6.4 and `completed-proposals/013-bytecode-ffi.md`).

Two VM/ABI constraints matter for async design:

1. **ABI boundary is small**: only `unit | bool | int | float | string | bytes | continuation`
   cross the VM/host boundary (`BYTECODE_SPEC.md` §3.1).
2. **Single outstanding external request per VM**: after `StepResult::Request`, the VM is
   suspended until resumed/dropped; only one suspension exists at a time (`BYTECODE_SPEC.md` §6.4).

Implication: if we implement every `.await()` as a separate externalized effect (the simplest
Tokio mapping), then *the entire VM blocks on the first await*, which makes **single-VM `spawn`
multitasking impossible**.

Therefore, this proposal uses an **in-VM scheduler** (implemented with effects/continuations) and
limits the host boundary to:

- **sync host imports** that start/cancel/query async operations without suspending the VM, and
- a single **externalized** “wait for any completion” effect (`wait_next`) used only when the VM
  has no runnable tasks.

## Goals

- Define a small `std::async` surface:
  - `Future<T>` with `.await()` as a method call,
  - `spawn` (single-VM tasks),
  - `yield()` (scheduling hint),
  - cancellation as `fut.cancel()`.
- Make common fallible operations return `Future<Result<T, E>>` (chosen over traps).
- Provide a Tokio-backed host implementation that:
  - runs the VM on a Tokio executor (`!Send` friendly),
  - starts async host operations eagerly,
  - resumes the VM when any operation completes.
- (Optional) Provide `std::http` APIs backed by `reqwest`.

## Non-goals

- Multi-VM concurrency as the primary model (VM swarms remain possible but out-of-scope here).
- A keyword-level `async fn` / `await` syntax.
- Preemptive scheduling inside one VM (this is cooperative).
- Structured values crossing the VM/host ABI boundary (still ABI-safe primitives only).
- Full async ecosystem in v1 (`select`, structured concurrency, cancellation scopes, channels).

## Design overview

The design is split into three layers:

1. **Rusk std API (`sysroot/std`)**
   - `std::async`: future abstraction, spawn/yield, and the scheduler contract.
   - `std::time`, `std::http` (optional): user-facing futures.
2. **In-VM scheduler (still “Rusk code”)**
   - Implemented as a top-level `match` effect handler over program execution.
   - Maintains a run queue of green tasks and a waiter map for pending futures.
3. **Tokio host**
   - Implements synchronous “start/cancel/take” host imports.
   - Implements one externalized effect `wait_next() -> int` that blocks until *any* host op
     completes and returns its `op_id`.

## Rusk-side API: `std::async`

### 1) Future abstraction

We define a minimal future interface:

```rusk
pub interface Future<T> {
  fn await(self) -> T;

  // Default: non-cancellable futures just return false.
  fn cancel(self) -> bool { false }
}
```

Semantics:

- `.await()` may suspend the *current green task* (not necessarily the whole VM).
- `.await()` is **sharable**: if multiple tasks call `.await()` on the same future value,
  all are resumed when the future completes and all observe the same cached completion value.
- `.await()` after completion returns immediately with the cached value.

### 2) `spawn` and joinability

We add single-VM multitasking:

```rusk
pub struct JoinHandle { /* opaque */ }

pub enum TaskError {
  Cancelled,
}

impl Future<Result<unit, TaskError>> for JoinHandle {
  fn await(self) -> Result<unit, TaskError>;
  fn cancel(self) -> bool;
}

pub fn spawn(f: fn() -> unit) -> JoinHandle;
```

Notes:

- `spawn` is cooperative and single-VM: tasks run only when scheduled by the in-VM scheduler.
- `.await()` on a `JoinHandle` is also **sharable**: multiple tasks may join the same handle and
  observe the same cached completion value.

### 3) `yield()` as a hint

```rusk
pub fn yield() -> unit;
```

Semantics:

- `yield()` is a **pure scheduling hint** to the in-VM scheduler.
- No guarantee is made about fairness, order, or timing.
- In practice, a reasonable implementation is: “move the current task to the back of the run
  queue”.

### 4) Scheduler hook (internal)

The public APIs above can be implemented as ordinary functions/methods that *internally* perform
effects on a private scheduler interface (not externalized to the host):

- (sketch)
  ```rusk
  // Private: handled in-language by the scheduler.
  interface _Scheduler {
    fn yield() -> unit;
    fn spawn(f: fn() -> unit) -> int; // returns `task_id`
    fn await_task(task_id: int) -> Result<unit, TaskError>;
    fn cancel_task(task_id: int) -> bool;
    fn await_host(op_id: int) -> bytes;
    fn cancel_host(op_id: int) -> bool;
  }
  ```

- `std::async::yield()` performs `@std::async::_Scheduler.yield()`
- `std::async::spawn(f)` performs `@std::async::_Scheduler.spawn(f)` and wraps the `task_id` into
  a `JoinHandle`.
- each host-backed `Future::await()` performs `@std::async::_Scheduler.await_host(op_id)` and then
  decodes the returned completion bytes to `Result<T, E>`.
- each host-backed `Future::cancel()` performs `@std::async::_Scheduler.cancel_host(op_id)`.
- `JoinHandle::await()` performs `@std::async::_Scheduler.await_task(task_id)`.
- `JoinHandle::cancel()` performs `@std::async::_Scheduler.cancel_task(task_id)`.

The scheduler itself is installed by wrapping the program entrypoint in a `match` with effect arms
for `std::async::_Scheduler.*` operations.

## Rusk-side API: eager host futures

### 1) Timers (`std::time`)

User-facing API (MVP):

```rusk
pub struct Sleep { /* opaque */ }

impl std::async::Future<Result<unit, std::time::Error>> for Sleep {
  fn await(self) -> Result<unit, std::time::Error>;
  fn cancel(self) -> bool;
}

pub fn sleep_ms(ms: int) -> Sleep;
```

Semantics:

- `sleep_ms(ms)` starts a host timer immediately and returns a future handle.
- `.await()` waits for completion and returns `Ok(())` or `Err(...)` (including cancellation).

### 2) HTTP (optional, `std::http`)

User-facing MVP:

```rusk
pub struct Get { /* opaque */ }

impl std::async::Future<Result<bytes, std::http::Error>> for Get {
  fn await(self) -> Result<bytes, std::http::Error>;
  fn cancel(self) -> bool;
}

pub fn get(url: string) -> Get;
```

Design choices:

- Return `Future<Result<T, E>>` (chosen) rather than trapping on network errors.
- Start minimal with `bytes`; richer responses (status, headers, streaming) can be added later via
  handles or stable encodings.

## In-VM scheduler (single VM, cooperative)

### High-level model

We implement a green-task scheduler entirely inside the VM using effects:

- Each green task runs until it:
  - completes (returns),
  - calls `yield()`, or
  - calls `.await()` on a pending future (becomes blocked).
- When the run queue is empty, the scheduler blocks the VM by performing the single externalized
  effect `@std::async::_HostAsync.wait_next()` to wait for any host op completion.

This satisfies the VM constraint “only one outstanding external request per VM” while still
supporting many in-flight operations and tasks.

### Scheduler state (conceptual)

Conceptually the scheduler maintains:

- `run_queue: [cont(unit) -> unit]`
- `waiters: Map<int, [cont(bytes) -> unit]>` keyed by host `op_id`
- `cached: Map<int, bytes>` for completed host ops (to support sharable `.await()`)
- `tasks: Map<int, TaskEntry>` for spawned tasks

Where a `TaskEntry` minimally contains:

- the task’s entry function `fn() -> unit` (before it starts),
- a completion slot `Option<Result<unit, TaskError>>` (to support sharable joins),
- a list of `join_waiters: [cont(Result<unit, TaskError>) -> unit]`,
- a `cancel_requested: bool` flag (cooperative cancellation).

The cached `bytes` values use the same tagged encoding as `op_take(...)` (see “Host result
encoding”).

### `.await()` lowering pattern

Each concrete future type implements `Future<T>::await()` by interacting with the scheduler.
Conceptually:

1. If the future already has a cached completion value, return it immediately.
2. Otherwise, perform a scheduler effect that:
   - captures the continuation of the current task,
   - registers it as a waiter on the future’s underlying `op_id`,
   - switches to the next runnable task (or blocks on `wait_next` if none).

Because continuations are one-shot, each `.await()` call registers a distinct continuation; when
the host op completes, the scheduler resumes all waiters once each and clears the waiter list.

`JoinHandle::await()` follows the same “cache or park” pattern, except the completion is produced
by the scheduler itself when the task finishes.

### `yield()` lowering pattern

`yield()` is a scheduler effect that:

- captures the current task continuation,
- re-enqueues it (implementation-defined),
- runs the next runnable task (or blocks if none).

### Cancellation (`fut.cancel()`)

Cancellation is cooperative and value-based:

- For host-backed futures, `fut.cancel()` calls into the scheduler, which:
  - requests cancellation from the host (if the future is backed by a host op),
  - marks the future as completed with `Err(Cancelled)` if it was still pending,
  - resumes all awaiting continuations with that error value.

- For spawned tasks, `handle.cancel()` requests cancellation from the scheduler:
  - if the task is still pending or blocked, the scheduler can complete it as
    `Err(TaskError::Cancelled)` and wake all joiners,
  - if the task is currently running, cancellation is cooperative and takes effect at the next
    scheduler boundary (`yield()` / host `.await()`), unless the task explicitly checks a
    cancellation flag (future extension).

Awaiting a cancelled operation yields an `Err(...)` value (no traps).

## Host ABI surface

### Core idea: start/cancel/take are sync; only `wait_next` suspends the VM

We define:

1. **Synchronous host imports** (non-yielding) to create and manage operations.
2. One **externalized effect** to block until any operation completes.

This keeps the host boundary compatible with:

- “no yield across FFI” (`CALL_HOST` is synchronous, see `completed-proposals/013-bytecode-ffi.md`),
- “single outstanding request per VM” (`BYTECODE_SPEC.md` §6.4).

### External effect: `wait_next`

```rusk
// In `std::async` (or an internal submodule):
interface _HostAsync {
  fn wait_next() -> int; // externalized effect
}
```

Host semantics:

- Suspend the VM until **any** in-flight host operation completes.
- Resume with the completed operation’s `op_id` (an `int`).

Notes:

- `wait_next` must be declared as an **externalized** effect with ABI signature `() -> int`
  (`BYTECODE_SPEC.md` §6.4). It should remain monomorphic (no runtime interface type args).

### Synchronous host imports: operation management

This proposal intentionally keeps the ABI small and monomorphic by using integer IDs.

Example host imports (HTTP; exact module name is bikeshed, shown as `loaf::_std_host_async`):

- `loaf::_std_host_async::http_get_start(url: string) -> int`
- `loaf::_std_host_async::op_cancel(op_id: int) -> bool`
- `loaf::_std_host_async::op_take(op_id: int) -> bytes`

Notes:

- `op_take` is **non-blocking**; it must return promptly.
- `op_take` is intended to be **consuming**:
  once it reports a terminal state (Ok/Err/Cancelled), the host may drop the operation and free
  resources. The scheduler caches the completion value in the VM heap to enable sharable awaits.

This same pattern can be used for timers (`sleep_start_ms`) and any other async-capable host
operation.

### Host result encoding (`bytes`)

Because the ABI cannot return structured values, `op_take(op_id)` returns a `bytes` blob encoding a
small tagged union:

- Tag byte `0x00`: `Pending`
- Tag byte `0x01`: `Ok(payload...)`
- Tag byte `0x02`: `Err(utf8_message...)`
- Tag byte `0x03`: `Cancelled` (optional; equivalent to `Err("cancelled")`)

Payload interpretation is operation-specific (e.g. HTTP `Ok` payload is response body bytes; timer
`Ok` has empty payload).

Rusk-side decoding maps this to `Result<T, E>` values, where:

- `T` is the operation’s success type (`bytes` for HTTP, `unit` for sleep, ...),
- `E` is an operation-specific error type (often a lightweight wrapper around `string`).

## Tokio host implementation (mapping to Tokio futures)

### Runtime constraints

The VM is `!Send` today (uses `Rc` internally), so the Tokio integration should be based on:

- a current-thread runtime and/or `tokio::task::LocalSet`,
- `spawn_local` for driving the VM.

### Host operation table

The host maintains a per-VM operation table:

- `op_id -> tokio::task::JoinHandle<OpResult>` plus a completion slot
- a completion queue (e.g. `tokio::sync::mpsc`) sending `op_id` when each op finishes

Operations are started by synchronous host imports like `http_get_start(url)`:

- spawn a Tokio task (e.g. `reqwest` request),
- allocate an `op_id`,
- on completion, store the result and enqueue `op_id` into the completion queue.

`wait_next()` external effect blocks until an `op_id` is available in the completion queue, then
resumes the VM with that integer.

### Driving the VM under Tokio

The Tokio runner drives `vm_step` in a loop:

- If the VM yields `Done` or `Trap`, return.
- If the VM yields `Yield`, re-schedule itself (normal executor fairness).
- If the VM yields `Request` for `_HostAsync.wait_next`, park the VM future until an op completes,
  then call `vm_resume(vm, k, AbiValue::Int(op_id))` and continue stepping.

Notably, in this design the host only needs to externalize **one effect** (`wait_next`) rather than
one effect per async operation.

## GC / host resource management

This proposal assumes:

- The *payload* of a completed operation (e.g. `bytes`) is copied into the VM heap and managed by
  the VM GC.
- Host-side resources (Tokio tasks, sockets, buffers) are managed by the host and freed when:
  - the operation completes and `op_take` consumes it, or
  - the Rusk code explicitly cancels via `fut.cancel()`, or
  - the entire VM instance is dropped (host clears the per-VM op table).

We do **not** require GC finalizers or “drop hooks” in v1. (We may revisit this if “never-awaited
futures leaking host ops” becomes a practical problem.)

## Follow-ups (optional)

### Typed task results (`spawn<T>`)

This proposal makes `spawn` joinable, but the MVP signature is intentionally:

```rusk
pub fn spawn(f: fn() -> unit) -> JoinHandle;
```

This keeps the in-VM scheduler implementation simple because all task entrypoints share the same
type (`fn() -> unit`) and all joins share the same result type (`Result<unit, TaskError>`).

A typed variant is desirable long-term (likely as a new type to avoid overloading `JoinHandle`):

```rusk
pub struct Task<T> { /* opaque */ }

impl<T> Future<Result<T, TaskError>> for Task<T> {
  fn await(self) -> Result<T, TaskError>;
  fn cancel(self) -> bool;
}

pub fn spawn<T>(f: fn() -> T) -> Task<T>;
```

Notes:

- Doing this in a single-VM scheduler likely needs some form of **type erasure** in the scheduler’s
  task table (e.g. an `Any`-like runtime value container) or a more capable closure/function value
  representation than `fn(...) -> ...`.
- Trap isolation is still unresolved: in a single VM, a trap is process-wide unless we introduce an
  exception effect model or VM-level fault isolation.

## Implementation plan (suggested phases)

1. **Phase 1 — Scheduler + timers**
   - Add `std::async` with scheduler-backed `.await()`, `spawn`, and `yield()`.
   - Add host op table + `wait_next` effect.
   - Implement `std::time::sleep_ms() -> Sleep`.
2. **Phase 2 — HTTP (reqwest feature)**
   - Add `std::http::get(url) -> Get`.
   - Map to `reqwest` in the Tokio host behind a Cargo feature.
3. **Phase 3 — Ergonomics + typed tasks**
   - Add a typed `spawn<T>(f: fn() -> T) -> Task<T>` API (optional).
   - Add basic combinators (`map`, `and_then`, `join_all`) in pure Rusk where possible.
4. **Phase 4 — Better structured I/O**
   - Consider handle-based APIs or stable encodings for richer HTTP responses.

## Alternatives considered

### A) Externalize every `.await()` (VM as a Tokio future)

Pros:

- Very simple host mapping: one `.await()` = one host request.
- Aligns with “VM swarm” concurrency philosophy in `completed-proposals/013-bytecode-ffi.md`.

Cons (blocking for this proposal’s requirements):

- Incompatible with **single-VM `spawn` multitasking** because of “single outstanding request per
  VM”.

### B) Multi-VM only (VM swarm)

Pros:

- Already fits the VM boundary perfectly.

Cons:

- Explicitly out-of-scope: the desired model is single-VM green tasks.

### C) Add richer ABI / generic external effects

Pros:

- Could allow a generic `await<T>` effect and richer host/stdlib APIs.

Cons:

- Significantly larger design and implementation effort; defers this proposal’s near-term goal.

## Open questions

1. **Where does the scheduler hook live?**  
   Chosen: when `std` is loaded, the compiler injects a synthetic entry wrapper
   (`__rusk_async_entry`) that calls `std::async::run(...)` (or `std::async::run_argv(...)`) around
   `main`. The Tokio runner handles only one externalized effect: `std::async::_HostAsync.wait_next`.
2. **How do we prevent host-op leaks from never-awaited futures long-term?**  
   v1 remains explicit: futures are cancellable via `cancel()`, and the host drops state on
   completion (`op_take`) or VM teardown. If this becomes a practical issue, revisit with GC
   finalizers and/or a refcounted host-handle protocol.
3. **Task trap isolation**  
   Not solved in v1: a trap is still VM-wide. Isolating task failures requires an exception effect
   model or multi-VM execution.
