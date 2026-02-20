# Java 版 Rusk Bytecode VM（原型实现）

本目录是 `uncommitted-proposals/java-vm.md` 的一个**可运行原型**：在 JVM 上解析/校验/解释执行
Rusk 的 `.rbc` 字节码，并提供与 Rust 参考 VM 类似的宿主驱动 API：

- `step(fuel)`：以 fuel 驱动解释执行
- `resume(k, value)`：恢复一次外部化 effect 的挂起点
- `dropContinuation(k)`：取消一次外部化 effect（进入 trapped: `cancelled`）

实现目标优先对齐仓库内 `BYTECODE_SPEC.md` 与 Rust 参考实现（`crates/rusk-bytecode`、`crates/rusk-vm`）
的语义。

## 目录结构

- `src/main/java/dev/rusk/bytecode`：`.rbc` 数据模型 + 编解码 + verifier
- `src/main/java/dev/rusk/vm`：Java 解释器 VM（JVM GC 托管对象）
- `src/test/java/dev/rusk/vm`：自包含测试（无外部依赖）
- `scripts/test.sh`：用 `javac/java` 编译并运行测试

## 运行测试

需要 JDK 21（本仓库开发环境已包含）。

```bash
cd extra/java-vm
./scripts/test.sh
```

## 运行 `.rbc`

该目录还提供一个最小 CLI（默认安装 `_std_host::print/_std_host::println`）：

```bash
cd extra/java-vm
./scripts/run.sh path/to/program.rbc
```

也可以直接从 `.rusk` 编译并用 Java VM 运行（会调用仓库根目录的 `cargo run --bin ruskc`）：

```bash
cd extra/java-vm
./scripts/run_rusk.sh ../../examples/01-hello-world.rusk
```

## 集成示例（embedding）

`examples/` 目录包含几个“把 VM 当作库嵌入宿主应用”的最小示例，使用同样的 `javac/java` 方式运行：

```bash
cd extra/java-vm
./scripts/run_example.sh dev.rusk.examples.EmbedHostImport
./scripts/run_example.sh dev.rusk.examples.ExternalEffectAsync
./scripts/run_example.sh dev.rusk.examples.RunRbcFromFile --dump ../../examples/01-hello-world.rbc
```

可选参数：

- `--dump`：打印 module 的 host imports / external effects 列表
- `--fuel N`：每次 `step(N)` 驱动（用于观察 `Yield` 行为）
- `--allow-test-host`：安装 `test::*` 这组测试用 host imports
- `--allow-test-ffi`：为 `TestFfi.*` 安装测试用 externalized effects 处理器

## 说明与约束

- 该 Java VM 目前是独立子工程，不参与 Rust workspace 的 `cargo test`。
- `.rbc` 版本以 `crates/rusk-bytecode/src/rbc.rs` 中的 `MAGIC` 与版本号为准。
- 运行时堆对象（struct/enum/array/tuple）直接用 Java 对象图表示，由 JVM GC 回收；不会维护全局强引用表。
