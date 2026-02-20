# Java VM 集成示例

本目录提供一些“把 Java VM 当作库嵌入宿主应用”的最小示例（不依赖 Maven/Gradle）。

运行方式：在 `extra/java-vm` 目录下使用统一脚本编译并运行示例：

```bash
cd extra/java-vm
./scripts/run_example.sh <MainClass> [args...]
```

## 示例列表

### 1) `dev.rusk.examples.EmbedHostImport`

演示：

- 在 Java 中构造一个最小 `ExecutableModule`
- 声明并调用 host import
- 通过 `HostImportRegistry` 安装宿主函数
- 使用 `Vm.step()` 驱动到结束并读取返回值

运行：

```bash
./scripts/run_example.sh dev.rusk.examples.EmbedHostImport
```

### 2) `dev.rusk.examples.ExternalEffectAsync`

演示：

- externalized effects 的 `Request -> resume/drop` 流程
- 用线程池模拟“异步完成后把结果送回 VM 线程再 resume”
- 通过 `--timeout-ms` 触发 `dropContinuation`（模拟取消/超时）

运行（正常 resume）：

```bash
./scripts/run_example.sh dev.rusk.examples.ExternalEffectAsync
```

运行（超时 drop，最终 trapped: cancelled）：

```bash
./scripts/run_example.sh dev.rusk.examples.ExternalEffectAsync --timeout-ms 1
```

### 3) `dev.rusk.examples.RunRbcFromFile`

演示：

- 从磁盘读取 `.rbc`，使用 `Rbc.fromBytes(...)` 解码 + verifier
- 安装最小 `_std_host::print/_std_host::println` host imports
- 以 `step()` 循环驱动，并在遇到 `Request` 时进行分派和 `resume`

运行（仓库根目录自带示例 `.rbc`）：

```bash
./scripts/run_example.sh dev.rusk.examples.RunRbcFromFile --dump ../../examples/01-hello-world.rbc
```

