这是 **Rusk (暂定名) 语言规范**。

这份规范旨在定义一个**内核极简、扩展性极强**的现代嵌入式语言。它保留了传统的循环关键字以确保即时性能，同时将大量语法糖（Effect, Async, Generators, Resource Management）统一到了**接口（Interface）**与**模式匹配（Match）**的机制中。

---

# Rusk Language Specification (v0.3 Draft)

## 1. 核心哲学 (Core Philosophy)

* **统一性 (Unification)**：副作用（Effect）即接口（Interface），处理（Handle）即匹配（Match）。
* **库优先 (Library First)**：语言只提供机制，并发 (`scope`)、模块 (`import`)、元编程 (`quote`) 下放到标准库。
* **值/引用语义 (Value/Reference Semantics)**：基础类型按值传递，复杂类型按引用传递；默认可变，`const/readonly` 可标记只读。
* **性能务实 (Pragmatic Performance)**：保留 `loop`/`while` 等原语，避免过度依赖编译器的高级优化（如 TCO）。

---

## 2. 词法与语法 (Lexical & Syntax)

### 2.1 关键字 (Keywords)

为了保持内核极简，我们只保留了以下必要的关键字：

| 类别 | 关键字 | 说明 |
| --- | --- | --- |
| **定义** | `fn`, `let`, `struct`, `enum`, `interface`, `impl` | 基础类型与函数定义 |
| **流程** | `if`, `else`, `match`, `return`, `loop`, `while`, `for`, `in`, `break`, `continue` | **保留循环原语**以保证基础性能 |
| **限定** | `const`, `readonly` | 只读绑定与只读视图 |

*(注：已移除 `effect`, `perform`, `resume`, `mut`, `ref`, `take`, `async`, `await`, `yield`, `try`, `catch`, `throw`, `with`, `class`)*

### 2.2 核心语法糖

* **Trailing Lambda (尾随闭包)**：
```rust
// 允许 func(arg) { code } 形式
std.concurrency.scope { ... } 

// 带参数的闭包
std.array.map(items, |x| { x + 1 });

```


* **Name Binding Expression (参数绑定)**：
```rust
// 允许在参数位置进行求值和重命名
send(m = msg);
open(f = file.open());

// 与尾随闭包搭配时，绑定进入闭包作用域
std.resource.with(file = File.open("log.txt")) {
    write_line(file, "hello");
}

```
*(注：当不与尾随闭包配合使用时，该语法仅等价于普通参数求值与传递，不会引入新的作用域或绑定效果。)*

* **Effect Marker (显式 Effect 标记)**：
```rust
// 使用 @ 前缀显式触发 Effect
@Logger.log("msg");
```

### 2.3 属性 (Attributes)

属性采用 Rust 风格语法，用于标注 item（函数、结构、接口、impl 等）：

```rust
#[rusk_export]
fn foo() { ... }
```

### 2.4 字面量与基础表达式 (Literals & Basics)

* **数字**：`123`, `3.14`
* **字符串**：`"text"`（UTF-8）
* **格式化字符串**：`f"Hello {name}"`
* **字节串**：`b"raw"`（bytes）
* **数组/列表**：`[1, 2, 3]`
* **结构体字面量**：`Point { x: 1, y: 2 }`
* **枚举变体**：`Result::Ok(val)`
* **单元值**：`()`（对应 `unit`）
* **注释**：`//` 行注释，`/* ... */` 块注释

`if`/`match`/`loop` 等均为表达式，可产生值。
表达式求值顺序为从左到右。

### 2.5 作用域与闭包 (Scopes & Closures)

* `{ ... }` 引入新的词法作用域；`let`/`const`/`readonly` 为块级作用域。
* 闭包通过 `|args| { ... }` 表达，捕获外部变量为共享引用（遵循其 `const/readonly` 约束）。
* `return` 结束当前函数；`break/continue` 作用于最近的循环。

### 2.6 模块与元编程 (Library-level)

* 模块系统与元编程（如 `import`/`quote`）不属于核心语法，交由标准库或宿主实现。
* 具体语法与加载策略由宿主决定（例如文件系统、内存模块或嵌入式资源）。



---

## 3. 类型系统 (Type System)

### 3.1 统一接口模型 (The Unified Interface Model)

**这是 Rusk 最核心的创新。** `interface` 关键字承载了三种语义，取决于调用方式：

1. **动态分派 (Dynamic Dispatch)**：`instance.method()`
* 对具体的对象实例调用。
* 通过 VTable 查找实现。


2. **静态/关联方法 (Static/Associated Method)**：`Interface::method()`
* 对接口类型进行静态调用（类似静态函数或工厂）。

3. **代数效果 (Algebraic Effect)**：`@Interface.method()`
* 显式触发 Effect 调用。
* 通过调用栈（Call Stack）查找 Handler。
* **无需 `effect` 或 `perform` 关键字，但必须使用 `@` 显式标记。**

*(注：`Interface.method()` 形式为语法错误；Effect 使用 `@Interface.method()`，静态方法使用 `Interface::method()`。)*

**实现语法 (Impl Syntax)**：

```rust
impl Logger for Console {
    fn log(msg: string) -> unit { ... }
}

impl Console {
    fn new() -> Console { ... }
}
```

* `impl Interface for Type` 提供接口实现，用于动态分派与类型检查。
* `impl Type` 提供该类型的固有方法（inherent methods）。
* 若固有方法与接口方法同名，固有方法优先；仍存在二义性时为语义错误。



### 3.2 数据类型

* **Primitives**: `int`, `float`, `bool`, `string`, `bytes`, `unit`.
* **字符串/字节串**：作为值类型，操作产生新值，不支持原地修改。
* **ADTs**: `enum` (带数据的枚举) 是核心逻辑载体。
* **Struct**: 数据聚合。
* **Array/List**: `[T]` 动态数组/列表，支持 `len()` 与索引访问 `arr[i]`。
* **Function**: `fn(T) -> U`（用于接口方法签名与类型标注）。
* **Std ADTs**: `Option<T>`, `Result<T, E>` 由标准库提供。

*(注：映射/字典等集合类型由标准库提供。)*
*(注：未来允许宿主通过 FFI 注册新的原生值类型，具体机制待定。)*

### 3.3 类型标注 (Type Annotations)

* 类型标注用于接口、结构体字段、函数签名以及可选的局部变量声明。
* 省略标注时，解释器以动态类型运行；静态类型检查属于可选特性。

### 3.4 结构体与枚举语法 (Struct & Enum)

```rust
struct Point { x: int, y: int }

enum Option<T> {
    Some(T),
    None
}
```

---

## 4. 内存管理 (Memory Management)

### 4.1 基础模型

* 单线程运行时，自动内存管理（引用计数或 tracing GC 属于实现细节）。
* 无生存期标注 (No Lifetimes)。

### 4.2 可变性与传参 (Mutability & Passing)

参数传递遵循按值/按引用的简单规则：**基础类型按值传递，复杂类型按引用传递**。对象默认可变，除非使用 `const`/`readonly` 标记。
绑定语法使用 `let`（可变）、`const`（绑定不可变）或 `readonly`（只读视图）。

```rust
fn sum(readonly items: [int]) -> int { ... } // 只读视图（不可修改 items）
fn push(items: [int], v: int) { ... }        // 通过引用修改共享对象
fn inc(x: int) -> int { x + 1 }              // 基础类型按值传递

const pi = 3.14;
readonly data = [1, 2, 3];
```

* **按值 (by value)**：仅适用于基础类型（`int`/`float`/`bool`/`string`/`bytes`/`unit`），传参时复制值。
* **按引用 (by reference)**：适用于结构体、枚举、数组、映射等复杂类型，传参时共享同一对象。
* **实现细节**：对 `string/bytes` 等值类型可采用拷贝优化（如 copy-on-write），不改变语义。
* **默认可变**：对象在引用语义下可被就地修改。
* **`let` 绑定**：默认可重新赋值。
* **`const` 绑定**：禁止重新赋值该变量（绑定不可变）。
* **`readonly` 视图**：可用于局部绑定或函数参数；通过该绑定禁止修改对象（只读），尝试修改会在运行时报错；不保证全局深度冻结。
* **可见性**：对引用类型的修改会被所有别名观察到（共享语义）。
* **赋值与修改**：`x = expr` 重新绑定变量；`obj.field = v` / `arr[i] = v` 修改对象本体（若目标为 `readonly` 则报错）。

---

## 5. 控制流与副作用 (Control Flow & Effects)

### 5.1 广义模式匹配 (Generalized Match)

`match` 是唯一的分发中心。它可以匹配**数据返回值**，也可以拦截**接口调用（Effect）**。

**语法结构：**

```rust
match <expression> {
    // 1. 值模式 (Value Pattern)
    <pattern> => <logic>,

    // 2. 接口模式 (Interface/Effect Pattern)
    // 语法：@InterfaceName.Method(args...)
    @InterfaceName.Method(arg1, arg2) => {
        // 在此块内，'resume' 是一个自动注入的函数
        // 调用 resume(val) 将值返回给触发点
        resume(result);
    }
}

```

**模式形式 (Patterns)**：

* `_` 通配
* 字面量（`1`, `"a"`, `true`, `()`）
* 绑定（`name`）
* 枚举变体：`Enum::Variant(p1, p2)`
* 结构体：`Type { field: p }`
* 数组前缀：`[p1, p2, ..]`

**语义规则：**

1. `match` 在求值 `<expression>` 时建立一个 **Effect Handler 作用域**，所有 `@Interface.method(...)` 分支在该作用域内生效。
2. 当 `<expression>` 内触发 `@Interface.method(...)` 时，运行时向上查找最近的 `match`，并按分支顺序寻找匹配的 Effect 分支；参数模式匹配成功即选中该分支。
3. 在 Effect 分支内自动注入 `resume`：  
   * `resume(v)` 会恢复**被挂起的调用点**，并继续执行其后续计算。  
   * `resume` 返回该 continuation 的最终结果，允许 handler 做后处理。  
   * `resume` **至多调用一次**；重复调用为运行时错误。  
   * 若分支结束且未调用 `resume`，则该分支的返回值直接作为该 Effect 调用的结果。
   * `v` 的类型必须符合该接口方法的返回类型。
   * continuation 可被实现为一等值；允许通过库 API 保存并在其他位置 `resume`，恢复时切换到捕获的执行栈。
4. 当 `<expression>` 正常结束后，`match` 只对**值模式**进行匹配；Effect 分支不参与最终值匹配。
5. 若 Effect 调用未被任何 `match` 处理，将向外传播；若到达顶层仍无人处理，则触发运行时错误。

*(注：`resume` 不是关键字，只在 Effect 分支中作为预定义标识符存在，且不可被重定义。)*

### 5.2 标准库 Effect 映射

语言不再内置 `async/generator`，而是通过标准库接口实现：

* **Async**: 定义为 `interface Suspend { fn await(id: u64); }`。
* **Generator**: 定义为 `interface Yield<T> { fn emit(val: T); }`。
* **Error**: 定义为 `interface Failure<E> { fn raise(err: E); }`。

*(注：未被处理的 `@Failure.raise` 将导致运行时错误；可通过 `match` 拦截实现 try/catch 等语义。)*

---

## 6. 并发 (Concurrency)

并发能力完全下放给标准库。语言默认单线程执行，通过库提供的容器（Container）与通道（Channel）实现隔离式并发。

```rust
// scope 是一个普通函数，内部 Handle 了 Spawn 接口
std.concurrency.scope {
    // launch 只是调用了 @Spawn.task() 接口
    std.concurrency.launch {
        task_logic();
    }
} // 函数返回前自动等待所有任务

```

### 6.1 容器与通道 (Container & Channel)

* **容器 (Container)**：脚本运行在隔离的单线程容器中。容器之间不共享内存与对象。
* **通道 (Channel)**：容器之间通过二进制 blob 通道通信，消息会被序列化/反序列化。
* **宿主与库**：
  * 宿主可创建多个容器并建立通道。
  * 标准库可提供 `std.container.spawn()` 等 API 允许脚本创建新容器。

### 6.2 可发送类型 (Sendable)

只有实现可序列化接口的类型可跨容器发送，且**不保留对象身份**（identity），不保证共享结构。

```rust
interface Serialize {
    fn serialize(self) -> bytes;
}

interface Deserialize {
    fn deserialize(bytes: bytes) -> Self;
}
```

* **Sendable 约束**：`T: Serialize + Deserialize` 的类型才能通过通道发送/接收。
* **资源类型**（文件句柄、socket、FFI 指针等）默认不可发送，除非提供显式代理或序列化策略。
* **示例**：`int`/`float`/`bool`/`string`/`bytes` 以及由它们构成的结构体或数组，可通过库提供的默认实现或手写实现成为 Sendable。对象图的共享关系与身份不保留，由序列化逻辑自行决定如何展开。

```rust
struct Point { x: int, y: int }

impl Serialize for Point { fn serialize(self) -> bytes { ... } }
impl Deserialize for Point { fn deserialize(bytes: bytes) -> Self { ... } }

std.channel.send(Point { x: 1, y: 2 });
let p = std.channel.recv<Point>();
```

---

## 7. 循环与迭代 (Loops)

为了保证解释器性能，保留原生循环关键字。

* `loop { ... }`: 无限循环。
* `while cond { ... }`: 条件循环。
* `for x in iter { ... }`: 语法糖，等价于 `loop + match Option` 的迭代展开。
* `break` / `continue`: 跳转控制。
* **迭代器**: 通过 `next()` 返回 `Option<T>` 约定（由标准库提供）。
* **返回值**: `loop/while/for` 默认返回 `unit`。

---

## 8. 互操作性 (FFI)

* **Rust -> Rusk**: `#[rusk_export]` 宏导出函数。Rust 的 `Future` 自动映射为 Rusk 的 `Suspend` 接口调用。
* **Rusk -> Rust**: 调用 Rust 函数时，Rust 端会收到 `&mut Context`，允许 Rust 代码“反向”调用 Rusk 接口（触发 Effect）。

---

## 9. 完整代码示例

这段代码展示了 Spec v0.3 的所有核心特性：**统一接口、广义匹配、显式 Effect 标记、以及保留的原生循环**。

```rust
// 1. 定义一个作为 Effect 使用的接口
interface Logger {
    fn log(msg: string) -> unit;
}

// 2. 业务逻辑 (无感知 Effect，像调用静态方法)
fn process_data(items: [int]) {
    let i = 0;
    // 保留的原生 while 循环，高性能
    while i < items.len() {
        let item = items[i];
        // 触发 Effect：直接调用接口
        @Logger.log(f"Processing: {item}");
        i = i + 1;
    }
}

// 3. 主程序
fn main() {
    let data = [1, 2, 3];

    // match: 同时处理逻辑返回值和副作用拦截
    match process_data(data) {
        // 正常结束
        () => print("Done"),

        // 拦截 Logger 接口调用
        @Logger.log(msg) => {
            print(f"[System Log]: {msg}");
            resume(()); // 恢复执行
        }
    }
}

```

### 总结

1. **极简内核**：去掉了几乎所有“特权”关键字（effect, async, try, etc.）。
2. **高性能基础**：保留 `loop/while` 确保了在没有 JIT/TCO 的情况下依然有可靠的性能。
3. **高度统一**：将 Effect 系统无缝融入 Interface 和 Match 语法中，并通过 `@` 显式标记消除歧义。
4. **值/引用语义**：基础类型按值传递、复杂类型按引用传递，默认可变，`const/readonly` 限制修改。
5. **隔离并发**：默认单线程容器 + 序列化通道模型，避免共享内存的复杂性。
