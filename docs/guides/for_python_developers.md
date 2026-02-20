# 面向 Python 开发者的 Rusk 指南

本指南从 Python 开发者的视角，介绍 Rusk 的语法与常见写法，并把一些日常结构（`None`/dict/generator/异常）映射到 Rusk 的对应模式。

如果你还没跑起来：先看 `docs/guides/quick_start.md`。

---

## 1. 你会觉得熟悉的地方

### 1.1 `f"..."` 风格的字符串插值

Python：

```py
msg = f"hello {name}, n={n}"
```

Rusk：

```rusk
let msg = f"hello {name}, n={n}";
```

Rusk 的 f-string 依赖 `core::fmt::ToString`，可为自定义类型实现它来控制插值行为。

### 1.2 `match`（模式匹配）

Python 3.10+：

```py
match v:
  case ("ok", x): ...
  case ("err", e): ...
```

Rusk 的 `match` 也是模式匹配，并且 **是表达式**（会产生值）。

### 1.3 动态数组

Python 的 `list` 可增长；Rusk 的 `[T]` 也可增长（更像 Rust 的 `Vec<T>`）：

```rusk
let xs = [1, 2, 3];
xs.push(4);
```

---

## 2. 关键差异（建议先读）

### 2.1 静态类型：很多错误更早暴露

Python 更偏动态；Rusk 是静态类型语言：

- `if` 条件必须是 `bool`（不会把整数/空列表当成真值）
- `list` 的元素类型固定（不是“混装任意对象”的容器）
- `struct` 字段固定（不支持运行时加字段）

### 2.2 `None` 不是默认空值：用 `Option<T>`

Python 常见写法：

```py
def find_user(id: str) -> User | None:
    ...
```

Rusk 推荐写成：

```rusk
fn find_user(id: string) -> Option<User> { ... }
```

并用 `match` 或组合子处理：

```rusk
match find_user(id) {
    Option::Some(u) => u.name,
    Option::None => "guest",
}
```

示例见 `examples/18-option-methods.rusk`。

### 2.3 字符串与字节要区分：`string` vs `bytes`

Python 有 `str`/`bytes`；Rusk 也类似：

- `string`：UTF-8，**不支持索引**（避免“按字节还是按字符”的歧义）
  - `slice(from, to)`：按字符（Unicode scalar / codepoints）下标切片
  - `byte_slice(from, to)`：按 UTF-8 字节偏移切片，返回 `bytes`
- `bytes`：二进制数据，支持 O(1) 索引、`get`、切片

同时 Rusk 还有 `byte`/`char` 两个基本类型，见 `examples/17-byte-char-slicing.rusk`。

### 2.4 异常不是默认控制流：更常用 `Option/Result`

Python 常用异常处理流程；Rusk 里你当然可以：

- 用 `panic(...)` 表示不可恢复错误（会陷阱）
- 用 `core::result` 的 `try/throw/catch/finally` 表达可恢复错误（底层基于 effects）

但在“可预期失败”的路径上，`Option`/`Result` 往往更清晰、更类型安全。

---

## 3. 常见结构映射（Python → Rusk）

### 3.1 `dict` → `core::map::Map`

Python：

```py
m: dict[str, int] = {}
m["a"] = 1
```

Rusk：

```rusk
use core::map::Map;

let m: Map<string, int> = Map::new();
m.insert("a", 1);
```

注意：`Map<K, V>` 对 key 有约束：`K` 必须实现 `Hash + Eq`。自定义 key 的写法见 `examples/16-core-map.rusk`。

### 3.2 dataclass / namedtuple → `struct`

Python：

```py
@dataclass
class Point:
    x: int
    y: int
```

Rusk：

```rusk
struct Point { x: int, y: int }
```

方法通过 `impl` 定义（不是运行时挂属性）：

```rusk
impl Point {
    fn sum() -> int { self.x + self.y }
}
```

### 3.3 代数数据类型：用 `enum` 表达状态机/树结构

Python 往往用“类层次 + if/elif”或“元组 + tag”。在 Rusk 中 `enum + match` 更自然：

```rusk
enum Expr {
    Lit(int),
    Add(Expr, Expr),
}

fn eval(e: Expr) -> int {
    match e {
        Expr::Lit(n) => n
        Expr::Add(a, b) => eval(a) + eval(b)
    }
}
```

### 3.4 generator → 用 effects 建模 `yield/resume`

Python：

```py
def gen():
    yield 1
    yield 2
```

Rusk 当前没有内建的 `yield` 关键字；推荐用 effects 表达生成器：

- 生成器内部 `@Yield<T>.yield(value)`
- 处理器里 `resume(())` 继续生成

示例见 `examples/22-effects-generator.rusk`。

### 3.5 协议/鸭子类型 → `interface` + `impl`

Python 里你可能依赖“只要有 `next()` 就行”的鸭子类型；Rusk 里需要显式接口与实现：

```rusk
use core::iter::Iterator;

struct Range { current: int, end: int }
impl Iterator for Range {
    type Item = int;
    fn next() -> Option<int> { ... }
}
```

---

## 4. 写法建议（更符合 Rusk 风格）

1. **把数据建模清楚**：用 `struct/enum` 定义域模型，靠类型系统替你挡掉一大类运行时错误。
2. **失败是显式的**：优先 `Option/Result`，再考虑 `throw` 或 `panic`。
3. **需要副作用就把边界写出来**：
   - 简单的 I/O：宿主函数（例如 `std::println`）
   - 可组合/可拦截的能力：effects + handler
4. **想“像 Python 一样随手写脚本”**：可以从 `examples/` 里挑最小的写法开始，然后逐步引入 `struct/enum/interface`。

---

## 5. 进一步阅读

- 概念与语法总览：`docs/guides/concepts_and_syntax.md`
- 宿主集成（把 Rusk 当嵌入脚本）：`docs/guides/host_integration.md`
- 示例目录（由易到难）：`examples/`
