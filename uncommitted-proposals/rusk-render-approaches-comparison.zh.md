# 渲染方法对比：三种不同的架构

## 问题的本质

**核心问题**：当状态变化时，如何高效地更新 DOM？

这里有三个关键步骤：
1. **检测变化**：哪些状态变了？
2. **计算更新**：需要做哪些 DOM 操作？
3. **应用更新**：如何批量执行这些操作？

不同的框架在这三个步骤上有不同的选择。

---

## 方案 1：Component-level VDOM + Diff（React/Vue）

### 工作流程

```rusk
状态变化
  ↓
标记组件需要更新（可选：通过响应式系统）
  ↓
重新执行 render 函数 → 生成新的 VNode 树
  ↓
Diff 新旧 VNode 树 → 得到 DOM 操作列表
  ↓
批量应用 DOM 操作
```

### 代码示例

```rusk
// 状态
let count = @State.new(0);

// 渲染函数：纯函数，每次更新都会重新执行
fn Counter() -> VNode {
    let current_count = count.get();  // 读取状态

    @VNode.element("div", [], [
        @VNode.element("p", [], [
            @VNode.text(format("Count: {}", current_count))
        ]),
        @VNode.element("button", [
            ("onClick", increment_handler)
        ], [
            @VNode.text("+1")
        ])
    ])
}

// 更新流程
fn on_state_change() {
    let old_vnode = current_vnode;
    let new_vnode = Counter();  // ← 重新执行整个 render 函数

    let ops = diff(old_vnode, new_vnode);  // ← Diff 算法
    batch_apply(ops);

    current_vnode = new_vnode;
}
```

### 特点

**优点**：
- ✅ **声明式**：只描述"UI 应该是什么样"
- ✅ **简单直观**：容易理解和调试
- ✅ **灵活**：可以有复杂的渲染逻辑
- ✅ **成熟**：被广泛验证的方案

**缺点**：
- ❌ **开销大**：每次都重新执行整个 render 函数
- ❌ **需要 diff**：对比两棵树的成本
- ❌ **粗粒度**：即使只有一个字段变化，也要重新渲染整个组件

**适用场景**：
- 组件结构经常变化
- 大范围的 UI 更新
- 开发体验优先

---

## 方案 2：Fine-grained Reactivity（Solid.js/Svelte）

### 工作流程

```rusk
状态变化
  ↓
触发订阅该状态的 effect
  ↓
Effect 直接执行 DOM 更新
  ↓
批量应用 DOM 操作
```

### 代码示例

```rusk
// 状态：响应式 Signal
let count = @Signal.create(0);

// 渲染函数：只执行一次，建立订阅关系
fn Counter() -> DomNode {
    // 创建 DOM 节点
    let div = @Dom.create_element("div");
    let p = @Dom.create_element("p");
    let button = @Dom.create_element("button");

    // 静态内容：直接设置
    @Dom.set_text(button, "+1");

    // 动态内容：建立响应式绑定
    @Effect.create(|| {
        let value = count.get();  // ← 自动注册依赖
        @Dom.set_text(p, format("Count: {}", value));
        // ↑ 当 count 变化时，只会重新执行这一行
    });

    // 组装
    @Dom.append_child(div, p);
    @Dom.append_child(div, button);

    div  // 返回真实的 DOM 引用（或句柄）
}

// 更新流程：自动的！
fn increment() {
    count.set(count.get() + 1);  // ← 自动触发相关的 effect
    // 不需要手动调用任何更新函数
}
```

### 特点

**优点**：
- ✅ **无需 diff**：直接知道要更新什么
- ✅ **精确更新**：只更新实际变化的部分
- ✅ **性能优秀**：O(变化数) 而不是 O(组件大小)
- ✅ **自动优化**：不需要 React.memo、useMemo 等手动优化

**缺点**：
- ❌ **首次渲染慢**：需要建立所有订阅关系
- ❌ **内存占用**：需要维护订阅图
- ❌ **心智模型不同**：需要理解响应式编程
- ❌ **失去了 VDOM**：无法做某些优化（如时间切片）

**适用场景**：
- 高频局部更新（如实时数据、动画）
- 更新通常是单个字段
- 性能是关键指标

---

## 方案 3：Compiled Components（Svelte compiler magic）

### 工作流程

```rusk
编译时：
  分析组件 → 识别动态部分 → 生成精确的更新代码

运行时：
  状态变化 → 直接调用生成的更新函数
```

### 代码示例（伪代码，展示编译后的效果）

```rusk
// 源代码（开发者写的）
fn Counter() {
    let count = 0;

    <div>
        <p>Count: {count}</p>
        <button onClick={() => count += 1}>+1</button>
    </div>
}

// 编译后（实际运行的）
fn Counter_compiled() {
    let count = 0;

    // 创建 DOM（只执行一次）
    let div = create_element("div");
    let p = create_element("p");
    let text_node = create_text_node("");
    let button = create_element("button");

    // 初始化
    set_text(button, "+1");
    append_child(p, text_node);
    append_child(div, p);
    append_child(div, button);

    // 生成精确的更新函数
    fn update_count(new_value: int) {
        count = new_value;
        set_text(text_node, format("Count: {}", count));
        // ↑ 编译器知道只有这个文本节点需要更新
    }

    // 绑定事件
    add_event_listener(button, "click", || {
        update_count(count + 1);
    });

    div
}
```

### 特点

**优点**：
- ✅ **最优性能**：编译器生成最精确的更新代码
- ✅ **无运行时开销**：没有 diff，没有复杂的响应式系统
- ✅ **包体积小**：不需要运行时库

**缺点**：
- ❌ **需要编译器**：增加构建复杂度
- ❌ **灵活性受限**：某些动态场景难以优化
- ❌ **调试困难**：生成的代码可能很复杂

**适用场景**：
- 静态内容居多
- 包体积敏感
- 愿意投入编译器开发

---

## 直接对比

| 维度 | VDOM + Diff | Fine-grained | Compiled |
|------|-------------|--------------|----------|
| **Diff？** | 是 | 否 | 否 |
| **Render 函数** | 每次执行 | 只执行一次 | 编译时分析 |
| **更新粒度** | 组件级 | 值级 | 值级 |
| **首次渲染** | 快 | 中 | 快 |
| **更新性能** | 中 | 快 | 最快 |
| **内存** | 两棵树 | 订阅图 | 最小 |
| **运行时大小** | 大 | 中 | 小 |
| **心智模型** | 简单 | 中等 | 简单 |
| **开发体验** | 好 | 中 | 好 |
| **代表** | React, Vue | Solid.js | Svelte |

---

## 你的问题的答案

回到你的两个方案：

### 你说的"方案 2"可能有两种理解

#### 理解 A：Incremental DOM
```rusk
fn render() {
    element_open("div");
      text("Hello");
    element_close("div");
    // ↑ 这些调用被记录为操作
}
```

**我的观点**：❌ **不推荐**
- 失去了声明式的优势
- 代码复杂、难以维护
- 控制流麻烦（条件、循环）

#### 理解 B：Fine-grained Reactivity
```rusk
fn render() {
    let div = create_element("div");

    @Effect.create(|| {
        set_text(div, count.get());
        // ↑ 这是一个 effect，会在 count 变化时自动执行
    });
}
```

**我的观点**：✅ **非常推荐**
- 性能优秀
- 符合 Rusk 的 effect 系统
- 减少边界跨越

---

## 我的建议

对于 Rusk，我建议采用 **混合方案**：

### 阶段 1：先实现 VDOM + Diff（原型）
```rusk
fn Counter() -> VNode {
    @VNode.element("div", [], [
        @VNode.text(format("Count: {}", count.get()))
    ])
}
```

**理由**：
- 快速验证可行性
- 简单直观，容易调试
- 可以先跑起来

### 阶段 2：引入 Fine-grained Reactivity（优化）
```rusk
fn Counter() -> VNode {
    @VNode.element("div", [], [
        // 叶子节点用 Signal
        @VNode.text_reactive(@Memo.create(|| {
            format("Count: {}", count.get())
        }))
    ])
}
```

**理由**：
- 组件结构仍然是声明式的（保持 VDOM）
- 但叶子节点的更新是细粒度的（用 Signal）
- 兼顾开发体验和性能

### 阶段 3（可选）：编译器优化
- 分析组件，自动识别静态部分
- 自动将合适的部分转换为细粒度更新
- 对开发者透明

---

## 示例：混合方案的代码

```rusk
interface VNode {
    // 常规节点
    fn element(tag: string, props: Props, children: [VNode]) -> VNode;
    fn text(content: string) -> VNode;

    // 响应式节点（新增）
    fn text_reactive(content: Signal<string>) -> VNode;
    fn element_reactive(
        tag: string,
        props: Signal<Props>,
        children: Signal<[VNode]>
    ) -> VNode;
}

// 使用示例
fn Counter(count: Signal<int>) -> VNode {
    @VNode.element("div", @Props.new(), [
        @VNode.element("h1", @Props.new(), [
            @VNode.text("Counter")  // 静态文本
        ]),
        @VNode.element("p", @Props.new(), [
            // 动态文本用响应式
            @VNode.text_reactive(@Memo.create(|| {
                format("Count: {}", count.get())
            }))
        ]),
        @VNode.element("button",
            @Props.new().set_event("click", || count.set(count.get() + 1)),
            [@VNode.text("+1")]
        )
    ])
}

// 运行时行为：
// 1. 首次渲染：构建完整的 VDOM 树，生成所有 DOM 节点
// 2. 建立订阅：text_reactive 会建立 count -> DOM 的直接映射
// 3. 后续更新：count 变化时，只更新那个特定的文本节点
//    - 不需要重新执行 Counter 函数
//    - 不需要 diff
//    - 直接更新 DOM
```

---

## 总结

1. **方案 1（VDOM + Diff）** = 简单、成熟，但有性能开销
2. **方案 2（你说的）** = 如果是 **Fine-grained Reactivity**，非常推荐！
3. **混合方案** = 最佳选择：结构用 VDOM（声明式），更新用 Signal（性能）

**Rusk 应该走哪条路？**
→ 我建议：**先实现 VDOM+Diff（快速原型），然后引入 Fine-grained Reactivity（性能优化）**

这样既保持了代码的声明式和可维护性，又能充分利用 Rusk 的 effect 系统获得极致性能。
