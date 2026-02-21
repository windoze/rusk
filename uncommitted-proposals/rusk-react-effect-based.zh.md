# 基于 Effect 的细粒度响应式 UI 框架

> 利用 Rusk 的 effect 系统实现无 VDOM、无 Diff 的高性能 UI 框架

## 核心思想

**传统方案（React/Virtual DOM）**：
```
状态变化 → 重新渲染整个组件 → 生成新 VDOM → Diff → 更新 DOM
```

**Effect-based 方案（SolidJS-like）**：
```
状态变化 → 触发依赖的 effect → 直接更新对应的 DOM
```

### 关键优势

1. **无需 Diff**：在创建时就建立了状态和 DOM 的直接映射
2. **精确更新**：只更新实际变化的 DOM 节点
3. **更少的边界跨越**：只传输真正需要的单个更新
4. **天然支持**：Rusk 本身就有强大的 effect 系统

---

## 1. 响应式信号系统

### 1.1 核心数据结构

```rusk
// 信号：可观察的值
interface Signal<T> {
    fn create(value: T) -> Signal<T>;
    fn get(self) -> T;           // 读取时会注册依赖
    fn set(self, value: T) -> unit;  // 写入时触发所有依赖的 effect
}

// Effect：声明副作用
interface Effect {
    fn create(computation: fn() -> unit) -> Effect;
    fn cleanup(self) -> unit;
}

// 示例
fn example() {
    let count = @Signal.create(0);

    // 创建 effect：当 count 变化时自动重新执行
    @Effect.create(|| {
        let value = count.get();  // 自动注册依赖
        std::println(format("Count changed to: {}", value));
    });

    count.set(1);  // 触发 effect，打印 "Count changed to: 1"
    count.set(2);  // 触发 effect，打印 "Count changed to: 2"
}
```

### 1.2 内部实现原理

```rusk
// 全局上下文（用于跟踪当前正在执行的 effect）
interface ReactiveContext {
    current_effect: Option<EffectId>;
    signal_subscribers: Map<SignalId, [EffectId]>;
    effect_cleanups: Map<EffectId, fn() -> unit>;
}

// Signal 实现
fn signal_create<T>(initial: T) -> Signal<T> {
    let signal_id = next_signal_id();
    let cell = @Cell.new(initial);  // 内部存储

    Signal {
        id: signal_id,
        get: || {
            // 读取时注册依赖
            if let Some(effect_id) = get_current_effect() {
                subscribe(signal_id, effect_id);
            }
            cell.get()
        },
        set: |new_value| {
            cell.set(new_value);
            // 触发所有订阅者
            for effect_id in get_subscribers(signal_id) {
                run_effect(effect_id);
            }
        }
    }
}

// Effect 实现
fn effect_create(computation: fn() -> unit) -> Effect {
    let effect_id = next_effect_id();

    let run = || {
        // 设置当前 effect
        set_current_effect(Some(effect_id));

        // 清理旧的订阅
        clear_subscriptions(effect_id);

        // 执行计算（期间会重新注册依赖）
        computation();

        // 清除当前 effect
        set_current_effect(None);
    };

    // 立即执行一次
    run();

    Effect { id: effect_id, cleanup: || cleanup_effect(effect_id) }
}
```

---

## 2. DOM 绑定系统

### 2.1 基础 DOM 操作封装

```rusk
// DOM 节点引用（不透明句柄）
interface DomNode {
    id: int;
}

// 声明 DOM 操作 effect
interface Dom {
    // 创建元素
    fn create_element(tag: string) -> DomNode;

    // 设置文本内容（这是我们要优化的关键！）
    fn set_text_content(node: DomNode, content: string) -> unit;

    // 设置属性
    fn set_attribute(node: DomNode, key: string, value: string) -> unit;

    // 移除属性
    fn remove_attribute(node: DomNode, key: string) -> unit;

    // 添加子节点
    fn append_child(parent: DomNode, child: DomNode) -> unit;

    // 移除子节点
    fn remove_child(parent: DomNode, child: DomNode) -> unit;

    // 插入到指定位置
    fn insert_before(parent: DomNode, new_node: DomNode, ref_node: DomNode) -> unit;

    // 事件监听
    fn add_event_listener(node: DomNode, event: string, handler_id: int) -> unit;
}
```

### 2.2 响应式 DOM 更新

```rusk
// 自动响应式文本节点
fn text_node(content: Signal<string>) -> DomNode {
    let node = @Dom.create_element("TEXT_NODE");  // 特殊标记

    // 创建 effect：当 content 变化时，直接更新 DOM
    @Effect.create(|| {
        let text = content.get();  // 读取 signal，自动注册依赖
        @Dom.set_text_content(node, text);  // 直接更新 DOM！
    });

    node
}

// 响应式属性
fn reactive_attr(node: DomNode, key: string, value: Signal<string>) {
    @Effect.create(|| {
        let v = value.get();
        if v == "" {
            @Dom.remove_attribute(node, key);
        } else {
            @Dom.set_attribute(node, key, v);
        }
    });
}

// 响应式样式
fn reactive_style(node: DomNode, prop: string, value: Signal<string>) {
    reactive_attr(node, format("style:{}", prop), value);
}

// 响应式 class
fn reactive_class(node: DomNode, class_name: string, enabled: Signal<bool>) {
    @Effect.create(|| {
        if enabled.get() {
            @Dom.add_class(node, class_name);
        } else {
            @Dom.remove_class(node, class_name);
        }
    });
}
```

### 2.3 示例：响应式计数器

```rusk
fn Counter() -> DomNode {
    let count = @Signal.create(0);

    // 创建容器
    let div = @Dom.create_element("div");

    // 标题（静态）
    let h1 = @Dom.create_element("h1");
    @Dom.set_text_content(h1, "计数器");
    @Dom.append_child(div, h1);

    // 显示计数（响应式！）
    let count_text = @Signal.create(format("Count: {}", count.get()));

    @Effect.create(|| {
        // 当 count 变化时，自动更新 count_text
        count_text.set(format("Count: {}", count.get()));
    });

    let p = @Dom.create_element("p");
    let text_node = text_node(count_text);  // 响应式文本节点
    @Dom.append_child(p, text_node);
    @Dom.append_child(div, p);

    // 按钮
    let button = @Dom.create_element("button");
    @Dom.set_text_content(button, "+1");

    let handler_id = register_handler(|| {
        count.set(count.get() + 1);  // 更新 signal，自动触发相关 effect
    });
    @Dom.add_event_listener(button, "click", handler_id);

    @Dom.append_child(div, button);

    div
}
```

**关键点**：
- `count.set()` 被调用时，自动触发依赖它的 effect
- Effect 内的 `count.get()` 被调用时，自动注册依赖
- **不需要重新渲染整个组件**，只更新相关的文本节点！

---

## 3. 条件渲染与列表

### 3.1 条件渲染

```rusk
// 方案 1：使用 effect 动态插入/移除
fn show(condition: Signal<bool>, create_node: fn() -> DomNode) -> DomNode {
    let placeholder = @Dom.create_element("PLACEHOLDER");
    let current_node: Option<DomNode> = None;

    @Effect.create(|| {
        if condition.get() {
            if current_node.is_none() {
                let node = create_node();
                @Dom.replace(placeholder, node);
                current_node = Some(node);
            }
        } else {
            if let Some(node) = current_node {
                @Dom.replace(node, placeholder);
                current_node = None;
            }
        }
    });

    placeholder
}

// 示例
fn example(show_message: Signal<bool>) -> DomNode {
    let div = @Dom.create_element("div");

    let message_node = show(show_message, || {
        let p = @Dom.create_element("p");
        @Dom.set_text_content(p, "Hello!");
        p
    });

    @Dom.append_child(div, message_node);
    div
}
```

### 3.2 列表渲染（关键优化）

这是最有挑战的部分！我们需要：
1. 跟踪列表中每个项的变化
2. 高效地插入/删除/移动节点

```rusk
// 响应式列表
interface SignalList<T> {
    fn create(items: [T]) -> SignalList<T>;
    fn get(self) -> [T];
    fn push(self, item: T) -> unit;
    fn remove(self, index: int) -> unit;
    fn set(self, index: int, item: T) -> unit;
}

// 列表渲染
fn for_each<T>(
    list: SignalList<T>,
    key_fn: fn(T) -> string,
    render_fn: fn(T, int) -> DomNode
) -> DomNode {
    let container = @Dom.create_element("div");
    let nodes: Map<string, DomNode> = @Map.new();

    @Effect.create(|| {
        let items = list.get();  // 读取列表，注册依赖

        // 简化版：重新渲染所有（后续可优化为增量更新）
        for (item, index) in items.enumerate() {
            let key = key_fn(item);

            if !nodes.contains(key) {
                // 创建新节点
                let node = render_fn(item, index);
                nodes.insert(key, node);
                @Dom.append_child(container, node);
            } else {
                // 节点已存在，可能需要更新或移动
                // (这里可以进一步优化)
            }
        }

        // 移除不再存在的节点
        let current_keys = items.map(key_fn).to_set();
        for (key, node) in nodes.iter() {
            if !current_keys.contains(key) {
                @Dom.remove_child(container, node);
                nodes.remove(key);
            }
        }
    });

    container
}

// 更细粒度的列表（每个项独立跟踪）
interface SignalArray<T> {
    fn create(items: [T]) -> SignalArray<T>;

    // 返回每个项的独立 signal
    fn signals(self) -> [Signal<T>];

    // 操作方法会触发最小化的更新
    fn push(self, item: T) -> unit;
    fn remove(self, index: int) -> unit;
    fn set(self, index: int, item: T) -> unit;
}

// 示例：Todo List
fn TodoList() -> DomNode {
    let todos = @SignalArray.create([
        "Task 1",
        "Task 2",
        "Task 3"
    ]);

    let container = @Dom.create_element("ul");

    // 为每个 todo 创建独立的响应式绑定
    for (todo_signal, index) in todos.signals().enumerate() {
        let li = @Dom.create_element("li");

        // 文本会响应该特定 todo 的变化
        let text = text_node(todo_signal);
        @Dom.append_child(li, text);

        @Dom.append_child(container, li);
    }

    container
}
```

---

## 4. 批量更新策略

虽然没有 Diff，但我们仍然需要批量更新来减少边界跨越！

### 4.1 更新队列

```rusk
// 全局更新队列
interface UpdateQueue {
    pending_effects: [EffectId];
    pending_dom_ops: [DomOp];
    is_flushing: bool;
}

// 调度更新
fn schedule_effect(effect_id: EffectId) {
    update_queue.pending_effects.push(effect_id);

    if !update_queue.is_flushing {
        // 在下一个 microtask 批量执行
        schedule_flush();
    }
}

// 批量执行所有待处理的 effect
fn flush_effects() {
    update_queue.is_flushing = true;

    while !update_queue.pending_effects.is_empty() {
        let effects = update_queue.pending_effects.drain();

        // 执行所有 effect（期间可能产生新的 effect）
        for effect_id in effects {
            run_effect(effect_id);
        }
    }

    // 现在批量发送所有 DOM 操作
    if !update_queue.pending_dom_ops.is_empty() {
        let batch = serialize_dom_ops(update_queue.pending_dom_ops);
        @Dom.apply_batch(batch);  // 一次性发送到 JS！
        update_queue.pending_dom_ops.clear();
    }

    update_queue.is_flushing = false;
}
```

### 4.2 DOM 操作拦截

关键技巧：**不立即调用 JS，而是加入队列**

```rusk
// 原本的 Dom.set_text_content 实现
interface Dom {
    fn set_text_content(node: DomNode, content: string) -> unit;
}

// 实际实现：加入队列而不是立即执行
fn dom_set_text_content_impl(node: DomNode, content: string) {
    let op = @DomOp.UpdateText(node.id, content);
    update_queue.pending_dom_ops.push(op);
    // 不立即跨界！
}
```

### 4.3 完整流程

```
1. 用户点击按钮
   ↓
2. count.set(count.get() + 1)
   ↓
3. 触发所有订阅 count 的 effect（加入队列）
   ↓
4. schedule_flush()（如果尚未调度）
   ↓
5. 在下一个 microtask：
   ├─ 执行所有 effect
   ├─ Effect 内的 Dom 调用加入 pending_dom_ops
   └─ 所有 effect 完成后
   ↓
6. 序列化 pending_dom_ops 为 bytes
   ↓
7. **一次性**调用 @Dom.apply_batch(batch)
   ↓
8. JS 侧批量应用所有 DOM 操作
```

---

## 5. 进阶优化

### 5.1 Memo（派生信号）

```rusk
// 计算属性：只在依赖变化时重新计算
interface Memo<T> {
    fn create(computation: fn() -> T) -> Memo<T>;
    fn get(self) -> T;
}

fn memo_create<T>(computation: fn() -> T) -> Memo<T> {
    let cache: Option<T> = None;
    let is_dirty = true;

    Memo {
        get: || {
            if is_dirty {
                // 追踪依赖
                set_current_effect(Some(this_memo_id));
                cache = Some(computation());
                set_current_effect(None);
                is_dirty = false;
            }
            cache.unwrap()
        }
    }
}

// 示例
fn example(first_name: Signal<string>, last_name: Signal<string>) {
    // full_name 只在 first_name 或 last_name 变化时重新计算
    let full_name = @Memo.create(|| {
        format("{} {}", first_name.get(), last_name.get())
    });

    // 使用 full_name 的地方会自动响应变化
    let text = text_node(full_name);
}
```

### 5.2 批量 Signal 更新

```rusk
// 批量更新多个 signal，只触发一次 effect
fn batch(update_fn: fn() -> unit) {
    start_batch();
    update_fn();
    end_batch();  // 这时才触发所有 effect
}

// 示例
fn example(x: Signal<int>, y: Signal<int>) {
    @Effect.create(|| {
        std::println(format("x={}, y={}", x.get(), y.get()));
    });

    // 不使用 batch：会打印两次
    x.set(1);  // 打印 "x=1, y=0"
    y.set(2);  // 打印 "x=1, y=2"

    // 使用 batch：只打印一次
    batch(|| {
        x.set(3);
        y.set(4);
    });  // 打印 "x=3, y=4"
}
```

### 5.3 Untracked 读取

```rusk
// 读取 signal 但不注册依赖
fn untracked<T>(signal: Signal<T>) -> T {
    let old_effect = get_current_effect();
    set_current_effect(None);
    let value = signal.get();
    set_current_effect(old_effect);
    value
}

// 示例
fn example(count: Signal<int>) {
    @Effect.create(|| {
        let current = count.get();  // 追踪依赖
        let previous = untracked(previous_count);  // 不追踪
        std::println(format("Changed from {} to {}", previous, current));
    });
}
```

---

## 6. 与 Virtual DOM 方案对比

| 维度 | Virtual DOM + Diff | Effect-based (细粒度) |
|------|--------------------|-----------------------|
| **更新粒度** | 组件级别 | 值级别 |
| **计算开销** | 需要 diff | 无需 diff |
| **边界跨越** | 批量操作（较多） | 批量操作（更少） |
| **内存占用** | 需要两棵树 | 只需订阅关系图 |
| **首次渲染** | 中等 | 稍慢（建立订阅） |
| **更新速度** | 快 | 非常快 |
| **心智模型** | 熟悉（类 React） | 需要理解响应式 |
| **适用场景** | 大规模重渲染 | 高频局部更新 |

### 6.1 性能对比示例

**场景：更新 1000 个列表项中的 1 个**

Virtual DOM 方案：
```
1. 重新渲染整个列表组件
2. 生成 1000 个新 VNode
3. Diff 1000 个节点（找到 1 个变化）
4. 发送 1 个 DOM 操作
时间：O(n) 其中 n = 1000
```

Effect-based 方案：
```
1. 触发该项的 signal
2. 执行该项对应的 effect
3. 发送 1 个 DOM 操作
时间：O(1)
```

---

## 7. 实际应用示例

### 7.1 完整的 Counter 应用

```rusk
fn Counter() -> DomNode {
    let count = @Signal.create(0);

    // 容器
    let div = @Dom.create_element("div");
    @Dom.set_attribute(div, "class", "counter");

    // 标题
    let h1 = @Dom.create_element("h1");
    @Dom.set_text_content(h1, "Counter Example");
    @Dom.append_child(div, h1);

    // 显示区域（响应式）
    let p = @Dom.create_element("p");
    let count_text = @Memo.create(|| format("Count: {}", count.get()));
    @Dom.append_child(p, text_node(count_text));
    @Dom.append_child(div, p);

    // 按钮容器
    let buttons = @Dom.create_element("div");

    // -1 按钮
    let dec_btn = @Dom.create_element("button");
    @Dom.set_text_content(dec_btn, "-1");
    @Dom.add_event_listener(dec_btn, "click", register_handler(|| {
        count.set(count.get() - 1);
    }));
    @Dom.append_child(buttons, dec_btn);

    // +1 按钮
    let inc_btn = @Dom.create_element("button");
    @Dom.set_text_content(inc_btn, "+1");
    @Dom.add_event_listener(inc_btn, "click", register_handler(|| {
        count.set(count.get() + 1);
    }));
    @Dom.append_child(buttons, inc_btn);

    // Reset 按钮（条件显示）
    let show_reset = @Memo.create(|| count.get() != 0);
    let reset_btn = show(show_reset, || {
        let btn = @Dom.create_element("button");
        @Dom.set_text_content(btn, "Reset");
        @Dom.add_event_listener(btn, "click", register_handler(|| {
            count.set(0);
        }));
        btn
    });
    @Dom.append_child(buttons, reset_btn);

    @Dom.append_child(div, buttons);

    div
}

fn main() -> unit {
    let root = @Dom.init_root("#app");
    let app = Counter();
    @Dom.append_child(root, app);

    // 启动事件循环
    event_loop();
}
```

### 7.2 Todo List 应用

```rusk
interface Todo {
    id: int;
    text: string;
    completed: bool;
}

fn TodoApp() -> DomNode {
    let todos = @SignalArray.create([]);
    let input = @Signal.create("");
    let next_id = @Signal.create(0);

    let container = @Dom.create_element("div");

    // 标题
    let h1 = @Dom.create_element("h1");
    @Dom.set_text_content(h1, "Todo List");
    @Dom.append_child(container, h1);

    // 输入区域
    let input_div = @Dom.create_element("div");

    let input_el = @Dom.create_element("input");
    reactive_attr(input_el, "value", input);
    @Dom.add_event_listener(input_el, "input", register_handler(|e| {
        input.set(e.target_value);
    }));
    @Dom.append_child(input_div, input_el);

    let add_btn = @Dom.create_element("button");
    @Dom.set_text_content(add_btn, "Add");
    @Dom.add_event_listener(add_btn, "click", register_handler(|| {
        if input.get() != "" {
            let todo = Todo {
                id: next_id.get(),
                text: input.get(),
                completed: false
            };
            todos.push(todo);
            next_id.set(next_id.get() + 1);
            input.set("");
        }
    }));
    @Dom.append_child(input_div, add_btn);

    @Dom.append_child(container, input_div);

    // 列表
    let list = for_each(
        todos,
        |todo| std::int_to_string(todo.id),
        |todo_signal, index| {
            let li = @Dom.create_element("li");

            // Checkbox
            let checkbox = @Dom.create_element("input");
            @Dom.set_attribute(checkbox, "type", "checkbox");

            let completed = @Memo.create(|| todo_signal.get().completed);
            reactive_attr(checkbox, "checked", completed);

            @Dom.add_event_listener(checkbox, "change", register_handler(|e| {
                let mut todo = todo_signal.get();
                todo.completed = !todo.completed;
                todo_signal.set(todo);
            }));
            @Dom.append_child(li, checkbox);

            // Text
            let text = @Memo.create(|| todo_signal.get().text);
            let span = @Dom.create_element("span");
            @Dom.append_child(span, text_node(text));

            // 删除按钮
            let del_btn = @Dom.create_element("button");
            @Dom.set_text_content(del_btn, "Delete");
            @Dom.add_event_listener(del_btn, "click", register_handler(|| {
                todos.remove(index);
            }));
            @Dom.append_child(li, del_btn);

            li
        }
    );

    @Dom.append_child(container, list);

    container
}
```

---

## 8. 混合方案？

也许最优解是**结合两种方案**：

### 8.1 分层策略

```
粗粒度（组件级别）：Virtual DOM + Diff
    ├─ 处理组件的挂载/卸载
    ├─ 处理复杂的结构变化
    └─ 列表的增删
         ↓
细粒度（值级别）：Effect-based
    ├─ 文本内容更新
    ├─ 属性更新
    ├─ 样式更新
    └─ 类名切换
```

### 8.2 示例：混合方案

```rusk
fn TodoItem(todo: Signal<Todo>) -> VNode {
    // 组件级别用 VNode（可以整体替换）
    @VNode.element("li", @Props.new(), [
        @VNode.element("input",
            @Props.new()
                .set("type", "checkbox")
                // 值级别用 Signal（精确更新）
                .set_reactive("checked", @Memo.create(|| todo.get().completed)),
            []
        ),
        // 文本用 reactive signal
        @VNode.text_reactive(@Memo.create(|| todo.get().text)),
        @VNode.element("button",
            @Props.new().set_event("click", || delete_todo(todo)),
            [@VNode.text("Delete")]
        )
    ])
}
```

---

## 9. 总结

### Effect-based 方案的优势

1. ✅ **无需 Diff**：建立直接映射，精确更新
2. ✅ **更少的边界跨越**：只传输真正变化的操作
3. ✅ **更快的更新**：O(变化数) vs O(组件大小)
4. ✅ **天然适配 Rusk**：利用现有的 effect 系统

### 权衡点

1. ⚠️ **首次渲染慢**：需要建立所有订阅关系
2. ⚠️ **内存占用**：需要维护订阅图
3. ⚠️ **列表优化复杂**：需要特殊处理（SignalArray）
4. ⚠️ **学习曲线**：需要理解响应式编程

### 建议

**如果你的应用：**
- 有大量高频更新（如实时数据、动画）
- 更新通常是局部的（单个字段）
- 重视极致性能

→ **选择 Effect-based**

**如果你的应用：**
- 有复杂的结构变化（如路由切换）
- 更新通常是整体的（切换视图）
- 重视开发体验和熟悉度

→ **选择 Virtual DOM**

**或者：**
→ **混合使用**：组件结构用 VDOM，叶子节点用 Signal！
