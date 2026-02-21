# Rusk React 框架设计

> 基于 Rusk VM 的声明式 UI 框架，通过批量 DOM 操作优化 WASM/JS 边界性能

## 设计目标

1. **性能优化**：批量处理 DOM 操作，最小化 WASM/JS 边界跨越
2. **声明式 API**：类似 React 的组件模型和 JSX 语法
3. **响应式状态**：自动追踪依赖，高效更新
4. **类型安全**：充分利用 Rusk 的类型系统

---

## 整体架构

```
┌──────────────────────────────────────────────────────┐
│  用户层：Rusk 组件                                     │
│  - 函数式组件                                         │
│  - Hooks (useState, useEffect, useMemo)              │
│  - 事件处理器                                         │
└──────────────────────────────────────────────────────┘
                         ↓
┌──────────────────────────────────────────────────────┐
│  框架核心层（Rusk 侧）                                 │
│  ┌────────────────┐  ┌──────────────┐                │
│  │ 组件调度器      │  │ 虚拟 DOM     │                │
│  │ - 渲染队列      │  │ - VNode 树   │                │
│  │ - 批量更新      │  │ - Diff 算法  │                │
│  └────────────────┘  └──────────────┘                │
│  ┌────────────────┐  ┌──────────────┐                │
│  │ Hooks 系统     │  │ 事件系统      │                │
│  │ - 状态管理      │  │ - 事件委托   │                │
│  │ - 副作用跟踪    │  │ - 合成事件   │                │
│  └────────────────┘  └──────────────┘                │
└──────────────────────────────────────────────────────┘
                         ↓
              [ABI 边界 - 批量数据传输]
              使用 bytes 类型传递序列化的操作批次
                         ↓
┌──────────────────────────────────────────────────────┐
│  运行时桥接层（JS 侧）                                 │
│  ┌────────────────┐  ┌──────────────┐                │
│  │ DOM 补丁应用器  │  │ 事件调度器   │                │
│  │ - 批量应用操作  │  │ - 原生事件   │                │
│  │ - 节点缓存      │  │ - 事件队列   │                │
│  └────────────────┘  └──────────────┘                │
└──────────────────────────────────────────────────────┘
                         ↓
┌──────────────────────────────────────────────────────┐
│  浏览器 DOM                                           │
└──────────────────────────────────────────────────────┘
```

---

## 1. 虚拟 DOM 设计

### 1.1 VNode 数据结构（Rusk 侧）

```rusk
// 虚拟节点类型
interface VNode {
    fn element(tag: string, props: Props, children: [VNode]) -> VNode;
    fn text(content: string) -> VNode;
    fn component(render: fn() -> VNode) -> VNode;
}

// 属性集合
interface Props {
    fn new() -> Props;
    fn set(self, key: string, value: string) -> Props;
    fn set_event(self, event: string, handler: fn(Event) -> unit) -> Props;
}

// 示例：创建虚拟 DOM
fn render_counter(count: int) -> VNode {
    @VNode.element("div",
        @Props.new()
            .set("class", "counter"),
        [
            @VNode.element("h1", @Props.new(), [
                @VNode.text("计数器")
            ]),
            @VNode.element("p", @Props.new(), [
                @VNode.text(std::int_to_string(count))
            ]),
            @VNode.element("button",
                @Props.new()
                    .set_event("click", |_| increment()),
                [@VNode.text("+1")]
            )
        ]
    )
}
```

### 1.2 内部表示（紧凑格式）

为了高效序列化，使用扁平化的数组表示：

```rusk
// VNode 的内部表示（用于序列化）
// 格式：[type, ...data]
// Element: [0, tag, props_start, props_end, children_start, children_end]
// Text: [1, content]
// Component: [2, component_id]
```

---

## 2. Diff 算法（Rusk 侧）

### 2.1 核心 Diff 逻辑

```rusk
// DOM 操作指令
interface DomOp {
    fn create(node_id: int, parent_id: int, tag: string) -> DomOp;
    fn create_text(node_id: int, parent_id: int, content: string) -> DomOp;
    fn update_text(node_id: int, content: string) -> DomOp;
    fn set_attr(node_id: int, key: string, value: string) -> DomOp;
    fn remove_attr(node_id: int, key: string) -> DomOp;
    fn remove(node_id: int) -> DomOp;
    fn insert_before(node_id: int, parent_id: int, before_id: int) -> DomOp;
    fn add_event_listener(node_id: int, event: string, handler_id: int) -> DomOp;
    fn remove_event_listener(node_id: int, event: string, handler_id: int) -> DomOp;
}

// Diff 算法（简化版）
fn diff(old: VNode, new: VNode, ops: [DomOp]) -> [DomOp] {
    match (old, new) {
        (Text(old_text), Text(new_text)) => {
            if old_text != new_text {
                ops.push(@DomOp.update_text(node_id, new_text))
            }
        }
        (Element(old_tag, old_props, old_children),
         Element(new_tag, new_props, new_children)) => {
            // 1. Diff props
            ops = diff_props(node_id, old_props, new_props, ops);

            // 2. Diff children (使用 key 优化)
            ops = diff_children(node_id, old_children, new_children, ops);
        }
        _ => {
            // 类型不同，替换节点
            ops.push(@DomOp.remove(old.id));
            ops.push(create_node(new, parent_id));
        }
    };
    ops
}
```

### 2.2 子节点 Diff（带 key 优化）

```rusk
fn diff_children(
    parent_id: int,
    old_children: [VNode],
    new_children: [VNode],
    ops: [DomOp]
) -> [DomOp] {
    // 简化的双端 diff 算法
    // 类似 Vue 的实现
    let old_start = 0;
    let old_end = old_children.len() - 1;
    let new_start = 0;
    let new_end = new_children.len() - 1;

    while old_start <= old_end && new_start <= new_end {
        // 1. 头头比较
        // 2. 尾尾比较
        // 3. 头尾比较
        // 4. 尾头比较
        // 5. 使用 key map 查找
    }

    // 处理剩余节点（删除或新增）
    ops
}
```

---

## 3. 批量操作协议

### 3.1 序列化格式

使用紧凑的二进制格式（通过 `bytes` 类型传输）：

```
批次格式：
┌────────┬────────────────────────────────────┐
│ Header │ Operations                         │
├────────┼────────────────────────────────────┤
│ 4B     │ Variable length                    │
│ count  │ Op1 | Op2 | ... | OpN             │
└────────┴────────────────────────────────────┘

单个操作格式：
┌──────┬────────┬─────────────────┐
│ OpId │ Length │ Data            │
├──────┼────────┼─────────────────┤
│ 1B   │ 2B     │ Variable        │
└──────┴────────┴─────────────────┘

OpId 枚举：
- 0x01: CreateElement(node_id, parent_id, tag)
- 0x02: CreateText(node_id, parent_id, content)
- 0x03: UpdateText(node_id, content)
- 0x04: SetAttr(node_id, key, value)
- 0x05: RemoveAttr(node_id, key)
- 0x06: Remove(node_id)
- 0x07: InsertBefore(node_id, parent_id, before_id)
- 0x08: AddEventListener(node_id, event, handler_id)
- 0x09: RemoveEventListener(node_id, event, handler_id)
```

### 3.2 Rusk 侧序列化

```rusk
// DOM 操作批次
interface DomBatch {
    fn new() -> DomBatch;
    fn add_op(self, op: DomOp) -> DomBatch;
    fn serialize(self) -> bytes;
}

// 序列化为 bytes
fn serialize_batch(ops: [DomOp]) -> bytes {
    let batch = @DomBatch.new();
    for op in ops {
        batch = batch.add_op(op);
    }
    batch.serialize()
}
```

### 3.3 JS 侧反序列化和应用

```javascript
// JS 运行时
class DomPatcher {
    constructor() {
        this.nodes = new Map(); // node_id -> HTMLElement
        this.eventHandlers = new Map(); // handler_id -> Function
    }

    // 应用操作批次
    applyBatch(bytes) {
        const view = new DataView(bytes.buffer);
        let offset = 0;

        // 读取操作数量
        const count = view.getUint32(offset, true);
        offset += 4;

        // 依次处理每个操作
        for (let i = 0; i < count; i++) {
            offset = this.applyOp(view, offset);
        }
    }

    applyOp(view, offset) {
        const opId = view.getUint8(offset);
        offset += 1;

        const length = view.getUint16(offset, true);
        offset += 2;

        const dataStart = offset;

        switch (opId) {
            case 0x01: // CreateElement
                offset = this.opCreateElement(view, offset);
                break;
            case 0x02: // CreateText
                offset = this.opCreateText(view, offset);
                break;
            case 0x03: // UpdateText
                offset = this.opUpdateText(view, offset);
                break;
            case 0x04: // SetAttr
                offset = this.opSetAttr(view, offset);
                break;
            // ... 其他操作
        }

        return dataStart + length;
    }

    opCreateElement(view, offset) {
        const nodeId = view.getUint32(offset, true);
        offset += 4;
        const parentId = view.getUint32(offset, true);
        offset += 4;
        const tagLen = view.getUint16(offset, true);
        offset += 2;
        const tag = this.readString(view, offset, tagLen);
        offset += tagLen;

        const element = document.createElement(tag);
        this.nodes.set(nodeId, element);

        const parent = this.nodes.get(parentId) || document.body;
        parent.appendChild(element);

        return offset;
    }

    // ... 其他操作的实现
}
```

---

## 4. 组件系统

### 4.1 组件定义

```rusk
// 函数式组件
fn Counter() -> VNode {
    let (count, set_count) = use_state(0);

    @VNode.element("div", @Props.new(), [
        @VNode.element("p", @Props.new(), [
            @VNode.text(format("Count: {}", count))
        ]),
        @VNode.element("button",
            @Props.new().set_event("click", |_| {
                set_count(count + 1)
            }),
            [@VNode.text("+1")]
        )
    ])
}

// 带 props 的组件
fn Greeting(props: GreetingProps) -> VNode {
    @VNode.element("h1", @Props.new(), [
        @VNode.text(format("Hello, {}!", props.name))
    ])
}

interface GreetingProps {
    name: string;
}
```

### 4.2 Hooks 系统

```rusk
// useState hook
interface StateHook<T> {
    fn use_state(initial: T) -> (T, fn(T) -> unit);
}

// useEffect hook
interface EffectHook {
    fn use_effect(effect: fn() -> unit, deps: [any]) -> unit;
}

// useMemo hook
interface MemoHook<T> {
    fn use_memo(compute: fn() -> T, deps: [any]) -> T;
}

// 实现示例
fn use_state<T>(initial: T) -> (T, fn(T) -> unit) {
    let ctx = get_current_component_context();
    let hook_index = ctx.next_hook_index();

    // 获取或初始化状态
    let state = match ctx.get_hook(hook_index) {
        Some(s) => s,
        None => {
            ctx.set_hook(hook_index, initial);
            initial
        }
    };

    // 创建 setter
    let setter = |new_value: T| {
        ctx.set_hook(hook_index, new_value);
        ctx.schedule_update(); // 触发重新渲染
    };

    (state, setter)
}
```

---

## 5. 事件系统

### 5.1 事件流程

```
用户点击 DOM
    ↓
JS 事件监听器捕获
    ↓
通过外部效果发送到 Rusk
    ↓
Rusk 事件处理器执行
    ↓
状态更新触发重新渲染
    ↓
生成 DOM 操作批次
    ↓
批量应用到 DOM
```

### 5.2 Rusk 侧事件声明

```rusk
// 声明事件效果
interface DomEvent {
    fn wait_event() -> Event;
}

interface Event {
    event_type: string;
    target_id: int;
    data: EventData;
}

interface EventData {
    // 鼠标事件
    client_x: int;
    client_y: int;
    // 键盘事件
    key: string;
    // 输入事件
    value: string;
}
```

### 5.3 JS 侧事件委托

```javascript
class EventDelegate {
    constructor(vm, effectId) {
        this.vm = vm;
        this.effectId = effectId;
        this.eventQueue = [];
        this.setupDelegation();
    }

    setupDelegation() {
        // 在根节点设置事件委托
        const eventTypes = ['click', 'input', 'keydown', 'submit'];

        for (const type of eventTypes) {
            document.body.addEventListener(type, (e) => {
                this.handleEvent(type, e);
            }, true); // 捕获阶段
        }
    }

    handleEvent(type, nativeEvent) {
        const target = nativeEvent.target;
        const nodeId = target.__rusk_node_id__;

        if (nodeId === undefined) return;

        // 构造合成事件
        const syntheticEvent = {
            type,
            nodeId,
            data: this.extractEventData(nativeEvent)
        };

        // 加入队列
        this.eventQueue.push(syntheticEvent);

        // 批量处理（在下一个 microtask）
        if (this.eventQueue.length === 1) {
            queueMicrotask(() => this.flushEvents());
        }
    }

    async flushEvents() {
        const events = this.eventQueue.splice(0);

        for (const event of events) {
            // 通过外部效果发送到 VM
            // 这里会触发 StepResult::Request
        }
    }
}
```

---

## 6. 调度器和批量更新

### 6.1 更新调度

```rusk
// 调度器
interface Scheduler {
    fn schedule_update(component_id: int) -> unit;
    fn flush_updates() -> bytes; // 返回 DOM 操作批次
}

// 全局调度器实例
let scheduler = @Scheduler.new();

fn schedule_render() {
    // 收集所有待更新组件
    let pending = scheduler.get_pending_updates();

    // 构建新的虚拟 DOM
    let new_vdom = render_tree(root_component);

    // 与旧 VDOM 进行 diff
    let ops = diff(old_vdom, new_vdom, []);

    // 序列化为 bytes
    let batch = serialize_batch(ops);

    // 通过宿主函数发送到 JS
    dom::apply_batch(batch);

    // 更新旧 VDOM
    old_vdom = new_vdom;
}
```

### 6.2 批量策略

1. **同步批处理**：在同一个事件循环 tick 内的所有状态更新合并
2. **时间切片**：利用 `vm_step` 的 fuel 机制，避免长时间阻塞
3. **优先级调度**：区分高优先级更新（用户交互）和低优先级更新（数据获取）

```rusk
interface Priority {
    Immediate,  // 立即执行（用户输入）
    Normal,     // 正常执行（一般更新）
    Low,        // 低优先级（预渲染）
}

fn schedule_update_with_priority(component_id: int, priority: Priority) {
    scheduler.enqueue(component_id, priority);
}
```

---

## 7. 宿主函数接口

### 7.1 Rusk 侧声明

```rusk
// 在编译选项中注册
interface dom {
    // 应用 DOM 操作批次
    fn apply_batch(batch: bytes) -> unit;

    // 初始化根节点
    fn init_root(selector: string) -> int; // 返回 root_id
}
```

### 7.2 JS 侧实现

```javascript
// 注册宿主函数
const applyBatchId = findHostImportId(vm, 'dom::apply_batch');
vm.registerHostImport(applyBatchId, (args) => {
    const [batchBytes] = args;
    patcher.applyBatch(batchBytes);
    return undefined; // unit
});

const initRootId = findHostImportId(vm, 'dom::init_root');
vm.registerHostImport(initRootId, (args) => {
    const [selector] = args;
    const root = document.querySelector(String(selector));
    const rootId = nextNodeId++;
    patcher.nodes.set(rootId, root);
    return BigInt(rootId);
});
```

---

## 8. 完整示例

### 8.1 Rusk 应用代码

```rusk
// main.rusk
fn App() -> VNode {
    let (items, set_items) = use_state(["Task 1", "Task 2"]);
    let (input, set_input) = use_state("");

    @VNode.element("div", @Props.new().set("class", "app"), [
        @VNode.element("h1", @Props.new(), [
            @VNode.text("Todo List")
        ]),

        // 输入框
        @VNode.element("input",
            @Props.new()
                .set("type", "text")
                .set("value", input)
                .set_event("input", |e| {
                    set_input(e.target_value)
                }),
            []
        ),

        // 添加按钮
        @VNode.element("button",
            @Props.new()
                .set_event("click", |_| {
                    if input != "" {
                        set_items(items.push(input));
                        set_input("");
                    }
                }),
            [@VNode.text("Add")]
        ),

        // 列表
        @VNode.element("ul", @Props.new(),
            items.map(|item, index| {
                @VNode.element("li",
                    @Props.new().set("key", std::int_to_string(index)),
                    [@VNode.text(item)]
                )
            })
        )
    ])
}

fn main() -> unit {
    // 初始化根节点
    let root_id = dom::init_root("#app");

    // 挂载应用
    mount(root_id, App);

    // 启动事件循环
    event_loop();
}
```

### 8.2 JS 初始化代码

```javascript
import init, { Vm } from './pkg/rusk_react.js';

async function main() {
    await init();

    // 加载编译后的字节码
    const rbc = await loadRbcBytes('./app.rbc');
    const vm = new Vm(rbc);

    // 创建 DOM 补丁器
    const patcher = new DomPatcher();

    // 注册宿主函数
    setupHostFunctions(vm, patcher);

    // 设置事件委托
    const eventEffectId = findExternalEffectId(vm, 'DomEvent', 'wait_event');
    const eventDelegate = new EventDelegate(vm, eventEffectId);

    // 运行 VM
    await runVm(vm, {
        fuelPerTick: 100_000,
        effects: new Map([
            [eventEffectId, (event) => eventDelegate.handleEvent(event)]
        ])
    });
}

main();
```

---

## 9. 性能优化策略

### 9.1 减少边界跨越

- ✅ **批量操作**：一次传输多个 DOM 操作（当前设计的核心）
- ✅ **二进制格式**：使用紧凑的二进制序列化（bytes 类型）
- ✅ **操作合并**：在 diff 阶段合并相邻操作

### 9.2 虚拟 DOM 优化

- **Key 优化**：使用 key 进行精确的子节点匹配
- **组件记忆化**：通过 `useMemo` 避免不必要的重新渲染
- **Fragment 支持**：减少不必要的包装元素

### 9.3 调度优化

- **时间切片**：利用 VM 的 fuel 机制
- **优先级队列**：区分交互更新和后台更新
- **跳帧策略**：在高负载时跳过中间帧

### 9.4 内存优化

- **对象池**：复用 VNode 对象
- **增量 GC**：与 Rusk VM 的 GC 配合
- **字符串驻留**：复用常见的字符串（标签名、类名）

---

## 10. 渐进式采用路径

### Phase 1：最小可行原型
- ✅ 基础 VNode 结构
- ✅ 简单的 diff 算法（无 key）
- ✅ 批量 DOM 操作协议
- ✅ 基础事件系统

### Phase 2：组件系统
- ✅ 函数式组件
- ✅ useState hook
- ✅ Props 传递
- ✅ 生命周期（useEffect）

### Phase 3：性能优化
- ✅ Key-based diff
- ✅ 组件记忆化
- ✅ 时间切片调度
- ✅ 事件委托优化

### Phase 4：生态扩展
- ✅ 路由系统
- ✅ 状态管理库
- ✅ DevTools 集成
- ✅ SSR 支持

---

## 11. 与现有方案对比

| 特性 | Rusk React | React | Yew (Rust) |
|------|-----------|-------|------------|
| 语言 | Rusk | JavaScript | Rust |
| 虚拟 DOM | ✅ | ✅ | ✅ |
| Hooks | ✅ | ✅ | ⚠️ (部分) |
| 批量更新 | ✅ 二进制批次 | ✅ Fiber | ✅ WASM 边界 |
| 类型安全 | ✅ 静态类型 | ⚠️ TypeScript | ✅ 强类型 |
| 文件大小 | 小 (~50KB) | 中 (~130KB) | 大 (~200KB+) |
| 启动速度 | 快 | 中 | 慢（WASM 初始化）|

---

## 12. 未来展望

1. **编译时优化**
   - 静态分析优化 VNode 生成
   - 编译时 diff（对于静态内容）
   - 自动代码分割

2. **并发渲染**
   - 利用 Web Workers
   - 多 VM 实例协同
   - 离屏渲染

3. **Web Components 互操作**
   - 将 Rusk 组件编译为 Web Components
   - 与现有前端框架集成

4. **原生移动支持**
   - 通过类似的协议支持原生 UI
   - 跨平台组件库

---

## 总结

这个设计通过以下核心策略优化 WASM/JS 边界性能：

1. **批量操作**：所有 DOM 变更在一次调用中完成
2. **二进制协议**：高效的序列化格式
3. **差异计算在 Rusk 侧**：只传输必要的变更
4. **事件委托**：减少事件处理的边界跨越
5. **智能调度**：利用 VM 的协作式调度能力

这种架构既保持了声明式 UI 的开发体验，又充分优化了性能瓶颈。
