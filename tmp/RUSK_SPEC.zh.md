# Rusk 语言规范 (v0.4)

本文件是 **Rusk 语言规范**（Rusk Language Spec）。

Rusk 的目标是：在语法上接近 Rust（块表达式、`match`、显式可变性控制），在工程体验上接近 TypeScript（类型推断、泛型、接口驱动的抽象），并以 **代数效应（Algebraic Effects）** 作为一等机制统一异常/异步/生成器等控制流扩展。

本仓库的实现是一个"脚本 → MIR → 解释执行"的参考实现：

- `RUSK_SPEC.md` 定义源语言（本文件）。
- `MIR_SPEC.md` 定义中间表示 MIR。
- 解释器执行 MIR，并提供宿主函数（host functions）作为"标准库"实现。

本规范以 **可实现** 为前提：规范中出现的语法与语义都必须能完整编译到当前版本的 MIR 并在解释器中运行（不允许"未来再实现"的占位条款）。

---

## 0. 符号说明

- `unit` 表示单元值 `()`。
- "Trap" 表示运行时错误（MIR `trap`）。
- "Host function"（宿主函数）表示由外部提供、可通过 MIR `call` 调用的函数。

---

## 1. 源文本

### 1.1 编码

源文件使用 UTF-8 编码。

### 1.2 空白符和注释

- 空白符用于分隔标记，但在其他情况下无意义（字符串/字节字面量内部除外）。
- 行注释：`// ...` 直到行尾。
- 块注释：`/* ... */` 可以嵌套。

---

## 2. 词法标记

### 2.1 标识符

标识符匹配规则：

- 首字符：`_` 或 Unicode XID_Start
- 后续字符：Unicode XID_Continue

Rusk 将关键字视为保留字，不能用作标识符。

### 2.2 关键字

项和声明：

`fn`, `let`, `const`, `readonly`, `struct`, `enum`, `interface`, `impl`

控制流：

`if`, `else`, `match`, `return`, `loop`, `while`, `for`, `in`, `break`, `continue`

### 2.3 运算符和分隔符

分隔符：

`(` `)` `{` `}` `[` `]` `,` `:` `;` `.` `::` `=>`

运算符：

`=` `+` `-` `*` `/` `%`

`==` `!=` `<` `<=` `>` `>=`

`&&` `||` `!`

效应标记：

`@`

> 注意：运算符是语法糖；它们会降级为标准库函数调用。

---

## 3. 语法 (EBNF)

下面的语法是描述性的，旨在为实现提供明确的规则。

### 3.1 程序

```
Program        := Item* ;

Item           := FnItem | StructItem | EnumItem | InterfaceItem | ImplItem ;
```

### 3.2 项

#### 3.2.1 函数

```
FnItem         := "fn" Ident GenericParams? "(" ParamList? ")" ReturnType? Block ;
ParamList      := Param ("," Param)* (",")? ;
Param          := ParamMut? Ident ":" Type ;
ParamMut       := "readonly" ;
ReturnType     := "->" Type ;
```

注意：
- 参数是不可变绑定；Rusk 中的可变性指的是*对象可变性*（堆对象），而非重新绑定。
- 参数上的 `readonly` 表示该参数是只读视图（尝试通过它进行修改会导致 trap）。

#### 3.2.2 结构体

```
StructItem     := "struct" Ident GenericParams? "{" FieldList? "}" ;
FieldList      := Field ("," Field)* (",")? ;
Field          := Ident ":" Type ;
```

结构体值是堆分配的具有命名字段的对象。

#### 3.2.3 枚举

```
EnumItem       := "enum" Ident GenericParams? "{" VariantList? "}" ;
VariantList    := Variant ("," Variant)* (",")? ;
Variant        := Ident TupleFields? ;
TupleFields    := "(" TypeList? ")" ;
TypeList       := Type ("," Type)* (",")? ;
```

枚举值是堆分配的标记联合。

#### 3.2.4 接口

接口定义：
- (1) 用于静态分派的方法签名（编译时解析调用），以及
- (2) 用于 `@Interface.method(...)` 的效应签名。

```
InterfaceItem  := "interface" Ident GenericParams? "{" InterfaceMember* "}" ;
InterfaceMember:= "fn" Ident GenericParams? "(" ParamList? ")" ReturnType? ";" ;
```

#### 3.2.5 实现

```
ImplItem       := "impl" ImplHeader "{" ImplMember* "}" ;
ImplHeader     := Ident GenericArgs?                    // 固有实现: impl Type { ... }
               | Ident GenericArgs? "for" Ident GenericArgs? ; // 接口实现: impl Interface for Type { ... }

ImplMember     := FnItem ;
```

### 3.3 泛型

Rusk 以有限的、可实现的形式支持高阶类型参数（HKTs）。

v0.4 中的种类（kinds）限制为：
- `Type`（arity 0），写作 `T`
- arity `n >= 1` 的类型构造器，写作 `F<_, _, ...>`，有 `n` 个下划线

```
GenericParams  := "<" GenericParam ("," GenericParam)* (",")? ">" ;
GenericParam   := Ident | Ident "<" ("_"
                                  | "_" ("," "_")+
                                  ) ">" ;

GenericArgs    := "<" Type ("," Type)* (",")? ">" ;
```

示例：

```rust
fn id<T>(x: T) -> T { x }

// F 是一元类型构造器（种类 Type -> Type）
fn map<F<_>, A, B>(f: fn(A) -> B, xs: F<A>) -> F<B> { ... }
```

### 3.4 类型

```
Type           := ("readonly")? TypeAtom ;

TypeAtom       := PrimType
               | "[" Type "]"                      // 动态数组类型
               | "fn" "(" TypeList? ")" "->" Type  // 函数类型
               | PathType ;

PrimType       := "unit" | "bool" | "int" | "float" | "string" | "bytes" ;

PathType       := Ident GenericArgs? ("::" Ident GenericArgs?)* ;
```

注意：
- `PathType` 用于命名类型（结构体/枚举/接口）和关联名称。
- 对于此实现，泛型类型参数在**运行时被擦除**（它们不影响运行时表示）。
- `readonly T` 是类似引用的值（数组/结构体/枚举）的*视图类型*。通过 `readonly` 视图进行写入在编译时是错误，在运行时也会 trap。

### 3.5 语句和块

```
Block          := "{" Stmt* Expr? "}" ;

Stmt           := LetStmt
               | ConstStmt
               | ReadonlyStmt
               | ReturnStmt
               | BreakStmt
               | ContinueStmt
               | ExprStmt ;

LetStmt        := "let" Ident (":" Type)? ("=" Expr)? ";" ;
ConstStmt      := "const" Ident (":" Type)? "=" Expr ";" ;
ReadonlyStmt   := "readonly" Ident (":" Type)? "=" Expr ";" ;

ReturnStmt     := "return" Expr? ";" ;
BreakStmt      := "break" ";" ;
ContinueStmt   := "continue" ";" ;

ExprStmt       := Expr ";" ;
```

注意：
- `let x;` 声明一个未初始化的局部变量（在赋值前读取它是运行时错误）。
- `let x = e;` 初始化 `x`。
- `const x = e;` 防止 `x` 的重新绑定（但不会深度冻结引用的对象）。
- `readonly x = e;` 等同于 `const x = e;` 加上只读视图：它防止重新绑定并禁止通过 `x` 进行修改。

### 3.6 表达式

Rusk 表达式类似 Rust：块、`if` 和 `match` 都是表达式。

优先级（从高到低）：

1. 后缀：调用、字段、索引
2. 一元：`!` `-`
3. 乘法：`*` `/` `%`
4. 加法：`+` `-`
5. 比较：`<` `<=` `>` `>=`
6. 相等：`==` `!=`
7. 逻辑与：`&&`
8. 逻辑或：`||`
9. 赋值：`=`

```
Expr           := AssignExpr ;

AssignExpr     := OrExpr ( "=" AssignExpr )? ;
OrExpr         := AndExpr ( "||" AndExpr )* ;
AndExpr        := EqExpr ( "&&" EqExpr )* ;
EqExpr         := CmpExpr ( ( "==" | "!=" ) CmpExpr )* ;
CmpExpr        := AddExpr ( ( "<" | "<=" | ">" | ">=" ) AddExpr )* ;
AddExpr        := MulExpr ( ( "+" | "-" ) MulExpr )* ;
MulExpr        := UnaryExpr ( ( "*" | "/" | "%" ) UnaryExpr )* ;
UnaryExpr      := ( "!" | "-" ) UnaryExpr | PostfixExpr ;

PostfixExpr    := PrimaryExpr Postfix* ;
Postfix        := Call | Field | Index ;

Call           := "(" ArgList? ")" ;
ArgList        := Expr ("," Expr)* (",")? ;
Field          := "." Ident ;
Index          := "[" Expr "]" ;

PrimaryExpr    := Literal
               | PathExpr
               | ArrayLit
               | StructLit
               | EnumLit
               | EffectCall
               | Lambda
               | IfExpr
               | MatchExpr
               | LoopExpr
               | WhileExpr
               | ForExpr
               | Block
               | "(" Expr ")" ;

PathExpr       := Ident ("::" Ident)* ;

EffectCall     := "@" PathExpr "." Ident "(" ArgList? ")" ;
```

#### 3.6.1 字面量

```
Literal        := "()" | BoolLit | IntLit | FloatLit | StringLit | BytesLit | FStringLit ;
BoolLit        := "true" | "false" ;
IntLit         := DecIntLit | HexIntLit | OctIntLit | BinIntLit ;
DecIntLit      := Digit (Digit | "_")* ;
HexIntLit      := "0x" HexDigit (HexDigit | "_")* ;
OctIntLit      := "0o" OctDigit (OctDigit | "_")* ;
BinIntLit      := "0b" BinDigit (BinDigit | "_")* ;

FloatLit       := DecIntLit "." DecIntLit (ExponentPart)? | DecIntLit ExponentPart ;
ExponentPart   := ("e" | "E") ("+" | "-")? DecIntLit ;

// StringLit / BytesLit / FStringLit 是带有转义的词法标记（见下文）。

Digit          := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
HexDigit       := Digit | "a" | "b" | "c" | "d" | "e" | "f" | "A" | "B" | "C" | "D" | "E" | "F" ;
OctDigit       := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ;
BinDigit       := "0" | "1" ;
```

字符串字面量（`StringLit`）：

- 写作 `"..."`。
- 表示 UTF-8 字符串。
- 支持转义：
  - `\\`, `\"`
  - `\n`, `\r`, `\t`, `\0`
  - `\u{...}`（1+ 个十六进制数字，Unicode 标量值）

字节字符串字面量（`BytesLit`）：

- 写作 `b"..."`。
- 表示原始 `bytes`。
- 支持转义：
  - `\\`, `\"`
  - `\n`, `\r`, `\t`, `\0`
  - `\xHH`（恰好两个十六进制数字）
- 未转义的字符必须是 ASCII（0x20..=0x7E，不包括 `"` 和 `\`）。

格式化字符串：
- `f"..."` 通过 `{ Expr }` 段支持插值。
- `{{` 和 `}}` 分别编码字面量 `{` 和 `}`。
- 脱糖定义见 §9.3。

#### 3.6.2 数组、结构体、枚举字面量

```
ArrayLit       := "[" (Expr ("," Expr)*)? (",")? "]" ;

StructLit      := Ident "{" (StructFieldInit ("," StructFieldInit)*)? (",")? "}" ;
StructFieldInit:= Ident ":" Expr ;

EnumLit        := Ident "::" Ident "(" (Expr ("," Expr)*)? (",")? ")" ;
```

#### 3.6.3 Lambda

```
Lambda         := "|" LambdaParams? "|" Block ;
LambdaParams   := LambdaParam ("," LambdaParam)* (",")? ;
LambdaParam    := Ident (":" Type)? ;
```

Lambda 参数类型可以从上下文推断；如果无法推断，则必须显式注解。

#### 3.6.4 If

```
IfExpr         := "if" Expr Block ("else" (IfExpr | Block))? ;
```

#### 3.6.5 Match

Match 分支可以是值分支或效应分支（见 §7）。

```
MatchExpr      := "match" Expr "{" MatchArm* "}" ;
MatchArm       := (EffectPat | ValuePat) "=>" BlockOrExpr ","? ;
BlockOrExpr    := Block | Expr ;
ValuePat       := Pattern ;
EffectPat      := "@" PathExpr "." Ident "(" PatList? ")" ;
PatList        := Pattern ("," Pattern)* (",")? ;
```

#### 3.6.6 循环

```
LoopExpr       := "loop" Block ;
WhileExpr      := "while" Expr Block ;
```

`for` 作为语法提供：

```
ForExpr        := "for" Ident "in" Expr Block ;
```

并脱糖（在类型系统和编译器中）为标准库中迭代器协议上的 `while` 循环（见 §9.4）。

### 3.7 模式

模式用于 `match` 值分支和效应分支的参数模式。

```
Pattern        := "_"                       // 通配符
               | Ident                     // 绑定
               | LiteralPat
               | EnumPat
               | StructPat
               | ArrayPrefixPat ;

LiteralPat     := "()" | BoolLit | IntLit | FloatLit | StringLit | BytesLit ;

EnumPat        := Ident "::" Ident "(" (Pattern ("," Pattern)*)? (",")? ")" ;
StructPat      := Ident "{" (StructPatField ("," StructPatField)*)? (",")? "}" ;
StructPatField := Ident ":" Pattern ;

ArrayPrefixPat := "[" (Pattern ("," Pattern)*)? (",")? (".." )? "]" ;
```

注意：
- 模式中的 `Ident` 总是绑定（没有名称的"常量模式"）。
- 数组前缀模式匹配 `[p1, p2, ..]`（前缀）和 `[p1, p2]`（精确长度）。

---

## 4. 运行时模型

### 4.1 值

运行时值包括：

- `unit`, `bool`, `int`, `float`, `string`, `bytes`
- 堆引用：结构体、枚举、数组（共享引用）
- 函数值（一等公民，引用命名函数）
- 延续令牌（由效应捕获）

### 4.2 堆对象和共享

结构体/枚举/数组是堆分配的，并通过引用传递。复制引用会创建别名；修改通过所有别名可见。

### 4.3 只读视图

`readonly` **不会**深度冻结对象图。它创建一个*视图*，禁止**通过该特定绑定/引用**进行修改。

- 通过只读视图进行修改会在运行时 trap。
- 通过非只读别名进行修改仍然是允许的且可观察的。

这与 MIR 的 `as_readonly` 行为一致。

### 4.4 未初始化的局部变量

`let x;` 引入一个未初始化的局部变量。在初始化之前读取 `x` 是运行时错误（trap）。

### 4.5 求值顺序

求值是从左到右：

- 函数调用参数
- 结构体字段初始化器
- 数组元素
- 二元运算符操作数
- match 审查表达式求值

这是一个**语义保证**（不仅仅是实现细节）。

### 4.6 赋值（左值）

赋值是表达式，但产生 `unit`（类似 Rust）。

`=` 的左侧必须是以下之一：

- 局部名称：`x = expr`（仅当 `x` 用 `let` 声明时，不能是 `const`/`readonly`）
- 结构体字段：`obj.field = expr`
- 数组槽：`arr[index] = expr`

求值顺序是从左到右：

- `x = expr`：求值 `expr`，然后赋值。
- `obj.field = expr`：求值 `obj`，然后求值 `expr`，然后修改字段。
- `arr[index] = expr`：求值 `arr`，然后 `index`，然后 `expr`，然后修改槽。

如果目标是只读视图（`readonly T`），则修改会被类型检查器拒绝，如果到达运行时也会 trap。

---

## 5. 控制流语义

### 5.1 块

`{ s1; s2; expr }` 按顺序求值语句，如果存在最终表达式则产生其值；否则产生 `unit`。

### 5.2 If

`if cond { a } else { b }` 求值 `cond`；如果为真则求值并产生 `a`，否则求值并产生 `b`。除非 `if` 用于语句位置（以 `;` 结尾），否则需要 `else`，在这种情况下缺少 `else` 产生 `unit`。

### 5.3 While / Loop / For

- `loop { ... }` 永远重复直到 `break`。
- `while cond { ... }` 在 `cond` 求值为真时重复。
- `break` 退出最内层循环并为该循环表达式产生 `unit`。
- `continue` 跳转到最内层循环的下一次迭代。

`for x in iter { body }` 通过 §9.4 脱糖。

### 5.4 Return

`return expr;` 以值 `expr` 退出当前函数。`return;` 返回 `unit`。

---

## 6. 接口和方法

Rusk 使用 `interface` + `impl`，类似于 Rust trait，但也将接口重用为效应命名空间（见 §7）。

### 6.1 接口方法（非效应调用）

使用显式限定调用接口方法：

```rust
Logger::log(console, "hello");
```

这总是由编译器根据所选的 `impl` 静态解析。

允许方法调用语法糖：

```rust
console.log("hello")
```

并且等同于 `Logger::log(console, "hello")`，**仅当**调用是无歧义的（恰好有一个名为 `log` 的接口方法适用）。否则，编译器报告歧义错误并要求显式限定。

> 理由：这保持了"类似 TypeScript 的人体工程学"，同时在重要的地方保持"类似 Rust"的显式性。

### 6.2 固有方法

固有方法用 `impl Type { ... }` 定义，并如下调用：

```rust
Point::new(1, 2)
point.len()
```

解析规则：

1. 接收者命名类型上的固有方法
2. 接口方法（如果无歧义）
3. 否则：错误

### 6.3 重载

没有按参数类型进行的特设重载。应用上述方法解析规则后，名称最多解析为一个调用目标。

---

## 7. 效应和处理器

### 7.1 效应调用

效应调用写作：

```rust
@Logger.log("msg")
```

并且与普通接口方法调用不同。

语义：

- 在运行时，解释器在动态处理器栈（由 `match` 效应分支安装；§7.2）中搜索匹配的处理器子句。
- 如果找不到，程序以"未处理的效应"错误 trap。

### 7.2 Match 作为效应处理器

`match` 有两个角色：

1. 它对其审查表达式产生的值进行模式匹配（值分支），
2. 它在求值审查表达式时安装效应处理器（效应分支）。

示例：

```rust
match compute() {
  @Logger.log(msg) => { print(msg); resume(()) }
  0 => 1
  n => n + 1
}
```

规则：

- 效应分支**仅在求值审查表达式时**活动。
- 效应分支不参与最终值模式匹配。
- 多个效应分支按源顺序尝试；选择参数模式匹配效应参数的第一个。

### 7.3 `resume`

在效应分支内，特殊标识符 `resume` 在作用域内（它不能被重新定义）。它的行为类似函数：

```
resume(value_for_effect_call) -> handled_result
```

其中：

- `value_for_effect_call` 成为挂起的效应调用的返回值。
- `handled_result` 是恢复后求值*被处理区域*（即 `match` 审查表达式求值）的最终结果。

`resume` 是**一次性的**：多次调用它会 trap。

如果效应分支在不调用 `resume` 的情况下完成，则捕获的延续被放弃，效应分支的值成为整个 `match` 表达式的结果。

### 7.4 界定保证

Rusk 规定由 `match` 创建的效应处理器被*界定*到审查表达式求值。

底层 MIR 运行时捕获完整延续；因此编译器必须通过将被处理区域降级到辅助函数来实现界定，以便恢复在该辅助函数返回后返回到处理器。

这是源语言的语义要求。

---

## 8. 类型系统

Rusk 有一个静态类型系统，在编译时进行全局检查。类型在运行时被擦除。

### 8.1 设计目标

- 优先考虑健全性：没有隐式 `any`。
- 尽可能类似 TypeScript 的推断（局部变量和泛型参数）。
- 在公共边界上类似 Rust 的显式性（项必须有类型）。
- 一等泛型，带有有限的 HKT。

### 8.2 必需的类型注解

以下内容必须完全注解：

- 所有 `fn` 项参数和返回类型
- 所有 `struct` 字段类型
- 所有 `enum` 变体字段类型
- 所有 `interface` 方法参数和返回类型

局部 `let` 绑定可以省略类型（推断）。

### 8.3 类型形式

命名类型：

- 原始类型：`unit`, `bool`, `int`, `float`, `string`, `bytes`
- 数组：`[T]`
- 结构体：`Name<...>`
- 枚举：`Name<...>`
- 函数：`fn(T1, T2, ...) -> R`
- 接口：`Interface<...>`
- 只读视图：`readonly T`（仅当 `T` 是数组/结构体/枚举时有意义；它禁止通过该引用进行修改）

### 8.4 泛型和种类

每个泛型参数都有一个种类：

- `T` 有种类 `Type`
- `F<_...>` 有种类 `Type^n -> Type`，其中 `n` 是下划线的数量

种类检查规则：

- arity `n` 的类型构造器 `F` 必须恰好应用于 `n` 个类型参数。
- 所有类型参数必须有种类 `Type`。
- 任何类型应用的结果都有种类 `Type`。

这足以表达常见的 HKT，如 `Functor<F<_>>` 和 `Monad<M<_>>`。

### 8.5 类型推断（概述）

Rusk 使用一种有意可实现的局部类型推断形式：

- 每个表达式都被分配一个类型，使用基于统一的约束。
- 局部 `let` 绑定是**单态的**（它们不会隐式引入 `forall`）。多态性仅通过项（`fn`、`struct`、`enum`、`interface`）上的显式泛型参数引入。
- 函数调用中的泛型参数通过将参数类型与被调用者的参数类型统一来推断（类似 TypeScript 的"泛型参数推断"）。
- Lambda 参数类型可以从预期的函数类型推断；否则它们必须注解。

当推断无法确定唯一类型时，需要显式注解。

### 8.6 接口作为约束

`interface` 可以用作泛型参数列表中的约束：

```rust
fn log_all<T: Logger>(xs: [T]) -> unit { ... }
```

约束满足基于 `impl Logger for T` 的存在性。

### 8.7 效应类型

对于声明为以下的接口方法：

```rust
interface Logger {
  fn log(msg: string) -> unit;
}
```

那么：

- `@Logger.log(e)` 有类型 `unit` 并要求 `e: string`。
- 效应分支 `@Logger.log(pat) => body` 是良类型的，如果模式绑定声明的参数类型的值，并且 `resume` 的使用是一致的：
  - `resume(v)` 要求 `v: unit`（效应的声明返回类型）。

整体 `match` 表达式的类型由值分支和效应分支共同确定（所有分支必须统一为单个结果类型）。

### 8.8 一般递归和不动点

Rusk 是一种通用语言，允许**一般递归**：`fn` 项可以通过名称引用自身。

因为递归是可用的，用户可以将不动点组合器（Y/Z 风格）实现为普通库代码，并且泛型类型系统必须能够对它们进行类型检查。一个规范的 "fix" 辅助函数是：

```rust
fn fix<A, B>(f: fn(fn(A) -> B) -> fn(A) -> B) -> fn(A) -> B {
    |x: A| {
        let g = fix(f);
        let h = f(g);
        h(x)
    }
}
```

编译器不需要优化它；它作为类型系统能力和定义示例提供。

---

## 9. 脱糖和必需的标准库表面

Rusk 核心语法包括运算符和 `for`，但它们脱糖为对 `std` 模块中名称的普通调用。

编译器**必须**发出这些调用，运行时**必须**将它们作为宿主函数提供。

### 9.1 数值运算符

对于 `int`：

- `a + b` 降级为 `std::int_add(a, b)`
- `a - b` 降级为 `std::int_sub(a, b)`
- `a * b` 降级为 `std::int_mul(a, b)`
- `a / b` 降级为 `std::int_div(a, b)`
- `a % b` 降级为 `std::int_mod(a, b)`

比较：

- `a < b` 降级为 `std::int_lt(a, b)`（返回 `bool`）
- `a <= b` 降级为 `std::int_le(a, b)` 等
- `a == b` 降级为 `std::int_eq(a, b)` 等（用于原始相等）

类似地，`float` 通过 `std::float_*`。

### 9.2 布尔运算符

- `!x` 降级为 `std::bool_not(x)`
- `a && b` 降级为短路 `if a { b } else { false }`
- `a || b` 降级为短路 `if a { true } else { b }`

### 9.3 格式化字符串

`f"prefix {e1} middle {e2} suffix"` 降级为：

1. 从左到右求值 `e1`、`e2`
2. 通过 `std::to_string` 将每个转换为 `string`
3. 通过 `std::string_concat` 连接

### 9.4 `for` 脱糖（迭代器协议）

`for x in iter { body }` 脱糖为：

```rust
let it = std::into_iter(iter);
loop {
  match std::next(it) {
    Option::Some(x) => { body; }
    Option::None(()) => break;
  }
}
```

必需的 std 函数和类型：

- `std::into_iter<T, It>(T) -> It`
- `std::next<It, Item>(It) -> Option<Item>`
- `enum Option<T> { Some(T), None(unit) }`

---

## 10. 编译到 MIR（规范性）

编译器将 Rusk 降级到 `MIR_SPEC.md` 中定义的 MIR。

关键要求：

- 保持从左到右的求值顺序。
- 将堆聚合（数组/结构体/枚举）编译为 MIR 堆分配。
- 将 `readonly` 绑定编译为 MIR `as_readonly`。
- 将 `match` 值分支编译为带有模式的 MIR `switch`。
- 遵循 §7.4 中的界定规则，将效应分支编译为 MIR `push_handler`/`perform`/`resume`。

---

## 11. 程序入口

可运行程序必须定义：

```rust
fn main() -> unit { ... }
```

CLI 工具将程序编译为 MIR，安装所需的 `std::*` 宿主函数，运行 `main`，并将任何 trap 视为非零进程退出。
