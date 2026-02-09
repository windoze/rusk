# Rusk Language Specification (v0.4)

本文件是 **Rusk 语言规范**（Rusk Language Spec）。

Rusk 的目标是：在语法上接近 Rust（块表达式、`match`、显式可变性控制），在工程体验上接近 TypeScript（类型推断、泛型、接口驱动的抽象），并以 **代数效果（Algebraic Effects）** 作为一等机制统一异常/异步/生成器等控制流扩展。

本仓库的实现是一个“脚本 → MIR → 解释执行”的参考实现：

- `RUSK_SPEC.md` 定义源语言（本文件）。
- `MIR_SPEC.md` 定义中间表示 MIR。
- 解释器执行 MIR，并提供宿主函数（host functions）作为“标准库”实现。

本规范以 **可实现** 为前提：规范中出现的语法与语义都必须能完整编译到当前版本的 MIR 并在解释器中运行（不允许“未来再实现”的占位条款）。

---

## 0. Notation

- `unit` means the unit value `()`.
- “Trap” means a runtime error (MIR `trap`).
- “Host function” means an externally provided function callable from MIR via `call`.

---

## 1. Source Text

### 1.1 Encoding

Source files are UTF-8.

### 1.2 Whitespace and Comments

- Whitespace separates tokens but is otherwise insignificant (except inside string/bytes literals).
- Line comments: `// ...` until end-of-line.
- Block comments: `/* ... */` may nest.

---

## 2. Lexical Tokens

### 2.1 Identifiers

Identifiers match:

- first character: `_` or Unicode XID_Start
- subsequent characters: Unicode XID_Continue

Rusk treats keywords as reserved and they cannot be used as identifiers.

### 2.2 Keywords

Items and declarations:

`fn`, `let`, `const`, `readonly`, `struct`, `enum`, `interface`, `impl`

Control flow:

`if`, `else`, `match`, `return`, `loop`, `while`, `for`, `in`, `break`, `continue`

### 2.3 Operators and Delimiters

Delimiters:

`(` `)` `{` `}` `[` `]` `,` `:` `;` `.` `::` `=>`

Operators:

`=` `+` `-` `*` `/` `%`

`==` `!=` `<` `<=` `>` `>=`

`&&` `||` `!`

Effect marker:

`@`

> Note: Operators are syntactic sugar; they lower to standard-library function calls.

---

## 3. Grammar (EBNF)

The grammar below is descriptive and intended to be unambiguous for implementation.

### 3.1 Program

```
Program        := Item* ;

Item           := FnItem | StructItem | EnumItem | InterfaceItem | ImplItem ;
```

### 3.2 Items

#### 3.2.1 Functions

```
FnItem         := "fn" Ident GenericParams? "(" ParamList? ")" ReturnType? Block ;
ParamList      := Param ("," Param)* (",")? ;
Param          := ParamMut? Ident ":" Type ;
ParamMut       := "readonly" ;
ReturnType     := "->" Type ;
```

Notes:
- Parameters are immutable bindings; mutability in Rusk refers to *object mutability* (heap objects), not rebinding.
- `readonly` on a parameter means the parameter is a readonly view (attempting to mutate through it traps).

#### 3.2.2 Structs

```
StructItem     := "struct" Ident GenericParams? "{" FieldList? "}" ;
FieldList      := Field ("," Field)* (",")? ;
Field          := Ident ":" Type ;
```

Struct values are heap-allocated objects with named fields.

#### 3.2.3 Enums

```
EnumItem       := "enum" Ident GenericParams? "{" VariantList? "}" ;
VariantList    := Variant ("," Variant)* (",")? ;
Variant        := Ident TupleFields? ;
TupleFields    := "(" TypeList? ")" ;
TypeList       := Type ("," Type)* (",")? ;
```

Enum values are heap-allocated tagged unions.

#### 3.2.4 Interfaces

Interfaces define:
- (1) method signatures for static dispatch (compile-time resolved calls), and
- (2) effect signatures for `@Interface.method(...)`.

```
InterfaceItem  := "interface" Ident GenericParams? "{" InterfaceMember* "}" ;
InterfaceMember:= "fn" Ident GenericParams? "(" ParamList? ")" ReturnType? ";" ;
```

#### 3.2.5 Implementations

```
ImplItem       := "impl" ImplHeader "{" ImplMember* "}" ;
ImplHeader     := Ident GenericArgs?                    // inherent impl: impl Type { ... }
               | Ident GenericArgs? "for" Ident GenericArgs? ; // interface impl: impl Interface for Type { ... }

ImplMember     := FnItem ;
```

### 3.3 Generics

Rusk supports higher-kinded type parameters (HKTs) in a limited, implementable form.

Kinds in v0.4 are restricted to:
- `Type` (arity 0), written `T`
- type constructors of arity `n >= 1`, written `F<_, _, ...>` with `n` underscores

```
GenericParams  := "<" GenericParam ("," GenericParam)* (",")? ">" ;
GenericParam   := Ident | Ident "<" ("_"
                                  | "_" ("," "_")+
                                  ) ">" ;

GenericArgs    := "<" Type ("," Type)* (",")? ">" ;
```

Examples:

```rust
fn id<T>(x: T) -> T { x }

// F is a unary type constructor (kind Type -> Type)
fn map<F<_>, A, B>(f: fn(A) -> B, xs: F<A>) -> F<B> { ... }
```

### 3.4 Types

```
Type           := ("readonly")? TypeAtom ;

TypeAtom       := PrimType
               | "[" Type "]"                      // dynamic array type
               | "fn" "(" TypeList? ")" "->" Type  // function type
               | PathType ;

PrimType       := "unit" | "bool" | "int" | "float" | "string" | "bytes" ;

PathType       := Ident GenericArgs? ("::" Ident GenericArgs?)* ;
```

Notes:
- `PathType` is used for nominal types (structs/enums/interfaces) and associated names.
- For this implementation, generic type arguments are **erased at runtime** (they do not affect runtime representation).
- `readonly T` is a *view type* for reference-like values (arrays/structs/enums). Writing through a `readonly` view is a compile-time error and also traps at runtime.

### 3.5 Statements and Blocks

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

Notes:
- `let x;` declares an uninitialized local (reading it before assignment is a runtime error).
- `let x = e;` initializes `x`.
- `const x = e;` prevents rebinding of `x` (but does not deep-freeze referenced objects).
- `readonly x = e;` is equivalent to `const x = e;` plus a readonly view: it prevents rebinding and forbids mutation through `x`.

### 3.6 Expressions

Rusk expressions are Rust-like: blocks, `if`, and `match` are expressions.

Precedence (high → low):

1. postfix: call, field, index
2. unary: `!` `-`
3. multiplicative: `*` `/` `%`
4. additive: `+` `-`
5. comparison: `<` `<=` `>` `>=`
6. equality: `==` `!=`
7. logical and: `&&`
8. logical or: `||`
9. assignment: `=`

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

#### 3.6.1 Literals

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

// StringLit / BytesLit / FStringLit are lexical tokens with escapes (see below).

Digit          := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
HexDigit       := Digit | "a" | "b" | "c" | "d" | "e" | "f" | "A" | "B" | "C" | "D" | "E" | "F" ;
OctDigit       := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ;
BinDigit       := "0" | "1" ;
```

String literals (`StringLit`):

- Written as `"..."`.
- Represent UTF-8 strings.
- Support escapes:
  - `\\`, `\"`
  - `\n`, `\r`, `\t`, `\0`
  - `\u{...}` (1+ hex digits, Unicode scalar value)

Byte string literals (`BytesLit`):

- Written as `b"..."`.
- Represent raw `bytes`.
- Support escapes:
  - `\\`, `\"`
  - `\n`, `\r`, `\t`, `\0`
  - `\xHH` (exactly two hex digits)
- Unescaped characters must be ASCII (0x20..=0x7E excluding `"` and `\`).

Formatted strings:
- `f"..."` supports interpolation via `{ Expr }` segments.
- `{{` and `}}` encode literal `{` and `}` respectively.
- Desugaring is defined in §9.3.

#### 3.6.2 Array, Struct, Enum literals

```
ArrayLit       := "[" (Expr ("," Expr)*)? (",")? "]" ;

StructLit      := Ident "{" (StructFieldInit ("," StructFieldInit)*)? (",")? "}" ;
StructFieldInit:= Ident ":" Expr ;

EnumLit        := Ident "::" Ident "(" (Expr ("," Expr)*)? (",")? ")" ;
```

#### 3.6.3 Lambdas

```
Lambda         := "|" LambdaParams? "|" Block ;
LambdaParams   := LambdaParam ("," LambdaParam)* (",")? ;
LambdaParam    := Ident (":" Type)? ;
```

Lambda parameter types may be inferred from context; if they cannot be inferred,
they must be explicitly annotated.

#### 3.6.4 If

```
IfExpr         := "if" Expr Block ("else" (IfExpr | Block))? ;
```

Parsing note (struct literals vs blocks):

Because `{ ... }` is used for both blocks and struct literals (`Type { field: expr, ... }`),
in positions where a block follows immediately (`if` / `while` conditions, `for` iterables,
`match` scrutinees), a struct literal must be parenthesized to disambiguate:

```rust
// ok
if (Point { x: 1, y: 2 }).x == 1 { ... }

// also ok
match (Point { x: 1, y: 2 }) { ... }
```

#### 3.6.5 Match

Match arms can be value arms or effect arms (see §7).

```
MatchExpr      := "match" Expr "{" MatchArm* "}" ;
MatchArm       := (EffectPat | ValuePat) "=>" BlockOrExpr ","? ;
BlockOrExpr    := Block | Expr ;
ValuePat       := Pattern ;
EffectPat      := "@" PathExpr "." Ident "(" PatList? ")" ;
PatList        := Pattern ("," Pattern)* (",")? ;
```

#### 3.6.6 Loops

```
LoopExpr       := "loop" Block ;
WhileExpr      := "while" Expr Block ;
```

`for` is provided as syntax:

```
ForExpr        := "for" Ident "in" Expr Block ;
```

and desugars (in the type system and compiler) to a `while` loop over an iterator
protocol in the standard library (see §9.4).

### 3.7 Patterns

Patterns are used in `match` value arms and in effect arms’ parameter patterns.

```
Pattern        := "_"                       // wildcard
               | Ident                     // binding
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

Notes:
- `Ident` in a pattern always binds (there are no “constant patterns” for names).
- Array prefix patterns match `[p1, p2, ..]` (prefix) and `[p1, p2]` (exact length).

---

## 4. Runtime Model

### 4.1 Values

Runtime values are:

- `unit`, `bool`, `int`, `float`, `string`, `bytes`
- heap references: structs, enums, arrays (shared references)
- function values (first-class, referring to a named function)
- continuation tokens (captured by effects)

### 4.2 Heap Objects and Sharing

Structs/enums/arrays are heap-allocated and passed by reference. Copying a reference
creates an alias; mutations are observable through all aliases.

### 4.3 Readonly Views

`readonly` does **not** deep-freeze an object graph. It creates a *view* that forbids
mutation **through that particular binding/reference**.

- Mutating through a readonly view traps at runtime.
- Mutating through a non-readonly alias remains allowed and is observable.

This matches MIR’s `as_readonly` behavior.

### 4.4 Uninitialized Locals

`let x;` introduces an uninitialized local. Reading `x` before it is initialized is
a runtime error (trap).

### 4.5 Evaluation Order

Evaluation is left-to-right:

- function call arguments
- struct field initializers
- array elements
- binary operator operands
- match scrutinee evaluation

This is a **semantic guarantee** (not merely an implementation detail).

### 4.6 Assignment (Lvalues)

Assignment is an expression but yields `unit` (Rust-like).

The left-hand side of `=` must be one of:

- a local name: `x = expr` (only if `x` was declared with `let`, not `const`/`readonly`)
- a struct field: `obj.field = expr`
- an array slot: `arr[index] = expr`

Evaluation order is left-to-right:

- `x = expr`: evaluate `expr`, then assign.
- `obj.field = expr`: evaluate `obj`, then evaluate `expr`, then mutate the field.
- `arr[index] = expr`: evaluate `arr`, then `index`, then `expr`, then mutate the slot.

If the target is a readonly view (`readonly T`), the mutation is rejected by the type
checker and also traps at runtime if reached.

---

## 5. Control Flow Semantics

### 5.1 Blocks

`{ s1; s2; expr }` evaluates statements in order, and yields the value of the final
expression if present; otherwise it yields `unit`.

### 5.2 If

`if cond { a } else { b }` evaluates `cond`; if true evaluates and yields `a`,
otherwise evaluates and yields `b`. `else` is required unless the `if` is used in a
statement position (ends with `;`), in which case missing `else` yields `unit`.

### 5.3 While / Loop / For

- `loop { ... }` repeats forever until a `break`.
- `while cond { ... }` repeats while `cond` evaluates to true.
- `break` exits the innermost loop and yields `unit` for that loop expression.
- `continue` jumps to the next iteration of the innermost loop.

`for x in iter { body }` desugars via §9.4.

### 5.4 Return

`return expr;` exits the current function with value `expr`. `return;` returns `unit`.

---

## 6. Interfaces and Methods

Rusk uses `interface` + `impl` similarly to Rust traits, but also reuses interfaces
as effect namespaces (see §7).

### 6.1 Interface Methods (non-effect calls)

Interface methods are called using explicit qualification:

```rust
Logger::log(console, "hello");
```

This is always statically resolved by the compiler based on the selected `impl`.

Method-call sugar is allowed:

```rust
console.log("hello")
```

and is equivalent to `Logger::log(console, "hello")` **only if** the call is
unambiguous (exactly one interface method named `log` is applicable). Otherwise,
the compiler reports an ambiguity error and requires explicit qualification.

> Rationale: this keeps “TypeScript-like ergonomics” while maintaining “Rust-like”
> explicitness where it matters.

### 6.2 Inherent Methods

Inherent methods are defined with `impl Type { ... }` and called as:

```rust
Point::new(1, 2)
point.len()
```

Resolution rules:

1. Inherent methods on the receiver’s nominal type
2. Interface methods (if unambiguous)
3. Otherwise: error

### 6.3 Overloading

There is no ad-hoc overloading by argument types. A name resolves to at most one
call target after the method resolution rules above are applied.

---

## 7. Effects and Handlers

### 7.1 Effect Calls

An effect call is written:

```rust
@Logger.log("msg")
```

and is distinct from a normal interface method call.

Semantics:

- At runtime, the interpreter searches the dynamic handler stack (installed by
  `match` effect arms; §7.2) for a matching handler clause.
- If none is found, the program traps with an “unhandled effect” error.

### 7.2 Match as an Effect Handler

`match` has two roles:

1. it pattern-matches the produced value of its scrutinee expression (value arms),
2. it installs effect handlers while evaluating the scrutinee (effect arms).

Example:

```rust
match compute() {
  @Logger.log(msg) => { print(msg); resume(()) }
  0 => 1
  n => n + 1
}
```

Rules:

- Effect arms are active **only while evaluating the scrutinee expression**.
- Effect arms do not participate in the final value pattern matching.
- Multiple effect arms are tried in source order; the first whose parameter
  patterns match the effect arguments is selected.

### 7.3 `resume`

Inside an effect arm, a special identifier `resume` is in scope (it cannot be
redefined). It behaves like a function:

```
resume(value_for_effect_call) -> handled_result
```

Where:

- `value_for_effect_call` becomes the return value of the suspended effect call.
- `handled_result` is the final result of evaluating the *handled region*
  (i.e., the entire `match` expression) after resuming: the resumed scrutinee
  evaluation *followed by* value-arm matching.

`resume` is **one-shot**: calling it more than once traps.

If an effect arm completes without calling `resume`, the captured continuation is
abandoned and the effect arm’s value becomes the result of the whole `match`
expression.

### 7.4 Delimitation Guarantee

Rusk specifies that effect handlers created by `match` are *delimited* to the
scrutinee evaluation.

The MIR runtime captures a **delimited** continuation: resuming a continuation
returns when the selected handler’s *owning frame* returns (see `MIR_SPEC.md`).

Therefore, the compiler must ensure that a `match` expression with effect arms
creates an owning frame for the handled region. The reference implementation does
this by lowering such `match` expressions into a helper function (so the helper
frame becomes the handler owner), ensuring `resume(...)` returns to the `match`
boundary rather than unwinding to an outer function.

This is a semantic requirement of the source language.

---

## 8. Type System

Rusk has a static type system with global checking at compile time. Types are erased
at runtime.

### 8.1 Design Goals

- Soundness-first: no implicit `any`.
- TypeScript-like inference where possible (locals and generic arguments).
- Rust-like explicitness at public boundaries (items must have types).
- First-class generics with limited HKTs.

### 8.2 Required Type Annotations

The following must be fully annotated:

- all `fn` item parameters and return types
- all `struct` field types
- all `enum` variant field types
- all `interface` method parameter and return types

Local `let` bindings may omit types (inferred).

### 8.3 Type Forms

Nominal types:

- primitives: `unit`, `bool`, `int`, `float`, `string`, `bytes`
- arrays: `[T]`
- structs: `Name<...>`
- enums: `Name<...>`
- functions: `fn(T1, T2, ...) -> R`
- interfaces: `Interface<...>`
- readonly views: `readonly T` (only meaningful when `T` is an array/struct/enum; it forbids mutation through that reference)

### 8.4 Generics and Kinds

Each generic parameter has a kind:

- `T` has kind `Type`
- `F<_...>` has kind `Type^n -> Type` where `n` is the number of underscores

Kind checking rules:

- A type constructor `F` of arity `n` must be applied to exactly `n` type arguments.
- All type arguments must have kind `Type`.
- The result of any type application has kind `Type`.

This is sufficient to express common HKTs such as `Functor<F<_>>` and `Monad<M<_>>`.

### 8.5 Type Inference (Overview)

Rusk uses a deliberately implementable form of local type inference:

- Each expression is assigned a type, using unification-based constraints.
- Local `let` bindings are **monomorphic** (they do not implicitly introduce `forall`).
  Polymorphism is introduced only by explicit generic parameters on items (`fn`, `struct`,
  `enum`, `interface`).
- Generic arguments in function calls are inferred by unifying argument types with the
  callee’s parameter types (TypeScript-like “generic argument inference”).
- Lambda parameter types may be inferred from an expected function type; otherwise they
  must be annotated.

Where inference cannot determine a unique type, an explicit annotation is required.

### 8.6 Interfaces as Constraints

An `interface` can be used as a constraint in a generic parameter list:

```rust
fn log_all<T: Logger>(xs: [T]) -> unit { ... }
```

Constraint satisfaction is based on the existence of an `impl Logger for T`.

### 8.7 Effects Typing

For an interface method declared as:

```rust
interface Logger {
  fn log(msg: string) -> unit;
}
```

Then:

- `@Logger.log(e)` has type `unit` and requires `e: string`.
- An effect arm `@Logger.log(pat) => body` is well-typed if the patterns bind values
  of the declared argument types, and if `resume` is used consistently:
  - `resume(v)` requires `v: unit` (the declared return type of the effect).

The type of the overall `match` expression is determined by the value arms and
effect arms collectively (all arms must unify to a single result type).

### 8.8 General Recursion and Fixpoints

Rusk is a general-purpose language and allows **general recursion**: a `fn` item may
refer to itself by name.

Because recursion is available, users can implement fixpoint combinators (Y/Z-style)
as ordinary library code, and the generic type system must be able to type-check
them. A canonical “fix” helper is:

```rust
fn fix<A, B>(f: fn(fn(A) -> B) -> fn(A) -> B) -> fn(A) -> B {
    |x: A| {
        let g = fix(f);
        let h = f(g);
        h(x)
    }
}
```

This is not required to be optimized by the compiler; it is provided as a
type-system capability and a definitional example.

---

## 9. Desugarings and Required Standard Library Surface

Rusk core syntax includes operators and `for`, but they desugar to ordinary calls
to names in the `std` module.

The compiler **must** emit these calls, and the runtime **must** provide them as
host functions.

### 9.1 Numeric Operators

For `int`:

- `a + b` lowers to `std::int_add(a, b)`
- `a - b` lowers to `std::int_sub(a, b)`
- `a * b` lowers to `std::int_mul(a, b)`
- `a / b` lowers to `std::int_div(a, b)`
- `a % b` lowers to `std::int_mod(a, b)`

Comparisons:

- `a < b` lowers to `std::int_lt(a, b)` (returns `bool`)
- `a <= b` lowers to `std::int_le(a, b)` etc
- `a == b` lowers to `std::int_eq(a, b)` etc (for primitive equality)

Similarly for `float` via `std::float_*`.

Other primitive equality:

- `bool`: `std::bool_eq`, `std::bool_ne`
- `string`: `std::string_eq`, `std::string_ne`
- `bytes`: `std::bytes_eq`, `std::bytes_ne`
- `unit`: `std::unit_eq`, `std::unit_ne`

### 9.2 Boolean Operators

- `!x` lowers to `std::bool_not(x)`
- `a && b` lowers to short-circuiting `if a { b } else { false }`
- `a || b` lowers to short-circuiting `if a { true } else { b }`

### 9.3 Formatted Strings

`f"prefix {e1} middle {e2} suffix"` lowers to:

1. evaluate `e1`, `e2` left-to-right
2. convert each to `string` via `std::to_string`
3. concatenate via `std::string_concat`

### 9.4 `for` Desugaring (Iterator Protocol)

`for x in iter { body }` desugars to:

```rust
let it = std::into_iter(iter);
loop {
  match std::next(it) {
    Option::Some(x) => { body; }
    Option::None(()) => break;
  }
}
```

Required std functions and types:

- `struct std::ArrayIter<T> { arr: [T], idx: int }` (iterator state object)
- `std::into_iter<T>([T]) -> std::ArrayIter<T>`
- `std::next<T>(std::ArrayIter<T>) -> Option<T>`
- `enum Option<T> { Some(T), None(unit) }`

---

## 10. Compilation to MIR (Normative)

The compiler lowers Rusk to MIR as defined in `MIR_SPEC.md`.

Key requirements:

- Preserve left-to-right evaluation order.
- Compile heap aggregates (arrays/structs/enums) to MIR heap allocations.
- Compile `readonly` bindings to MIR `as_readonly`.
- Compile `match` value arms to MIR `switch` with patterns.
- Compile effect arms to MIR `push_handler`/`perform`/`resume` following the delimitation
  rule in §7.4.

---

## 11. Program Entry

A runnable program must define:

```rust
fn main() -> unit { ... }
```

The CLI tool compiles the program to MIR, installs the required `std::*` host
functions, runs `main`, and treats any trap as a non-zero process exit.
