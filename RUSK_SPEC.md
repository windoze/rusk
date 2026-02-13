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
- A Unix-style “shebang” line is permitted as the first line of a source file: if the file begins with `#!`, the rest of that line is ignored (treated like a line comment).

---

## 2. Lexical Tokens

### 2.1 Identifiers

Identifiers match:

- first character: `_` or Unicode XID_Start
- subsequent characters: Unicode XID_Continue

Rusk treats keywords as reserved and they cannot be used as identifiers.

### 2.2 Keywords

Items and declarations:

`pub`, `use`, `mod`, `as`, `is`, `fn`, `cont`, `let`, `const`, `readonly`, `static`, `struct`, `enum`, `interface`, `impl`

Control flow:

`if`, `else`, `match`, `return`, `loop`, `while`, `for`, `in`, `break`, `continue`

Reserved identifiers:

- In an **instance method** body (including default interface method bodies), `self` is a reserved identifier that refers to the implicit receiver. It cannot be declared or shadowed by locals, parameters, or patterns within that method.
- In a `static fn` body, `self` is not in scope.
- `self::...` in paths continues to mean “relative to the current module” (§3.2.7); this is distinct from the receiver `self` value, which is accessed as an expression (`self`), field (`self.x`), or method call (`self.m(...)`).

### 2.3 Operators and Delimiters

Delimiters:

`(` `)` `{` `}` `[` `]` `,` `:` `;` `.` `::` `=>`

Operators:

`=` `+` `-` `*` `/` `%`

`==` `!=` `<` `<=` `>` `>=`

`&&` `||` `!`

`?` (used only as part of `as?` in v0.4)

Effect marker:

`@`

> Note: Operators are syntactic sugar; they lower to standard-library function calls.

---

## 3. Grammar (EBNF)

The grammar below is descriptive and intended to be unambiguous for implementation.

### 3.1 Program

```
Program        := Item* ;

Item           := Visibility? (FnItem | StructItem | EnumItem | InterfaceItem | ImplItem | ModItem | UseItem) ;

Visibility     := "pub" ;
```

### 3.2 Items

#### 3.2.1 Functions

```
FnItem         := "fn" Ident GenericParams? "(" ParamList? ")" ReturnType? Block ;
ParamList      := Param ("," Param)* (",")? ;
Param          := ParamMut? Pattern ":" Type ;
ParamMut       := "readonly" ;
ReturnType     := "->" Type ;
```

Notes:
- Parameters are immutable bindings; mutability in Rusk refers to *object mutability* (heap objects), not rebinding.
- `readonly` on a parameter means the parameter is a readonly view (attempting to mutate through it traps).
- Parameters may use destructuring patterns; if a parameter pattern does not match at runtime, the function traps.
- If `ReturnType` is omitted, it defaults to `unit` (i.e., `fn f(...) { ... }` is sugar for `fn f(...) -> unit { ... }`).

#### 3.2.2 Structs

```
StructItem     := "struct" Ident GenericParams? ( NamedStructBody | NewTypeStructBody ) ;

NamedStructBody:= "{" FieldList? "}" ;
NewTypeStructBody
              := "(" Type ")" ";" ;

FieldList      := Field ("," Field)* (",")? ;
Field          := Ident ":" Type ;
```

Struct values are heap-allocated objects.

Rusk supports two struct body forms:

- **named-field structs** (`struct Point { x: int, y: int }`) with named field access `p.x`.
- **new-type structs** (`struct UserId(int);`) which are nominal wrappers around a single field:
  - they are constructed with call syntax `UserId(value)` (not a struct literal)
  - their single field is accessed by index `u.0`
  - they do not introduce any implicit conversions to/from the underlying field type.

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
InterfaceItem  := "interface" Ident GenericParams?
                 ( ":" PathType ( "+" PathType )* )?
                 "{" InterfaceMember* "}" ;
InterfaceMember:= ("readonly")? "fn" Ident GenericParams? "(" ParamList? ")" ReturnType?
                  ( ";" | Block ) ;
```

Notes:
- If `ReturnType` is omitted in an interface member, it defaults to `unit`.
- `interface` members are always instance methods (they take an implicit receiver). `static fn` is not allowed in interfaces.
- An interface member with a body (`{ ... }`) is a **default method**. An `impl` may omit methods that have defaults.
- `interface` items may inherit from one or more super-interfaces using `: A + B + ...`.
- Super-interfaces must resolve to interfaces (not structs/enums).
- Cycles in the interface inheritance graph are rejected.
- In v0.4, multiple inheritance is rejected if two inherited interfaces introduce the same method
  name from different origins (diamond duplication of the same origin is allowed).

#### 3.2.5 Implementations

```
ImplItem       := "impl" ImplHeader "{" ImplMember* "}" ;
ImplHeader     := Ident GenericArgs?                    // inherent impl: impl Type { ... }
               | Ident GenericArgs? "for" Ident GenericArgs? ; // interface impl: impl Interface for Type { ... }

ImplMember     := ("readonly" | "static")? "fn" Ident GenericParams? "(" ParamList? ")" ReturnType? Block ;
```

Notes:
- In `impl Type { ... }`, `fn` defines an instance method with an implicit receiver (`self`).
- `readonly fn` defines an instance method whose receiver is a readonly view inside the method body.
- `static fn` defines a receiver-less static method, callable only as `Type::method(...)`.
- In `impl Interface for Type { ... }`, `static fn` is forbidden and method receiver mutability (`readonly` vs non-`readonly`) must match the interface declaration.

#### 3.2.6 Modules

Rusk modules are Rust-like and form a tree rooted at the implicit crate root module `crate`.

```
ModItem        := "mod" Ident ";"          // file/directory module (loaded by the module loader)
               | "mod" Ident "{" Item* "}" // inline module
               ;
```

Notes:

- `mod name { ... }` defines an inline submodule.
- `mod name;` declares a file/directory submodule. Before parsing/typechecking, the module loader
  must replace it by an inline module whose contents are loaded from disk:
  - file module: `<dir>/<name>.rusk`
  - directory module: `<dir>/<name>/mod.rusk`
  where `<dir>` is the directory of the current module file.
  It is an error if both exist or if neither exists.
- Module names `core` and `std` are reserved.

#### 3.2.7 Imports and Re-exports (`use`)

```
UseItem        := "use" Path ("as" Ident)? ";" ;
```

Notes:

- Rusk follows Rust’s **separate namespaces**: module namespace, type namespace, value namespace.
  A `use` may introduce a binding in one or more namespaces depending on what the path resolves to.
- `pub use ...;` is a public re-export. A public re-export requires the target item to already be
  public.
- Paths may use Rust-like module anchors at the beginning:
  - `crate::...` is absolute from the crate root.
  - `self::...` is relative to the current module.
  - `super::...` is relative to the parent module (multiple `super::super::...` are allowed).
- The built-in module `core` is always available as `core::...`.
- `core::prelude` is automatically imported into every module (like Rust’s prelude). In v0.4, it
  contains `panic`.

### 3.3 Generics

Rusk supports higher-kinded type parameters (HKTs) in a limited, implementable form.

Kinds in v0.4 are restricted to:
- `Type` (arity 0), written `T`
- type constructors of arity `n >= 1`, written `F<_, _, ...>` with `n` underscores

```
GenericParams  := "<" GenericParam ("," GenericParam)* (",")? ">" ;
GenericParam   := Ident HktArity? Bounds? ;
HktArity       := "<" "_" ("," "_")* ">" ;
Bounds         := ":" PathType ("+" PathType)* ;

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
               | "fn" "(" TypeList? ")" ("->" Type)?  // function type (defaults to `unit`)
               | "cont" "(" Type ")" "->" Type     // continuation token type
               | "(" ")"                           // empty tuple type (aka unit)
               | "(" Type ")"                      // parenthesized type
               | "(" Type "," TypeList? ")"        // tuple type (comma required)
               | PathType ;

PrimType       := "unit" | "bool" | "int" | "float" | "string" | "bytes" ;

PathType       := Ident GenericArgs? ("::" Ident GenericArgs?)* ;
```

Notes:
- `PathType` is used for nominal types (structs/enums/interfaces) and associated names.
- Type arguments may appear on any path segment. Semantically, all type arguments on the path are
  collected left-to-right and applied to the nominal type named by the path.
- This implementation reifies arity-0 type arguments (kind `Type`) at runtime as an internal
  `TypeRep` for operations that require type-precise behavior (notably `is` / `as?` and effect
  identity). `readonly` remains a compile-time view and is not a distinct runtime type.
- `readonly T` is a *view type* for reference-like values (arrays/structs/enums/tuples). Writing through a `readonly` view is a compile-time error and also traps at runtime.
- Tuples use Rust-like comma disambiguation: `(T)` is a parenthesized type, `(T,)` is a 1-tuple type, and `()` is `unit`.

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
               | BlockLikeExpr ";"? ;

BlockLikeExpr  := Block
               | IfExpr
               | MatchExpr
               | LoopExpr
               | WhileExpr
               | ForExpr ;
```

Notes:
- `let x: T;` declares an uninitialized local (reading it before assignment is a runtime error). In this implementation, an uninitialized `let` binding must have an explicit type annotation.
- `let x = e;` initializes `x`.
- `const x = e;` prevents rebinding of `x` (but does not deep-freeze referenced objects).
- `readonly x = e;` is equivalent to `const x = e;` plus a readonly view: it prevents rebinding and forbids mutation through `x`.
- Like Rust, *block-like* expression statements (`if` / `match` / `loop` / `while` / `for` / `{ ... }`) may omit the trailing `;` when used as a statement inside a block.
  - If such an expression appears in the final position of a block and does **not** have a trailing `;`, it is parsed as the block's tail expression and becomes the value of the block (`Block := "{" Stmt* Expr? "}"`).

### 3.6 Expressions

Rusk expressions are Rust-like: blocks, `if`, and `match` are expressions.

Precedence (high → low):

1. postfix: type-args, call, field, index
2. unary: `!` `-`
3. cast: `as`, `as?`
4. multiplicative: `*` `/` `%`
5. additive: `+` `-`
6. type test: `is`
7. comparison: `<` `<=` `>` `>=`
8. equality: `==` `!=`
9. logical and: `&&`
10. logical or: `||`
11. assignment: `=`

```
Expr           := AssignExpr ;

AssignExpr     := OrExpr ( "=" AssignExpr )? ;
OrExpr         := AndExpr ( "||" AndExpr )* ;
AndExpr        := EqExpr ( "&&" EqExpr )* ;
EqExpr         := CmpExpr ( ( "==" | "!=" ) CmpExpr )* ;
CmpExpr        := TypeTestExpr ( ( "<" | "<=" | ">" | ">=" ) TypeTestExpr )* ;
TypeTestExpr   := AddExpr ( "is" Type )? ;
AddExpr        := MulExpr ( ( "+" | "-" ) MulExpr )* ;
MulExpr        := CastExpr ( ( "*" | "/" | "%" ) CastExpr )* ;
CastExpr       := UnaryExpr ( "as" ("?")? Type )* ;
UnaryExpr      := ( "!" | "-" ) UnaryExpr | PostfixExpr ;

PostfixExpr    := PrimaryExpr Postfix* ;
Postfix        := TypeArgs | Call | Field | Index ;

TypeArgs       := "::<" TypeList? ">" ;
Call           := "(" ArgList? ")" ;
ArgList        := Expr ("," Expr)* (",")? ;
Field          := "." (Ident | IntLit) ;
Index          := "[" Expr "]" ;

PrimaryExpr    := Literal
               | PathExpr
               | ArrayLit
               | TupleLit
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

EffectCall     := "@" PathType "." Ident "(" ArgList? ")" ;

TupleLit       := "(" Expr "," ArgList? ")" ;
```

`::<...>` is a Rust-style *turbofish* that supplies explicit type arguments for a generic call.
It is only valid immediately before a call (either `(...)` or the trailing-closure call sugar
`callee { ... }`).

`expr as I` performs an explicit interface upcast. The RHS must be an `interface` type. The cast
does not allocate and does not change runtime representation; it only changes the static type, and
it preserves `readonly` views.

`expr as? T` performs a checked cast and returns `Option<T>`. It evaluates `expr` once, then:

- if the runtime type test succeeds, returns `Option::Some(expr)` (no wrapper allocation for the cast itself)
- otherwise returns `Option::None`

`expr is T` performs the same runtime type test but returns `bool`.

The target type `T` for `is` / `as?` must be a **runtime-checkable nominal type**:

- a `struct` type (possibly instantiated, e.g. `Pair<int, bool>`), or
- an `enum` type (possibly instantiated, e.g. `Option<int>`), or
- an `interface` type (possibly instantiated, e.g. `Iterator<int>`).

`readonly` is not a runtime type and is rejected as an `is` / `as?` target.

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

#### 3.6.2 Array, Tuple, Struct, Enum literals

```
ArrayLit       := "[" (Expr ("," Expr)*)? (",")? "]" ;

TupleLit       := "(" Expr "," ArgList? ")" ;

StructLit      := Ident "{" (StructFieldInit ("," StructFieldInit)*)? (",")? "}" ;
StructFieldInit:= Ident ":" Expr ;

EnumLit        := Ident "::" Ident "(" (Expr ("," Expr)*)? (",")? ")" ;
```

Notes:
- Tuple literals use Rust-like comma disambiguation: `(x)` is a parenthesized expression, `(x,)` is a 1-tuple, and `()` is `unit`.
- Struct literals are parsed only when `Ident` looks like a nominal type name (it must start with an ASCII uppercase letter, e.g. `Point { x: 1 }`).
- A zero-field enum variant may be written without parentheses as `Enum::Variant`; this is sugar for `Enum::Variant()`.
- New-type structs are **not** constructed with struct literals. Use call syntax:
  - `UserId(42)` / `Box::<int>(123)`
  - access the wrapped field via `.0` or by pattern destructuring (§3.7).

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

Because `{ ... }` is used for both blocks and struct literals (`Point { x: 1, y: 2 }`),
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
EffectPat      := "@" PathType "." Ident "(" PatList? ")" ("->" Ident)? ;
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

#### 3.6.7 Trailing closures (syntax sugar)

Rusk supports trailing-closure call syntax for *zero-argument* lambdas.

This syntax is **purely syntactic sugar** and does not introduce any new semantics.

Desugaring:

- `f(a, b) { body }` is equivalent to `f(a, b, || { body })`.
- `f { body }` is equivalent to `f(|| { body })`.

Named-bind form:

- `f(x=ex, y=ey) { body }` is equivalent to:
  ```rust
  { let x = ex; let y = ey; f(x, y, || { body }) }
  ```

Rules:
- The named-bind form requires **every** argument to be of the form `name = expr` (mixing positional and named arguments is a parse error).
- Named-bind names must be unique within the call.

### 3.7 Patterns

Patterns are used in:
- `match` value arms
- effect arms’ parameter patterns
- function parameters (§3.2.1)

```
Pattern        := "_"                       // wildcard
               | Ident                     // binding
               | LiteralPat
               | TuplePat
               | CtorPat
               | EnumPat
               | StructPat
               | ArrayPat ;

LiteralPat     := "()" | BoolLit | IntLit | FloatLit | StringLit | BytesLit ;

TuplePat       := "(" TuplePatItemList? ")" ;
TuplePatItemList
              := TuplePatItem ("," TuplePatItem)* (",")? ;
TuplePatItem   := Pattern | RestPat ;

CtorPat        := PathExpr "(" (Pattern ("," Pattern)*)? (",")? ")" ;

EnumPat        := Ident ("::" Ident)* "::" Ident ("(" (Pattern ("," Pattern)*)? (",")? ")")? ;
StructPat      := Ident "{" (StructPatItem ("," StructPatItem)*)? (",")? "}" ;
StructPatItem  := StructPatField | ".." ;
StructPatField := Ident (":" Pattern)? ;

ArrayPat       := "[" (ArrayPatItem ("," ArrayPatItem)*)? (",")? "]" ;
ArrayPatItem   := Pattern | RestPat ;

RestPat        := ".." Ident? ;
```

Notes:
- `Ident` in a pattern always binds (there are no “constant patterns” for names).
- Parentheses are Rust-like: `(p)` is a parenthesized pattern; a tuple pattern requires a comma and/or a `..` rest marker (e.g. `(p,)`, `(a, b)`, `(..rest)`).
- A zero-field enum variant may be written without parentheses as `Enum::Variant`; this is sugar for `Enum::Variant()`.
- `CtorPat` is resolved by name resolution:
  - if `PathExpr` refers to an enum variant constructor, it is an enum pattern
  - if `PathExpr` refers to a new-type struct, it destructures the single `.0` field.
- Tuple and array patterns may contain `..` at most once; it matches “the rest”:
  - `(a, ..b, c)` binds `b` to the middle slice as a tuple (or `()` if empty).
  - `[a, ..b, c]` binds `b` to the middle slice as an array.
- Struct patterns support `..` only as an ignore marker (it cannot bind): `{x, ..}` is allowed but `{x, ..rest}` is rejected.
  - Without `..`, all fields must be listed exactly once (Rust-like).
  - With `..`, unspecified fields are ignored.
- Struct field shorthand is supported: `{x}` means `{x: x}`.

---

## 4. Runtime Model

### 4.1 Values

Runtime values are:

- `unit`, `bool`, `int`, `float`, `string`, `bytes`
- heap references: structs, enums, arrays, tuples (shared references)
- function values (first-class, referring to a named function)
- continuation tokens (captured by effects)

### 4.2 Heap Objects and Sharing

Structs/enums/arrays/tuples are heap-allocated and passed by reference. Copying a reference
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
- tuple elements
- binary operator operands
- match scrutinee evaluation

This is a **semantic guarantee** (not merely an implementation detail).

### 4.6 Assignment (Lvalues)

Assignment is an expression but yields `unit` (Rust-like).

The left-hand side of `=` must be one of:

- a local name: `x = expr` (only if `x` was declared with `let`, not `const`/`readonly`)
- a struct field: `obj.field = expr`
- a tuple field (or new-type struct field): `t.0 = expr`, `t.1 = expr`, ...
- an array slot: `arr[index] = expr`

Evaluation order is left-to-right:

- `x = expr`: evaluate `expr`, then assign.
- `obj.field = expr`: evaluate `obj`, then evaluate `expr`, then mutate the field.
- `t.<n> = expr`: evaluate `t`, then evaluate `expr`, then mutate the tuple slot.
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

Methods have an **implicit receiver**:

- In an **instance method**, the receiver is in scope as `self` inside the method body.
- The receiver is **not written** in the method’s parameter list. Calls pass the receiver separately:
  - `I::m(recv, args...)` (explicit qualification), or
  - `recv.m(args...)` (method-call sugar, if unambiguous).
- `readonly fn m(...)` declares that `self` is a readonly view inside `m`.
- A non-`readonly` (mutable) method cannot be called on a `readonly T` receiver; a `readonly fn` method can be called on both `T` and `readonly T` receivers.
- `static fn m(...)` declares a receiver-less method, callable only as `Type::m(...)`. Static methods are allowed only in inherent `impl Type { ... }` blocks.
- Interface members may provide a body (`{ ... }`) as a **default method**. An `impl` may omit methods that have defaults.

### 6.1 Interface Methods (non-effect calls)

Interface methods are called using explicit qualification:

```rust
Logger::log(console, "hello");
```

If `Logger` is declared as:

```rust
interface Logger { fn log(msg: string) -> unit; }
```

then the call `Logger::log(console, "hello")` passes:
- `console` as the implicit receiver (`self` inside the implementation),
- `"hello"` as the `msg` parameter.

If the static receiver type is a concrete nominal type (struct/enum), this may be statically
resolved by the compiler based on the selected `impl`.

If the static receiver type is an `interface` type (or an interface-constrained generic), the call
is dynamically dispatched at runtime based on the receiver’s dynamic type.

Method-call sugar is allowed:

```rust
console.log("hello")
```

and is equivalent to `Logger::log(console, "hello")` **only if** the call is
unambiguous (exactly one canonical interface method named `log` is applicable). Otherwise,
the compiler reports an ambiguity error and requires explicit qualification.

> Rationale: this keeps “TypeScript-like ergonomics” while maintaining “Rust-like”
> explicitness where it matters.

### 6.2 Inherent Methods

Inherent methods are defined with `impl Type { ... }` and called as:

```rust
Point::new(1, 2)
point.len()
```

Within an inherent impl:

- `fn m(...) { ... }` declares an instance method with an implicit receiver `self`.
- `readonly fn m(...) { ... }` declares an instance method whose receiver is a readonly view.
- `static fn m(...) { ... }` declares a static method with no receiver.

Resolution rules:

1. Inherent methods on the receiver’s nominal type
2. Interface methods (if unambiguous)
3. Otherwise: error

### 6.3 Overloading

There is no ad-hoc overloading by argument types. A name resolves to at most one
call target after the method resolution rules above are applied.

### 6.4 Interface Inheritance and Upcasts

Interfaces may inherit from one or more super-interfaces:

```rust
interface J: I { fn bar() -> unit; }
interface L: J + K { fn boo() -> unit; }
```

Interfaces may be generic:

```rust
interface Boxed<T> { fn get() -> T; }
```

Interface methods may also be generic:

```rust
interface Pair<T> { fn pair<U>(u: U) -> (T, U); }
```

Method-generic parameters may include interface bounds:

```rust
interface Pair<T> { fn pair<U: Show>(u: U) -> (T, U); }
```

In an interface impl, the method-generic arities and bounds must match the interface method
signature.

Rules:

- An interface’s *full* method set includes all methods declared in its transitive super-interfaces.
- Interface methods are identified by their **origin interface** and method name. In diamond graphs,
  inherited methods with the same origin are treated as a single method.
- In v0.4, multiple inheritance is rejected if two unrelated origins introduce the same method name.
- `impl X for T { ... }` must implement every method in `X`’s full method set (including inherited ones), **except** methods that have a default body in the origin interface.
- `expr as I` performs an explicit upcast to an interface type. Upcasts are the primary mechanism for
  producing interface-typed values without introducing implicit subtyping into inference.

Initial-stage coherence restriction (“no specialization yet”):

- An interface impl header must not select an impl body by choosing concrete interface type arguments.
  In particular, `impl I<int> for S { ... }` is rejected; only “forall instantiations” headers such as
  `impl<T> I<T> for S<T> { ... }` are accepted in this stage.

---

## 7. Effects and Handlers

### 7.1 Effect Calls

An effect call is written:

```rust
@Logger.log("msg")
```

and is distinct from a normal interface method call.

Effect calls may also name an instantiated interface type:

```rust
@Yield<int>.yield(1)
```

If the interface type arguments can be inferred from the operation signature and arguments, they
may be omitted:

```rust
@Yield.yield(1) // infers `Yield<int>`
```

Effect identity is based on:

- the **origin interface** of the method,
- the instantiated interface type arguments, and
- the method name.

For inherited interface methods, the origin interface is used for identity. For example, if
`J<T>: I<T>` and `foo` is declared in `I`, then `@J<int>.foo(...)` is treated as the same effect as
`@I<int>.foo(...)` (origin canonicalization + the same instantiated arguments).

Method-generic effect operations such as `@I.foo<U>(...)` are deferred in the initial stage.

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
  @Logger.log(msg) -> k => { print(msg); k(()) }
  0 => 1
  n => n + 1
}
```

Rules:

- Effect arms are active **only while evaluating the scrutinee expression**.
- Effect arms do not participate in the final value pattern matching.
- Multiple effect arms are tried in source order; the first whose parameter
  patterns match the effect arguments is selected.
- A `match` expression must include at least one value arm.

### 7.3 Continuations and `resume`

When an effect arm handles an effect call, it receives a **captured continuation**
as a value.

Syntax:

- Explicit binder:
  ```rust
  @Interface.method(pats...) -> k => body
  ```
- Omitted binder (backwards compatible): the continuation is bound to the
  predefined name `resume`:
  ```rust
  @Interface.method(pats...) => body   // implicitly: `-> resume`
  ```

The binder name cannot be redefined within the same function.

Calling a continuation uses normal call syntax:

```
k(value_for_effect_call) -> handled_result
```

Where:

- `value_for_effect_call` becomes the return value of the suspended effect call.
- `handled_result` is the final result of evaluating the *handled region*
  (i.e., the entire `match` expression) after resuming: the resumed scrutinee
  evaluation *followed by* value-arm matching.

Continuations are **one-shot**: calling the same continuation more than once
traps (including calling `resume(...)` more than once).

If an effect arm completes without calling its continuation, the effect arm’s
value becomes the result of the whole `match` expression.

The captured continuation value may be stored (e.g. in a struct/array) or
returned from the handler to be resumed later. If it is not stored and becomes
unreachable, it is effectively abandoned and will never run.

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

Rusk has a static type system with global checking at compile time. Most types are erased at
runtime; however, instantiated arity-0 type arguments are reified internally (as `TypeRep`) where
type-precise runtime behavior is required (e.g. `is` / `as?` and effect identity).

### 8.1 Design Goals

- Soundness-first: no implicit `any`.
- TypeScript-like inference where possible (locals and generic arguments).
- Rust-like explicitness at public boundaries (items must have types).
- First-class generics with limited HKTs.

### 8.2 Required Type Annotations

The following must be fully annotated:

- all `fn` item parameters and non-`unit` return types (a `unit` return type may be omitted)
- all `struct` field types
- all `enum` variant field types
- all `interface` method parameter and non-`unit` return types (a `unit` return type may be omitted)

Local `let` bindings may omit types (inferred).

### 8.3 Type Forms

Nominal types:

- primitives: `unit`, `bool`, `int`, `float`, `string`, `bytes`
- arrays: `[T]`
- tuples: `(T1, T2, ...)` (where `()` is `unit`)
- structs: `Name<...>`
- enums: `Name<...>`
- functions: `fn(T1, T2, ...) -> R` (or `fn(T1, T2, ...)` as sugar for `-> unit`)
- continuations: `cont(T) -> R` (values are introduced by effect handlers, but the type is usable in annotations)
- interfaces: `Interface<...>`
- readonly views: `readonly T` (only meaningful when `T` is a reference-like type; it forbids mutation through that reference)

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
- If inference cannot determine type arguments for a generic call, they may be supplied explicitly
  using a Rust-style turbofish: `f::<T>(...)`.
- Lambda parameter types may be inferred from an expected function type; otherwise they
  must be annotated.

Where inference cannot determine a unique type, an explicit annotation is required.

### 8.6 Interfaces as Constraints

One or more `interface` bounds can be used as constraints in a generic parameter list:

```rust
fn log_all<T: Logger + Show>(xs: [T]) -> unit { ... }
```

Rules:

- Bounds use `+` as a separator: `T: I + J + K`.
- Each bound element must resolve to an interface type (possibly instantiated), e.g. `Iterator<int>`.
- Bounds are supported only on arity-0 type parameters in `fn`/method generics (bounds on HKTs and
  on `impl`/`struct`/`enum`/`interface` generics are deferred).

Constraint satisfaction is based on the existence of an `impl` of each bound interface for the
chosen type.

Interface inheritance is respected for constraints: if `J` inherits from `I`, then `T: J` also
satisfies `T: I`.

### 8.7 Effects Typing

For an interface method declared as:

```rust
interface Logger {
  fn log(msg: string) -> unit;
}
```

Then:

- `@Logger.log(e)` has type `unit` and requires `e: string`.
- An effect arm `@Logger.log(pat) -> k => body` (or `@Logger.log(pat) => body` with the
  implicit binder `resume`) is well-typed if the patterns bind values of the declared
  argument types, and if the continuation is called consistently:
  - `k(v)` requires `v: unit` (the declared return type of the effect).

For generic interfaces used as effects, the same rules apply after instantiation. For example:

```rust
interface Yield<T> { fn yield(value: T) -> unit; }
```

- `@Yield<int>.yield(e)` has type `unit` and requires `e: int`.
- `@Yield.yield(e)` is allowed only if `T` can be inferred; otherwise explicit type arguments are
  required.
- `@Yield<int>.yield(...)` and `@Yield<string>.yield(...)` are distinct effect operations at
  runtime.

The type of the overall `match` expression is determined by the value arms; all effect arms must
also unify to the same result type. A `match` expression must include at least one value arm.

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

## 9. Desugarings and Required Core Library Surface

Rusk core syntax includes operators and `for`, but they desugar to ordinary calls
to names in the built-in `core::intrinsics` module.

The compiler **must** emit these calls, and the runtime **must** provide them as
host functions.

### 9.1 Numeric Operators

For `int`:

- `a + b` lowers to `core::intrinsics::int_add(a, b)`
- `a - b` lowers to `core::intrinsics::int_sub(a, b)`
- `a * b` lowers to `core::intrinsics::int_mul(a, b)`
- `a / b` lowers to `core::intrinsics::int_div(a, b)`
- `a % b` lowers to `core::intrinsics::int_mod(a, b)`

Comparisons:

- `a < b` lowers to `core::intrinsics::int_lt(a, b)` (returns `bool`)
- `a <= b` lowers to `core::intrinsics::int_le(a, b)` etc
- `a == b` lowers to `core::intrinsics::int_eq(a, b)` etc (for primitive equality)

Similarly for `float` via `core::intrinsics::float_*`.

Other primitive equality:

- `bool`: `core::intrinsics::bool_eq`, `core::intrinsics::bool_ne`
- `string`: `core::intrinsics::string_eq`, `core::intrinsics::string_ne`
- `bytes`: `core::intrinsics::bytes_eq`, `core::intrinsics::bytes_ne`
- `unit`: `core::intrinsics::unit_eq`, `core::intrinsics::unit_ne`

### 9.2 Boolean Operators

- `!x` lowers to `core::intrinsics::bool_not(x)`
- `a && b` lowers to short-circuiting `if a { b } else { false }`
- `a || b` lowers to short-circuiting `if a { true } else { b }`

### 9.3 Formatted Strings

`f"prefix {e1} middle {e2} suffix"` lowers to:

1. evaluate `e1`, `e2` left-to-right
2. convert each to `string` via `core::intrinsics::to_string`
3. concatenate via `core::intrinsics::string_concat`

### 9.4 `for` Desugaring (Iterator Protocol)

`for x in iter { body }` desugars to:

```rust
let it = core::intrinsics::into_iter(iter);
loop {
  match core::intrinsics::next(it) {
    Option::Some(x) => { body; }
    Option::None => break;
  }
}
```

Required core intrinsics functions and types:

- `struct core::intrinsics::ArrayIter<T> { arr: [T], idx: int }` (iterator state object)
- `core::intrinsics::into_iter<T>([T]) -> core::intrinsics::ArrayIter<T>`
- `core::intrinsics::next<T>(core::intrinsics::ArrayIter<T>) -> Option<T>`
- `enum Option<T> { Some(T), None }`

### 9.5 Panic

Rusk provides a minimal panic surface as an intrinsic host function:

- `core::intrinsics::panic<T>(msg: string) -> T` traps with a panic message.

`core::prelude` is automatically imported into every module and (in v0.4) re-exports `panic`,
so `panic("...")` is available unqualified.

---

### 9.6 Arrays (Library Intrinsics)

Rusk arrays (`[T]`) are heap-allocated, reference-like dynamic arrays. Indexing (`xs[i]`) and
assignment (`xs[i] = v`) are core syntax, but operations that change the array length are provided
as intrinsic host functions in `core::intrinsics`.

Required array intrinsics (v0.4 reference implementation):

- Length:
  - `core::intrinsics::array_len<T>(xs: [T]) -> int`
  - `core::intrinsics::array_len_ro<T>(xs: readonly [T]) -> int`
- Push/pop:
  - `core::intrinsics::array_push<T>(xs: [T], value: T) -> unit`
  - `core::intrinsics::array_pop<T>(xs: [T]) -> Option<T>` (returns `Option::None` if empty)
- Insert/remove:
  - `core::intrinsics::array_insert<T>(xs: [T], idx: int, value: T) -> unit`
  - `core::intrinsics::array_remove<T>(xs: [T], idx: int) -> T`
- Resize/clear/extend:
  - `core::intrinsics::array_clear<T>(xs: [T]) -> unit`
  - `core::intrinsics::array_resize<T>(xs: [T], new_len: int, fill: T) -> unit`
    - if `new_len < len(xs)`: truncates
    - if `new_len > len(xs)`: appends `fill` copies
  - `core::intrinsics::array_extend<T>(xs: [T], other: [T]) -> unit` (shallow element copies)
- Construction from existing arrays:
  - `core::intrinsics::array_concat<T>(a: [T], b: [T]) -> [T]`
  - `core::intrinsics::array_concat_ro<T>(a: readonly [T], b: readonly [T]) -> [readonly T]`
  - `core::intrinsics::array_slice<T>(xs: [T], start: int, end: int) -> [T]`
  - `core::intrinsics::array_slice_ro<T>(xs: readonly [T], start: int, end: int) -> [readonly T]`

Error behavior:

- The mutating intrinsics (`array_push`, `array_pop`, `array_insert`, `array_remove`, `array_clear`,
  `array_resize`, `array_extend`) cannot be called with a `readonly [T]` receiver (type error), and
  also trap at runtime if a readonly reference is somehow passed.
- `array_insert` traps if `idx` is out of bounds (`idx < 0` or `idx > len(xs)`).
- `array_remove` traps if `idx` is out of bounds (`idx < 0` or `idx >= len(xs)`).
- `array_slice`/`array_slice_ro` trap if `start` or `end` is out of bounds (`start < 0`,
  `end < 0`, `end > len(xs)`) or if `start > end`.


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

The CLI tool compiles the program to MIR, installs the required `core::intrinsics::*` host
functions, runs `main`, and treats any trap as a non-zero process exit.
