# TODO

## MIR

### Tuple type

Tuple is a lightweight alternative to variadic generic, the latter is too complex to implement and use. And tuple can cover most use cases of variadic generic.

- Tuple type should be added as a first-class type in MIR.
- Tuple type is **not** a primitive type, it is a composite type like struct.
- Tuple should be unified with unit, a tuple without fields is just unit.
- Tuple should fully support type inference and can be used with generic types.
- Tuple should support field access via `get_field` and `set_field` instructions, with fields named as `.0`, `.1`, etc.
- Tuple should support destructuring.
- Question 1: should tuple support dynamic field access via `get_index` and `set_index`? (probably not necessary, leave it out for now)
- Question 2: should tuple be read-only like in Python? Or mutable like any other struct? (better to be readonly, but you can leave it mutable for now if it simplifies implementation)

## Interpreter

### Trailing closure syntax sugar

In function call expression, if the last argument is a closure expression, it can be written outside of the parentheses as a trailing closure, e.g.

```
foo(a, b, {
    // closure body
})
```
can be written as
```
foo(a, b) {
    // closure body
}
```

And if the function call has no other arguments, the parentheses can be omitted entirely, e.g.

```
foo({
    // closure body
})
```
can be written as
```
foo {
    // closure body
}
```

Also we can add extra name binding for the closure when using trailing closure syntax, e.g.

```
fn foo(x, y, closure) {
    // ...
}
// `a` and `b` must match the parameters of `foo`
foo(a=expr1, b=expr2) {
    // can use `a` and `b` here
}
```
is equivalent to
```
let a = expr1;
let b = expr2;
foo(a, b, {
    // can use `a` and `b` here as they're captured by the closure
})
```
This syntax sugar is useful for implementing `with` statements and similar constructs. E.g.

```
with (f=open("file.txt")) {
    // use resource here
}
```

**Above syntax is purely syntactic sugar and does not introduce any new semantics, it is just a more convenient way to write certain function calls.**

### Destructuring

- Add support for variadic patterns (e.g. `..` in tuple and array patterns). Rule: `..` can only appear once in a pattern, some examples of `..` usage in patterns:
  - `(a, ..b)`: `a` matches the first element, `b` matches the rest as a tuple
  - `(..a, b)`: `b` matches the last element, `a` matches the rest as a tuple
  - `(a, .., b)`: `a` matches the first element, `b` matches the last element, the rest is ignored
  - `(a, ..b, c)`: `a` matches the first element, `c` matches the last element, `b` matches the rest as a tuple
  - Similar rules apply to array patterns, but `..` matches as an array
  - `{x, ..}`: `x` matches the field `x`, the rest fields are ignored, `{x, ..b}` or similar are not allowed because struct fields are not ordered
- Function declaration patterns should support destructuring (e.g. `fn foo((x, y): (i32, i32))`, `fn foo({title, ..}: Book)`), as well as variadic patterns above.

### First-class continuations

In `@Interface.handle` matching arm, it should support capturing the continuation as a first-class value, so that the handler can choose to store it for later use.
This is vital feature for implementing advanced control flow features like async/await, generators, etc.

  ```
  match x {
    ...
    @Interface.handler(args...) -> cont => {   // `-> cont` can be omitted, still use a predefined name `resume` in that case so old code still works
        // Now `cont` is the continuation it self, can be called like a normal function, itself is not necessarily a function, just reuse the function call syntax for convenience
        ...
        /// You can store `cont` somewhere for later use
        /// The continuation will **not** be abandoned after this handler returns if you store it without calling it
        /// And you can resume it from anywhere later
        /// Calling the continuation will resume the computation from the point where it was captured, and it will **consume** the continuation, make it invalid after calling (one-shot)
        // someplace.store(cont);  // store the continuation for later invocation
        ...
        /// Or you can call it to resume the continuation immediately
        // cont(args_for_continuation...) // replace the predefined/hardcoded `resume` function
    }
  }
  ```