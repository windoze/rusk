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
- Question 1: should tuple support dynamic field access via `get_index` and `set_index`? (probably not necessary)
- Question 2: should tuple be read-only like in Python? Or mutable like any other struct? (probably mutable)

## Interpreter

### Destructuring

- Add support for variadic patterns (e.g. `..` in tuple and array patterns). Rule: `..` can only appear once in a pattern
- Function declaration patterns should support destructuring (e.g. `fn foo((x, y): (i32, i32))`, `fn foo({title, ..}: Book)`),

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
        /// The continuation will **not** be abandoned after this handler returns if you store it
        /// And you can resume it from anywhere later
        // someplace.store(cont);  // store the continuation for later invocation
        ...
        /// Or you can call it to resume the continuation immediately
        // cont(args_for_continuation...) // replace the predefined/hardcoded `resume` function
    }
  }
  ```