## Omit unit in some cases

### Summary

The `unit` type should be able to be omitted in certain contexts to improve code readability and reduce verbosity.
This change should be purely syntactic and should not affect the underlying semantics of the language.

- Clarify that `unit` is equivalent to an empty tuple `()` whenever there is no ambiguity. An extra pair of parentheses may be required in some cases to avoid ambiguity.

- `fn` return type when it is `unit`, should be able to be omitted.
    ```
    // Before
    fn foo() -> unit {}

    // After
    fn foo() {}
    ```

- `enum` variant with `unit` type should be able to be defined and used without explicitly specifying `unit`.
    ```
    // Before
    enum MyEnum {
        A(unit),
        B(i32),
    }

    fn foo(e: MyEnum) {
        match e {
            MyEnum::A(unit) => { /* ... */ },
            MyEnum::B(val) => { /* ... */ },
        }
    }

    // After
    enum MyEnum {
        A,
        B(i32),
    }
    fn foo(e: MyEnum) {
        match e {
            MyEnum::A => { /* ... */ },
            MyEnum::B(val) => { /* ... */ },
        }
    }
    ```


- `fn` type signatures with `unit` parameters should allow omission of `unit`.
    ```
    // Before
    type MyFn = fn(unit) -> i32;
    fn bar(f: fn(unit) -> i32) -> i32 {}

    // After
    type MyFn = fn() -> i32;
    fn bar(f: fn() -> i32) -> i32 {}
    ```

- `fn` type signatures with `unit` return type should allow omission of `unit`.
    ```
    // Before
    type MyFn = fn(i32) -> unit;
    fn baz(f: fn(i32) -> unit) -> i32 {}

    // After
    type MyFn = fn(i32);
    fn baz(f: fn(i32)) -> i32 {}
    ```

- Return unit should be able to be omitted.
    ```
    // Before
    fn qux() -> unit {
        return unit;
    }

    // After
    fn qux() {
        return;
    }
    // Or
    fn qux() {
        // implicit return of unit
    }
    ```

    ```
    // Before
    let a = {
        // some code
        let x = 1;
        return unit;
    };
    // After
    let a = {
        let x =1;
        return;
    };
    // Or
    let a = {
        let x =1;
    }
    ```