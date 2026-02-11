## Omit unit in some cases

### Summary

The `unit` type should be able to be omitted in certain contexts to improve code readability and reduce verbosity.
This change should be purely grammar/parser change and should not affect the underlying semantics of the language.
Syntax and SPEC should be updated accordingly.

- `fn` return type may be omitted iff it's `unit`. Below 2 function definitions are equivalent.
    ```
    // Before
    fn foo() -> unit {...}

    // After
    fn foo() {...}

    // It applies to interface method signatures as well
    // Before
    interface I {
        fn bar() -> unit;
    }

    // After
    interface I {
        fn bar();
    }
    ```

- `fn` type signatures return type may be omitted iff it’s `unit`. Below 2 sets are equivalent.
    ```
    // Before
    fn baz(f: fn(int) -> unit) -> int {}

    // After
    fn baz(f: fn(int)) -> int {}
    ```
