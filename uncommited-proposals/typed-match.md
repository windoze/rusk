## Typed match

When matching against a value, you can specify the type in the match arms.
Syntax and SPEC should be updated to support typed match arms.

```
interface E { fn boom() -> int; }

match foo() {
    v: int => print("It's an integer: {}", v),
    v: string => print("It's a string: {}", v),
    true => print("It's the boolean true"), // Matching against the literal true can be used at the same time
    @E.boom() => resume(42),    // As well as matching effects
    x: SomeInterface => print("It's an object that implements SomeInterface"), // Matching against a interface type
    x: SomeOtherInterface => print("It's an object that implements SomeOtherInterface"), // Matching against another interface type
    _: SomeStruct{x, y} => print("It's a SomeStruct with x: {} and y: {}", x, y), // Matching against a struct with specific fields
    _: SomeOtherStruct{struct_field1: x, struct_field2: y} => print("It's a SomeOtherStruct with struct_field1: {} and struct_field2: {}", x, y), // Matching against another struct with destructuring
    _ => print("It's something else"),   // Catch-all for any other types
}
```

Generic types in typed match must be fully known at the point of matching.
```
fn foo<T>() {
    match some_value {
        // This should compile
        v: T => print("It's of type T"),
        // This should compile as T is a known type name in foo<T> context
        v: SomeInterface<T> => print("It's of type SomeInterface<T>"),
        // This should not compile unless U is known at this point
        // v: SomeInterface<U> => print("It's an object that implements SomeInterface"),
        // This should not compile as it has an unbounded generic parameter
        // v: SomeOtherInterface<T, _> => print("It's an object that implements SomeOtherInterface"),
        _ => print("It's something else"),
    }
}

```

And matching should consider interface implementation.
```
interface I1 {}

interface I2 {}

struct S; 

impl I1 for S {}
impl I2 for S {}

fn foo() {
    let value= S{};
    match value {
        v: I2 => print("It's an I2"), // This should match because S implements I2
        v: I1 => print("It's an I1"), // This arm may be collapsed during the compilation as the previous arm already matches S
        _ => print("It's something else"),
    }
}

```

The resolution should be done at compile-time.

The matching should be done in following way:
1. Since the types are resolved at compile-time, the compiler should do type checks for each type arm, and filter out all arms that do not match the type of the value being matched. Even in a generic context.
2. For the remaining arms, match from top to bottom, and execute the first arm that matches the value, either a type arm or a value arm.
3. Effect handler matching is done in the other stage and should not interfere with type matching.

