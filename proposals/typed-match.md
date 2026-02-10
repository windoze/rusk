## Typed match

When matching against a value, you can specify the type in the match arms.

```
match value {
    v: i32 => print("It's an integer: {}", v),
    v: f64 => print("It's a float: {}", v),
    string => print("It's a string"),    // We don't care about the value here, just that it's a string,
    x: SomeInterface => print("It's an object that implements SomeInterface"), // Matching against a interface type
    x: SomeOtherInterface => print("It's an object that implements SomeOtherInterface"), // Matching against another interface type
    SomeStruct{x, y} => print("It's a SomeStruct with x: {} and y: {}", x, y), // Matching against a struct with specific fields
    SomeOtherStruct{struct_field1: x, struct_field2: y} => print("It's a SomeOtherStruct with struct_field1: {} and struct_field2: {}", x, y), // Matching against another struct with destructuring
    _ => print("It's something else"),   // Catch-all for any other types
}
```

If the value matches multiple arms, the first one will be chosen.

Generic types in typed match must be fully bounded at the time of matching.
```
fn foo<T>() {
    match some_value {
        // This should compile
        v: T => print("It's of type T"),
        // This should compile as T is bounded by SomeInterface
        v: SomeInterface<T> => print("It's of type SomeInterface<T>"),
        // This should not compile unless U is a type parameter of the function or is otherwise resolvable
        // v: SomeInterface<U> => print("It's an object that implements SomeInterface"),
        // This should not compile as it has an unbounded generic parameter
        // v: SomeOtherInterface<T, _> => print("It's an object that implements SomeOtherInterface"),
        _ => print("It's something else"),
    }
}

```

And matching should consider inheritance and interface implementation.
```
interface I1 {}

interface I2: I1 {}

struct S: I2 {}

fn foo() {
    let value: I1 = S{};
    match value {
        v: I2 => print("It's an I2"), // This should match because S implements I2 which extends I1
        v: I1 => print("It's an I1"), // This should not be reached
        _ => print("It's something else"),
    }
}

```

The resolution can be done at runtime for simplicity for now, future plan is TBD.

Questions:
What if there is an tuple arm that contains `..`? How do we check types in that case?
