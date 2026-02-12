# Dynamic dispatch and RTTI

The interface should support dynamic dispatch and RTTI (Run-Time Type Information) to allow for more flexible and extensible code.

## Interface inheritance

Interfaces should be able to extend other interfaces, and a value that implements a child interface should be able to be treated as implementing the parent interface as well.

```
interface I1 {}

interface I2: I1 {}

// Multiple inheritance should be supported as well
interface I3: I1, I2 {}

// Implementing I3 should also mean implementing I1 and I2
struct S;

impl I3 for S {}    // This should also mean S implements I1 and I2

// Diamond inheritance should be supported as well
interface I4: I1 {}

interface I5: I3, I4 {}

// Inheritance should be applied to generic interfaces as well
interface IGeneric<T> {}
interface IGenericChild<T>: IGeneric<T> {}
struct SGeneric<T>;
impl IGenericChild<T> for SGeneric<T> {}
fn foo<T>(s: IGeneric<T>) {
    // ...
}

fn main() {
    let sg = SGeneric<int>{};
    foo(sg); // This should work as SGeneric<int> implements IGeneric<int> through IGenericChild<int>
}

// Generic methods in interfaces are forbidden for simplicity (thinking of C++ template, which also forbids template virtual methods)
interface IWithGenericMethod {
    // This should be forbidden
    // fn generic_method<T>() -> T;
}

```


## Dynamic dispatch

Value should be able to be casted to an interface type that it implements, and the methods of the interface can be called on the value through dynamic dispatch.

```
interface I {
    fn foo() -> int;
}

struct S;

impl I for S {
    fn foo() -> int {
        return 42;
    }
}

fn call_foo(i: I) -> int {
    return i.foo(); // This should be dispatched to S's implementation of foo when an S is passed in
}

fn main() {
    let s = S{};
    let i: I = s; // Cast S to I
    let result = call_foo(i); // This should call S's implementation of foo and return 42
    print("Result: {}", result); // Should print "Result: 42"
}
```
