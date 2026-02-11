## Generic bounds

Generic type parameters can have type bounds to restrict the types that can be used. Bounds can be combined using `+` to require multiple interfaces.

```
fn process_value<T: SomeInterface + AnotherInterface>(value: T) {
    // Function body that can use methods from SomeInterface and AnotherInterface
}

struct Container<T: SomeInterface> {
    item: T,
}

interface AnotherInterface<T: SomeInterface> {
    fn do_something(&self, item: T);
}

```

Bounds should be checked at compile-time to ensure type safety and correctness, and at runtime if necessary for dynamic type checks.

Bound checks should support interface inheritance, allowing a type to satisfy bounds through inherited interfaces.

```
interface BaseInterface {
    fn base_method(&self);
}

interface DerivedInterface: BaseInterface {
    // ...
}

fn use_base<T: BaseInterface>(item: T) {
    item.base_method();
}

fn foo () {
    let obj: DerivedInterface = ...;
    use_base(obj); // Valid since DerivedInterface inherits from BaseInterface
}
```
