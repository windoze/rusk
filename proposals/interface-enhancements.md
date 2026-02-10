## Interface enhancements

### Default Methods

Interface may contain default methods, which are methods with a default implementation. This allows interfaces to provide common functionality without forcing all implementing classes to override the method.

Default methods have a body and can be called on instances of the implementing class. Implementations can choose to override the default method to provide a specific behavior, or they can use the default implementation provided by the interface.

Methods with default implementations should not be used as effects, because effects handlers are always "implemented" in `match` effect handler arms, which is conflict with default method semantics.


### Properties

Interfaces can have properties, which are "pseudo" variables that can be defined in the interface. These properties are abstract and can have getters and setters. Both getters and setters can have default implementations, allowing implementing classes to use the default behavior or override it as needed.
```
interface MyInterface {
    // Readonly property without a default getter, must be implemented by the class
    readonly_property: string {
        get
    }
    // Readonly property with a default getter, implementing class can choose to override the default getter or use it as is, setter is not allowed to be defined for readonly properties
    readonly_property_with_default_getter: string {
        get => { "Default Value" }
    }
    // Writeonly property without a default getter, must be implemented by the class
    writeonly_property: string {
        set
    }
    // Writeonly property with a default setter, implementing class can choose to override the default setter or use it as is, getter is not allowed to be defined for writeonly properties
    writeonly_property_with_default_setter: string {
        set(value) => { /* Default setter implementation */ }
    }
    // Read-write property, getter and setter must be implemented by the class
    read_write_property: string {
        get,
        set
    }
    // Read-write property with a default getter, setting must be implemented by the class, and can choose to override the default getter
    read_write_property_with_default_getter: string {
        get => { "Default Value" }
        set
    }
    // Read-write property with default getter and sett, both can be overridden by the implementing class
    read_write_property_with_default_getter_and_setter: string {
        get => { "Default Value" }
        set(value) => { /* Default setter implementation */ }
    }
}
```

The compiler should generate the method names for the getters and setters of the properties in a **consistent way**, that is, for a specific property in a specific interface, the getter and setter method names should always remain unchanged regardless of where they are accessed in the codebase. This consistency allows for predictable behavior when accessing properties and triggering effects.

Property accessing is purely syntax sugar, it will be desugared to calls to generated methods mentioned above.

Property accessing desugaring should also happen in effect triggering:
```
interface MyInterface {
    my_property: string {
        get,
        set
    }
}

fn foo() {
    // This will trigger the effect `MyInterface.get_my_property()`, `get_my_property` is an imaginary method name as an example here, the actual method name will depend on the compiler, as mentioned above.
    let c = @MyInterface.my_property;

    // This will trigger the effect `MyInterface.set_my_property("New Value")`, `set_my_property` is an imaginary method name as an example here, the actual method name will depend on the compiler, as mentioned above.
    @MyInterface.my_property = "New Value";
}

```
Property setter or getter with default implementation should not be used as effects, for the same reason as default methods mentioned above.


Name resolution for properties should also be consistent with method name resolution. Explicit type cast is required when there are multiple candidates, for example:
```
struct A {
    v: int
}

interface I {
    v: int {
        get,
        set
    }
}

impl I for A {
    v {
        get => self.v * 2
        set(value) => self.v = value / 2
    }
}

fn foo() {
    let a = A { v: 10 };
    let i = a as I;

    // This will access A::v, which is the struct field, value is 10
    let x = a.v;

    // This will call the getter of `I::v`, which is implemented by `A`, getter returns value 20
    let y = i.v;

    // This will set A::v to 30
    a.v = 30;

    // This will call the setter of `I::v`, which is implemented by `A`, setter will set A::v to 20
    i.v = 40;
}
```