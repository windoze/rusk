# Proposal to add 'is' and 'as?' operator

`is` operator checks if an object is of a certain type and returns a boolean value. This check may be done at runtime, or at compile time in some contexts, choosing anyone for convenience.

`as?` operator attempts to cast an object to a specified type and returns `Option<T>`, where `T` is the target type. If the cast is successful, it returns `Some(value)`, otherwise it returns `None`. Similar to `is`, This check may be done at runtime, or at compile time in some contexts, choosing anyone for convenience.

```
interface Animal {}

interface Drawable {}

struct Dog {}

impl Animal for Dog {}
impl Drawable for Dog {}

// Example usage of `is` operator
fn foo(animal: Animal) {
    if animal is Dog {
        // It's a dog ...
    } else {
        // It's not a dog...
    }
}

// Example usage of `as?` operator
fn bar(animal: Animal) {
    match animal as? Dog {
        Some(dog) => {
            // Successfully cast to Dog, proceed with dog-specific logic
        },
        None => {
            // Handle the case where the cast failed
        }
    }
}

// Example usage with for interface types
fn baz(drawable: Drawable) {
    // Using `is` operator
    if drawable is Animal {
        // It's an animal ...
    } else {
        // It's not an animal...
    }

    // Using `as?` operator
    match drawable as? Animal {
        Some(animal) => {
            // Successfully cast to Animal, proceed with animal-specific logic
        },
        None => {
            // Handle the case where the cast failed
        }
    }
}
```

This proposal may introduce new syntax and keywords to the language, so SPEC updates are necessary.