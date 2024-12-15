# Pernix Programming Language

**Pernix** is a system-level programming language designed to provide memory
safety and a robust type system, similar to Rust. Its goal is to enable
developers to write performant and safe code for system-level applications.
While Pernix shares many similarities with Rust, it introduces unique features
that set it apart.

## Key Features

-   **Memory Safety with Borrow Checker**: Pernix incorporates a borrow checker to
    ensure memory safety, preventing common bugs such as null pointer dereferences
    and data races.

-   **Algebraic Data Types (ADT)**: Supports enums and structs for expressive and
    composable data modeling.

-   **Powerful Type System**: Pernix offers a rich and flexible type system,
    inspired by Rust, to help developers write correct and maintainable code.

### Unique Features of Pernix

1. **Specialized Trait Implementations**: Unlike Rust, Pernix allows for the
   specialization of trait implementations, giving developers more control and
   flexibility.

2. **Support for Variadic Generics**: Pernix supports variadic generics through
   tuple unpacking, making it easier to work with functions or types that accept a
   variable number of arguments.

### Planned Features

In future iterations of Pernix, the following features are planned:

-   **Algebraic Effects**: A mechanism for handling side effects (e.g., I/O,
    state, or exceptions) in a structured and reusable way. Algebraic effects also
    simplify complex control flows like asynchronous programming, coroutines,
    and exception handling by treating them as first-class effects. This allows
    developers to write more modular and declarative code while separating the
    definition of effects from their execution.

## Current State

At this stage, Pernix is largely a clone of Rust with some syntax differences
and the aforementioned unique features. The goal is to gradually evolve the
language, experimenting with innovative concepts while maintaining its core
principles of safety and performance.

---

Stay tuned for updates as Pernix continues to grow and differentiate itself from
other system-level programming languages!
