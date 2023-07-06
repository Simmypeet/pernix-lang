# Pernix's Generics Support

Pernix programming language supports generics. Generics are a way for the language to generate
various versions of a symbol based on the arguments passed to it at compile time. This is a powerful
feature/technique allowing greater code reuse and performance improvements.

## Approach

Pernix's generics are based on the **trait** system (like Rust or similar to typeclasses found
in Haskell). Pernix contains various components that are used to implement a powerful generics
system.

### Trait and Implements

A `trait` is a declaration containing a set of members (behaviors or properties) that the user must
**implement** in order to use them. The available members are function and `type` declarations.

``` rust
/// Declares a trait `Printer` that allows printing a value of the input.
///
/// The generics parameter `T` represents a type that will be used as the printer object.
public trait Printer<T> {
    /// The type of the input that can be printer.
    type Input;

    /// Prints the `Input` value with this `T` printer.
    print(self: &T, input: Input): void;
}
```

Once the trait is implemented for a particular parameter, the user can then use the trait's
implementation in their code.

``` rust
public struct MyPrinter { /*...*/ }

// Now, the function `Printer:<MyPrinter>::print` is available.
implements Printer<MyPrinter> {
    type Input = i32;

    print(self: &MyPrinter, input: Input /*int32*/): void {
        // ...
    }
}
```

``` rust
/*
 * The implements can also accept a generic parameter.
 *
 * This means `Printer` can accept any generic parameter `T` and this implementation will be used.
 */
implements<T> Printer<T> {
    type Input = i32;

    print(self: &T, input: Input /*int32*/): void {
        // ...
    }
}
```

### Constraints and Where Clause

The trait implementation will not be readily available for use without any additional context. The
`where` clause is used to provide additional information and constraint to the compiler specifying
which trait implementation must exist and is accessible. After the where clause is specified in a
particular scope, the trait's implementation defined in the where clause can be used in that scope.

``` rust
foo(input: Printer<int32>::Input): void 
where:
    Printer<int32> 
{
    let int32Number = 32i32;
    Printer:<int32>::print(&int32Number, input); // OK 

    let float64Number = 32.0f64;
    Printer:<float64>::print(&float64Number, input); // Error: `Printer<float64>` is not available.

    /*
     * Although `Printer<float64>` might be implemented, but the where clause only specifies 
     * `Printer<int32>`.
     */                                               
}

// alternatively
bar<T>(printer: &T, input: Printer<T>::Input): void
where:
    Printer<T>,
    Printer<int32>
{
    Printer:<T>::print(printer, input); // OK

    let int32Number = 32i32;
    Printer:<int32>::print(&int32Number, input); // OK 
}
```

``` rust
public trait Simple<T> {
    sayHello(self: &T): void;
}

implements Simple<int32> {
    sayHello(): void {
        print("Hello from `implements<int32>`");
    }
}

implements Simple<float32> {
    sayHello(): void {
        print("Hello from `implements<float32>`");
    }
}

foo<T>(): void
where:
    Simple<T>
{
    Simple:<T>::sayHello(input);
}

main(): void {
    Simple:<int32>::sayHello(); // "Hello from `implements<int32>`"
    Simple:<float32>::sayHello(); // "Hello from `implements<float32>`"
}
```

### Opaque Type, Substitution, and Unification

The generics system plays a huge role in the type system of Pernix. The type system has special
support/rules specifically for generic type parameters.

**Opaque Type** refers to a type whose its precise type is not yet known at the time of its usage.
This refers to a generic type parameter or a type that has atleast **one** generic type parameter in
its type arguments.

