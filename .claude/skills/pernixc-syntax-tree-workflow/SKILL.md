---
name: pernixc-syntax-tree-workflow
description: Workflow for creating and updating syntax trees in the `pernixc_syntax` crate, including the `abstract_tree` macro pattern and the arbitrary module for property-based testing.
---

# Syntax Tree Workflow in `pernixc_syntax`

This document describes the workflow for creating and updating syntax trees in the `pernixc_syntax` crate, including the `abstract_tree` macro pattern and the arbitrary module for property-based testing.

## Overview

The `pernixc_syntax` crate defines the Abstract Syntax Tree (AST) for the Pernix programming language. It consists of two main components:

1. **Syntax Tree Definitions** (`lib.rs` and related modules) - Define the AST structure using the `abstract_tree!` macro
2. **Arbitrary Implementations** (`arbitrary.rs` modules) - Define reference syntax trees for property-based testing

These two components work together to ensure that:

- The parser correctly parses source code into AST nodes
- The arbitrary generators create valid syntax trees
- Property-based tests verify round-trip correctness (generate → display → parse → compare)

---

## Abstract Tree Macro

The `abstract_tree!` macro from `pernixc_parser` generates parser implementations and AST node structures. It supports both `struct` and `enum` syntax trees.

### Struct Syntax Trees

Structs represent sequential parsing of multiple components.

#### Basic Structure

```rust
abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct StructName {
        pub field_name: FieldType = parser_expression,
        pub optional_field: OptionalType = parser_expression.optional(),
        pub multi_field: #[multi] MultiType = parser_expression.repeat()
    }
}
```

#### Field Attributes

- **`#[multi]`** - Field accepts multiple values (becomes an iterator)
- No attribute - Field is required (single value)
- `.optional()` - Field is optional (becomes `Option<T>`)

#### Parser Expressions

Common parser expressions used in field definitions:

1. **Character/Punctuation Literals**

    ```rust
    pub first_colon = ':'
    pub second_colon = ':'.no_prior_insignificant()  // No whitespace allowed before
    ```

2. **Keyword Expectations**

    ```rust
    pub public_keyword: Keyword = expect::Keyword::Public
    pub identifier: Identifier = expect::Identifier
    ```

3. **Recursive AST Parsing**

    ```rust
    pub lifetime: Lifetime = ast::<Lifetime>()
    pub ty: Type = ast::<Type>()
    ```

4. **Fragment Parsing** (for delimited content)

    ```rust
    pub generic_args: GenericArguments = ast_always_step_into_fragment::<GenericArguments>().optional()
    ```

5. **Repetition**
    ```rust
    pub arguments: #[multi] Argument = ast::<Argument>().repeat_all_with_separator(',')
    pub items: #[multi] Item = ast::<Item>().repeat()
    pub commits: #[multi] Subsequent = ast::<Subsequent>().repeat_with_commit(ast::<Separator>())
    ```

#### Example: Elided Syntax

```rust
abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        StableHash,
        Encode,
        Decode,
    )]
    pub struct Elided {
        pub first_dot = '.',
        pub second_dot = '.'.no_prior_insignificant()
    }
}
```

This parses the `..` token as two consecutive dots with no whitespace between them.

#### Example: Lifetime Syntax

```rust
abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        StableHash,
        Encode,
        Decode,
    )]
    pub struct Lifetime {
        pub apostrophe: Punctuation = '\'',
        pub identifier: LifetimeIdentifier = ast::<LifetimeIdentifier>(),
    }
}
```

This parses `'identifier` or `'static` or `'..` as a lifetime.

### Enum Syntax Trees

Enums represent alternative parsing choices.

#### Basic Structure

```rust
abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum EnumName {
        VariantOne(Type = parser_expression),
        VariantTwo(Type = parser_expression),
        VariantThree(Type = parser_expression)
    }
}
```

#### Example: Access Modifier

```rust
abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        EnumAsInner,
        Encode,
        Decode,
        StableHash
    )]
    pub enum AccessModifier {
        Public(Keyword = expect::Keyword::Public),
        Private(Keyword = expect::Keyword::Private),
        Internal(Keyword = expect::Keyword::Internal)
    }
}
```

The parser tries each variant in order and returns the first successful match.

#### Example: Generic Argument

```rust
abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum GenericArgument {
        Lifetime(Lifetime = ast::<Lifetime>()),
        Elided(Elided = ast::<Elided>()),
        InstanceValue(InstanceValue = ast::<InstanceValue>()),
        Type(Type = ast::<Type>()),
        Constant(ConstantArgument = ast::<ConstantArgument>()),
    }
}
```

**Order matters!** Variants are tried sequentially. Put more specific variants before more general ones.

### Fragment Attributes

For delimited content (brackets, braces, parentheses):

```rust
abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
    pub struct GenericArguments {
        pub arguments: #[multi] GenericArgument
            = ast::<GenericArgument>().repeat_all_with_separator(',')
    }
}
```

This parses `[arg1, arg2, arg3]` where the content inside brackets is a comma-separated list.

---

## Arbitrary Module

The arbitrary module creates reference syntax trees for property-based testing. These are simplified versions of the AST that can be easily generated, displayed, and compared.

### Reference Macro

The `reference!` macro creates a parallel structure to the syntax tree:

```rust
reference! {
    #[derive(Debug, Clone)]
    pub struct StructName for super::StructName {
        pub field_name (FieldType),
        pub optional_field (Option<FieldType>),
        pub multi_field (Vec<FieldType>)
    }
}
```

Or for enums:

```rust
reference! {
    #[derive(Debug, Clone)]
    pub enum EnumName for super::EnumName {
        VariantOne(Type),
        VariantTwo(Type),
        VariantThree
    }
}
```

### Field Verification Attributes

The reference macro supports verification attributes to customize how fields are compared:

#### `map_input_assert`

Used when the reference type differs from the syntax tree type:

```rust
reference! {
    pub enum LifetimeIdentifier for super::LifetimeIdentifier {
        #{map_input_assert(Identifier, &Identifier.kind)}
        Identifier(kind::Identifier),

        Static,
        Elided(Elided),
    }
}
```

This maps the reference `Identifier` to `Identifier.kind` when comparing with the parsed output.

#### `prop_assert`

Used for custom assertions on the parsed output:

```rust
reference! {
    pub enum AccessModifier for super::AccessModifier {
        #[display("public")]
        #{prop_assert(|output| output.kind == expect::Keyword::Public)}
        Public,

        #[display("private")]
        #{prop_assert(|output| output.kind == expect::Keyword::Private)}
        Private,
    }
}
```

This verifies that the parsed keyword has the correct kind.

### Arbitrary Trait Implementation

After defining the reference structure, implement the `Arbitrary` trait from proptest:

#### For Simple Structs

```rust
impl Arbitrary for Elided {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Just(Self {}).boxed()
    }
}
```

#### For Structs with Fields

```rust
impl Arbitrary for Label {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        kind::Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}
```

#### For Enums

```rust
impl Arbitrary for AccessModifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Public),
            Just(Self::Private),
            Just(Self::Internal),
        ]
        .boxed()
    }
}
```

Use `prop_oneof!` to randomly choose between variants.

#### For Enums with Variants

```rust
impl Arbitrary for LifetimeIdentifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            kind::Identifier::arbitrary().prop_map(Self::Identifier),
            Just(Self::Static),
            Elided::arbitrary().prop_map(Self::Elided),
        ]
        .boxed()
    }
}
```

#### With Parameters for Recursive Types

For types that have circular dependencies or need to limit depth:

```rust
impl Arbitrary for GenericArgument {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((ty, qual, expr): Self::Parameters) -> Self::Strategy {
        let qual = qual.unwrap_or_else(|| {
            QualifiedIdentifier::arbitrary_with((ty.clone(), expr.clone()))
        });

        let ty = ty.unwrap_or_else(|| {
            Type::arbitrary_with((expr.clone(), Some(qual.clone())))
        });

        prop_oneof![
            Lifetime::arbitrary().prop_map(Self::Lifetime),
            ty.prop_map(|x| {
                match x {
                    Type::QualifiedIdentifier(qualified_identifier) => {
                        Self::InstanceValue(InstanceValue {
                            higher_ranked_lifetimes: None,
                            qualified_identifier,
                        })
                    }
                    Type::Elided(elided) => Self::Elided(elided),
                    x => Self::Type(x),
                }
            }),
            ConstantArgument::arbitrary_with(expr).prop_map(Self::Constant),
        ]
        .boxed()
    }
}
```

**Special handling**: When a type can appear in multiple forms (e.g., `Type::Elided` should become `GenericArgument::Elided`), use pattern matching in the `prop_map` closure to transform it appropriately.

### IndentDisplay Trait

Implement `IndentDisplay` to convert the reference structure back to source code:

```rust
impl IndentDisplay for GenericArgument {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Lifetime(i) => i.fmt(f),
            Self::Elided(i) => i.fmt(f),
            Self::Type(i) => i.indent_fmt(f, indent),
            Self::Constant(i) => i.indent_fmt(f, indent),
            Self::InstanceValue(i) => i.indent_fmt(f, indent),
        }
    }
}
```

For simple display:

```rust
impl IndentDisplay for Elided {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        _indent: usize,
    ) -> std::fmt::Result {
        write!(f, "..")
    }
}
```

Or use the `#[display]` attribute on the reference struct:

```rust
reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    #[display("..")]
    pub struct Elided for super::Elided {}
}
```

---

## Complete Workflow

### 1. Adding a New Syntax Node

Let's add a new `Elided` variant to `GenericArgument` as an example.

#### Step 1: Define Syntax Tree (lib.rs)

```rust
// First, ensure the base type exists
abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        StableHash,
        Encode,
        Decode,
    )]
    pub struct Elided {
        pub first_dot = '.',
        pub second_dot = '.'.no_prior_insignificant()
    }
}

// Then add it to the enum where needed
abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum GenericArgument {
        Lifetime(Lifetime = ast::<Lifetime>()),
        Elided(Elided = ast::<Elided>()),  // ← New variant
        InstanceValue(InstanceValue = ast::<InstanceValue>()),
        Type(Type = ast::<Type>()),
        Constant(ConstantArgument = ast::<ConstantArgument>()),
    }
}
```

**Important**: Order matters in enums! Put more specific variants before general ones.

#### Step 2: Define Reference Structure (arbitrary.rs)

```rust
reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    #[display("..")]
    pub struct Elided for super::Elided {}
}

reference! {
    #[derive(Debug, Clone)]
    pub enum GenericArgument for super::GenericArgument {
        Lifetime(Lifetime),
        Elided(Elided),  // ← New variant
        InstanceValue(InstanceValue),
        Type(Type),
        Constant(ConstantArgument),
    }
}
```

#### Step 3: Implement Arbitrary for Base Type

```rust
impl Arbitrary for Elided {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Just(Self {}).boxed()
    }
}
```

#### Step 4: Update Arbitrary for Container Type

```rust
impl Arbitrary for GenericArgument {
    type Parameters = (
        Option<BoxedStrategy<Type>>,
        Option<BoxedStrategy<QualifiedIdentifier>>,
        Option<BoxedStrategy<Expression>>,
    );
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((ty, qual, expr): Self::Parameters) -> Self::Strategy {
        // ... setup code ...

        prop_oneof![
            Lifetime::arbitrary().prop_map(Self::Lifetime),
            ty.prop_map(|x| {
                match x {
                    Type::QualifiedIdentifier(qualified_identifier) => {
                        Self::InstanceValue(InstanceValue {
                            higher_ranked_lifetimes: None,
                            qualified_identifier,
                        })
                    }
                    Type::Elided(elided) => Self::Elided(elided),  // ← Transform Type::Elided
                    x => Self::Type(x),
                }
            }),
            ConstantArgument::arbitrary_with(expr).prop_map(Self::Constant),
        ]
        .boxed()
    }
}
```

**Key transformation**: When `Type` can be `Type::Elided`, transform it to `GenericArgument::Elided` instead of `GenericArgument::Type(Type::Elided)`.

#### Step 5: Update IndentDisplay

```rust
impl IndentDisplay for GenericArgument {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Lifetime(i) => i.fmt(f),
            Self::Elided(i) => i.fmt(f),  // ← New case
            Self::Type(i) => i.indent_fmt(f, indent),
            Self::Constant(i) => i.indent_fmt(f, indent),
            Self::InstanceValue(i) => i.indent_fmt(f, indent),
        }
    }
}
```

#### Step 6: Update Downstream Code

Any code that matches on the syntax tree needs to handle the new variant:

```rust
// In resolution or other semantic analysis crates
match generic_argument {
    GenericArgument::Lifetime(lt) => { /* ... */ }
    GenericArgument::Elided(elided) => {
        // Handle elided generic argument
        self.create_elided_type().unwrap_or_else(|| {
            self.receive_diagnostic(Diagnostic::UnexpectedInference(
                UnexpectedInference {
                    unexpected_span: elided.span(),
                    generic_kind: GenericKind::Type,
                },
            ));
            Type::Error(Error)
        })
    }
    GenericArgument::Type(ty) => { /* ... */ }
    // ... other cases ...
}
```

### 2. Modifying an Existing Syntax Node

When modifying an existing node:

1. Update the `abstract_tree!` definition
2. Update the `reference!` definition
3. Update the `Arbitrary` implementation
4. Update the `IndentDisplay` implementation
5. Fix any compilation errors in downstream code
6. Run tests to ensure property-based tests pass

---

## Best Practices

### 1. Variant Ordering in Enums

**Rule**: More specific variants should come before more general ones.

```rust
// ❌ Bad: Type is too general and will match before InstanceValue
pub enum GenericArgument {
    Type(Type = ast::<Type>()),              // Matches QualifiedIdentifier
    InstanceValue(InstanceValue = ast::<InstanceValue>()),  // Never reached!
}

// ✅ Good: Specific variants first
pub enum GenericArgument {
    Lifetime(Lifetime = ast::<Lifetime>()),
    Elided(Elided = ast::<Elided>()),
    InstanceValue(InstanceValue = ast::<InstanceValue>()),
    Type(Type = ast::<Type>()),
    Constant(ConstantArgument = ast::<ConstantArgument>()),
}
```

### 2. Derive Macros

Always include these derives for syntax trees:

- **Required for all**: `Debug`, `Clone`, `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash`
- **For serialization**: `StableHash`, `Encode`, `Decode`
- **For enums**: `EnumAsInner` (provides `.as_*()` and `.into_*()` methods)

### 3. Fragment vs Non-Fragment

Use fragment attribute for delimited content:

```rust
// For [T1, T2, T3] syntax
#{fragment = expect::Fragment::Delimited(DelimiterKind::Bracket)}
pub struct GenericArguments { ... }

// For {expr} syntax
#{fragment = expect::Fragment::Delimited(DelimiterKind::Brace)}
pub enum ConstantArgument { ... }

// For (params) syntax
#{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
pub struct FunctionParameters { ... }
```

### 4. Arbitrary Parameters

For recursive types, use parameters to control depth:

```rust
type Parameters = (
    Option<BoxedStrategy<Type>>,      // Depth-limited Type strategy
    Option<BoxedStrategy<Expression>>, // Depth-limited Expression strategy
);
```

This prevents infinite recursion in property-based testing.

### 5. Type Transformations in Arbitrary

When a type can appear in multiple forms, transform it in the arbitrary implementation:

```rust
ty.prop_map(|x| {
    match x {
        // Transform specific variants
        Type::QualifiedIdentifier(qi) => Self::InstanceValue(InstanceValue {
            higher_ranked_lifetimes: None,
            qualified_identifier: qi,
        }),
        Type::Elided(elided) => Self::Elided(elided),
        // Default case
        x => Self::Type(x),
    }
})
```

### 6. Testing

After making changes:

1. **Compilation**: `cargo check -p pernixc_syntax`
2. **Unit tests**: `cargo test -p pernixc_syntax`
3. **Property-based tests**: Tests automatically verify round-trip correctness
4. **Downstream**: `cargo check --workspace` to ensure no breakage

### 7. Documentation

Add doc comments for complex syntax nodes:

````rust
abstract_tree::abstract_tree! {
    /// Represents an instance value in generic arguments.
    ///
    /// This syntax tree is used for both type arguments (when
    /// `higher_ranked_lifetimes` is `None`) and instance arguments.
    ///
    /// # Example
    /// ```pnx
    /// Foo[SomeType]              // higher_ranked_lifetimes = None
    /// Foo[for['a] SomeInstance]  // higher_ranked_lifetimes = Some(...)
    /// ```
    pub struct InstanceValue {
        pub higher_ranked_lifetimes: HigherRankedLifetimes = ast::<HigherRankedLifetimes>().optional(),
        pub qualified_identifier: QualifiedIdentifier = ast::<QualifiedIdentifier>(),
    }
}
````

---

## Summary

The workflow for working with syntax trees in `pernixc_syntax` involves:

1. **Define** the syntax tree using `abstract_tree!` macro
2. **Create** a reference structure using `reference!` macro
3. **Implement** `Arbitrary` trait for property-based testing
4. **Implement** `IndentDisplay` trait for displaying generated trees
5. **Test** to ensure parser and generator work correctly
6. **Update** downstream code to handle new variants

This dual-structure approach ensures that:

- The parser correctly handles all valid syntax
- Generated test cases are valid and diverse
- Round-trip testing (generate → display → parse → compare) verifies correctness
- Refactoring is safe because tests catch incompatibilities
