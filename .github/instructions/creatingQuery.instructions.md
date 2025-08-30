---
applyTo: "**/*.rs"
---

# Creating Queries for the Pernix Compiler

This guide provides step-by-step instructions for creating new queries in the
Pernix compiler's incremental query system. The query system powers incremental
compilation and semantic analysis by caching computed results and invalidating
them only when necessary.

## What is the Query System?

The Pernix query system is an incremental computation framework that
automatically manages dependencies and caching. When you ask for information
(like "what is the type of this variable?"), the system:

1. **Computes the answer** if it hasn't been calculated before
2. **Caches the result** for future use
3. **Tracks dependencies** to know when to invalidate cached results
4. **Reuses cached results** when nothing has changed

### Core Concepts

-   **Query**: A request for computed information (e.g., "get the name of
    symbol X")
-   **Key**: A unique identifier for what you want to compute
-   **Value**: The result returned by the query
-   **Executor**: The function that performs the actual computation
-   **Engine**: The central system that manages caching and dependencies

## Quick Start: Your First Query

Let's start with a simple example. Suppose you want to create a query that
formats symbol names in different styles:

```rust
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_target::Global;
use crate::ID;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FormatStyle {
    CamelCase,
    SnakeCase,
}

// This single macro creates everything you need!
#[pernixc_query::query(
    key(FormatNameKey),               // Generated key struct name
    id((Global<ID>, FormatStyle)),    // What uniquely identifies this query
    value(String),                    // What the query returns
    executor(FormatNameExecutor),     // Generated executor name
    extend(method(get_formatted_name), no_cyclic)  // Generated helper method
)]
pub async fn format_name_executor(
    (id, style): (Global<ID>, FormatStyle),
    engine: &TrackedEngine,
) -> Result<String, CyclicError> {
    // Your computation logic
    let name = engine.get_name(id).await;

    let formatted = match style {
        FormatStyle::CamelCase => to_camel_case(&name),
        FormatStyle::SnakeCase => to_snake_case(&name),
    };

    Ok(formatted)
}

// Helper functions
fn to_camel_case(s: &str) -> String { s.to_string() }
fn to_snake_case(s: &str) -> String { s.to_lowercase() }
```

That's it! This single macro automatically generates:

-   A `FormatNameKey` struct that identifies your query
-   A `FormatNameExecutor` that runs your computation
-   A `get_formatted_name` method you can call like:
    ```rust
    let formatted = engine.get_formatted_name(symbol_id, FormatStyle::CamelCase).await;
    ```

## The Declarative Approach (Recommended)

**🎯 We strongly recommend using the `#[pernixc_query::query]` macro for all**
**new queries.** It provides a declarative way to create queries and
automatically handles all the boilerplate.

### Why Use the Declarative Macro?

1. **Less Verbose**: Write only the logic, not the infrastructure
2. **Better Ergonomics**: Clean, readable syntax that focuses on what you want
   to compute
3. **Consistency**: All queries follow the same patterns automatically
4. **Less Noise**: No repetitive trait implementations or struct definitions
5. **Built-in Features**: Automatic extension methods and error handling

### When You Might Need Manual Implementation

The declarative macro covers 90% of use cases. You only need manual
implementation for:

-   Complex custom key structures that don't fit the standard pattern
-   Specialized executor logic that can't be expressed in a simple function
-   Legacy code that requires specific trait implementations

## Understanding Query Components

### Keys: What to Compute

A key uniquely identifies what you want to compute. It should contain all the
information needed to determine the result:

```rust
// Good: Contains everything needed to identify the computation
#[pernixc_query::query(
    key(TypeOfExpressionKey),
    id((Global<ExpressionID>, Global<ContextID>)),  // Expression + context
    value(TypeInfo),
    executor(TypeOfExpressionExecutor)
)]

// Bad: Missing context information
#[pernixc_query::query(
    key(BadTypeKey),
    id(Global<ExpressionID>),  // Missing context - same expression might have different types
    value(TypeInfo),
    executor(BadTypeExecutor)
)]
```

### Values: What You Return

Choose value types that are cheap to clone, since the query system will clone
them frequently:

```rust
// ❌ Expensive to clone
#[pernixc_query::query(/* ... */, value(Vec<String>))]

// ✅ Cheap to clone
#[pernixc_query::query(/* ... */, value(Arc<[SharedStr]>))]
```

**Performance tip**: Use `Arc<T>` for large data, `SharedStr` for strings,
and `Arc<[T]>` for arrays.

### Executors: How to Compute

Your executor function contains the computation logic. It can query other
information using the engine:

```rust
#[pernixc_query::query(/* ... */)]
pub async fn my_executor(
    id: MyIdType,
    engine: &TrackedEngine,
) -> Result<MyValueType, CyclicError> {
    // Query dependencies
    let dependency1 = engine.query(&SomeOtherKey::new(id.related_id)).await?;
    let dependency2 = engine.get_some_data(id.another_field).await;

    // Perform computation
    let result = compute_something(dependency1, dependency2);

    Ok(result)
}
```

## Handling Cyclic Dependencies

Sometimes queries can depend on each other in cycles (A depends on B, B depends
on A). The query system detects these and needs to know what to return in such
cases.

### The Problem

```rust
// This could create a cycle:
// TypeOfVariable(x) -> TypeOfFunction(f) -> TypeOfVariable(x)
```

### The Solution: SCC Values

**By default, queries will panic if cycles are detected.** For queries that
might be involved in cycles, provide a sensible default value:

```rust
#[derive(/* ... */, Key)]
#[value(Option<TypeInfo>)]
#[scc_value(None)]  // Return None when cycles are detected
pub struct TypeResolutionKey { /* ... */ }

#[derive(/* ... */, Key)]
#[value(HashSet<ID>)]
#[scc_value(HashSet::default())]  // Return empty set when cycles are detected
pub struct DependenciesKey { /* ... */ }
```

### The `no_cyclic` Attribute

For simple queries that you know will never be part of cycles, use `no_cyclic`
to improve ergonomics:

```rust
#[pernixc_query::query(
    key(NameKey),
    id(Global<ID>),
    value(SharedStr),
    executor(NameExecutor),
    extend(method(get_name), no_cyclic)  // ← This unwraps errors automatically
)]
```

**Safe for `no_cyclic`**: Simple data retrieval (names, spans), syntax tree
reading

**Unsafe for `no_cyclic`**: Complex semantic analysis, type resolution

## Registration and Integration

After creating your query, you need to register it with the compiler:

```rust
// In your library's lib.rs
pub fn register_executors(
    executor: &mut pernixc_query::runtime::executor::Registry,
) {
    executor.register(Arc::new(YourExecutor));
}

pub fn register_serde<S, D, Registry>(serde_registry: &mut Registry)
where
    S: Serializer<Registry>,
    D: Deserializer<Registry>,
    Registry: DynamicRegistry<S, D> + Send + Sync,
    S::Error: Send + Sync,
{
    serde_registry.register::<YourQueryKey>();
}

// In the main compiler
your_crate::register_executors(&mut engine.runtime.executor_registry);
your_crate::register_serde(&mut engine.runtime.serde_registry);
```

## Common Query Patterns

### 1. Simple Data Retrieval

```rust
#[pernixc_query::query(
    key(SpanKey),
    id(Global<ID>),
    value(Option<RelativeSpan>),
    executor(SpanExecutor),
    extend(method(get_span), no_cyclic)
)]
pub async fn span_executor(
    id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<Option<RelativeSpan>, CyclicError> {
    let table = engine.get_table_of_symbol(id).await;
    Ok(table.spans.get(&id.id).copied().flatten())
}
```

### 2. Queries with Dependencies

```rust
#[pernixc_query::query(
    key(QualifiedNameKey),
    id(Global<ID>),
    value(String),
    executor(QualifiedNameExecutor),
    extend(method(get_qualified_name), no_cyclic)
)]
pub async fn qualified_name_executor(
    mut id: Global<ID>,
    engine: &TrackedEngine,
) -> Result<String, CyclicError> {
    let mut parts = Vec::new();

    loop {
        let name = engine.get_name(id).await;
        parts.push(name);

        if let Some(parent_id) = engine.get_parent(id).await {
            id = Global::new(id.target_id, parent_id);
        } else {
            break;
        }
    }

    parts.reverse();
    Ok(parts.join("::"))
}
```

### 3. Batch Queries

```rust
#[pernixc_query::query(
    key(AllFunctionsKey),
    id(TargetID),
    value(Arc<[Global<ID>]>),
    executor(AllFunctionsExecutor)
)]
pub async fn all_functions_executor(
    target_id: TargetID,
    engine: &TrackedEngine,
) -> Result<Arc<[Global<ID>]>, CyclicError> {
    let symbols = engine.query(&AllSymbolsKey(target_id)).await?;

    let mut functions = Vec::new();
    for &symbol_id in symbols.iter() {
        let global_id = Global::new(target_id, symbol_id);
        if engine.get_kind(global_id).await == Kind::Function {
            functions.push(global_id);
        }
    }

    Ok(functions.into())
}
```

### 4. Queries with Diagnostics

```rust
#[derive(Debug, Serialize, Deserialize, StableHash)]
pub struct TypeCheckResult {
    pub type_info: Option<TypeInfo>,
    pub diagnostics: Arc<HashSet<TypeError>>,
}

#[pernixc_query::query(
    key(TypeCheckKey),
    id(Global<ExpressionID>),
    value(Arc<TypeCheckResult>),
    executor(TypeCheckExecutor)
)]
pub async fn type_check_executor(
    expr_id: Global<ExpressionID>,
    engine: &TrackedEngine,
) -> Result<Arc<TypeCheckResult>, CyclicError> {
    let mut diagnostics = HashSet::new();

    // Perform type checking...
    let type_info = match check_expression_type(expr_id, engine, &mut diagnostics).await {
        Ok(ty) => Some(ty),
        Err(_) => None,
    };

    Ok(Arc::new(TypeCheckResult {
        type_info,
        diagnostics: Arc::new(diagnostics),
    }))
}

// Separate query for just diagnostics
#[pernixc_query::query(
    key(TypeErrorsKey),
    id(Global<ExpressionID>),
    value(Arc<HashSet<TypeError>>),
    executor(TypeErrorsExecutor)
)]
pub async fn type_errors_executor(
    expr_id: Global<ExpressionID>,
    engine: &TrackedEngine,
) -> Result<Arc<HashSet<TypeError>>, CyclicError> {
    let result = engine.query(&TypeCheckKey::new(expr_id)).await?;
    Ok(result.diagnostics.clone())
}
```

## Best Practices

1. **Start with the declarative macro** - Use `#[pernixc_query::query]` for all new queries
2. **Keep keys minimal** - Only include what affects the computation result
3. **Use cheap-to-clone types** - Prefer `Arc<T>`, `SharedStr`, `Arc<[T]>` over owned types
4. **Think about cycles** - Use `#[scc_value(...)]` for queries that might cycle
5. **Use `no_cyclic` carefully** - Only for simple queries that definitely won't cycle
6. **Separate data from diagnostics** - Create separate queries for different concerns
7. **Use consistent collections** - Always use `pernixc_hash::{HashMap, HashSet}`

## Manual Implementation Reference

If you need maximum customization, you can implement queries manually, but this is much more verbose:

```rust
// Manual key definition
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash,
    Serialize, Deserialize, StableHash, Key,
)]
#[value(String)]
pub struct ManualFormatNameKey {
    pub id: Global<ID>,
    pub style: FormatStyle,
}

// Manual executor
#[derive(Debug)]
pub struct ManualFormatNameExecutor;

impl Executor<ManualFormatNameKey> for ManualFormatNameExecutor {
    async fn execute(
        &self,
        engine: &TrackedEngine,
        key: &ManualFormatNameKey,
    ) -> Result<String, CyclicError> {
        let name = engine.get_name(key.id).await;
        let formatted = match key.style {
            FormatStyle::CamelCase => to_camel_case(&name),
            FormatStyle::SnakeCase => to_snake_case(&name),
        };
        Ok(formatted)
    }
}

// Manual extension method
#[extend]
pub async fn get_formatted_name_manual(
    self: &TrackedEngine,
    id: Global<ID>,
    style: FormatStyle,
) -> String {
    self.query(&ManualFormatNameKey { id, style }).await.unwrap()
}
```

As you can see, the manual approach requires much more boilerplate code for the
same functionality.

## Deep Dive: Understanding the Core Architecture

For developers who want to gain a deeper understanding of how the query system
works internally, this section explores the core traits and dependency injection
mechanisms that power the system.

### The `Key` Trait

The `Key` trait is the foundation of the query system. It defines what can be
used as a query identifier:

```rust
pub trait Key:
    'static
    + Send
    + Sync
    + Eq
    + Clone
    + std::hash::Hash
    + Identifiable
    + StableHash
    + Debug
{
    /// If `true`, the query will always re-verify the value everytime the
    /// database version is incremented.
    const ALWAYS_REVERIFY: bool = false;

    /// The corresponding value type for this key
    type Value: 'static + Send + Sync + Clone + Debug + StableHash;

    /// A value returned by the key when the key is a part of a strongly
    /// connected component (SCC) in the cyclic dependencies.
    fn scc_value() -> Self::Value {
        panic!(
            "SCC `{}` value for cyclic dependencies is not defined",
            std::any::type_name::<Self>()
        )
    }
}
```

#### Key Trait Requirements Explained

1. **Lifetime and Threading**: `'static + Send + Sync` ensures keys can be
   shared across threads and stored indefinitely
2. **Equality and Hashing**: `Eq + Hash` enables using keys in hash maps for
   caching
3. **Cloneability**: `Clone` allows the system to duplicate keys for dependency
   tracking
4. **Identification**: `Identifiable + StableHash` provides consistent identity
   across compiler runs
5. **Debugging**: `Debug` enables logging and debugging query execution

#### The Associated Type `Value`

The `Value` associated type must also be:

-   **Thread-safe**: `Send + Sync` for parallel computation
-   **Cloneable**: `Clone` for caching multiple references
-   **Debuggable**: `Debug` for introspection
-   **StableHash**: `StableHash` for comparing results between compiler runs

#### Understanding SCC Values

The `scc_value()` method is crucial for cycle handling:

```rust
impl Key for TypeResolutionKey {
    type Value = Option<TypeInfo>;

    fn scc_value() -> Self::Value {
        None  // When cycles are detected, assume type is unknown
    }
}

impl Key for DependencyGraphKey {
    type Value = HashSet<ID>;

    fn scc_value() -> Self::Value {
        HashSet::new()  // When cycles are detected, assume no dependencies
    }
}
```

### The `Executor` Trait

The `Executor` trait defines how computations are performed:

```rust
pub trait Executor<K: Key>: Any + Send + Sync + std::fmt::Debug {
    /// Computes the result for the given key.
    fn execute<'a>(
        &'a self,
        engine: &'a TrackedEngine,
        key: &'a K,
    ) -> impl Future<'a, K::Value>;
}
```

#### Executor Design Principles

1. **Stateless**: Executors should be stateless for cacheable results
2. **Async**: All computations are async to enable parallel execution
3. **Pure Functions**: The same key should always produce the same result
   (given the same engine state)
4. **Dependency Tracking**: Use `engine.query()` to establish dependencies

#### Executor Lifecycle

```rust
// 1. Executor is created and registered
let executor = Arc::new(MyExecutor);
engine.runtime.executor_registry.register(executor);

// 2. When a query is made, the engine:
//    - Checks cache first
//    - If not cached, finds the registered executor
//    - Calls executor.execute()
//    - Caches the result
//    - Tracks dependencies
```

### The `Engine` and Dependency Injection

The query engine uses dependency injection to manage executors and provide
services:

#### Engine Structure

```rust
pub struct Engine {
    /// The main database for caching query results
    pub database: database::Database,

    /// Runtime services including executor registry
    pub runtime: runtime::Runtime,
}

pub struct Runtime {
    /// Registry of all executors
    pub executor_registry: executor::Registry,

    /// Serialization registry for persistence
    pub serde_registry: /* ... */,

    /// Persistence layer
    pub persistence: /* ... */,
}
```

#### Executor Registry Deep Dive

The executor registry is a type-erased container that maps key types to their executors:

```rust
pub struct Registry {
    // Maps TypeId of Key to its executor and metadata
    executors_by_key_type_id: DashMap<TypeId, Entry>,
}

struct Entry {
    // Type-erased executor
    executor: Arc<dyn Any + Send + Sync>,

    // Function to invoke the executor
    invoke_executor: InvokeExecutorFn,

    // Function to re-verify queries
    re_verify_query: ReVerifyQueryFn,
}
```

#### Registration Process

```rust
impl Registry {
    pub fn register<K: Key, E: Executor<K>>(
        &mut self,
        executor: Arc<E>,
    ) -> Option<Arc<dyn Any + Send + Sync>> {
        self.executors_by_key_type_id.insert(
            TypeId::of::<K>(),
            Entry::new(executor)
        )
    }
}

impl Entry {
    fn new<K: Key, E: Executor<K>>(executor: Arc<E>) -> Self {
        Self {
            executor: executor as Arc<dyn Any + Send + Sync>,
            invoke_executor: invoke_executor::<E, K>,
            re_verify_query: database::re_verify_query::<K>,
        }
    }
}
```

#### Type Erasure and Dynamic Dispatch

The system uses type erasure to store different executor types in the same
registry:

```rust
// Type-erased function that can invoke any executor
type InvokeExecutorFn = for<'a> fn(
    key: &'a dyn Any,           // Type-erased key
    executor: &'a dyn Any,      // Type-erased executor
    engine: &'a mut TrackedEngine,
) -> Pin<Box<dyn Future<'a, DynamicValue> + 'a>>;

// Concrete implementation for specific types
fn invoke_executor<'a, E: Executor<K> + 'static, K: Key + 'static>(
    key: &'a dyn Any,
    executor: &'a dyn Any,
    engine: &'a mut TrackedEngine,
) -> Pin<Box<dyn Future<'a, DynamicValue> + 'a>> {
    // Downcast to concrete types
    let key = key.downcast_ref::<K>().expect("Key type mismatch");
    let executor = executor.downcast_ref::<E>().expect("Executor type mismatch");

    Box::pin(async {
        executor.execute(engine, key).await.map(|x| {
            // Type-erase the result
            let dynamic_value: DynamicValue = smallbox::smallbox!(x);
            dynamic_value
        })
    })
}
```

#### Query Execution Flow

```rust
// 1. Client calls engine.query(&SomeKey::new(id))
impl TrackedEngine {
    pub async fn query<K: Key>(&self, key: &K) -> Result<K::Value, CyclicError> {
        // 2. Check if result is cached
        if let Some(cached) = self.database.get_cached(key) {
            return Ok(cached);
        }

        // 3. Find executor for this key type
        let entry = self.runtime.executor_registry
            .get_entry::<K>()
            .expect("No executor registered for this key type");

        // 4. Execute with dependency tracking
        let result = (entry.get_invoke_executor())(
            key as &dyn Any,
            entry.get_any_executor().as_ref(),
            self
        ).await?;

        // 5. Cache result and return
        self.database.cache(key, result.clone());
        Ok(result)
    }
}
```

#### Dependency Tracking

When an executor calls `engine.query()` during execution, the system automatically tracks dependencies:

```rust
impl TrackedEngine {
    pub async fn query<K: Key>(&self, key: &K) -> Result<K::Value, CyclicError> {
        // Record that current query depends on this key
        self.record_dependency(key);

        // ... rest of query execution
    }

    fn record_dependency<K: Key>(&self, key: &K) {
        if let Some(current_query) = self.current_execution_context() {
            self.database.add_dependency(current_query, key);
        }
    }
}
```

#### Cache Invalidation

When input data changes, the system invalidates dependent queries:

```rust
impl Database {
    pub fn invalidate<K: Key>(&mut self, key: &K) {
        // 1. Remove cached result
        self.cache.remove(key);

        // 2. Find all queries that depend on this key
        let dependents = self.dependency_graph.get_dependents(key);

        // 3. Recursively invalidate dependent queries
        for dependent in dependents {
            self.invalidate_dynamic(dependent);
        }
    }
}
```

## Conclusion

The Pernix query system provides a powerful foundation for incremental
compilation. By using the declarative `#[pernixc_query::query]` macro,
you can create efficient, cached computations with minimal boilerplate.
Start simple, understand the concepts, and gradually work up to more complex
queries as needed.

For most use cases, the declarative approach will give you everything you need
with clean, readable code that focuses on the computation logic rather than
infrastructure concerns.
