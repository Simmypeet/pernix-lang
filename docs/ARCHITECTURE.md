# Compiler Architecture

## Compiler Crates

The compiler is divided into several smaller crates, each responsible for a
specific aspect of the compilation process. The cohesion of each crate is
questionable, but the division helps with compilation times and parallelism.

The crates are categorized mainly into four folders:

-   `base`: Contains the fondational infrastructure for the compiler, such as
    error handling, diagnostics, and query systems.

-   `syntax`: Contains the lexical and syntactic analysis components, including
    tokenization and parsing.

-   `semantic`: The largest folder, containing crates for semantic analysis,
    type checking, and intermediate representations.

-   `codegen`: Contains the code generation components, currently focused on
    LLVM IR generation.

## The Main Compiler Computation Model

Unlike traditional pipeline-based compilers, this compiler uses a query-based
model (similar to Rust's compiler). The main idea is that the all the compiler's
knowledge is represented as a set of queries. For example, "What is the return
type of this function?" or "What are the fields of this struct?". Each query
can depend on other queries, forming a directed acyclic graph (DAG) of
dependencies. There're several articles that explain this model in more
detail about how it works and its benefits.

Next we'll go over the main components that powers the query system. The main
components are the `Key` trait, the `Executor` trait, and the `Engine`.

### Key Trait

In short, the `Key` trait is simply defined as:

```rust
pub trait Key {
    type Value;
}
```

That's it! The `Key` trait simply defines what the input and output types of a
query are. The input type is the type that implements the `Key` trait, and the
output type is the associated `Value` type.

It's important to note that it doesn't define how the query is computed, it
simply defines the contract of a query.

It can be common to see that some crates only define the structs that
implement the `Key` trait and left out the implementation of the `Executor`
trait for later.

### Executor

The `Executor` defines how queries are computed. It's trait is defined as:

```rust
pub trait Executor<K: Key> {
    async fn execute(&self, key: &K, engine: &TrackedEngine)
        -> Result<K::Value, CyclicError>;
}
```

This specifies what to do when a query is executed. The `execute` method takes
in the key (the input type) and the engine (more on this later) and returns the
output type. This can be thought of as the "implementation" of the query
whereas the `Key` trait is the "interface".

### Engine

The `Engine` is the main component that ties everything together. This answers
the question: "How the `Key` and `Executor` traits work together to compute
queries?". During the startup of the compiler, an `Engine` is created and
the `Executor` implementations for each `Key` are registered with the engine.

```rust
let mut engine = Engine::default();
engine.runtime.executor.register::<FizzKey, _>(Arc::new(FizzExecutor));
engine.runtime.executor.register::<BuzzKey, _>(Arc::new(BuzzExecutor));
```

This similarly resembles a dependency injection container that you might see in
many popular web frameworks. With the engine set up, queries can be executed
by using the `TrackedEngine` wrapper:

```rust
let engine = Arc::new(engine); // make it sharable across threads
let tracked_engine = engine.tracked();
let result = tracked_engine.query(&FizzKey(64)).await;
```

It's important to note that the `TrackedEngine` is simply a thin wrapper over
`Arc<Engine>` that allows queries to be executed. Additionally, the
`TrackedEngine` as its name suggests, tracks the queries that are executed and
in order to build the dependency graph and support incremental compilation.

In the previous section about the `Executor` trait, the `execute` method takes
in a `TrackedEngine` as a parameter. This allows queries to depend on other
queries. For example, the `FizzExecutor` can depend on the `BuzzKey` query
like so:

```rust
pub struct FizzExecutor;

impl Executor<FizzKey> for FizzExecutor {
    // this method represents the implementation of the FizzKey query
    async fn execute(
        &self,
        key: &FizzKey,
        engine: &TrackedEngine,
    ) -> Result<FizzValue, CyclicError> {
        // when querying for the `BuzzKey`, the engine will track this dependency
        // and ensure that if the `BuzzKey` query changes, the `FizzKey`
        // query will be recomputed.
        let buzz_value = engine.query(&BuzzKey(key.0)).await?;

        // now `FizzKey` depends on `BuzzKey` and the engine will track this
        // dependency.

        // use the result of the `BuzzKey` query to compute the result of the
        // `FizzKey`
        Ok(FizzValue(buzz_value.0 * 2))
    }
}
```

### Supporting Infrastructure

In addition to the main components that power the query system, there are
several supporting components that are worth mentioning:

-   **Serialization**: In order to support incremental compilation, the engine
    needs to be able to serialize and deserialize the queries and their results.
    This is done using the internal `pernixc_serialize` crate which has a
    similar API to `serde` but is written from scratch to support the specific
    needs of the compiler.
-   **Stable Type ID**: You might have noticed that the `Engine` is a big
    type-erased container that holds all the `Executor` and `Key`
    implementations across different types. In order to store and retrieve these
    components, a type identifier is needed. For example, you can imagine
    this data structure:

    ```rust
    pub struct MiniEngine {
        query_cache: HashMap<TypeId, Box<dyn Any>>
    }

    impl MiniEngine {
        pub fn insert_value<K>(&mut self, value: K::Value)
        where
            K: Key + 'static,
        {
            let type_id = TypeId::of::<K>();
            self.query_cache.insert(type_id, Box::new(value));
        }

        pub fn get_value<K>(&self) -> Option<&K::Value>
        where
            K: Key + 'static,
        {
            let type_id = TypeId::of::<K>();
            self.query_cache.get(&type_id)?.downcast_ref::<K::Value>()
        }
    }
    ```

    There're lots of components in the compiler that need to be stored and
    retrieved in a type-erased manner, and the standard library's `TypeId`
    doesn't guarantee that the same type will have the same `TypeId` across
    different compilations. To solve this, the compiler uses a custom
    `StableTypeId` implementation that guarantees that the same type will have
    the same `StableTypeId` across different compilations as long as the type
    definition doesn't change.

-   **Stable Hash**: This hashing serves different purposes to the
    `std::hash::Hash` trait. It primarily serves as a way to compare the results
    of queries across different compilations. This is important for incremental
    compilation, as the engine needs to know if the result of a query has
    changed in order to determine if dependent queries need to be recomputed.

-   **Embeded Database**: The engine uses an embedded key-value database to
    store the results of queries and dependency graph. This allows the engine
    to persist the state of the compilation across different runs of the
    compiler.

### Tradeoffs

**Pros:**

-   **Easy Incremental Compilation**: The compiler logic can be naturally
    expressed as a set of queries, and the engine can automatically
    determine which queries need to be recomputed when the input changes.
    It requires little to no effort from the compiler developer to support
    incremental compilation.
-   **Parallelism**: Due to the nature of "everything is a query", the engine
    can easily execute independent queries in parallel, making it easy to
    take advantage of multi-core processors.
-   **Testability**: In theory, the engine is a dependency injection container
    that allows different implementations of queries to be swapped in and out.
    This makes it easy to test different components of the compiler in
    isolation.

**Cons:**

-   **Boilerplate**: There's a lot of boilerplate code that needs to be written
    in order to define new queries. Each query requires a `Key` struct, an
    `Executor` struct, and the registration of the `Executor` with the engine.
    This can be mitigated with code generation, but it still requires some
    effort from the compiler developer.
-   **Async/Await Complexity**: The compiler uses async/await for query
    execution. Generally, async/await tends to add complexity to the codebase
    such as `Send`, `'static`, and `Future`. The later section goes into more
    detail about why async/await is used in this compiler.
-   **DI Registration**: The registration of the `Executor` implementations with
    the engine can be cumbersome and error-prone. There's no compile-time
    guarantee that all `Key` implementations have a corresponding `Executor`
    implementation registered with the engine. This can lead to runtime errors
    if a query is executed without a registered `Executor`.
-   **Poor Incremental Compilation Performance on Very Large Codebases**: The
    current implementation of the engine has poor performance on very large
    codebases (100k+ lines of code). Although we've tried to leverage the
    multithreading capabilities of the engine, the performance is still not
    ideal. This is due to the fact currently, the engine has to traverse the
    whole dependency graph in order to determine which queries need to be
    recomputed. In a large codebase, it's common to have a large number of
    queries (up to millions) and traversing the whole graph can be
    time-consuming. There's possible design solutions to this problem such as
    bottom-up invalidation, but they haven't been explored yet.

### Going Async

It's uncommon to see a compiler that uses async/await, but this compiler does.
The main reason is that thread-pools like `rayon` are simply not flexible enough
and can cause thread starvation in certain scenarios.

The main challenge with using sync thread-pools is that the query-engine
has a specific locking mechanism in order to prevent the work being done
twice. These locking is generally a big anti-pattern in thread-pools as it
can severely degrade performance and cause thread starvation in worst-case
scenarios.

Here's an example of how locking could happen in a query engine. Imagine
there're two queries `A` and `B` that are being executed in different
threads. Both of them depend on query `C`. Generally, the engine would
compute the query `C` once and reuse the result for subsequent queries. But
if both threads try to compute query `C` at the same time, only one of
them should be allowed to compute it, and the other one should wait for
the result. This is a classic locking scenario.

In a sync version, the locking can mean that one thread is blocked
waiting/sleeping until the other thread is done computing the query
and notified. This causes poor CPU utilization as the blocked thread is not
doing any work.

With async/await, the thread that is waiting for the result of query `C` can
yield its execution and allow other tasks to run. This improves CPU utilization
and prevents thread starvation.
