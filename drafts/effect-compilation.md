# Effect Compilation

Effect compilation has two main concerns:

1. How can suspended computations be represented efficiently in a systems programming language?
2. How can handlers that intercept operations be represented?

## Suspended Computations

### Rust `async fn` State Machine Case Study

Rust's `async fn` is a well-known example of lowering suspendable code into an
efficient representation. In Rust, every `await` is a suspension point: it
pauses execution and returns control to the caller. When the caller resumes
execution, control returns to the most recent suspension point and continues
from there.

Rust lowers an `async fn` into a state machine represented by an ordinary,
efficient struct. The state machine is generally a first-class value that can
be passed around, subject to the usual `Pin` and `Unpin` considerations.

Rust does not manipulate the **OS stack or thread** to suspend execution or to
treat a suspended computation as a first-class value. This is why these are
called **stackless** state machines.

### Stackless State Machine Coroutines

Building on Rust's `async fn` state-machine model, we can represent effectful
code as **stackless state-machine coroutines**. We will use **coroutines** as
the primary representation of effectful code.

Rust's `Coroutine` trait is a good starting point:

```rust
pub enum CoroutineState<Y, R> {
    Yielded(Y),
    Complete(R),
}

pub trait Coroutine<R> {
    type Yield;
    type Return;

    fn resume(
        self: &mut Self,
        arg: R,
    ) -> CoroutineState<Self::Yield, Self::Return>;
}
```

Suppose we have the following effect signature:

```pnx
pub eff Random:
    fn normal(range: Range[int32]) -> int32
```

And we have an effectful function that uses this effect by querying two
random numbers and returning their sum:

```pnx
pub def addTwoRandoms() -> int32 \ {Random}:
    let a = do Random.normal(0..100)
    let b = do Random.normal(100..200)
    return a + b
```

Lowering this function to coroutine code produces something very similar to
the original:

```pnx
pub coroutine[yield Range[int32], resume int32] addTwoRandoms() -> int32:
    let a = yield Range[int32](0..100)
    let b = yield Range[int32](100..200)
    return a + b
```

Lowering that coroutine to Rust-like state-machine code gives us:

```rust
pub enum AddTwoRandomsState {
    Start,
    YieldedA,
    YieldedB { saved_a: i32 },
    Returned
}

impl Coroutine<i32> for AddTwoRandomsState {
    type Yield = Range<i32>;
    type Return = i32;

    fn resume(
        self: &mut Self,
        arg: i32,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        match self {
            AddTwoRandomsState::Start => {
                *self = AddTwoRandomsState::YieldedA;
                CoroutineState::Yielded(Range { start: 0, end: 100 })
            }
            AddTwoRandomsState::YieldedA => {
                let a = arg;
                *self = AddTwoRandomsState::YieldedB { saved_a: a };
                CoroutineState::Yielded(Range { start: 100, end: 200 })
            }
            AddTwoRandomsState::YieldedB { saved_a } => {
                let b = arg;
                let result = saved_a + b;
                *self = AddTwoRandomsState::Returned;
                CoroutineState::Complete(result)
            }
            AddTwoRandomsState::Returned => {
                panic!("Coroutine has already completed");
            }
        }
    }
}
```

A function can, of course, perform multiple distinct operations. We can encode
its `Yield` type as a sum type over every operation it may perform. For
example, for the `Random` and `Log` effects:

```rust
pub enum YieldType {
    Random(Range<i32>),
    Log(String),
}
```

Correspondingly, the `R` resume type can be a sum type over the operations'
return types. If `Random` returns `i32` and `Log` returns `()`, we can encode
the `Return` type as follows:

```rust
pub enum ReturnType {
    Random(i32),
    Log(()),
}
```

### Nested Coroutines and Recursion

Effectful operations are commonly invoked deep in the call stack. For example,
the following function calls another effectful function:

```pnx
pub def addTwoRandoms() -> int32 \ {Random}:
    let a = do Random.normal(0..100)
    let b = do Random.normal(100..200)
    return a + b

pub def addFourRandoms() -> int32 \ {Random}:
    let a = addTwoRandoms()
    let b = addTwoRandoms()
    return a + b
```

Lowering this code to coroutines gives us:

```pnx
pub coroutine[yield Range[Int32], resume int32] addFourRandoms() -> int32:
    let firstAddTwoRandomsCoro = addTwoRandoms()

    let a = null
    loop:
        let mut resumingValue = null

        match firstAddTwoRandomsCoro.resume(resumingValue):
            CoroutineState::Yielded(yieldedValue):
                resumingValue = yield yieldedValue
            
            CoroutineState::Complete(returnedValue):
                a = returnedValue
                break 

    let b = null
    let secondAddTwoRandomsCoro = addTwoRandoms()
    loop:
        let mut resumingValue = null

        match secondAddTwoRandomsCoro.resume(resumingValue):
            CoroutineState::Yielded(yieldedValue):
                resumingValue = yield yieldedValue

            CoroutineState::Complete(returnedValue):
                b = returnedValue
                break 

    return a + b
```

In general, we repeatedly poll the inner coroutine and propagate its yielded
values through the outer coroutine. Once the inner coroutine completes, its
return value lets the outer coroutine continue execution.

The generated state-machine representation of `addFourRandoms` would look like
this:

```rust
pub enum AddFourRandomsState {
    Start,
    PollingFirstAddTwoRandoms { first_coro: AddTwoRandomsState },
    PollingSecondAddTwoRandoms { first_result: i32, second_coro: AddTwoRandomsState },
    Returned,
}
```

We will not examine the full translation of this coroutine. The key idea is
that nested coroutines can be represented by composing generated enum states.

This also shows why recursive coroutines are problematic: they require
indirection. For example, consider this recursive function:

```pnx
pub def effectfulCode() -> int32 \ {Random}:
    return effectfulCode()
```

The generated state machine code would look like this:

```rust
pub enum EffectfulCodeState {
    Start,
    PollingEffectfulCode { inner_coro: EffectfulCodeState },
    Returned,
}
```

This is the classic infinitely recursive type. The usual solution is to box
the inner coroutine, breaking the infinite-size cycle. The same issue arises
with [Rust's `async fn`][rust-async-fn-recursive], where the programmer must
explicitly box the inner coroutine with `Box::pin`.

### Conclusion on Suspended Computations

We settled on representing suspended computations as stackless state-machine
coroutines. This representation has the following characteristics:

1. The suspended computation is a first-class value that can be passed around.
2. The suspended computation is modeled as a mutable state machine whose state
  is updated in place. This contrasts with many existing effect systems, which
  model suspended computations as immutable functions.
3. A downside of this representation is that a continuation can be resumed at
  most once. This follows from modeling suspended computations as mutable state
  machines. Multiple resumptions are still possible: one simple approach is to
  clone the state machine before resuming it. This sacrifices some generality
  for efficiency, a tradeoff that is common in systems programming languages.
4. The representation is stackless, meaning that suspending execution does not
  manipulate the OS stack. This makes it efficient and portable.
5. As with Rust's `async fn`, we must take great care with pointer stability.
  The state machine can have interior references, so we need to ensure that
  the state machine is not moved in memory while it is suspended. This is
  the primary reason why [`Pin` and `Unpin`][rust-pin-unpin] exist in Rust.
  However, this will not be a concern for the first prototype.

## Handlers 

[rust-async-fn-recursive]: https://blog.rust-lang.org/2024/03/21/Rust-1.77.0/#support-for-recursion-in-async-fn
[rust-pin-unpin]: https://blog.cloudflare.com/pin-and-unpin-in-rust/
