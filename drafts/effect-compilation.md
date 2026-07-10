# Effect Compilation

The main focus of the compilation is two fold:

1. How to represent the suspended computations efficiently in a system level programming language?
2. How to represent the handlers that can intercept the operations?

## Suspended Computations

### Rust `async fn` State Machine Case Study

Rust `async fn` is a good and well-known case study for lowering suspendable
code into an efficient representation. In Rust, every `await` point is a
suspension point that stops the execution and returns control back to the
caller. When the caller resumes the execution, the control flows back to the
latest suspension point and continues the execution from there.

Rust lowers the `async fn` into a state machine that is represented as a normal
efficient struct. The state machine is a first-class value and can be 
passed around (with some nuances of `Pin` and `Unpin` types, but generally it 
is a first-class value).

Rust doesn't manipulate the **OS Stack/Thread** to suspend the execution and
treat the suspended computation as a first-class value. That's why they are
called **stackless** state machines.

### Stackless State Machine Coroutines

Building on top of the Rust `async fn` state machine, we can represent the 
effectful code using **stackless state machine coroutines**. We'll use 
**coroutines** as the main representation of the effectful code.

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

Suppose we have an effect signature like this:

```pnx
pub eff Random:
    fn nromal(range: Range[int32]) -> int32
```

And we have an effectful function that uses this effect by querying two
random numbers and returning their sum:

```pnx
pub def addTwoRandoms() -> int32 \ {Random}:
    let a = do Random.nromal(0..100)
    let b = do Random.nromal(100..200)
    return a + b
```

If we lower this effectful function into a coroutine code, it would still look
very similar to the original code:

```pnx
pub coroutine[yield Range[int32], resume int32] addTwoRandoms() -> int32:
    let a = yield Range[int32](0..100)
    let b = yield Range[int32](100..200)
    return a + b
```

If we lower this coroutine code into a Rust-ish state machine code, it would look like this:

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

Of course, a function can perform multiple distinct operations, we can encode
the `Yield` type as a sum type of all the operations that the function can perform. For example, if we have two effects `Random` and `Log`, we can encode the `Yield` type as follows:

```rust
pub enum YieldType {
    Random(Range<i32>),
    Log(String),
}
```

And consequently, the `R` resume type can also be a sum type of all the return types of the operations. For example, if `Random` returns `i32` and `Log` returns `()`, we can encode the `Return` type as follows:

```rust
pub enum ReturnType {
    Random(i32),
    Log(()),
}
```

### Nested Coroutines and Recursion

Is it common that an effectful operation is called deep down in the call stack.
For instance, this example shows a function that calls another effectful function:

```pnx
pub def addTwoRandoms() -> int32 \ {Random}:
    let a = do Random.nromal(0..100)
    let b = do Random.nromal(100..200)
    return a + b

pub def addFourRandoms() -> int32 \ {Random}:
    let a = addTwoRandoms()
    let b = addTwoRandoms()
    return a + b
```

WHen we lower this code into a coroutine code, it would look like this:

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

Generally, we repeatedly poll the inner coroutine and propagate their yielded
values to the outer coroutine. Once the inner coroutine completes, we can use its returned value to continue the execution.

The generated state machine code for the `addFourRandoms` function would 
look like this:

```rust
pub enum AddFourRandomsState {
    Start,
    PollingFirstAddTwoRandoms { first_coro: AddTwoRandomsState },
    PollingSecondAddTwoRandoms { first_result: i32, second_coro AddTwoRandomsState },
    Returned,
}
```

We wouldn't go into the details of the translated function of the above 
coroutine. The main idea is that we can represent the nested coroutines as a
composition of generated enumerated states. 

We can quickly see why recursive coroutines are problematic and requires 
indirection. For example, if we have a recursive function like this:

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

The above code is the classic infinite recursive type and the usual treatment
is to box the inner coroutine to break the infinite recursion type size. The
same problem is also present in [Rust's `async fn`][rust-async-fn-recursive] 
and they require programmer to explicitly box (`Box::pin`) the inner coroutine 
to break the infinite recursive type size.

[rust-async-fn-recursive]: https://blog.rust-lang.org/2024/03/21/Rust-1.77.0/#support-for-recursion-in-async-fn