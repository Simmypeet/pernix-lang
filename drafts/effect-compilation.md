# Effect Compilation

Effect compilation has two main concerns:

1. How can suspended computations be represented efficiently in a systems 
  programming language?
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
    Start,
    Random(Range<i32>),
    Log(String),
}
```

Correspondingly, the `R` resume type can be a sum type over the operations'
answer (we prefer to use "answer" over "return" to avoid confusion) types. If 
`Random` answers `i32` and `Log` answers `()`, we can encode the `Answer` type 
as follows:

```rust
pub enum Answer {
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
    let mut resumingValue = Start

    loop:
        match firstAddTwoRandomsCoro.resume(resumingValue):
            CoroutineState::Yielded(yieldedValue):
                resumingValue = yield yieldedValue
            
            CoroutineState::Complete(returnedValue):
                a = returnedValue
                break 

    let b = null
    let secondAddTwoRandomsCoro = addTwoRandoms()
    let mut resumingValue = Start

    loop:
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

### Lowering Coroutines Language Into a Stackless State Machine

We must define a pass that takes a CFG that has following properties:

1. Has a *yield* instruction as a terminal instruction in a block. This nicely
  partitions the CFG into blocks that are separated by suspension points. 
  Polling nested coroutines is also considered a suspension point, so we'll 
  treat them as yield instructions as well for the sake of simplicity.
2. At each yield instruction, we must have a set of "live" variables that are
  used after the yield. These variables must be saved in the state machine so
  that they can be restored when the coroutine is resumed. Of course, this 
  can be done with a simple liveness analysis pass.

We'll then have a pass that takes such CFG and produces three components:

1. A sum type state machine that represents the coroutine's state.
3. A `step` function that dispatches on the state machine and executes the next 
  block of code.

The `step` functions will have these following signatures:

```rust
// for instance if we have the following effect signature
// 
// pub eff Random:
//   fn normal(range: Range[int32]) -> int32
//
// pub eff Log:
//   fn log(msg: String) -> Unit
//
// and the effectful function has the following effect row {Random, Log}
// then the YieldType and AnswerType will be as follows:
pub enum YieldType {
    RandomYield(Range<i32>),
    LogYield(String),
}
pub enum AnswerType {
    Start,
    RandomAnswer(i32),
    LogAnswer(()),
}

// whereas CoroutineState is defined as
pub enum CoroutineState<Y, R> {
    Yielded(Y),
    Complete(R),
}

// the `FuncRetType` is the original return type of the effectful function
fn coro_step(coro: &mut Coro, handler_answer: AnswerType) 
    -> CoroutineState<YieldType, FuncRetType>
```

Certainly, we'll have to generate `YieldType` and `AnswerType` for each
effect row, but the details of that are not important for this discussion. 

Surprisingly, the transformation from a CFG to `coro_step` is not too difficult.

1. For each variable that lives across a yield instruction, those variables will
  live on the state machine instead of the native call stack. Meaning that 
  memory load/store instructions on those variables will be replaced with memory 
  load/store instructions on the state machine. This transformation should be
  very straightforward.
2. Yield instructions (located at terminal instructions) will be replaced with 
  state machine updates and a `CoroutineState::Yielded` return.
3. The `coro_step` function will have a `match` statement at the entry point
  that dispatches on the state machine. Each branch of the `match` statement
  will correspond to an unconditional jump to a basic block in the CFG.

#### Compilation Example

Take this high-level effectful function written in the Pernix language:

```pnx
pub eff Yield:
    def yield(val: int32) -> bool # `true` to continue, `false` to stop

pub def yieldAccumulateStep(nums: Vector[Int32]) -> Unit \ {Yield}:
    let mut acc = 0
    let mut iter = nums.iter()

    # yield point 1: Live variables: acc, iter, nums
    let shouldContinue1 = do Yield.yield(acc)

    if not shouldContinue1:
        return

    loop:
        match iter.next():
            as Some(num):
                # yield point 2: Live variables: acc, iter, nums, num
                let shouldContinue2 = do Yield.yield(num) 

                if not shouldContinue2:
                    return
                
                acc += num
                
            as None:
                return
```

Assuming we have a `Yield` handler that prints out the yielded values and 
always returns `true` and the input to the function is `[1, 2, 3]`, we would 
expect the following output:

```
0 1 3 6
```

Now transform this into an ordinary CFG with yield instructions as terminal
instructions:

```
start:
    let mut acc = 0
    let mut iter = nums.iter()
    
    @answerYield1 <- yield YieldType.Yield(acc), goto yield_point_1

yield_point_1:
    let shouldContinue1 = @answerYield1 unwrap into AnswerType.YieldAnswer
    check shouldContinue1, goto if1_true, goto if1_false

if1_false:
    return ()

if1_true:
    goto loop_header

loop_header:
    let @matchScrutinee = iter.next()
    switch @matchScrutinee:
        as Some: goto match_some
        as None: goto match_none
    
match_none:
    return ()

match_some:
    let num = @matchScrutinee unwrap into Some
    @answerYield2 <- yield YieldType.Yield(num), goto yield_point_2

yield_point_2:
    let shouldContinue2 = @answerYield2 unwrap into AnswerType.YieldAnswer
    check shouldContinue2, goto if2_true, goto if2_false

if2_false:
    return ()

if2_true:
    acc += num
    goto loop_header
```

The generated state machine enum will look like this:

```rust
enum CoroutineState {
    Start { nums: Vector<i32> },
    YieldPoint1 { 
        nums: Vector<i32>, 
        acc: i32, 
        iter: Iterator<'this, i32>,
    },
    YieldPoint2 { 
        nums: Vector<i32>, 
        acc: i32, 
        iter: Iterator<'this, i32>,
        num: i32,
    },
    Done,
}
```

However, the shown enum is generally for illustration. In practice, it should
have been something like this:

```rust
pub enum State {
    Start,
    YieldPoint1,
    YieldPoint2,
    Done,
}

pub struct CoroutineState {
    state: State,
    // Frame is some stack storage that can contain all the live variables at 
    // each yield point. 
    frame: Frame, 
}
```

And the step function will look like this in CFG form.

```
entry:
    match coro_state.state:
        as State.Start: goto start
        as State.YieldPoint1: goto yield_point_1
        as State.YieldPoint2: goto yield_point_2
        as State.Done: goto done

done:
    panic("Coroutine has already completed")

start:
    coro_state.frame.acc = 0
    coro_state.frame.iter = coro_state.frame.nums.iter()

    coro_state.state = State.YieldPoint1
    
    return CoroutineState.Yield(YieldType.Yield(coro_state.frame.acc))

yield_point_1:
    let shouldContinue1 = handler_answer unwrap into AnswerType.YieldAnswer
    check shouldContinue1, goto if1_true, goto if1_false

if1_false:
    return CoroutineState.Complete(())

if1_true:
    goto loop_header

loop_header:
    let @matchScrutinee = coro_state.frame.iter.next()
    switch @matchScrutinee:
        as Some: goto match_some
        as None: goto match_none
    
match_none:
    return CoroutineState.Complete(())

match_some:
    coro_state.frame.num = @matchScrutinee unwrap into Some

    coro_state.state = State.YieldPoint2
    return CoroutineState.Yield(YieldType.Yield(coro_state.frame.num))

yield_point_2:
    let shouldContinue2 = handler_answer unwrap into AnswerType.YieldAnswer
    check shouldContinue2, goto if2_true, goto if2_false

if2_false:
    return CoroutineState.Complete(())

if2_true:
    coro_state.frame.acc += coro_state.frame.num
    goto loop_header
```

One can see that the `coro_step` function is almost identical to the original 
CFG.

We would like to include one more example of an effectful function that calls 
another effectful function. The following code is a simple example of that:

```pnx
pub def yieldAccumulateStep(nums: Vector[Int32]) -> Unit \ {Yield}:
    # the same as before
    ...

pub def indirect() -> Unit \ {Yield}:
    let result = do yieldAccumulateStep([1, 2, 3]) # yield point 1
    return result
```

The generated state machine enum will look like this:

```rust
pub enum CoroutineState {
    Start,
    PollingYieldAccumulateStep {
        yieldAccumulateStepCoro: YieldAccumulateStepState,
    }
    Done,
}
```


The following is the `coro_step` function of `indirect` should give you a clearer picture of how this works:

```
entry:
    match coro_state.state:
        as State.Start: goto start
        as State.PollingYieldAccumulateStep: goto polling_yield_accumulate_step
        as State.Done: goto done   

done:
    panic("Coroutine has already completed")

start:
    coro_state.frame.yieldAccumulateStepCoro = 
        YieldAccumulateStepState.startWith([1, 2, 3])

    let @pollRes = yieldAccumulateStepCoro(
        &mut coro_state.frame.yieldAccumulateStepCoro,
        AnswerType.Start
    )

    match @pollRes:
        as CoroutineState.Complete: goto yield_point_1
        as CoroutineState.Yield:    goto propagate_yield_accumulate_step

propagate_yield_accumulate_step:
    coro_state.state = State.PollingYieldAccumulateStep
    return CoroutineState.Yield(@pollRes unwrap into YieldType)

polling_yield_accumulate_step:
    let @pollRes = yieldAccumulateStepCoro(
        &mut coro_state.frame.yieldAccumulateStepCoro,
        handler_answer
    )

    match @pollRes:
        as CoroutineState.Complete: goto yield_point_1
        as CoroutineState.Yield:    goto propagate_yield_accumulate_step

yield_point_1:
    let result = @pollRes unwrap into CoroutineState.Complete
    return CoroutineState.Complete(result)
```

The transformation is a bit more involved, but the key idea is that:

1. When encountering call to effectful function, we obtain the coroutine state
  machine and calls its `coro_step` function with the `AnswerType.Start` value. 
  If the result is a `CoroutineState::Yielded`, we propagate that yield to the
  caller. If the result is a `CoroutineState::Complete`, we continue execution.
2. After the first `coro_step` call, we enter a polling loop that repeatedly 
  calls the inner coroutine's `coro_step` function with the `handler_answer` 
  value. Similarly, if the result is a `CoroutineState::Yielded`, we propagate 
  that yield and if it's complete, we continue execution.

#### Conclusion on Lowering Coroutines

This shows the main idea of lowering a CFG with yield instructions into a 
`coro_step` function. The nuances of the transformation haven't discussed here
are:

1. The cleanup of the state machine if the coroutine is dropped before it 
  completes. Apart from `coro_step`, we will also generate a `coro_drop` 
  function that dispatches on the state machine and drops all the live 
  variables. Moreover, it gets a bit more complicated if the live variables
  are partially moved/initialized. This should be a straightforward extension.
2. Generating a space-optimized `Frame` struct that can hold all the live 
  variables at each yield point. There are some tedious requirements to ensure
  that the variables are properly aligned and that those variables stored on
  the frame should be pinned in memory..

## Handlers 

[rust-async-fn-recursive]: https://blog.rust-lang.org/2024/03/21/Rust-1.77.0/#support-for-recursion-in-async-fn
[rust-pin-unpin]: https://blog.cloudflare.com/pin-and-unpin-in-rust/
