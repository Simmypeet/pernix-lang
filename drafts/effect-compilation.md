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

### Lowering the Coroutine Language into a Stackless State Machine

We must define a lowering pass that operates on a CFG with the following
properties:

1. Each *yield* instruction terminates its basic block. This naturally
  partitions the CFG into regions separated by suspension points. Polling a
  nested coroutine is also a suspension point, so, for simplicity, we treat it
  like a yield instruction.
2. At each yield instruction, we know which variables remain live because they
  are used after the yield. These variables must be stored in the state machine
  so that they are available when the coroutine resumes. A standard liveness
  analysis can compute this set.

The lowering pass takes such a CFG and produces two components:

1. A sum type that represents the coroutine's state.
2. A `step` function that dispatches on the state machine and executes the next
  block of code.

The `step` function has the following signature:

```rust
// For example, suppose we have the following effect signatures:
// 
// pub eff Random:
//   fn normal(range: Range[int32]) -> int32
//
// pub eff Log:
//   fn log(msg: String) -> Unit
//
// If an effectful function has the effect row {Random, Log}, its YieldType and
// AnswerType are as follows:
pub enum YieldType {
    RandomYield(Range<i32>),
    LogYield(String),
}
pub enum AnswerType {
    Start,
    RandomAnswer(i32),
    LogAnswer(()),
}

// CoroutineState is defined as follows:
pub enum CoroutineState<Y, R> {
    Yielded(Y),
    Complete(R),
}

// `FuncRetType` is the original return type of the effectful function.
fn coro_step(coro: &mut Coro, handler_answer: AnswerType) 
    -> CoroutineState<YieldType, FuncRetType>
```

We must generate `YieldType` and `AnswerType` for each effect row, but those
details are not important to this discussion.

The transformation from a CFG to `coro_step` is relatively straightforward:

1. Variables that remain live across a yield are stored in the state machine
  rather than on the native call stack. Loads and stores of these variables are
  therefore rewritten to access the state machine's storage.
2. Each yield instruction is replaced by a state-machine update followed by a
  `CoroutineState::Yielded` return.
3. At its entry point, `coro_step` uses a `match` statement to dispatch on the
  current state. Each branch unconditionally jumps to the corresponding basic
  block in the CFG.

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

Suppose a `Yield` handler prints every yielded value and always returns `true`.
Given `[1, 2, 3]` as input, we would expect the following output:

```
0 1 3 6
```

We first transform the function into an ordinary CFG in which each yield
instruction terminates its basic block:

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

The generated state-machine enum might initially look like this:

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

This enum is only illustrative. In practice, we would use a separate state tag
and frame, as follows:

```rust
pub enum State {
    Start,
    YieldPoint1,
    YieldPoint2,
    Done,
}

pub struct CoroutineState {
    state: State,
    // Frame is storage that can contain all variables live at each yield point.
    frame: Frame,
}
```

In CFG form, the step function then looks like this:

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

The resulting `coro_step` function closely resembles the original CFG.

The transformation becomes slightly more involved when one effectful function
calls another. Consider the following example:

```pnx
pub def yieldAccumulateStep(nums: Vector[Int32]) -> Unit \ {Yield}:
    # the same as before
    ...

pub def indirect() -> Unit \ {Yield}:
    let result = do yieldAccumulateStep([1, 2, 3]) # yield point 1
    return result
```

The generated state-machine enum looks like this:

```rust
pub enum CoroutineState {
    Start,
    PollingYieldAccumulateStep {
        yieldAccumulateStepCoro: YieldAccumulateStepState,
    },
    Done,
}
```

The `coro_step` function for `indirect` illustrates how the outer coroutine
drives the inner one:

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

The key steps are as follows:

1. When we encounter a call to an effectful function, we create its coroutine
  state machine and call its `coro_step` function with `AnswerType::Start`. If
  it returns `CoroutineState::Yielded`, we propagate the yield to the caller.
  If it returns `CoroutineState::Complete`, we continue executing the outer
  coroutine.
2. After the initial `coro_step` call, the outer coroutine repeatedly polls the
  inner coroutine with `handler_answer`. Each yielded value is propagated to
  the caller. Once the inner coroutine completes, execution continues in the
  outer coroutine.

#### Conclusion on Lowering Coroutines

This demonstrates the central idea behind lowering a CFG with yield
instructions into a `coro_step` function. Two details remain to be addressed:

1. We must clean up the state machine if the coroutine is dropped before it
  completes. In addition to `coro_step`, we will generate a `coro_drop`
  function that dispatches on the current state and drops every live variable.
  Partially moved or initialized variables make this logic more involved, but
  the same basic approach still applies.
2. We must generate a space-efficient `Frame` struct capable of storing the
  variables live at each yield point. The frame must lay out and align these
  variables correctly, and any frame values that require a stable address must
  remain pinned in memory.

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

The most difficult part of effect compilation is already done: we have a 
representation for suspended computations. The next step is to represent 
handlers. 

The handlers drive the execution of coroutines by repeatedly polling them and 
providing answers to their yielded values.

### Searching for the Right Handler

Once the handler polls the coroutine and receives a yielded value, it inspects
whether the yielded value is one that it can handle. If so, it dispatches the
yielded value to the appropriate handler function. If not, it propagates the
yielded value to the next handler in the stack.

### Shallow vs Deep Handler

Deep handler is the desired behavior: once the operation is intercepted by 
the handler, the handler implicitly wraps the continuation and continues to
intercept every subsequent operation. This is the behavior of most effect 
systems since it avoids the need for the programmer to reinstall the handler
after every operation. 

However, wrapping the handler around the continuation is not trivial in a 
systems programming language since handler might not outlive the continuation. 
For example, consider the following code:

```pnx
let mut escapingResume = None
scope:
    let mut number = 0
    let mut numberRef = &mut number

    do:
        Yield.yield(1)
        Yield.yield(2)
    
    with:
        handler Yield:
            def yield(val):
                *numberRef += val
                escapingResume = Some(resume)

# what if we resume the `escapingResume` continuation here assuming that we
# have a deep handler? The `numberRef` reference is dangling since the scope
# has already ended.
```

However, we wouldn't like to have shallow handlers either since it destroys
the ergonomics. We propose a compromise: when resuming a continuation under
a handler, the handler is automatically reinstalled since the handler is still
in scope. Technically, this is a shallow handler, but the handler is only 
reinstalled when the condition is safe.

[rust-async-fn-recursive]: https://blog.rust-lang.org/2024/03/21/Rust-1.77.0/#support-for-recursion-in-async-fn
[rust-pin-unpin]: https://blog.cloudflare.com/pin-and-unpin-in-rust/

