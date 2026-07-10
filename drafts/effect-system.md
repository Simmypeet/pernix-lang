# Effect System

## Summary

The effect system is based on row-polymorphic effect types. A function type
contains both its value type and the effects that may be performed while
evaluating it:

```pnx
A -> B \ Effects
```

The main design is close to [Koka's row-polymorphic][koka-row] effects (almost
an exact replication), with one important adaptation: an effect row contains 
named effect slots. Each slot has:

- a nominal label, used to invoke and handle the effect in source code; and
- an effect signature, which defines the operations available through that
  label.

For example:

```pnx
{ Throw: std.Throw[Error], Async: std.Async, IO: std.IO }
```

This row says that the computation may throw `Error`, perform async operations,
and perform I/O. The names `Throw`, `Async`, and `IO` are the labels of the
effect slots. The types `std.Throw[Error]`, `std.Async`, and `std.IO` are the
effect signatures.

Most rows do not need explicit labels. When the label is omitted, it is derived
from the effect signature name:

```pnx
{ std.Throw[Error], std.Async, std.IO }
```

is shorthand for:

```pnx
{ Throw: std.Throw[Error], Async: std.Async, IO: std.IO }
```

An explicit label is needed when the same effect signature appears more than
once in the same row.

## Terminology

**Effect signature**

An effect signature is a nominal type that declares a set of effect operations.
For example, `std.Yield` may declare an operation named `yield`, while
`std.Throw[Error]` may declare an operation named `throw`.

**Effect label**

An effect label is the source-level name of one effect slot in an effect row.
The label is what code uses to invoke and handle operations:

```pnx
YieldOne.yield(1)
```

Here `YieldOne` is the label.

**Effect slot**

An effect slot is one row field of the form:

```pnx
Label: EffectSignature
```

For example:

```pnx
YieldOne: std.Yield
```

**Effect row**

An effect row is a row of effect slots. It describes the effects a computation
may perform.

```pnx
{ Console: std.Console, Throw: std.Throw[Error] }
```

**Effect row variable**

An effect row variable stands for the unknown remainder of an effect row. It is
used for effect polymorphism:

```pnx
{ std.Console | E }
```

This means "at least `std.Console`, plus whatever effects are in `E`".

**Handler**

A handler provides an implementation for the operations of one effect slot. A
handler is selected by label, and the label determines which slot is removed
from the row.

## Effect Rows

An effect row is written with braces:

```pnx
{ std.Console, std.Throw[Error] }
```

Rows can be closed or open.

A closed row contains exactly the listed effect slots:

```pnx
{ std.Console, std.Throw[Error] }
```

An open row contains the listed effect slots plus an unknown tail:

```pnx
{ std.Console | E }
```

The row above means that the computation may perform console effects and any
effects described by `E`.

Rows are identified by labels, not only by effect signatures. Therefore these
two rows are different:

```pnx
{ Left: std.Yield, Right: std.Yield }
{ Yield: std.Yield }
```

The first row has two independent slots with the same signature. The second row
has one slot.

For the surface language, labels should be unique within a row. A row such as
this should be rejected or reported as ambiguous:

```pnx
{ X: std.Console, X: std.IO }
```

This keeps operation lookup and handler selection simple: `X.operation(...)`
always refers to exactly one slot.

## Multiple Slots With The Same Signature

The same effect signature can appear multiple times when each occurrence has a
different label:

```pnx
{ YieldOne: std.Yield, YieldTwo: std.Yield }
```

This is useful when one computation needs two independent instances of the same
effect interface. For example:

```pnx
pub def yieldMulti() -> Unit \ { YieldOne: std.Yield, YieldTwo: std.Yield }:
  YieldOne.yield(1)
  YieldTwo.yield(2)
  YieldOne.yield(3)
  YieldTwo.yield(4)
```

The labels distinguish the two `std.Yield` slots. A handler for `YieldOne`
does not handle operations performed through `YieldTwo`.

```pnx
pub def main() -> Unit \ { std.Console }:
  run:
    yieldMulti()

  with:
    handler YieldOne for std.Yield:
      def yield(v):
        Console.print("YieldOne: " + v)
        resume()

    handler YieldTwo for std.Yield:
      def yield(v):
        Console.print("YieldTwo: " + v)
        resume()
```

In most programs, an effect signature will appear only once in a row, so the
shorthand form is enough. Explicit labels are mainly for cases where a program
needs multiple independent handlers for the same signature.

## Invoking Effects

An operation call is resolved through the effect label:

```pnx
Throw.throw(error)
Console.print("hello")
```

If a function body calls `Console.print`, then the function's effect row must
contain a `Console` slot whose signature defines `print`.

Using shorthand, the function type can be written as:

```pnx
pub def sayHello() -> Unit \ { std.Console }:
  Console.print("hello")
```

which is equivalent to:

```pnx
pub def sayHello() -> Unit \ { Console: std.Console }:
  Console.print("hello")
```

## Handling Effects

A handler handles one label at a time:

```pnx
handler YieldOne for std.Yield:
  def yield(v):
    resume()
```

Informally, if a computation has this type:

```pnx
A \ { YieldOne: std.Yield | E }
```

then handling `YieldOne` removes that slot and leaves the remaining effects:

```pnx
A \ E
```

The handler body may perform its own effects. Those effects are added to the
resulting row. For example, if the handler prints to the console, then the
handled computation may still require `std.Console`.

Handlers are selected by label, not merely by effect signature. Given this row:

```pnx
{ YieldOne: std.Yield, YieldTwo: std.Yield }
```

a handler for `YieldOne` removes only `YieldOne`; the `YieldTwo` slot remains
unhandled until another handler handles it.

## Effect Polymorphism

Effect polymorphism is expressed with row variables. A higher-order function can
preserve the effects of a callback by including the callback's row variable in
its own effect row.

### Simple Example

A simple example is `map`, which applies a callback to the wrapped value of an
`Option`:

```pnx
pub def map[a, b, E](opt: Option[a], f: a -> b \ E) -> Option[b] \ E:
  return match opt:
    case None:      None
    case Some(x):   Some(f(x))
```

The effect row `E` is a variable that stands for the unknown effects of the
callback. Since `map` has the same effect row as the callback, `map` is
polymorphic in the callback's effects.

A more complex example is a function that performs effects itself while also
accepting a callback that may perform effects. Suppose we modify `map` so that
it prints the value being mapped before calling the callback.

### Mixing Effects from the Function and the Callback

The function signature becomes:

```pnx
pub def map[a, b, E](
  opt: Option[a], f: a -> b \ {std.Console | E}
) -> Option[b] \ {std.Console | E}:
  match opt:
    case None: return None
    case Some(x):
      Console.print("mapping: " + x)
      return Some(f(x))
```

Here, the callback `f` may perform console effects and any other effects in `E`.
A particular callback passed to `map` does not necessarily have to perform a
console effect; the row `{std.Console | E}` is simply an over-approximation of
the effects that `f` may perform.

In the typing rule described by the Koka paper, the effect row of `f` must
exactly match the effect row of `map`. This is why the effect row of `f` must
mention `std.Console`, and it allows type inference to work correctly.

We may want to investigate whether this restriction can be relaxed by allowing
the effect row of `f` to be `E` instead of `{std.Console | E}`. That would
change the signature of `map` to:

```pnx
pub def map[a, b, E](
  opt: Option[a], f: a -> b \ E
) -> Option[b] \ {std.Console | E}:
  ...
```

However, the following example demonstrates why this relaxation may not be a
good idea. Suppose the relaxed signature is allowed, and the caller passes a
callback that actually performs console effects:

```pnx
map(Some(1), x -> Console.print("mapping: " + x); x + 1)
```

The expression `x -> Console.print("mapping: " + x); x + 1` would have the
effect row `{std.Console | ?e}` for some inferred row variable `?e`. This is a
standard technique for making type inference work. If we instantiate the type
variable `E` in the signature of `map` with `{std.Console | ?e}`, then the
return type of `map` becomes
`Option[b] \ {std.Console | {std.Console | ?e}}`, or equivalently,
`Option[b] \ {std.Console, std.Console | ?e}`. The duplicated label is allowed
in our system, but it is not ideal: the caller of `map` would have to handle
the `std.Console` effect twice just to satisfy the type system.

## Trait/Instance Associated Effect Row

Inspired by [Flix's associated effects][flix-aef], we would like to allow a
trait to declare an associated effect row and let each instance provide the
concrete row. Our trait system is closer to OCaml's module system or explicit
dictionary passing than to Haskell's type-class system, but the idea of an
associated effect row from Flix still applies.

For example, a trait with an associated effect row could be written like this:

```pnx
pub trait Reader:
  pub eff Aef
  pub type Rd
  def read(self: &mut this.Rd, buf: 8mut [uint8]) -> Int32 \ this.Aef
```

Each instance implementation can then provide a concrete effect row:

```pnx
pub inst FileReader: Reader:
  pub eff Aef = {std.Fs}
  pub type Rd = File
  pub def read(self: &mut File, buf: 8mut [uint8]) -> Int32 \ {std.Fs}:
    ...
```

```pnx
pub inst NetworkReader: Reader:
  pub eff Aef = {std.Net}
  pub type Rd = TcpStream
  pub def read(self: &mut TcpStream, buf: 8mut [uint8]) -> Int32 \ {std.Net}:
    ...
```

This looks good at first, but the following example shows a limitation of this
approach when the associated effect row is used in a higher-order function:

```pnx
pub def readTwo[inst I: Reader, inst J: Reader](
  r1: &mut I.Rd, r2: &mut J.Rd, buf: 8mut [uint8]
) ->  Int32 \ ???
```

What should we put in the effect row of `readTwo`? We want to express something
like `I.Aef + J.Aef`, but the row-polymorphic effect system has no direct way
to express that. Row extension only allows one row variable in the tail, so we
could not write something like `{std.Console | I.Aef | J.Aef}`.

This is where we lose composability in the row-polymorphic effect system.

### Or Multiple Parameter Traits

One possible solution is to rewrite the trait so that it takes an additional 
effect row parameter:

```pnx
pub trait Reader[E]:
  pub type Rd
  def read(self: &mut this.Rd, buf: 8mut [uint8]) -> Int32 \ E
```

```pnx
pub inst FileReader[R]: Reader[{std.Fs | R}]:
  pub type Rd = File
  pub def read(self: &mut File, buf: 8mut [uint8]) -> Int32 \ {std.Fs | R}:
    ...
```

```pnx
pub inst NetworkReader[Y]: Reader[{std.Net | Y}]:
  pub type Rd = TcpStream
  pub def read(self: &mut TcpStream, buf: 8mut [uint8]) -> Int32 \ {std.Net | Y}:
    ...
```

```pnx
pub def readTwo[E, inst I: Reader[E], inst J: Reader[E]](
  r1: &mut I.Rd, r2: &mut J.Rd, buf: 8mut [uint8]
) ->  Int32 \ E:
  ...
```

This works. Suppose we want to instantiate `readTwo` with `FileReader` and 
`NetworkReader`; the instantiation would look like this:

```pnx
readTwo[{std.Fs, std.Net}, FileReader[{std.Net}], NetworkReader[{std.Fs}]]
```

`FileReader[{std.Net}]` does not mean that `FileReader` itself uses `std.Net`.
It means that the effect row parameter `R` in `FileReader[R]` is instantiated
with `{std.Net}`. As a result, the trait signature of `FileReader[{std.Net}]`
becomes `Reader[{std.Fs | {std.Net}}]`, or equivalently
`Reader[{std.Fs, std.Net}]`. The same applies to `NetworkReader[{std.Fs}]`.

### Is This The Best Approach?

The initial promise of an associated effect row is that it lets a trait leave an
effect row abstract and have the instance implementation fill it in. Associated
effect rows support that use case, but as the example above shows, they limit
the composability of effect rows. The alternative approach of adding effect row
parameters to the trait restores composability, but it also makes the trait
signature more complex and less elegant.

This is an interesting open problem, and we may want to investigate it further
in the future.

## Formal Grammar and Rules

This section adapts Koka's core grammar and typing rules to summarize the
central ideas behind our effect system. The [Koka paper][koka-row] and
[Leijen's Scoped Labels][leijen-scoped-labels] are the main references for the 
formal presentation of effect rows and their typing rules.

We'll simply show the Type and Kind grammars, Row Equivalence rules, and the 
core typing rules just enough to explain the main ideas. The unification and
type inference rules are omitted here but are described in those references.

### Basic Grammar

The following is the portion of the kind grammar that is relevant to effects.
The language has other kinds, but they are omitted here.

```
Kind ::= EffSig | EffRow
```

The main constructors for effect signatures and effect rows are:

```
Label ::= Identifier

EffSigConstructor :: EffSig
EffSigConstructor ::= std.Throw[Error] | std.Yield | std.Console | ...

EffRowExtension :: EffSig -> EffRow -> EffRow
EffRowExtension ::= { Label: _ | _ } 

EffRowEmpty :: EffRow
EffRowEmpty ::= {}
```

We also have inference row variables and associated effect-row kinds. They are
straightforward extensions of this grammar and are omitted here.

### Open or Closed Rows

An `EffRow` is **open** when it has a tail variable and **closed** when it does
not. A tail variable can be either a polymorphic effect-row variable or an
associated effect-row variable. For example, the following are open rows:

```
{ std.Console | E }
{ std.Console | I.Aef }
E
```

In contrast, the following are closed rows:

```
{ std.Console | {} }
{}
```

### Row Equivalence Rules

The row equivalence rules are esssentially the same as 
[Leijen's Scoped Labels][leijen-scoped-labels]. In essence, the row allows 
duplicate labels, and the order of the labels does not matter but the order
between the equal labels does matter. For instance, the following three
rows are equivalent:

```
{ X: std.Console, Y: std.IO, X: std.Console }
{ Y: std.IO, X: std.Console, X: std.Console }
{ X: std.Console, X: std.Console, Y: std.IO }
```

However, these two rows aren't equivalent:

```
{ X: std.Console  }
{ X: std.Console, X: std.Console }
```

And also these two rows aren't equivalent:

```
{ Y: std.Console, Y: std.IO }
{ Y: std.IO, Y: std.Console }
```

### Core Typing Rules

The following typing rules summarize the core ideas behind effect rows. The
[Koka paper][koka-row] provides the complete formal presentation.


### Expression Typing Rule With Effect

We use the following notable expression-typing rules from Koka to explain the
core ideas and relate them to our language. Our judgment has the form
$\Gamma \vdash e : \tau \ | \ \epsilon$, read as: "under the context $\Gamma$,
expression $e$ has type $\tau$ and effect row $\epsilon$." Throughout this
section, $\tau$ denotes types, $\epsilon$ denotes effect rows, $e$ denotes
expressions, and $\Gamma$ denotes a typing context.

#### (VAR) Variable Typing Rule

$$
\frac{
  \Gamma(\text{x}) = \tau
}{
  \Gamma \vdash \text{x} : \tau \ | \  \epsilon
}
$$

The key idea is that a variable expression may be assigned any effect row
$\epsilon$. For example, if `x` has type `Int32`, the expression `x` can have
an effect row such as `{std.Console}`, `{std.IO}`, or the open row
`{std.Console | E}`.

#### (SEQ) Sequencing Typing Rule

$$
\frac{
  \Gamma \vdash e_1 : \tau_1 \ | \ \epsilon
  \quad
  \Gamma, x: \tau_1 \vdash e_2 : \tau_2 \ | \ \epsilon
}
{
  \Gamma \vdash x \leftarrow e_1; e_2 : \tau_2 \ | \ \epsilon
}
$$

This rule captures variable declaration and sequencing. Both $e_1$ and $e_2$
must have the same effect row $\epsilon$. This is not as restrictive as it may
appear: other rules can extend an effect row, including (VAR) and the
(CLOSED-APP) rule below.

#### (CLOSED-APP) Closed-Effect-Row Application Typing Rule

This rule deals with calling a function that has a closed effect row.

The following auxiliary rule extends a closed effect row with another effect
row. We write it as $\epsilon_1 + \epsilon_2 \ | \ \epsilon_3$, read as:
"given the closed effect row $\epsilon_1$ and another effect row $\epsilon_2$,
the resulting effect row is $\epsilon_3$."

$$
\frac{}
{\langle\rangle + \epsilon \ | \ \epsilon}

\quad

\frac{\epsilon_1 + \epsilon_2 \ | \ \epsilon_3}
{
  \langle \text{Label}: \text{EffSig} \ | \ \epsilon_1 \rangle + \epsilon_2
  \ | \ \langle \text{Label}: \text{EffSig} \ | \ \epsilon_3 \rangle
}
$$

$\tau_1 \rightarrow \tau_2 \ \epsilon$ denotes a function type that accepts an
argument of type $\tau_1$, returns a value of type $\tau_2$, and may perform the
effects described by $\epsilon$. The closed-effect-row application rule is:

$$
\frac{
  \Gamma \vdash e_1 : \tau_1 \to \tau_2 \ \epsilon_1 \ | \ \epsilon_3
  \quad
  \Gamma \vdash e_2 : \tau_1 \ | \ \epsilon_3
  \quad 
  \epsilon_1 \text{ is closed}
  \quad
  \epsilon_1 + \epsilon_2 \ | \ \epsilon_3
} {
  \Gamma \vdash e_1(e_2) : \tau_2 \ | \ \epsilon_3
}
$$

Intuitively, if a function may perform the closed effect row $\epsilon_1$, its
effects can be extended with an arbitrary effect row $\epsilon_2$ to obtain
$\epsilon_3$.

For example, consider two calls sequenced with (SEQ): `f` has the closed effect
row `{std.Console}`, and `g` has the closed effect row `{std.Yield}`. Because
(SEQ) requires both expressions to have the same effect row, the calls in
`let x = f(...); g(...)` would otherwise be rejected. With (CLOSED-APP), we can
extend `f`'s row to `{std.Console, std.Yield}` and `g`'s row to
`{std.Yield, std.Console}`. The sequenced calls are therefore well typed.

#### (OPEN-APP) Open-Effect-Row Application Typing Rule

This rule applies when calling a function with an open effect row:

$$
\frac{
  \Gamma \vdash e_1 : \tau_1 \to \tau_2 \ \epsilon \ | \ \epsilon
  \quad
  \Gamma \vdash e_2 : \tau_1 \ | \ \epsilon
  \quad
  \epsilon \text{ is open}
} {
  \Gamma \vdash e_1(e_2) : \tau_2 \ | \ \epsilon
}
$$

Here, all effect rows are the same $\epsilon$. Because $\epsilon$ is open, it
cannot be extended with another effect row. This contrasts with (CLOSED-APP),
which can extend a closed effect row with another effect row.

The following example illustrates the limitation of the open-effect-row
application rule. As in the previous example, `f` and `g` are called in
sequence with (SEQ). This time, `f` has the open effect row
`{std.Console | E}`, and `g` has the open effect row `{std.Yield | E}`. This
does not type check.

#### (HANDLER) Effect Handler Typing Rule

This rule describes handling an effect:

$$
\frac{
  \Gamma \vdash e_1 : \tau \ | \ \langle \text{Label}: \text{EffSig} \ | \ \epsilon \rangle
  \quad
  \Gamma \vdash e_2 : \tau \ | \ \epsilon
} { 
  \Gamma \vdash \text{handle } e_1 \text{ with } \text{Label} : \text{EffSig}\ e_2 \ \tau \ | \ \epsilon
}
$$

$e_1$ is the expression that may perform the effect, and $e_2$ is the handler
for that effect.

The rule removes the handled effect slot from the effect row of the expression
being handled. The resulting row contains the remaining effects.

#### (OP) Effect Operation Typing Rule

This rule describes invoking an effect operation:

$$
\frac{
  \Gamma \vdash e : \tau \ | \ \langle \text{Label}: \text{EffSig} \ | \ \epsilon \rangle
} {
  \Gamma \vdash \text{Label}.\text{operation}(e) : \tau \ | \ \langle \text{Label}: \text{EffSig} \ | \ \epsilon \rangle
}
$$

## Flix's Set Formula

Flix provides an alternative approach to effect systems through a concept called
a **set formula**. In Koka, an effect is represented as a row of effect slots.
In Flix, an effect is represented as a set of effect labels. The expressive
part of Flix's effect system is that it supports set operations on those labels,
such as union, intersection, and even difference. This means the problematic
example from the trait/instance associated effect row section can be expressed
naturally in Flix using set union:

```flix
def readTwo[I: Reader, J: Reader](
  r1: &mut I.Rd, r2: &mut J.Rd, buf: 8mut [uint8]
) -> Int32 \ I.Aef + J.Aef:
  ...
```

Flix uses **boolean algebra** to reason about set formulas, with a **boolean
unification** algorithm at the heart of its type inference. However, one major
limitation of Flix's approach is that it does not support parameterized effect
signatures, so an effect signature such as `std.Throw[Error]` cannot be
expressed in Flix. It is also unclear how this approach would interact with
subtyping and region variance. For example, how should boolean unification
handle `std.Throw[&'a str]` and `std.Throw[&'b str]`?

[flix-aef]: https://dl.acm.org/doi/epdf/10.1145/3656393
[koka-row]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/koka-effects-2013.pdf
[leijen-scoped-labels]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf