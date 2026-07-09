# Effect System

## Introduction

This document describes the effect system used in our programming language.
In short, the effect system is very simple and is based on the row polymorphism
or effect rows.

Inspired from these papers:

- [Extensible records with scoped labels](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf): 
  A brief introduction to row polymorphism and its "scoped labels" which are
  used to represent effects in our system.
- [Koka: Programming with Row-polymorphic Effect Types](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/koka-effects-2013.pdf):
  A real-world application of row polymorphism to effect systems in a 
  programming language. Our effect system is heavily inspired by Koka's 
  approach.

## Effect System Overview

In summary, our effect system is based on the Koka's Row-polymorphic Effect 
Types. The collection of effects is represented as a row type. In Koka, it
has the following syntax:

```
<io, exn, div>
```

In Koka, the effect constants, such as `io`, `exn`, and `div`, are used to 
represent the **labels** of the effect row. This makes each field of the effect
rows consists of only the **label**. This is in contrast to the traditional 
records types, where each field consists of a **label** and a **type** like:

```
{ name: String, age: Int }
```

We take the idea of "records with scoped labels" and Koka's effect rows to 
represent the effects in our programming language. Our modest modification is
that we allow each field of the effect row to have a **nominal label** and a
**effect type**. This is similar to the traditional record types, but we use
the term "effect type" instead of "type" to emphasize that the field represents
an effect rather than a value. The syntax of our effect rows is as follows:

```
{ Throw: std.Throw[Error], Async: std.Async, IO: std.IO, YieldOne: std.Yield, YieldTwo: std.Yield }
```

Of course, in the above example, the effect row becomes very long and verbose.
To address this, we have a syntactic sugar that allows us to omit the nominal
label and the label will be inferred from the name of the effect type. For 
example, the above effect row can be written as:

```
{ std.Throw[Error], std.Async, std.IO, YieldOne: std.Yield, YieldTwo: std.Yield }
```

In practice, we would expect that most of the time, omitting the nominal label
is sufficient and should rarely need to specify the nominal label.

In the above example, notice that there are two occurrences of the `std.Yield`
effect type with different nominal labels, `YieldOne` and `YieldTwo`. This is
also a feature that allows us to have multiple distinct handlers for the same
effect type. Conceptually, it allows us to do something like this:

```pnx
pub def yieldMulti() -> Unit \ {YieldOne: std.Yield, YieldTwo: std.Yield}:
  YieldOne.yield(1)
  YieldTwo.yield(2)
  YieldOne.yield(3)
  YieldTwo.yield(4)


pub def main():
  run:
    yieldMulti()

  with:
    handler YieldOne for std.Yield:
      def yield(v):
        print("YieldOne: " + v)
        resume()
    
    handler YieldTwo for std.Yield:
      def yield(v):
        print("YieldTwo: " + v)
        resume()
```

Of course, the practical use case of having multiple distinct handlers for the 
same effect type is not very common, and most of the time, we would expect that 
there is only one handler for each effect type.

## Effect Polymorphism

The effect polymorphism is very trivial in our effect system. We can have a
polymorphic effect row by using a type variable in the effect row. We give
some examples below:

```
impl[T] Option[T]:
    pub def mapAndSayHello[U, eff E](
        self: Option[T], 
        f: T -> U \ E
    ) -> Option[U] \ { std.Console | E }:
        match self:
            case Some(value):
                print("Hello!")
                Some(f(value))
            case None:
                None
```

Another example is:

```
impl[T, E] Result[T, E]:
    pub def mapAndSayHello[U, V, eff Ef](
        self: Result[T, E],
        ok: T -> U \ Ef,
        err: E -> V \ Ef
    ) -> Result[U, V] \ { std.Console | Ef }:
        match self:
            case Ok(value):
                print("Hello!")
                Ok(ok(value))
            case Err(error):
                print("Hello!")
                Err(err(error))
```

Of course, the above example requries both `ok` and `err` functions to have
the same effect row `Ef`. This can be a limitation in some cases, but remember
that type inference can always widen the effect row to include more effects.
For example, if `ok` function invokes the effect `std.Yield` and `err` function
invokes effect `std.Async` in the source code, the type inference will widen
the effect row of both `ok` and `err` functions to be `{ std.Yield, std.Async }`.

