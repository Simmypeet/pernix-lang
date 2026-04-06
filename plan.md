# Update New Qualified Identifier Syntax

Plan for updating the new syntax for the qualified identifier (e.g. `module::option::Option::Some`, etc.).

From now on, we'll simply use dot (`.`) to connect between identifiers instead of double-colon (`::`). So the syntax will now look something like this `module.option.Option.Some`.
This is to reduce syntax clutter in the programming language.

However, the important challenge is that it will collide with the struct member access syntax (e.g., `myClass.fieldName`).

Base on my current understanding, the ambiguity only appears inside the function where the struct member access syntax can occur.

The ambiguity handling should happen in the `pernixc_bind` crate, where we get more context about what symbol means.

My rought proposal is that, if we see qualified identifier use as an expression, we can't immediately conclude that it's a qualified identifier expession (like referring to a constant in a
module like `module.math.PI`). We should first see if the first segment of the qualified identifier refers to any local variables, if it is, we'll treat them as a field access.

I also want you to update many test suites my recommendation is try run all the test using `--release` mode as some test suites take lots of time.

