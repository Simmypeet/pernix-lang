---
name: code-style
description: The general coding style for the project. This includes the general API design guide and acceptance criterias
---

# Prefer Private Fields and Constructors over Public Ones

**Rationale**: This hides the implementation details as well as allows us to
change the object construction logic without breaking the API.

# Prefer Delegation Methods over Exposing Getters

**Don't** Expose getters to the internal object just to allow users to call
methods on it (e.g. `get_generic_parameters().get_parameter("T")`).

**Rationale**: To reduce as much exposure of the internal object as possible.
This allows us to change the internal object without breaking the API. It also
allows us to add additional logic in the delegation method if needed (e.g. error
handling, logging, etc.).

# Avoid Wildcard Matching in `match`

**Don't** Use wildcard matching in `match` statements (e.g. `match x { _ => ...
}`) for our own created enums (of course, it's fine to use it for external enums
that we don't control).

**Rationale**: This makes the compiler helps notify us when there's a new
variant added to the enum and where we need to handle it.

# Always run `cargo clippy`

**Rationale**: Clippy can catch many common programming mistakes. And the CI
will fail if there are any warnings from Clippy, so it's better to run it locally
before pushing code.

# Always run `cargo +nightly fmt`

**Rationale**: Make the code looks consistent. Again, the CI will fail if the
code is not formatted properly.
