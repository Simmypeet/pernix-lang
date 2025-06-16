---
applyTo: "**/*.rs"
---

# Working with HashMap and HashSet

When working with `HashMap` and `HashSet`, always use from the `pernixc_hash`
crate which has a re-export of the `HashMap` and `HashSet` types with custom
hasher and builder. The custom hasher is designed to be more efficient and
always has a consistent iteration order between runs, thus making it suitable
for testing and serialization.
