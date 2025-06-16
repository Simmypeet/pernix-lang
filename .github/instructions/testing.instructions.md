---
applyTo: "**/*.rs"
---

# Testing Module

When creating a test, always create a new submodule in the current module
and named it `test`.

## Example

```rust
fn function() -> { ... }

// create a test module file
#[cfg(test)]
mod test;
```

# Naming Symbols in Test Module

Naming symbols in the test module or any module in general should always avoid
repeating the name of the module itself. For example, `test_function`,
`TestBasicStruct`, `TestSomeEnum`, etc. are not acceptable names. Instead,
remove the `test_*` or `Test*` prefix.

# Snapshot Testing with Insta

If there's any need for snapshot testing with the `insta` crate, for the first
run, you should always run with `INSTA_FORCE_PASS=1 cargo test --no-fail-fast`
so that the snapshots are created. After that, you can run `cargo insta review`
where I will review the snapshots and approve them if they are correct. From
there on, you can look at the accepted and rejected snapshots in the terminal
or in the `snapshots` directory.
