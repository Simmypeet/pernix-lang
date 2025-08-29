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

# End to End Testing

Testing funcionality related to diagnostics or semantic checking/analyzing are
better done via end to end test with snapshot testing. Simply, create a
directory with descriptive name stating which feature to testing for in the
"compiler/e2e/test/snapshot/" directory, then create a `main.pnx` file with the
source code that will trigger the diagnostic outputs. The whole compiler
procedure will be ran on the `main.pnx` file and diagnostics will be emitted
which we can validate and save as a snapshot later on.

```bash
cargo nextest r -p pernixc_e2e -- --skip regression
```
