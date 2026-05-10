---
name: e2e-testing
description: The general approach for end to end testing in the project. Especially, when adding new features or diagnostics.
---

## Test Modules

- Always create a new submodule named `test` in the current module (e.g., `#[cfg(test)] mod test;`).
- Avoid repeating `test` or `Test` in symbol names within the test module (e.g., no `test_function` or `TestBasicStruct`, just use the bare descriptive name).

## Snapshot Testing

Generally, when adding new features or diagnostics, it's better to add an end-to-end
test with snapshot testing. This can be found under the `compiler/e2e/test/snapshot/`
directory.

- Define a directory with descriptive name for the feature being tested.
- Create a `main.pnx` file with source code that will trigger the diagnostic outputs.
- Run the `cargo nextest r -p pernixc_e2e -- --skip regression` command to execute the test and generate snapshots. (NOTE: It's recommended to skip regression tests as they can be time-consuming and often unproductive).
- Review and approve snapshots with `cargo insta review`. Approved snapshots will be saved alongside the test files for future reference.
