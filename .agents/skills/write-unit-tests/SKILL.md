---
name: write-unit-tests
description: Write or update focused unit tests in the Pernix compiler codebase. Use when adding Rust test functions, covering a bug or feature with unit tests, or reviewing test clarity. Apply concise input, premise, and output contract comments especially to tests for pernixc_type and pernixc_solver; inspect and follow local conventions in other crates.
---

# Write Unit Tests

Write each test as a small behavioral specification. Follow nearby test helpers and
conventions, and make the scenario understandable without tracing its setup code.

## Workflow

1. Inspect the implementation, adjacent tests, and shared test helpers.
2. Identify one behavior per test, including the relevant edge or failure case.
3. For tests related to `pernixc_type` or `pernixc_solver`, express the case as
   `input`, `premise`, and `output` comments immediately above every `#[test]` or
   `#[tokio::test]` attribute.
4. Name the test after the observable behavior, not the implementation steps.
5. Arrange only the state needed by the premise, invoke the behavior once, and
   assert the complete relevant result.
6. Run the narrowest applicable test command, then `cargo +nightly fmt` and
   `cargo clippy` as required by the project code-style skill.

## Test Contract Comments

Apply this contract-comment style especially to test cases for definitions and
behavior in `pernixc_type` and `pernixc_solver`. Their type and solver operations
map naturally to explicit inputs, premises, and outputs.

Do not impose this format mechanically on tests in other crates. Inspect nearby
tests and use the crate's established style; adopt the contract comments only
when they clarify the behavior or local convention already supports them.

Use this exact order and lowercase labels:

```rust
// input: <value or operation supplied to the subject>
// premise: <facts, rules, or state assumed by the case>
// output: <expected value, constraints, side effects, or error>
#[tokio::test]
async fn subject_exhibits_behavior() {
    // ...
}
```

- Keep each line brief and use domain notation instead of restating Rust setup.
- Write `premise: {}` when the case needs no facts beyond normal initialization.
- Include all semantically relevant result parts in `output`, such as both a
  reduced value and emitted constraints.
- Name expected failures in `output`, for example `Overflow` or the diagnostic
  kind, rather than pretending the case returns a normal value.
- Align continuation lines beneath the comment value when a contract cannot fit
  clearly on one line:

```rust
// input: (&skolem(0) bool, &skolem(1) int32)
// premise: forall 'a, 'b. (&'a bool, &'b int32) =
//          (&'b uint64, &'a int32)
// output: (&skolem(1) uint64, &skolem(0) int32), {}
```

Treat the premise as the distinction between ambient setup and the rule that
makes the expected output valid. For solver tests, it normally lists predicates
inserted into `Premise`. For other units, use it for relevant initial state,
configuration, mocks, or invariants.

## Test Quality

- Keep one reason to fail per test. Split unrelated behaviors into separate cases.
- Prefer public or behavior-level entry points unless the unit specifically
  requires testing a private invariant.
- Reuse nearby constructors and helpers when they improve the scenario's signal.
- Assert exact values, errors, constraints, and side effects relevant to the
  contract; avoid assertions that only prove execution completed.
- Add regression cases that fail before the fix and pass after it.
- Do not add the three-line contract to helper functions that are not test cases.

Use `compiler/semantic/solver/src/reduction/test.rs` as the canonical example for
the contract-comment notation and placement.
