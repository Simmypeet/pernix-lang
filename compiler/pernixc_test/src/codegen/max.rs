//! Target on reference/dereference, branching, and function call

use crate::compile_file;

const SOURCE: &str = r"
public function max['a](a: &'a int32, b: &'a int32): &'a int32 {
    if (*a > *b) {
        return a;
    }

    return b;
}

public function main(): int32 {
    let a = 0;
    let b = 10;

    let maxRef = max(&a, &b);

    return *maxRef;
}
";

#[test]
fn max() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(10));
}
