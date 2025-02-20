//! Target on reference/dereference, branching, and function call

use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...): int32;
    public function scanf(format: &uint8, ...): int32;
}

public function max['a](a: &'a int32, b: &'a int32): &'a int32 {
    if (*a > *b) {
        return a;
    }

    return b;
}

public function main() {
    let mutable a: int32 = 0;
    let mutable b: int32 = 0;

    scanf(&"%d %d\0"->[0], &mutable a, &mutable b);

    let maxRef = max(&a, &b);

    printf(&"the max is %d\0"->[0], *maxRef);
}
"#;

#[test]
fn max() {
    let output = compile_file_with(SOURCE, |output| {
        output.write_stdin("10 5\n");
    });

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "the max is 10");
}
