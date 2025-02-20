//! Target on array, indexing, loop, and mutable variable

use crate::compile_file;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...): int32;
}

public function main() {
    let arr = [1, 2, 3, 4, 5];
    let count = 5;

    let mutable sum = 0;
    let mutable i = 0;

    while (i < count)  {
        sum += arr.[i];
        i = i + 1;
    }

    printf(&"%d\0"->[0], sum);
}
"#;

#[test]
fn array_index() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "15");
}
