//! Target on array, indexing, loop, and mutable variable

use crate::compile_file;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32


public function main():
    let arr = [1, 2, 3, 4, 5]
    let count = 5

    let mut sum = 0
    let mut i = 0

    while i < count:
        sum += arr.[i]
        i = i + 1
    

    unsafe scope:
        printf(&"%d\0"->[0], sum)
"#;

#[test]
fn array_index() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
    assert_eq!(super::get_output_string(output.stdout), "15");
}
