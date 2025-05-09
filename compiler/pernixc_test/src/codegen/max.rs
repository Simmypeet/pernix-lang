//! Target on reference/dereference, branching, and function call

use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public function max['a](a: &'a int32, b: &'a int32) -> &'a int32:
    if *a > *b:
        return a

    return b


public function main():
    unsafe scope:
        let mut a: int32 = 0
        let mut b: int32 = 0

        scanf(&"%d %d\0"->[0], &mut a, &mut b)

        let maxRef = max(&a, &b)

        printf(&"the max is %d\0"->[0], *maxRef)

"#;

#[test]
fn max() {
    let output = compile_file_with(SOURCE, |output| {
        output.write_stdin("10 5\n");
    });

    assert!(output.status.success());
    assert_eq!(super::get_output_string(output.stdout), "the max is 10");
}
