//! Target on the struct literal and access

use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public struct Pair[T, U]:
    public first: T
    public second: U


public function main():
    let mut pair = Pair {
        first: 0,
        second: 0,
    }

    unsafe scope:
        scanf(&"%d %d\0"->[0], &mut pair.first, &mut pair.second)

        printf(&"first: %d, second: %d\0"->[0], pair.first, pair.second)

"#;

#[test]
fn struct_access() {
    let output = compile_file_with(SOURCE, |output| {
        output.write_stdin("10 20\n");
    });

    assert!(output.status.success());
    assert_eq!(
        super::get_output_string(output.stdout),
        "first: 10, second: 20"
    );
}
