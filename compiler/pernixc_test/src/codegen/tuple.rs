//! Target on the tuple literal and tuple inedxing

use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public struct Zst:
    pass


public function main():
    unsafe scope:
        let mut test = (0i32, (), 0i32, Zst{}, 0i32, (), 0i32)

        scanf(&"%d %d %d %d\0"->[0],
            &mut test.0,
            &mut test.2,
            &mut test.-2,
            &mut test.-0
        )

        printf(&"%d\0"->[0], test.0 + test.2 + test.-2 + test.-0)
"#;

#[test]
fn struct_access() {
    let output = compile_file_with(SOURCE, |output| {
        output.write_stdin("1 2 3 4\n");
    });

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "10");
}
