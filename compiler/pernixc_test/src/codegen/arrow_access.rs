//! Targets on struct access, reference, and dereference

use crate::compile_file;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32


public struct Pair[T, U]:
    public first: T
    public second: U


public function main():
    let pair = Pair {
        first: 1,
        second: 2,
    }

    let pairRef = &pair

    unsafe scope:
        printf(&"%d\0"->[0], pairRef->first + pairRef->second)
"#;

#[test]
fn struct_access() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "3");
}
