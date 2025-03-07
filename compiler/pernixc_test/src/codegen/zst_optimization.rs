//! Target on the Zero Sized Type (ZST) optimization

use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) ->int32 
    public function scanf(format: &uint8, ...) -> int32


public struct Zst:
    pass


public function annoyingSum(
    a: int32,
    emptyTuple: (),
    b: int32,
    emptyStruct: Zst,
    c: int32,
) -> int32:
    return a + b + c


public function main():
    let mut a = 0i32
    let mut b = 0i32
    let mut c = 0i32

    scanf(&"%d %d %d\0"->[0], &mut a, &mut b, &mut c)

    let sum = annoyingSum(a, (), b, Zst{}, c)

    printf(&"%d\0"->[0], sum)
"#;

#[test]
fn zst_optimization() {
    let output = compile_file_with(SOURCE, |output| {
        output.write_stdin("1 2 3\n");
    });

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "6");
}
