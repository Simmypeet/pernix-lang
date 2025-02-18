//! Target on the Zero Sized Type (ZST) optimization

use crate::compile_file;

const SOURCE: &str = r"
public struct Zst {}

public function annoyingSum(
    a: int32,
    emptyTuple: (),
    b: int32,
    emptyStruct: Zst,
    c: int32,
): int32 {
    return a + b + c;
}

public function main(): int32 {
    return annoyingSum(1, (), 2, Zst {}, 3);
}
";

#[test]
fn zst_optimization() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(6));
}
