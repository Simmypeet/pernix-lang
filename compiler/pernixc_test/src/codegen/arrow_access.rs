//! Targets on struct access, reference, and dereference

use crate::compile_file;

const SOURCE: &str = r"
public struct Pair[T, U] {
    public first: T,
    public second: U,
}

public function main(): int32 {
    let pair = Pair {
        first: 1,
        second: 2,
    };

    let pairRef = &pair;

    return pairRef->first + pairRef->second;
}
";

#[test]
fn struct_access() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(3));
}
