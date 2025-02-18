//! Target on the struct literal and access

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

    let first = pair.first;
    let second = pair.second;

    return first + second;
}
";

#[test]
fn struct_access() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(3));
}
