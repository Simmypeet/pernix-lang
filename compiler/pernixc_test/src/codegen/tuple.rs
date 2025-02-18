//! Target on the tuple literal and tuple inedxing

use crate::compile_file;

const SOURCE: &str = r"
public struct Zst {}

public function main(): int32 {
    let test = (1, (), 2, Zst{}, 3, (), 4);

    return test.0 + test.2 + test.-2 + test.-0;
}
";

#[test]
fn struct_access() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(10));
}
