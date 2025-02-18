//! Target on array, indexing, loop, and mutable variable

use crate::compile_file;

const SOURCE: &str = r"
public function main(): int32 {
    let arr = [1, 2, 3, 4, 5];
    let count = 5;

    let mutable sum = 0;
    let mutable i = 0;

    while (i < count)  {
        sum += arr.[i];
        i = i + 1;
    }

    return sum;
}
";

#[test]
fn array_index() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(15));
}
