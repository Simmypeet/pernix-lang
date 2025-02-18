//! Target on success exit code return

use crate::compile_file;

const SOURCE: &str = r"
public function main(): int32 {
    return 0;
}
";

#[test]
fn exit_success() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
}
