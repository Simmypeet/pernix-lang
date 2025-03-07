//! Target on success exit code return

use crate::compile_file;

const SOURCE: &str = r"
public function main():
    pass
";

#[test]
fn exit_success() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
}
