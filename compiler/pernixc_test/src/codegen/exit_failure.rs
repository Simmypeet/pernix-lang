//! Target on failure exit code return

use crate::compile_file;

const SOURCE: &str = r#"
extern "C" {
    public function exit(code: int32);
}

public function main() {
    exit(1);
}
"#;

#[test]
fn exit_failure() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(1));
}
