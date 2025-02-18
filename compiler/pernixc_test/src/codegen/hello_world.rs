//! Target external function call and string literal
//!
//! Assuming that `int` is 32-bit

use crate::compile_file;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...): int32;
}

public function main(): int32 {
    printf(&"Hello, World!\n\0"->[0]);

    return 0;
}
"#;

#[test]
fn hello_world() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());

    let out = String::from_utf8(output.stdout).unwrap();
    assert_eq!(out, "Hello, World!\n");
}
