//! Target external function call and string literal
//!
//! Assuming that `int` is 32-bit

use crate::compile_file;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32


public function main():
    unsafe scope:
        printf(&"Hello, World!\n\0"->[0])

"#;

#[test]
fn hello_world() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());

    let out = super::get_output_string(output.stdout);
    assert_eq!(out, "Hello, World!\n");
}
