//! Target the borrow of the zst type; it should create a non-null, aligned
//! pointer

use crate::compile_file;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32


public enum Option[T]:
    Some(T)
    None


public function main():
    unsafe scope:
        let value = ()
        let option = Option::Some(&value)

        match option:
            case Some(value):
                printf(&"is some\n\0"->[0])

            case None:
                printf(&"is none\n\0"->[0])
"#;

#[test]
fn zst_borrowing() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
    assert_eq!(super::get_output_string(output.stdout), "is some\n");
}
