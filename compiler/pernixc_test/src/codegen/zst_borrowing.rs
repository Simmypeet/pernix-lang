use crate::compile_file;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...): int32;
}

public enum Option[T] {
    Some(T),
    None,
}

public function main() {
    let value = ();
    let option = Option::Some(&value);

    match (option) {
        case Some(value): {
            printf(&"is some\n\0"->[0]);
        },
        case None: {
            printf(&"is none\n\0"->[0]);
        }
    }
}
"#;

#[test]
fn drop() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "is some\n");
}
