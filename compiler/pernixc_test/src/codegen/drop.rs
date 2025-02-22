//! Target custom drop implementation for `Drop` trait.
use crate::compile_file_with;

const SOURCE: &str = r#"
using {Drop, Copy} from core;

extern "C" {
    public function printf(format: &uint8, ...): int32;
    public function scanf(format: &uint8, ...): int32;
}

public struct LoudDrop {
    private value: int32,
}

final implements Drop[LoudDrop] {
    public function drop(self: &mutable LoudDrop) {
        printf(&"Dropping %d\n\0"->[0], self->value);
    }
}

final implements Copy[LoudDrop] delete;

public function main() {
    let mutable first = LoudDrop { value: 0 };
    let mutable second = LoudDrop { value: 1 };

    scanf(&"%d %d\0"->[0], &mutable first.value, &mutable second.value);
}
"#;

#[test]
fn drop() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("12 34\n");
    });

    assert!(output.status.success());

    let stdout = String::from_utf8(output.stdout).unwrap();
    assert_eq!(stdout, "Dropping 34\nDropping 12\n");
}
