//! Target `NoDrop` intrinsic implementation.
use crate::compile_file_with;

const SOURCE: &str = r#"
from core import Drop, Copy, NoDrop

extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public struct LoudDrop:
    private value: int32


final implements Drop[LoudDrop]:
    function drop(self: &mut LoudDrop):
        unsafe scope:
            printf(&"Dropping %d\n\0"->[0], self->value)


final implements Copy[LoudDrop] delete


public function main():
    let mut first = NoDrop {
        value: LoudDrop { value: 0 }
    } 
    let mut second = NoDrop {
        value: LoudDrop { value: 0 }
    } 

    unsafe scope:
        scanf(&"%d %d\0"->[0], &mut first.value.value, &mut second.value.value)
    
"#;

#[test]
fn drop() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("12 34\n");
    });

    assert!(output.status.success());

    let stdout = String::from_utf8(output.stdout).unwrap();
    assert!(stdout.is_empty());
}
