//! Targeting the drop implementation type having its fields also implementingo
//! the `Drop` trait.

use crate::compile_file_with;

const SOURCE: &str = r#"
from core import Drop, Copy

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


public struct Pair[T, U]:
    private first: T
    private second: U


implements[T, U] Drop[Pair[T, U]]:
    function drop(self: &mut Pair[T, U]):
        unsafe scope:
            printf(&"Dropping Pair\n\0"->[0])


final implements[T, U] Copy[Pair[T, U]] delete


public enum LoudEnum:
    GtZero(LoudDrop)
    LtZero(LoudDrop)
    Zero


implements Drop[LoudEnum]:
    function drop(self: &mut LoudEnum):
        unsafe scope:
            match (self):
                case GtZero(..): printf(&"Dropping GtZero\n\0"->[0])
                case LtZero(..): printf(&"Dropping LtZero\n\0"->[0])
                case Zero:       printf(&"Dropping Zero\n\0"->[0])


final implements Copy[LoudEnum] delete


public function createLoudEnum() -> LoudEnum:
    let mut value = 0

    unsafe scope:
        scanf(&"%d\0"->[0], &mut value)

    if value > 0:
        return LoudEnum::GtZero(LoudDrop { value: value })

    else if value < 0:
        return LoudEnum::LtZero(LoudDrop { value: value })

    else:
        return LoudEnum::Zero


public function main():
    let mut pair = Pair {
        first: LoudDrop { value: 0 },
        second: LoudDrop { value: 0 },
    }

    unsafe scope:
        scanf(
            &"%d %d\0"->[0], 
            &mut pair.first.value, 
            &mut pair.second.value
        )
    
    let first = createLoudEnum()
    let second = createLoudEnum()
    let third = createLoudEnum()
"#;

#[test]
fn nested_drop() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1 2\n3\n-4\n0\n");
    });

    assert!(output.status.success());

    let stdout = String::from_utf8(output.stdout).unwrap();
    assert_eq!(
        stdout,
        "Dropping Zero\nDropping LtZero\nDropping -4\nDropping \
         GtZero\nDropping 3\nDropping Pair\nDropping 1\nDropping 2\n"
    );
}
