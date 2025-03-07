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
        printf(&"Dropping %d\n\0"->[0], self->value)


final implements Copy[LoudDrop] delete


public function takeTuple[T, M: tuple, L](a: (T, ...M, L)):
    pass


public function main():
    let mut nums = (
        LoudDrop { value: 0 },
        LoudDrop { value: 0 },
        LoudDrop { value: 0 },
        LoudDrop { value: 0 },
        LoudDrop { value: 0 },
    )

    scanf(&"%d %d %d %d %d\0"->[0], 
        &mut nums.0.value, 
        &mut nums.1.value, 
        &mut nums.2.value, 
        &mut nums.3.value, 
        &mut nums.4.value
    )

    takeTuple[.., .., LoudDrop](nums)
"#;

#[test]
fn drop_packed_tuple() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1 2 3 4 5\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "Dropping 1\nDropping 2\nDropping 3\nDropping 4\nDropping 5\n"
    );
}
