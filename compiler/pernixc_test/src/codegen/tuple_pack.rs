use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public trait Add[T]:
    public function add(a: T, b: T) -> T


implements Add[int32]:
    function add(a: int32, b: int32) -> int32:
        return a + b


public trait SumTuple[T: tuple]:
    public type Output

    public function sum(elements: T) -> this::Output


implements[T: Add] SumTuple[(T,)]:
    type Output = T

    function sum(elements: (T,)) -> this::Output:
        return elements.0


implements[T: Add, Rest: SumTuple + tuple] SumTuple[(T, ...Rest)]:
    where:
        SumTuple[Rest]::Output = T

    type Output = T

    function sum((first, ...rest): (T, ...Rest)) -> this::Output:
        return first.add(rest.sum())


public function main():
    let mut nums = (
        0i32,
        0i32,
        0i32,
        0i32,
        0i32,
        0i32,
    )

    unsafe scope:
        scanf(&"%d %d %d %d %d %d\0"->[0], 
            &mut nums.0,
            &mut nums.1,
            &mut nums.2,
            &mut nums.3,
            &mut nums.4,
            &mut nums.5,
        )

        printf(&"%d\0"->[0], nums.sum())
"#;

#[test]
fn tuple_pack() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1 2 3 4 5 6\n");
    });

    assert!(output.status.success());
    assert_eq!(super::get_output_string(output.stdout).as_str(), "21");
}
