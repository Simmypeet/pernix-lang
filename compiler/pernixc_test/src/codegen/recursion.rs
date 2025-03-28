//! Target on function call, branching, and recursion

use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public function fib(n: int32) -> int32:
    if n <= 1:
        return n

    return fib(n - 1) + fib(n - 2)


public function main():
    unsafe scope:
        let mut input: int32 = 0

        scanf(&"%d\0"->[0], &mut input)

        let result = fib(input)

        printf(&"fib(%d) = %d\0"->[0], input, result)
"#;

#[test]
fn recursion() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("6\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "fib(6) = 8"
    );
}
