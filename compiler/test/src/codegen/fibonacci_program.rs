//! Target on extern function, function call, recursion, branching, and string
//! literal
//!
//! Assuming that `int` is 32-bit

use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public function fibonacci(n: int32) -> int32:
    if n <= 1:
        return 1
    
    let mut prev = 0
    let mut curr = 1
    let mut next = 0

    let mut i = 2
    while i <= n:
        next = prev + curr  // Compute the next Fibonacci number
        prev = curr         // Move prev forward
        curr = next         // Move curr forward

        i += 1
    
    
    return curr


public function main():
    unsafe scope:
        printf(&"Enter the number: \0"->[0])

        let mut number = 0
        scanf(&"%d\0"->[0], &mut number)

        let result = fibonacci(number)
        printf(&"fibonacci(%d) = %d\n\0"->[0], number, result)

"#;

#[test]
fn fibonacci_program() {
    let output = compile_file_with(SOURCE, |cmd| {
        cmd.write_stdin("6\n");
    });

    assert!(output.status.success());

    let out = super::get_output_string(output.stdout);

    assert_eq!(out, "Enter the number: fibonacci(6) = 8\n");
}
