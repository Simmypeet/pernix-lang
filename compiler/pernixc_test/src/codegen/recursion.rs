//! Target on function call, branching, and recursion

use crate::compile_file;

const SOURCE: &str = r"
public function fib(n: int32): int32 {
    if (n <= 1) { 
        return n;
    }

    return fib(n - 1) + fib(n - 2);
}

public function main(): int32 {
    return fib(6);
}
";

#[test]
fn recursion() {
    let output = compile_file(SOURCE);

    assert_eq!(output.status.code(), Some(8));
}
