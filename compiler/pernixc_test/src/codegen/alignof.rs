use crate::compile_file;

const SOURCE: &str = r"
from core import alignof


public function assert(condition: bool):
    if not condition:
        panic


public function max(a: usize, b: usize) -> usize:
    if a > b:
        return a

    return b


public struct Pair[T, U]:
    public first: T
    public second: U


public function main():
    assert(alignof[uint8]() == 1)
    assert(alignof[int8]() == 1)
    assert(alignof[()]() == 0)
    assert(
        alignof[Pair[uint8, uint64]]() 
        == max(alignof[uint8](), alignof[uint64]())
    )


";

#[test]
fn number_match() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
}
