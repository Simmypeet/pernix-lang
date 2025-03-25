use crate::compile_file;

const SOURCE: &str = r"
from core import sizeof

public function assert(condition: bool):
    if not condition:
        panic

public function main():
    assert(sizeof[uint8]() == 1)
    assert(sizeof[uint16]() == 2)
    assert(sizeof[uint32]() == 4)
    assert(sizeof[uint64]() == 8)
    assert(sizeof[int8]() == 1)
    assert(sizeof[int16]() == 2)
    assert(sizeof[int32]() == 4)
    assert(sizeof[int64]() == 8)
    assert(sizeof[usize]() == sizeof[*mut ()]())
    assert(sizeof[isize]() == sizeof[*mut ()]())
    assert(sizeof[bool]() == 1)
    assert(sizeof[float32]() == 4)
    assert(sizeof[float64]() == 8)
    assert(sizeof[()]() == 0)

";

#[test]
fn number_match() {
    let output = compile_file(SOURCE);

    assert!(output.status.success());
}
