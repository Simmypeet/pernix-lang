use crate::compile_file;

const TUPLE_UNPACK: &str = r"
public function add[T](a: T): (int32, ...T, int32)
where
    tuple T 
{
    return (1, ...a, 5);
}

public function main(): int32 {
    let test = add((2, 3, 4));

    if (test.0 != 1) {
        return 1;
    }

    if (test.1 != 2) {
        return 2;
    }

    if (test.2 != 3) {
        return 3;
    }

    if (test.3 != 4) {
        return 4;
    }

    if (test.4 != 5) {
        return 5;
    }

    return test.0 + test.1 + test.2 + test.3 + test.4;
}
";

#[test]
fn tuple_unpack() {
    let output = compile_file(TUPLE_UNPACK);

    assert_eq!(output.status.code(), Some(15));
}
