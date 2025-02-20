use crate::compile_file_with;

const TUPLE_UNPACK: &str = r#"
extern "C" {
    public function scanf(format: &uint8, ...): int32;
    public function exit(code: int32);
}

public function add[T](a: T): (int32, ...T, int32)
where
    tuple T 
{
    return (1, ...a, 5);
}

public function main() {
    let mutable nums = (0i32, 0i32, 0i32);

    scanf(&"%d %d %d\0"->[0], 
        &mutable nums.0, 
        &mutable nums.1, 
        &mutable nums.2
    );

    let test = add(nums);

    if (test.0 == 1 
        and test.1 == nums.0 
        and test.2 == nums.1 
        and test.3 == nums.2 
        and test.4 == 5) {
        exit(0);
    } else {
        exit(1);
    }
}
"#;

#[test]
fn tuple_unpack() {
    let output = compile_file_with(TUPLE_UNPACK, |x| {
        x.write_stdin("1 2 3\n");
    });

    assert!(output.status.success());
}
