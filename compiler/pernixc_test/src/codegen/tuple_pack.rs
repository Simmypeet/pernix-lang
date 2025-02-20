use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...): int32;
    public function scanf(format: &uint8, ...): int32;
}

public trait Add[T] {
    public function add(a: T, b: T): T;
}

implements Add[int32] {
    public function add(a: int32, b: int32): int32 {
        return a + b;
    }
}

public trait SumTuple[T]
where
    tuple T
{
    public type Output;

    public function sum(elements: T): this::Output;
}

implements[T] SumTuple[(T,)]
where
    trait Add[T]
{
    public type Output = T;

    public function sum(elements: (T,)): this::Output {
        return elements.0;
    }
}

implements[T, Rest] SumTuple[(T, ...Rest)]
where
    trait Add[T] + SumTuple[Rest],
    SumTuple[Rest]::Output = T,
    tuple Rest
{
    public type Output = T;

    public function sum((first, ...rest): (T, ...Rest)): this::Output {
        return first.add(rest.sum());
    }
}

public function main() {
    let mutable nums = (
        0i32,
        0i32,
        0i32,
        0i32,
        0i32,
        0i32,
    );

    scanf(&"%d %d %d %d %d %d\0"->[0], 
        &mutable nums.0,
        &mutable nums.1,
        &mutable nums.2,
        &mutable nums.3,
        &mutable nums.4,
        &mutable nums.5,
    );

    printf(&"%d\0"->[0], nums.sum());
}
"#;

#[test]
fn tuple_pack() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1 2 3 4 5 6\n");
    });

    assert!(output.status.success());
    assert_eq!(String::from_utf8(output.stdout).unwrap().as_str(), "21");
}
