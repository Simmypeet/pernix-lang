use crate::compile_file;

const TUPLE_PACK: &str = r"
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
        return first.add(SumTuple[Rest]::sum(rest));
    }
}

public function main(): int32 {
    let myTuple = (1i32, 2i32, 3i32, 4i32, 5i32, 6i32);
    return myTuple.sum();
}
";

#[test]
fn tuple_pack() {
    let output = compile_file(TUPLE_PACK);

    assert_eq!(output.status.code(), Some(21));
}
