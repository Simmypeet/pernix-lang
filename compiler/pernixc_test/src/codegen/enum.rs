//! Target enum construction, access, match, and enum tag retrieval

use crate::compile_file_with;

const ENUM: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...): int32;
    public function scanf(format: &uint8, ...): int32;
}

public struct Pair {
    public first: int64,
    public second: int64,
}

/*
assume:
    char = int8
    short = int16
    int = int32
    long long = int64

*/

public enum Fizz {
    First(int8),
    Second(int16),
    Third(int32),
    Fourth(int64),
    Fifth(Pair),
}

implements Fizz {
    public function isFirst(self: &this): bool{
        match (self)  {
            case First(..): return true,
            ..: return false,
        }
    }
}

public function assert(value: bool) {
    printf(&"assertion called\n\0"->[0]);
    if (value) {
        printf(&"assertion passed\n\0"->[0]);
    } else {
        printf(&"assertion failed\n\0"->[0]);
    }
}

public function main(): int32 {
    let mutable mode: int32 = 0;
    let format = &"%d\0"->[0];

    scanf(format, &mutable mode);

    if (mode == 0 ) {
        let mutable num: int8 = 0;
        let format = &"%hhd\0"->[0];

        scanf(format, &mutable num);

        let fizz = Fizz::First(num);

        printf(&"scan %d\n\0"->[0], num);

        if (fizz.isFirst()) {
            return 0;
        } else {
            return 1;
        }
    }

    return 0;
}
"#;

#[test]
fn r#enum() {
    let output = compile_file_with(ENUM, |x| {
        x.write_stdin("0\n1\n");
    });

    assert_eq!(output.status.code(), Some(0));
}
