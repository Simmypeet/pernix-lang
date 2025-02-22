//! Targeting the code generation of aggregate types with `Drop`
//! implementations.

use crate::compile_file_with;

const SOURCE: &str = r#"
using {Drop, Copy} from core;

extern "C" {
    public function printf(format: &uint8, ...): int32;
    public function scanf(format: &uint8, ...): int32;
}

public struct LoudDrop {
    private value: int32,
}

final implements Drop[LoudDrop] {
    public function drop(self: &mutable LoudDrop) {
        printf(&"Dropping %d\n\0"->[0], self->value);
    }
}

final implements Copy[LoudDrop] delete;

public struct Pair[T, U] {
    private first: T,
    private second: U,
}

public enum Option[T] {
    Some(T),
    None,
}

implements[T] Option[T] {
    public function unwrap(self: this): T {
        match (self) {
            case Some(value): return value,
            case None: panic,
        }
    }

    public function asRef(self: &this): Option[&T] {
        match (self) {
            case Some(value): return Option::Some(value),
            case None: return Option::None,
        }
    }

    public function asMutable(self: &mutable this): Option[&mutable T] {
        match (self) {
            case Some(value): return Option::Some(value),
            case None: return Option::None,
        }
    }
}

public function main() {
    let mutable inTuple = (LoudDrop { value: 0 }, LoudDrop { value: 0 });
    let mutable inArray = [LoudDrop { value: 0 }, LoudDrop { value: 0 }];
    let mutable inStruct = Pair {
        first: LoudDrop { value: 0 },
        second: LoudDrop { value: 0 },
    };

    let mutable inSome = Option::Some(LoudDrop { value: 0 });
    let mutable inNone = Option[LoudDrop]::None;

    scanf(&"%d %d %d %d %d %d %d\0"->[0],
        &mutable inTuple.0.value,
        &mutable inTuple.1.value,
        &mutable inArray.[0].value,
        &mutable inArray.[1].value,
        &mutable inStruct.first.value,
        &mutable inStruct.second.value,
        &mutable inSome.asMutable().unwrap()->value, 
    );
}
"#;

#[test]
fn drop() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1 2 3 4 5 6 7");
    });

    assert!(output.status.success());

    let stdout = String::from_utf8(output.stdout).unwrap();
    assert_eq!(
        stdout,
        "Dropping 7\nDropping 5\nDropping 6\nDropping 3\nDropping 4\nDropping \
         1\nDropping 2\n"
    );
}
