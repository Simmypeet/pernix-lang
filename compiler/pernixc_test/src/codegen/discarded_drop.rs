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

public struct ZstLoudDrop {}

final implements Drop[ZstLoudDrop] {
    public function drop(self: &mutable ZstLoudDrop) {
        printf(&"Dropping ZST\n\0"->[0]);
    }
}

final implements Copy[ZstLoudDrop] delete;

public function createLoudDrop(value: int32): LoudDrop {
    let loudDrop = LoudDrop { value: value };
    return loudDrop;
}

public function createZstLoudDrop(): ZstLoudDrop {
    let zstLoudDrop = ZstLoudDrop {};
    return zstLoudDrop;
}

public function main() {
    let mutable value = 0i32;
    scanf(&"%d\0"->[0], &mutable value);
    LoudDrop { value: value };

    scanf(&"%d\0"->[0], &mutable value);
    createLoudDrop(value);

    ZstLoudDrop {};
    createZstLoudDrop();
}
"#;

#[test]
fn drop() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("12\n34\n");
    });

    assert!(output.status.success());

    let stdout = String::from_utf8(output.stdout).unwrap();
    assert_eq!(
        stdout,
        "Dropping 12\nDropping 34\nDropping ZST\nDropping ZST\n"
    );
}
