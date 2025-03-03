use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...);
    public function scanf(format: &uint8, ...);
}

public function main() {
    let mutable first = 0i32;

    scanf(&"%d\0"->[0], &mutable first);

    let negativeFirst = -first;

    // assuming int64 is long long
    let sextNeg = negativeFirst as int64;
    let sextPos = first as int64;

    printf(&"%lld %lld\n\0"->[0], sextNeg, sextPos);

    let mutable second = 0u32;

    scanf(&"%u\0"->[0], &mutable second);

    // assuming uint64 is unsigned long long
    let zext = second as uint64;

    printf(&"%llu\n\0"->[0], zext); 

    let mutable third = 0i32;
    scanf(&"%d\0"->[0], &mutable third);

    let mutable unsigned = third as uint32;
    third = -third;

    let sitofp = third as float32;
    let uitofp = unsigned as float32;

    printf(&"%.2f %.2f\n\0"->[0], sitofp, uitofp);

    let fpext = sitofp as float64;
    let fprtunc = uitofp as float32;

    printf(&"%.2f %.2f\n\0"->[0], fpext, fprtunc);
}
"#;

#[test]
fn cast() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1\n2\n3\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "-1 1\n2\n-3.00 3.00\n-3.00 3.00\n"
    );
}
