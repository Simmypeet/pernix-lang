use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public function main():
    let mut first = 0i32

    scanf(&"%d\0"->[0], &mut first)

    let negativeFirst = -first

    // assuming int64 is long long
    let sextNeg = negativeFirst as int64
    let sextPos = first as int64

    printf(&"%lld %lld\n\0"->[0], sextNeg, sextPos)

    let mut second = 0u32

    scanf(&"%u\0"->[0], &mut second)

    // assuming uint64 is unsigned long long
    let zext = second as uint64

    printf(&"%llu\n\0"->[0], zext)

    let mut third = 0i32
    scanf(&"%d\0"->[0], &mut third)

    let mut unsigned = third as uint32
    third = -third

    let sitofp = third as float64
    let uitofp = unsigned as float64

    printf(&"%.2lf %.2lf\n\0"->[0], sitofp, uitofp)

    let fptrunc = uitofp as float32
    let fpnext = fptrunc as float64

    printf(&"%.2lf %.2lf\n\0"->[0], fpnext, -fpnext)

"#;

#[test]
fn cast() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1\n2\n3\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "-1 1\n2\n-3.00 3.00\n3.00 -3.00\n"
    );
}
