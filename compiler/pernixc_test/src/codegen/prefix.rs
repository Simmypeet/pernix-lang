use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...);
    public function scanf(format: &uint8, ...);
}

public function main() {
    let mutable first = 0i32;
    let mutable second = 0i32;
    let mutable third = 0i32;
    let mutable fourth = 0i32;

    scanf(&"%d %d %d %d\0"->[0], 
        &mutable first, 
        &mutable second, 
        &mutable third,
        &mutable fourth
    );

    printf(&"%d %d\n\0"->[0], -first, -second);

    let thirdIsNegative = third < 0;
    let fourthIsNegative = fourth < 0;

    if (!thirdIsNegative) {
        printf(&"Third number is positive\n\0"->[0]);
    } else {
        printf(&"Third number is negative\n\0"->[0]);
    }

    if (!fourthIsNegative) {
        printf(&"Fourth number is positive\n\0"->[0]);
    } else {
        printf(&"Fourth number is negative\n\0"->[0]);
    }
}
"#;

#[test]
fn prefix() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1 -2 3 -4\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        "-1 2\nThird number is positive\nFourth number is negative\n"
    );
}
