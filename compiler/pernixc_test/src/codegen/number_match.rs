use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C" {
    public function printf(format: &uint8, ...): int32;
    public function scanf(format: &uint8, ...): int32;
}

public function main() {
    let mutable input = 0i32;
    scanf(&"%d\0"->[0], &input);

    match (input) {
        0: printf(&"this is zero\0"->[0]),
        1: printf(&"this is one\0"->[0]),
        2: printf(&"this is two\0"->[0]),
        3: printf(&"this is three\0"->[0]),
        -1: printf(&"this is negative one\0"->[0]),
        a: printf(&"%d is something else\0"->[0], a),
    }
}
"#;

#[test]
fn number_match() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("0\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "this is zero"
    );

    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("1\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "this is one"
    );

    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("2\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "this is two"
    );

    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("3\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "this is three"
    );

    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("-1\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "this is negative one"
    );

    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("123456\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "123456 is something else"
    );
}
