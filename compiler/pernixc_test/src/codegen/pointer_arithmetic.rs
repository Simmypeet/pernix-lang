use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32
    public function malloc(size: usize) -> *mut ()
    public function free(ptr: *mut ()) 


public function main():
    // for int32
    // at the time of writing the test, we've not implemented `sizeof` intrinsic
    let intSize = 4

    let mut inputCount = 0i32
    scanf(&"%d\0"->[0], &inputCount)

    let mut nums = malloc(inputCount as usize * intSize) as *mut int32
    
    unsafe scope:
        let mut i = 0
        while i < inputCount as isize:
            let mut num = 0i32
            scanf(&"%d\0"->[0], &num)

            *((nums + i) as &mut int32) = num

            i += 1

    unsafe scope:
        let mut i = 0
        while i < inputCount as isize:
            printf(&"%d \0"->[0], *((nums + i) as &int32))
            i += 1

        printf(&"\n\0"->[0])

    free(nums as *mut ())
"#;

#[test]
fn number_match() {
    let output = compile_file_with(SOURCE, |x| {
        x.write_stdin("6\n1\n2\n4\n8\n16\n32\n");
    });

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().as_str(),
        "1 2 4 8 16 32 \n"
    );
}
