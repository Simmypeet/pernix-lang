use crate::compile_file_with;

const SOURCE: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32
    public function malloc(size: usize) -> *mut ()
    public function free(ptr: *mut ()) 


public function main():
    unsafe scope:
        // for int32
        // at the time of writing the test, we've not implemented `sizeof` intrinsic
        let intSize = 4

        let mut inputCount = 0i32
        scanf(&"%d\0"->[0], &inputCount)

        let mut nums = malloc(inputCount as usize * intSize) as *mut int32
        let end = nums + inputCount as isize
        
        scope:
            let mut begin = nums
            while begin < end:
                let mut num = 0i32
                scanf(&"%d\0"->[0], &num)

                *(begin as &mut int32) = num

                begin += 1

        scope:
            let mut begin = nums
            while begin < end:
                printf(&"%d \0"->[0], *(begin as &int32))
                begin += 1

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
        "1 2 4 8 16 32 "
    );
}
