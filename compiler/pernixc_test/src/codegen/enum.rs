//! Target enum construction, access, match, and enum tag retrieval

use crate::compile_file_with;

const ENUM: &str = r#"
extern "C":
    public function printf(format: &uint8, ...) -> int32
    public function scanf(format: &uint8, ...) -> int32


public struct Pair:
    public first: int32
    public second: int32


public struct Triplet:
    public first: int32
    public second: int32
    public third: int32


public struct Quaduplet:
    public first: int32
    public second: int32
    public third: int32
    public fourth: int32


public enum Fizz:
    Single(int32)
    Pair(Pair)
    Triplet(Triplet)
    Quaduplet(Quaduplet)


implements Fizz:
    public function printSum(self: &this):
        let sum = match self:
            case Single(num): *num

            case Pair(pair): pair->first + pair->second

            case Triplet(triplet): (
                triplet->first
                    + triplet->second
                    + triplet->third
                )

            case Quaduplet(quad): (
                quad->first
                    + quad->second
                    + quad->third
                    + quad->fourth
                )

        printf(&"%d\0"->[0], sum)


public function main():
    let mut mode: int32 = 0
    let format = &"%d\0"->[0]

    scanf(format, &mut mode)

    let fizz =  match mode:
        1: 
            let mut first: int32 = 0

            scanf(format, &mut first)

            express Fizz::Single(first)

        2:
            let mut first: int32 = 0
            let mut second: int32 = 0

            scanf(format, &mut first)
            scanf(format, &mut second)

            express Fizz::Pair(Pair {
                first: first,
                second: second
            })

        3: 
            let mut first: int32 = 0
            let mut second: int32 = 0
            let mut third: int32 = 0

            scanf(format, &mut first)
            scanf(format, &mut second)
            scanf(format, &mut third)

            express Fizz::Triplet(Triplet {
                first: first,
                second: second,
                third: third
            })

        4: 
            let mut first: int32 = 0
            let mut second: int32 = 0
            let mut third: int32 = 0
            let mut fourth: int32 = 0

            scanf(format, &mut first)
            scanf(format, &mut second)
            scanf(format, &mut third)
            scanf(format, &mut fourth)

            express Fizz::Quaduplet(Quaduplet {
                first: first,
                second: second,
                third: third ,
                fourth: fourth
            })

        other:
            printf(&"Invalid mode\0"->[0])
            return
        
    fizz.printSum()
"#;

#[test]
fn r#enum() {
    {
        let output = compile_file_with(ENUM, |x| {
            x.write_stdin("1\n1\n");
        });

        assert!(output.status.success());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "1");
    }

    {
        let output = compile_file_with(ENUM, |x| {
            x.write_stdin("2\n1\n2\n");
        });

        assert!(output.status.success());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "3");
    }

    {
        let output = compile_file_with(ENUM, |x| {
            x.write_stdin("3\n1\n2\n3\n");
        });

        assert!(output.status.success());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "6");
    }

    {
        let output = compile_file_with(ENUM, |x| {
            x.write_stdin("4\n1\n2\n3\n4\n");
        });

        assert!(output.status.success());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "10");
    }

    {
        let output = compile_file_with(ENUM, |x| {
            x.write_stdin("5\n");
        });

        assert!(output.status.success());
        assert_eq!(String::from_utf8(output.stdout).unwrap(), "Invalid mode");
    }
}
