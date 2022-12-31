use super::SourceFile;

static SOURCE_FILE: &str = "Hello World
This is a test 
Check if this works
I hope it does";

#[test]
fn source_file_test() {
    let source_file = SourceFile::new(SOURCE_FILE.to_string(), "test.pnx".to_string());

    assert_eq!(source_file.line(0), None);
    assert_eq!(source_file.line(1), Some("Hello World"));
    assert_eq!(source_file.line(2), Some("This is a test "));
    assert_eq!(source_file.line(3), Some("Check if this works"));
    assert_eq!(source_file.line(4), Some("I hope it does"));
    assert_eq!(source_file.line(5), None);
}
