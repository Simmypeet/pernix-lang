use super::*;

#[test]
fn test_line() {
    let source_code = SourceCode::new(
        "Hello, world!\n".to_string()
            + "My Name Is Pernix\n"
            + "I am a programming language\n"
            + "I am written in Rust\n"
            + "\n",
        "test".to_string(),
    );

    assert_eq!(source_code.line(1).unwrap(), "Hello, world!");
    assert_eq!(source_code.line(2).unwrap(), "My Name Is Pernix");
    assert_eq!(source_code.line(3).unwrap(), "I am a programming language");
    assert_eq!(source_code.line(4).unwrap(), "I am written in Rust");
    assert_eq!(source_code.line(5).unwrap(), "");
}
