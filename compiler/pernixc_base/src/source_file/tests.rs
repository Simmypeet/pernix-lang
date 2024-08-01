#[test]
fn get_line_byte_positions() {
    let text = "Hello\nworld\r\n!\n\rtes";
    let byte_positions = super::get_line_byte_positions(text);
    assert_eq!(byte_positions, vec![0..6, 6..13, 13..15, 15..16, 16..19]);
}

#[test]
fn mapped_file() {
    const TEST_FILE: &str = "test file";
    let source_file = super::SourceFile::temp(TEST_FILE).unwrap();
    assert_eq!(source_file.content(), TEST_FILE);
}
