#[test]
fn morton() {
    let lo = 0x0000_0000_0000_0000_FFFF_FFFF_FFFF_FFFFu128;
    let hi = 0x0000_0000_0000_0000_FFFF_FFFF_FFFF_FFFFu128;
    let (high, low) = super::morton(hi, lo);

    assert_eq!(low, u128::MAX);
    assert_eq!(high, 0);
}

#[test]
fn simple_morton() {
    // Test with simple values first
    let (high, low) = super::morton(1, 1);
    // Expected: 1 in even positions (0), 1 in odd positions (1) = binary 11 = 3
    assert_eq!(low, 3);
    assert_eq!(high, 0);

    let (high, low) = super::morton(2, 2);
    // Expected: 10 in even positions (bits 0,2), 10 in odd positions (bits 1,3)
    // = binary 1010 -> interleaved 1100 = 12
    assert_eq!(low, 12);
    assert_eq!(high, 0);
}
