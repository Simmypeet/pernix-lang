use super::*;

#[test]
fn value_implementations() {
    // Test that all Value implementations work correctly
    assert_eq!(5u8.wrapping_add(3u8), 8u8);
    assert_eq!(u8::MAX.wrapping_add(1u8), 0u8);

    assert_eq!(5u16.wrapping_add(3u16), 8u16);
    assert_eq!(u16::MAX.wrapping_add(1u16), 0u16);

    assert_eq!(5u32.wrapping_add(3u32), 8u32);
    assert_eq!(u32::MAX.wrapping_add(1u32), 0u32);

    assert_eq!(5u64.wrapping_add(3u64), 8u64);
    assert_eq!(u64::MAX.wrapping_add(1u64), 0u64);

    assert_eq!(5u128.wrapping_add(3u128), 8u128);
    assert_eq!(u128::MAX.wrapping_add(1u128), 0u128);

    assert_eq!(5usize.wrapping_add(3usize), 8usize);
    assert_eq!(usize::MAX.wrapping_add(1usize), 0usize);
}

#[test]
fn stable_sip_hasher_basic() {
    let mut hasher = StableSipHasher::new();
    hasher.write_u32(42);
    let hash1 = hasher.finish();

    let mut hasher2 = StableSipHasher::new();
    hasher2.write_u32(42);
    let hash2 = hasher2.finish();

    // Same input should produce same hash
    assert_eq!(hash1, hash2);
}

#[test]
fn stable_sip_hasher_different_inputs() {
    let mut hasher1 = StableSipHasher::new();
    hasher1.write_u32(42);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    hasher2.write_u32(43);
    let hash2 = hasher2.finish();

    // Different inputs should produce different hashes
    assert_ne!(hash1, hash2);
}

#[test]
fn stable_sip_hasher_with_custom_keys() {
    let mut hasher1 = StableSipHasher::new_with_keys(
        0x1234_5678_90ab_cdef,
        0xfedc_ba09_8765_4321,
    );
    hasher1.write_u32(42);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new_with_keys(
        0x1234_5678_90ab_cdef,
        0xfedc_ba09_8765_4321,
    );
    hasher2.write_u32(42);
    let hash2 = hasher2.finish();

    // Same keys and input should produce same hash
    assert_eq!(hash1, hash2);

    let mut hasher3 = StableSipHasher::new();
    hasher3.write_u32(42);
    let hash3 = hasher3.finish();

    // Different keys should produce different hash
    assert_ne!(hash1, hash3);
}

#[test]
fn write_methods() {
    let mut hasher = StableSipHasher::new();

    hasher.write_u8(255);
    hasher.write_i8(-128);
    hasher.write_u16(65535);
    hasher.write_i16(-32768);
    hasher.write_u32(4_294_967_295);
    hasher.write_i32(-2_147_483_648);
    hasher.write_u64(18_446_744_073_709_551_615);
    hasher.write_i64(-9_223_372_036_854_775_808);
    hasher.write_u128(340_282_366_920_938_463_463_374_607_431_768_211_455);
    hasher.write_i128(-170_141_183_460_469_231_731_687_303_715_884_105_728);
    hasher.write_usize(usize::MAX);
    hasher.write_isize(isize::MIN);
    hasher.write_f32(std::f32::consts::PI);
    hasher.write_f64(std::f64::consts::E);

    let hash = hasher.finish();
    assert_ne!(hash, u128::default());
}

#[test]
fn write_f32_nan_normalization() {
    let mut hasher1 = StableSipHasher::new();
    hasher1.write_f32(f32::NAN);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    hasher2.write_f32(f32::from_bits(0x7fc0_0001)); // A different NaN representation
    let hash2 = hasher2.finish();

    // All NaN values should hash to the same value
    assert_eq!(hash1, hash2);
}

#[test]
fn write_f64_nan_normalization() {
    let mut hasher1 = StableSipHasher::new();
    hasher1.write_f64(f64::NAN);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    hasher2.write_f64(f64::from_bits(0x7ff8_0000_0000_0001)); // A different NaN representation
    let hash2 = hasher2.finish();

    // All NaN values should hash to the same value
    assert_eq!(hash1, hash2);
}

#[test]
fn write_str() {
    let mut hasher1 = StableSipHasher::new();
    hasher1.write_str("hello");
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    hasher2.write_str("hello");
    let hash2 = hasher2.finish();

    assert_eq!(hash1, hash2);

    let mut hasher3 = StableSipHasher::new();
    hasher3.write_str("world");
    let hash3 = hasher3.finish();

    assert_ne!(hash1, hash3);
}

#[test]
fn sub_hash() {
    let mut hasher = StableSipHasher::new();

    let sub_hash = hasher.sub_hash(&mut |sub| {
        sub.write_u32(42);
        sub.write_str("test");
    });

    assert_ne!(sub_hash, u128::default());

    // Sub-hash should be deterministic
    let sub_hash2 = hasher.sub_hash(&mut |sub| {
        sub.write_u32(42);
        sub.write_str("test");
    });

    assert_eq!(sub_hash, sub_hash2);
}

#[test]
fn stable_hash_primitive_types() {
    let mut hasher = StableSipHasher::new();

    42u8.stable_hash(&mut hasher);
    43u16.stable_hash(&mut hasher);
    44u32.stable_hash(&mut hasher);
    45u64.stable_hash(&mut hasher);
    46u128.stable_hash(&mut hasher);
    47usize.stable_hash(&mut hasher);

    let hash = hasher.finish();
    assert_ne!(hash, u128::default());
}

#[test]
fn stable_hash_vec() {
    let vec1 = vec![1, 2, 3, 4, 5];
    let vec2 = vec![1, 2, 3, 4, 5];
    let vec3 = vec![1, 2, 3, 4];

    let mut hasher1 = StableSipHasher::new();
    vec1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    vec2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    vec3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn stable_hash_hashmap() {
    use std::collections::HashMap;

    let mut map1 = HashMap::new();
    map1.insert("key1", "value1");
    map1.insert("key2", "value2");

    let mut map2 = HashMap::new();
    map2.insert("key2", "value2");
    map2.insert("key1", "value1");

    let mut hasher1 = StableSipHasher::new();
    map1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    map2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    // HashMap should have stable hash regardless of insertion order
    assert_eq!(hash1, hash2);
}

#[test]
fn stable_hash_hashset() {
    use std::collections::HashSet;

    let mut set1 = HashSet::new();
    set1.insert("item1");
    set1.insert("item2");

    let mut set2 = HashSet::new();
    set2.insert("item2");
    set2.insert("item1");

    let mut hasher1 = StableSipHasher::new();
    set1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    set2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    // HashSet should have stable hash regardless of insertion order
    assert_eq!(hash1, hash2);
}

#[test]
fn stable_hash_btreemap() {
    use std::collections::BTreeMap;

    let mut map1 = BTreeMap::new();
    map1.insert("key1", "value1");
    map1.insert("key2", "value2");

    let mut map2 = BTreeMap::new();
    map2.insert("key2", "value2");
    map2.insert("key1", "value1");

    let mut hasher1 = StableSipHasher::new();
    map1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    map2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    // BTreeMap should have stable hash (naturally ordered)
    assert_eq!(hash1, hash2);
}

#[test]
fn stable_hash_btreeset() {
    use std::collections::BTreeSet;

    let mut set1 = BTreeSet::new();
    set1.insert("item1");
    set1.insert("item2");

    let mut set2 = BTreeSet::new();
    set2.insert("item2");
    set2.insert("item1");

    let mut hasher1 = StableSipHasher::new();
    set1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    set2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    // BTreeSet should have stable hash (naturally ordered)
    assert_eq!(hash1, hash2);
}

#[test]
fn stable_hash_references() {
    let value = 42u32;
    let ref_value = &value;
    let mut_ref_value = &mut 42u32;

    let mut hasher1 = StableSipHasher::new();
    value.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    ref_value.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    mut_ref_value.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    // All should produce the same hash
    assert_eq!(hash1, hash2);
    assert_eq!(hash2, hash3);
}

#[test]
fn hasher_default() {
    let hasher1 = StableSipHasher::default();
    let hasher2 = StableSipHasher::new();

    // Default should be equivalent to new()
    assert_eq!(hasher1.finish(), hasher2.finish());
}

#[test]
fn consistency_across_runs() {
    // This test verifies that the same operations produce the same hash
    // This is important for stable hashing guarantees

    let mut hasher = StableSipHasher::new();
    hasher.write_str("stable");
    hasher.write_u64(12345);
    hasher.write_f64(std::f64::consts::PI);

    let hash = hasher.finish();

    // Run the same operations again
    let mut hasher2 = StableSipHasher::new();
    hasher2.write_str("stable");
    hasher2.write_u64(12345);
    hasher2.write_f64(std::f64::consts::PI);

    let hash2 = hasher2.finish();

    assert_eq!(hash, hash2);
}

#[test]
fn large_data_handling() {
    let large_data = vec![0u8; 10000];

    let mut hasher1 = StableSipHasher::new();
    hasher1.write(&large_data);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    // Write in chunks
    for chunk in large_data.chunks(1000) {
        hasher2.write(chunk);
    }
    let hash2 = hasher2.finish();

    assert_eq!(hash1, hash2);
}
