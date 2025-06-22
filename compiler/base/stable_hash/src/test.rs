use super::*;

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
