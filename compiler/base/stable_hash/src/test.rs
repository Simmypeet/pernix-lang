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
    let hasher = StableSipHasher::new();

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

// Test struct types
#[derive(StableHash)]
struct UnitStruct;

#[derive(StableHash)]
struct TupleStruct(u32, String);

#[derive(StableHash)]
struct NamedStruct {
    id: u32,
    name: String,
    active: bool,
}

#[derive(StableHash)]
struct GenericStruct<T, U> {
    first: T,
    second: U,
}

#[derive(StableHash)]
struct NestedStruct {
    point: (i32, i32),
    values: Vec<u32>,
    optional: Option<String>,
}

// Test enum types
#[derive(StableHash)]
#[allow(unused)]
enum SimpleEnum {
    A,
    B,
    C,
}

#[derive(StableHash)]
#[allow(unused)]
enum TupleEnum {
    Variant1(u32),
    Variant2(String, bool),
    Variant3(i32, f64, char),
}

#[derive(StableHash)]
#[allow(unused)]
enum StructEnum {
    Variant1 { x: u32, y: u32 },
    Variant2 { name: String, count: usize },
    Unit,
}

#[derive(StableHash)]
#[allow(unused)]
enum GenericEnum<T> {
    Some(T),
    None,
    Multiple(T, T),
}

#[derive(StableHash)]
enum MixedEnum {
    Unit,
    Tuple(u32, String),
    Struct { field: bool },
}

#[test]
fn derive_unit_struct() {
    let s1 = UnitStruct;
    let s2 = UnitStruct;

    let mut hasher1 = StableSipHasher::new();
    s1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    s2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    assert_eq!(hash1, hash2);
}

#[test]
fn derive_tuple_struct() {
    let s1 = TupleStruct(42, "test".to_string());
    let s2 = TupleStruct(42, "test".to_string());
    let s3 = TupleStruct(43, "test".to_string());

    let mut hasher1 = StableSipHasher::new();
    s1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    s2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    s3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn derive_named_struct() {
    let s1 = NamedStruct { id: 1, name: "Alice".to_string(), active: true };
    let s2 = NamedStruct { id: 1, name: "Alice".to_string(), active: true };
    let s3 = NamedStruct { id: 2, name: "Alice".to_string(), active: true };

    let mut hasher1 = StableSipHasher::new();
    s1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    s2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    s3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn derive_generic_struct() {
    let s1 = GenericStruct { first: 42u32, second: "test".to_string() };
    let s2 = GenericStruct { first: 42u32, second: "test".to_string() };
    let s3 = GenericStruct { first: 43u32, second: "test".to_string() };

    let mut hasher1 = StableSipHasher::new();
    s1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    s2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    s3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn derive_nested_struct() {
    let s1 = NestedStruct {
        point: (10, 20),
        values: vec![1, 2, 3],
        optional: Some("value".to_string()),
    };
    let s2 = NestedStruct {
        point: (10, 20),
        values: vec![1, 2, 3],
        optional: Some("value".to_string()),
    };
    let s3 =
        NestedStruct { point: (10, 20), values: vec![1, 2, 3], optional: None };

    let mut hasher1 = StableSipHasher::new();
    s1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    s2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    s3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn derive_simple_enum() {
    let e1 = SimpleEnum::A;
    let e2 = SimpleEnum::A;
    let e3 = SimpleEnum::B;

    let mut hasher1 = StableSipHasher::new();
    e1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    e2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    e3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
}

#[test]
fn derive_tuple_enum() {
    let e1 = TupleEnum::Variant1(42);
    let e2 = TupleEnum::Variant1(42);
    let e3 = TupleEnum::Variant1(43);
    let e4 = TupleEnum::Variant2("test".to_string(), true);

    let mut hasher1 = StableSipHasher::new();
    e1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    e2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    e3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    let mut hasher4 = StableSipHasher::new();
    e4.stable_hash(&mut hasher4);
    let hash4 = hasher4.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
    assert_ne!(hash1, hash4);
}

#[test]
fn derive_struct_enum() {
    let e1 = StructEnum::Variant1 { x: 10, y: 20 };
    let e2 = StructEnum::Variant1 { x: 10, y: 20 };
    let e3 = StructEnum::Variant1 { x: 11, y: 20 };
    let e4 = StructEnum::Unit;

    let mut hasher1 = StableSipHasher::new();
    e1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    e2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    e3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    let mut hasher4 = StableSipHasher::new();
    e4.stable_hash(&mut hasher4);
    let hash4 = hasher4.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
    assert_ne!(hash1, hash4);
}

#[test]
fn derive_generic_enum() {
    let e1 = GenericEnum::Some(42u32);
    let e2 = GenericEnum::Some(42u32);
    let e3 = GenericEnum::Some(43u32);
    let e4 = GenericEnum::<u32>::None;

    let mut hasher1 = StableSipHasher::new();
    e1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    e2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    e3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    let mut hasher4 = StableSipHasher::new();
    e4.stable_hash(&mut hasher4);
    let hash4 = hasher4.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
    assert_ne!(hash1, hash4);
}

#[test]
fn derive_mixed_enum() {
    let e1 = MixedEnum::Unit;
    let e2 = MixedEnum::Unit;
    let e3 = MixedEnum::Tuple(42, "test".to_string());
    let e4 = MixedEnum::Struct { field: true };

    let mut hasher1 = StableSipHasher::new();
    e1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    e2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    let mut hasher3 = StableSipHasher::new();
    e3.stable_hash(&mut hasher3);
    let hash3 = hasher3.finish();

    let mut hasher4 = StableSipHasher::new();
    e4.stable_hash(&mut hasher4);
    let hash4 = hasher4.finish();

    assert_eq!(hash1, hash2);
    assert_ne!(hash1, hash3);
    assert_ne!(hash1, hash4);
    assert_ne!(hash3, hash4);
}

#[test]
fn derive_deterministic_hashing() {
    // Test that the same data always produces the same hash
    let data = NamedStruct {
        id: 12345,
        name: "deterministic test".to_string(),
        active: false,
    };

    let mut hashes = Vec::new();
    for _ in 0..10 {
        let mut stable_hasher = StableSipHasher::new();
        data.stable_hash(&mut stable_hasher);
        hashes.push(stable_hasher.finish());
    }

    // All hashes should be identical
    for hash in &hashes[1..] {
        assert_eq!(*hash, hashes[0]);
    }
}

#[test]
fn derive_complex_nested_structure() {
    #[derive(StableHash)]
    struct Complex {
        simple: SimpleEnum,
        tuple_struct: TupleStruct,
        named: NamedStruct,
        generic: GenericStruct<u32, String>,
    }

    let c1 = Complex {
        simple: SimpleEnum::B,
        tuple_struct: TupleStruct(100, "nested".to_string()),
        named: NamedStruct {
            id: 999,
            name: "complex".to_string(),
            active: true,
        },
        generic: GenericStruct { first: 42, second: "generic".to_string() },
    };

    let c2 = Complex {
        simple: SimpleEnum::B,
        tuple_struct: TupleStruct(100, "nested".to_string()),
        named: NamedStruct {
            id: 999,
            name: "complex".to_string(),
            active: true,
        },
        generic: GenericStruct { first: 42, second: "generic".to_string() },
    };

    let mut hasher1 = StableSipHasher::new();
    c1.stable_hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = StableSipHasher::new();
    c2.stable_hash(&mut hasher2);
    let hash2 = hasher2.finish();

    assert_eq!(hash1, hash2);
}
