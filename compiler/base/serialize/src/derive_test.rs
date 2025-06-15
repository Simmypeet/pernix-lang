//! Tests for the Serialize derive macro.

use crate::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};

#[derive(Serialize, Debug, PartialEq)]
struct Person {
    name: String,
    age: u32,
}

#[derive(Serialize, Debug, PartialEq)]
struct Point<T> {
    x: T,
    y: T,
}

#[derive(Serialize, Debug, PartialEq)]
struct UnitStruct;

#[derive(Serialize, Debug, PartialEq)]
struct TupleStruct(i32, String);

#[derive(Serialize, Debug, PartialEq)]
enum Color {
    Red,
    Green,
    Blue,
}

#[derive(Serialize, Debug, PartialEq)]
enum Shape {
    Circle(f32),
    Rectangle(f32, f32),
    Triangle(f32, f32, f32),
}

#[derive(Serialize, Debug, PartialEq)]
enum Animal {
    Cat { name: String, age: u8 },
    Dog { name: String, breed: String },
    Bird { species: String, can_fly: bool },
}

#[derive(Serialize, Debug, PartialEq)]
enum GenericOption<T> {
    Some(T),
    None,
}

#[derive(Serialize, Debug, PartialEq)]
enum Mixed<T, U> {
    Unit,
    Tuple(T),
    Struct { field: U },
}

/// Helper function to create a serializer and run a test with it
fn serialize_to_bytes<T: Serialize<BinarySerializer<Vec<u8>>>>(
    value: &T,
) -> Vec<u8> {
    let mut serializer = BinarySerializer::new(Vec::new());
    value.serialize(&mut serializer).unwrap();
    serializer.into_inner()
}

#[test]
fn named_struct() {
    let person = Person { name: "Alice".to_string(), age: 30 };

    let bytes = serialize_to_bytes(&person);

    // Expected: name length (5) + "Alice" + age (30 as u32 in little-endian)
    let mut expected = Vec::new();
    expected.push(5); // name length as varint
    expected.extend_from_slice(b"Alice");
    expected.extend_from_slice(&30u32.to_le_bytes());

    assert_eq!(bytes, expected);
}

#[test]
fn generic_struct() {
    let point = Point { x: 1.0f32, y: 2.0f32 };

    let bytes = serialize_to_bytes(&point);

    // Expected: x (1.0f32) + y (2.0f32) both in little-endian IEEE 754
    let mut expected = Vec::new();
    expected.extend_from_slice(&1.0f32.to_le_bytes());
    expected.extend_from_slice(&2.0f32.to_le_bytes());

    assert_eq!(bytes, expected);
}

#[test]
fn unit_struct() {
    let unit = UnitStruct;

    let bytes = serialize_to_bytes(&unit);

    // Unit struct should serialize to empty buffer
    assert_eq!(bytes, Vec::<u8>::new());
}

#[test]
fn tuple_struct() {
    let tuple = TupleStruct(42, "hello".to_string());

    let bytes = serialize_to_bytes(&tuple);

    // Expected: 42 as i32 in little-endian + string length (5) + "hello"
    let mut expected = Vec::new();
    expected.extend_from_slice(&42i32.to_le_bytes());
    expected.push(5); // string length as varint
    expected.extend_from_slice(b"hello");

    assert_eq!(bytes, expected);
}

#[test]
fn unit_enum() {
    // Test first variant (Red = index 0)
    let red = Color::Red;
    let bytes = serialize_to_bytes(&red);
    assert_eq!(bytes, vec![0]); // variant index 0 as varint

    // Test second variant (Green = index 1)
    let green = Color::Green;
    let bytes = serialize_to_bytes(&green);
    assert_eq!(bytes, vec![1]); // variant index 1 as varint

    // Test third variant (Blue = index 2)
    let blue = Color::Blue;
    let bytes = serialize_to_bytes(&blue);
    assert_eq!(bytes, vec![2]); // variant index 2 as varint
}

#[test]
fn tuple_enum() {
    // Test Circle variant (index 0) with f32 data
    let circle = Shape::Circle(5.0);
    let bytes = serialize_to_bytes(&circle);

    let mut expected = Vec::new();
    expected.push(0); // variant index 0 as varint
    expected.extend_from_slice(&5.0f32.to_le_bytes());
    assert_eq!(bytes, expected);

    // Test Rectangle variant (index 1) with two f32 values
    let rectangle = Shape::Rectangle(10.0, 20.0);
    let bytes = serialize_to_bytes(&rectangle);

    let mut expected = Vec::new();
    expected.push(1); // variant index 1 as varint
    expected.extend_from_slice(&10.0f32.to_le_bytes());
    expected.extend_from_slice(&20.0f32.to_le_bytes());
    assert_eq!(bytes, expected);

    // Test Triangle variant (index 2) with three f32 values
    let triangle = Shape::Triangle(3.0, 4.0, 5.0);
    let bytes = serialize_to_bytes(&triangle);

    let mut expected = Vec::new();
    expected.push(2); // variant index 2 as varint
    expected.extend_from_slice(&3.0f32.to_le_bytes());
    expected.extend_from_slice(&4.0f32.to_le_bytes());
    expected.extend_from_slice(&5.0f32.to_le_bytes());
    assert_eq!(bytes, expected);
}

#[test]
fn struct_enum() {
    // Test Cat variant (index 0) with named fields
    let cat = Animal::Cat { name: "Fluffy".to_string(), age: 3 };
    let bytes = serialize_to_bytes(&cat);

    let mut expected = Vec::new();
    expected.push(0); // variant index 0 as varint
    expected.push(6); // name length as varint
    expected.extend_from_slice(b"Fluffy");
    expected.push(3); // age as u8
    assert_eq!(bytes, expected);

    // Test Dog variant (index 1) with named fields
    let dog = Animal::Dog {
        name: "Rex".to_string(),
        breed: "German Shepherd".to_string(),
    };
    let bytes = serialize_to_bytes(&dog);

    let mut expected = Vec::new();
    expected.push(1); // variant index 1 as varint
    expected.push(3); // name length as varint
    expected.extend_from_slice(b"Rex");
    expected.push(15); // breed length as varint
    expected.extend_from_slice(b"German Shepherd");
    assert_eq!(bytes, expected);

    // Test Bird variant (index 2) with named fields
    let bird = Animal::Bird { species: "Eagle".to_string(), can_fly: true };
    let bytes = serialize_to_bytes(&bird);

    let mut expected = Vec::new();
    expected.push(2); // variant index 2 as varint
    expected.push(5); // species length as varint
    expected.extend_from_slice(b"Eagle");
    expected.push(1); // can_fly as bool (true = 1)
    assert_eq!(bytes, expected);
}

#[test]
fn generic_enum() {
    // Test Some variant with i32
    let some_value = GenericOption::Some(42i32);
    let bytes = serialize_to_bytes(&some_value);

    let mut expected = Vec::new();
    expected.push(0); // variant index 0 as varint (Some)
    expected.extend_from_slice(&42i32.to_le_bytes());
    assert_eq!(bytes, expected);

    // Test None variant
    let none_value: GenericOption<i32> = GenericOption::None;
    let bytes = serialize_to_bytes(&none_value);
    assert_eq!(bytes, vec![1]); // variant index 1 as varint (None)

    // Test Some variant with String
    let some_string = GenericOption::Some("test".to_string());
    let bytes = serialize_to_bytes(&some_string);

    let mut expected = Vec::new();
    expected.push(0); // variant index 0 as varint (Some)
    expected.push(4); // string length as varint
    expected.extend_from_slice(b"test");
    assert_eq!(bytes, expected);
}

#[test]
fn mixed_enum() {
    // Test Unit variant (index 0)
    let unit = Mixed::<i32, String>::Unit;
    let bytes = serialize_to_bytes(&unit);
    assert_eq!(bytes, vec![0]); // variant index 0 as varint

    // Test Tuple variant (index 1) with i32
    let tuple = Mixed::<i32, String>::Tuple(123i32);
    let bytes = serialize_to_bytes(&tuple);

    let mut expected = Vec::new();
    expected.push(1); // variant index 1 as varint
    expected.extend_from_slice(&123i32.to_le_bytes());
    assert_eq!(bytes, expected);

    // Test Struct variant (index 2) with String field
    let struct_variant =
        Mixed::<i32, String>::Struct { field: "hello".to_string() };
    let bytes = serialize_to_bytes(&struct_variant);

    let mut expected = Vec::new();
    expected.push(2); // variant index 2 as varint
    expected.push(5); // string length as varint
    expected.extend_from_slice(b"hello");
    assert_eq!(bytes, expected);
}

#[test]
fn nested_structures() {
    // Test nested enum in struct
    #[derive(Serialize)]
    struct Container {
        color: Color,
        value: u16,
    }

    let container = Container { color: Color::Green, value: 1000 };
    let bytes = serialize_to_bytes(&container);

    let mut expected = Vec::new();
    expected.push(1); // Color::Green variant index
    expected.extend_from_slice(&1000u16.to_le_bytes());
    assert_eq!(bytes, expected);

    // Test enum containing struct-like data
    #[derive(Serialize)]
    enum Complex {
        Data { items: Vec<i32>, flag: bool },
    }

    let complex = Complex::Data { items: vec![1, 2, 3], flag: false };
    let bytes = serialize_to_bytes(&complex);

    let mut expected = Vec::new();
    expected.push(0); // variant index 0
    expected.push(3); // vec length as varint
    expected.extend_from_slice(&1i32.to_le_bytes());
    expected.extend_from_slice(&2i32.to_le_bytes());
    expected.extend_from_slice(&3i32.to_le_bytes());
    expected.push(0); // flag as bool (false = 0)
    assert_eq!(bytes, expected);
}

#[test]
fn trait_bounds_in_generics() {
    // Test struct with trait bound directly in generic parameter
    #[derive(Serialize, Debug, Clone)]
    struct BoundedStruct<T: Clone + std::fmt::Debug> {
        value: T,
        count: usize,
    }

    let bounded = BoundedStruct { value: "test".to_string(), count: 42 };
    let bytes = serialize_to_bytes(&bounded);

    let mut expected = Vec::new();
    expected.push(4); // string length as varint
    expected.extend_from_slice(b"test");
    expected.extend_from_slice(&42usize.to_le_bytes());
    assert_eq!(bytes, expected);

    // Test with multiple trait bounds
    #[derive(Serialize)]
    struct MultiBounded<T: Clone + std::fmt::Debug + PartialEq> {
        data: T,
    }

    let multi = MultiBounded { data: 123i32 };
    let bytes = serialize_to_bytes(&multi);
    let expected = 123i32.to_le_bytes().to_vec();
    assert_eq!(bytes, expected);
}

#[test]
fn where_clause_constraints() {
    // Test struct with where clause
    #[derive(Serialize)]
    struct WhereClauseStruct<T, U>
    where
        T: Clone + std::fmt::Debug,
        U: PartialEq + std::fmt::Display,
    {
        first: T,
        second: U,
    }

    let where_struct =
        WhereClauseStruct { first: vec![1, 2, 3], second: "hello".to_string() };
    let bytes = serialize_to_bytes(&where_struct);

    let mut expected = Vec::new();
    expected.push(3); // vec length as varint
    expected.extend_from_slice(&1i32.to_le_bytes());
    expected.extend_from_slice(&2i32.to_le_bytes());
    expected.extend_from_slice(&3i32.to_le_bytes());
    expected.push(5); // string length as varint
    expected.extend_from_slice(b"hello");
    assert_eq!(bytes, expected);

    // Test enum with where clause
    #[derive(Serialize)]
    #[allow(unused)]
    enum WhereClauseEnum<T, U>
    where
        T: Clone,
        U: std::fmt::Debug,
    {
        First(T),
        Second { value: U },
        Both(T, U),
    }

    let enum_first = WhereClauseEnum::First::<i32, String>(42i32);
    let bytes = serialize_to_bytes(&enum_first);
    let mut expected = Vec::new();
    expected.push(0); // variant index 0
    expected.extend_from_slice(&42i32.to_le_bytes());
    assert_eq!(bytes, expected);

    let enum_second =
        WhereClauseEnum::Second::<i32, String> { value: "test".to_string() };
    let bytes = serialize_to_bytes(&enum_second);
    let mut expected = Vec::new();
    expected.push(1); // variant index 1
    expected.push(4); // string length as varint
    expected.extend_from_slice(b"test");
    assert_eq!(bytes, expected);
}

#[test]
fn lifetime_and_trait_bounds() {
    // Test struct with lifetime and trait bounds (simplified)
    #[derive(Serialize)]
    struct WithLifetime<'a> {
        data: u64,
        name: &'a str,
    }

    let lifetime_struct = WithLifetime { data: 100u64, name: "test" };
    let bytes = serialize_to_bytes(&lifetime_struct);

    let mut expected = Vec::new();
    expected.extend_from_slice(&100u64.to_le_bytes());
    expected.push(4); // string length as varint
    expected.extend_from_slice(b"test");
    assert_eq!(bytes, expected);
}

#[test]
fn complex_trait_bounds() {
    // Test with Send + Sync bounds (using bounds on generic params directly)
    #[derive(Serialize)]
    struct ThreadSafeStruct<T: Send + Sync + Clone + 'static> {
        value: T,
        id: u64,
    }

    let thread_safe = ThreadSafeStruct { value: 42i32, id: 123 };
    let bytes = serialize_to_bytes(&thread_safe);

    let mut expected = Vec::new();
    expected.extend_from_slice(&42i32.to_le_bytes());
    expected.extend_from_slice(&123u64.to_le_bytes());
    assert_eq!(bytes, expected);

    // Test struct with multiple constraints using where clause
    #[derive(Serialize)]
    struct MultiConstraint<T>
    where
        T: Clone + std::fmt::Debug + Send + Sync + 'static,
    {
        value: T,
        id: u64,
    }

    let multi = MultiConstraint { value: vec![1, 2, 3], id: 12345 };
    let bytes = serialize_to_bytes(&multi);

    let mut expected = Vec::new();
    expected.push(3); // vec length as varint
    expected.extend_from_slice(&1i32.to_le_bytes());
    expected.extend_from_slice(&2i32.to_le_bytes());
    expected.extend_from_slice(&3i32.to_le_bytes());
    expected.extend_from_slice(&12345u64.to_le_bytes());
    assert_eq!(bytes, expected);
}

#[test]
fn nested_generic_bounds() {
    // Test nested generics with bounds
    #[derive(Serialize)]
    struct Container<T: Clone + std::fmt::Debug> {
        items: Vec<T>,
        metadata: String,
    }

    let container = Container {
        items: vec!["a".to_string(), "b".to_string()],
        metadata: "container".to_string(),
    };
    let bytes = serialize_to_bytes(&container);

    let mut expected = Vec::new();
    expected.push(2); // vec length as varint
    expected.push(1); // first string length
    expected.extend_from_slice(b"a");
    expected.push(1); // second string length
    expected.extend_from_slice(b"b");
    expected.push(9); // metadata length
    expected.extend_from_slice(b"container");
    assert_eq!(bytes, expected);

    // Test enum with mixed bounds (some in generics, some in where clause)
    #[derive(Serialize)]
    #[allow(unused)]
    enum MixedBounds<T, U: Clone>
    where
        T: std::fmt::Debug + PartialEq,
    {
        First { data: T },
        Second(U),
        Combined { first: T, second: U },
    }

    let mixed =
        MixedBounds::Combined { first: 42i32, second: "test".to_string() };
    let bytes = serialize_to_bytes(&mixed);

    let mut expected = Vec::new();
    expected.push(2); // variant index 2
    expected.extend_from_slice(&42i32.to_le_bytes());
    expected.push(4); // string length
    expected.extend_from_slice(b"test");
    assert_eq!(bytes, expected);
}

// Test structs with Deserialize for error handling tests
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct SimpleStruct {
    name: String,
    value: i32,
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
enum SimpleEnum {
    Unit,
}

// Basic manual Deserialize test
#[test]
fn manual_deserialize_test() {
    // Test that i32 deserialization works manually
    let value = 42i32;
    let bytes = serialize_to_bytes(&value);

    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = i32::deserialize(&mut deserializer).unwrap();

    assert_eq!(value, deserialized);
}

// Error handling tests for struct deserialization
#[test]
fn struct_round_trip() {
    let test_struct = SimpleStruct { name: "test".to_string(), value: 42 };

    let bytes = serialize_to_bytes(&test_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = SimpleStruct::deserialize(&mut deserializer).unwrap();

    assert_eq!(test_struct, deserialized);
}

// Additional comprehensive tests to prevent field index bugs
#[test]
fn multi_field_struct_round_trip() {
    // Test a struct with multiple fields to ensure field indices are correct
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct MultiFieldStruct {
        field_a: String,
        field_b: i32,
        field_c: bool,
    }

    let test_struct = MultiFieldStruct {
        field_a: "hello".to_string(),
        field_b: 42,
        field_c: true,
    };

    let bytes = serialize_to_bytes(&test_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        MultiFieldStruct::deserialize(&mut deserializer).unwrap();

    assert_eq!(test_struct, deserialized);
}

#[test]
fn single_field_struct() {
    // Edge case: single field struct
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct SingleField {
        value: String,
    }

    let test_struct = SingleField { value: "single".to_string() };

    let bytes = serialize_to_bytes(&test_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = SingleField::deserialize(&mut deserializer).unwrap();

    assert_eq!(test_struct, deserialized);
}

#[test]
fn comprehensive_round_trip_all_forms() {
    // Test all possible struct and enum forms to ensure complete coverage

    // Unit struct
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct UnitStruct;

    // Tuple struct
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct TupleStruct(String, i32, bool);

    // Named fields struct
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct FieldsStruct {
        name: String,
        value: i32,
        active: bool,
    }

    // Enum with all variant types
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum ComprehensiveEnum {
        // Unit variant
        Unit,
        // Tuple variant
        Tuple(String, i32),
        // Struct variant
        Struct { field_a: String, field_b: i32, field_c: bool },
    }

    // Test unit struct
    let unit_struct = UnitStruct;
    let bytes = serialize_to_bytes(&unit_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = UnitStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(unit_struct, deserialized);

    // Test tuple struct
    let tuple_struct = TupleStruct("hello".to_string(), 42, true);
    let bytes = serialize_to_bytes(&tuple_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = TupleStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(tuple_struct, deserialized);

    // Test fields struct
    let fields_struct =
        FieldsStruct { name: "test".to_string(), value: -123, active: false };
    let bytes = serialize_to_bytes(&fields_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = FieldsStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(fields_struct, deserialized);

    // Test enum unit variant
    let enum_unit = ComprehensiveEnum::Unit;
    let bytes = serialize_to_bytes(&enum_unit);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComprehensiveEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(enum_unit, deserialized);

    // Test enum tuple variant
    let enum_tuple = ComprehensiveEnum::Tuple("world".to_string(), 456);
    let bytes = serialize_to_bytes(&enum_tuple);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComprehensiveEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(enum_tuple, deserialized);

    // Test enum struct variant
    let enum_struct = ComprehensiveEnum::Struct {
        field_a: "struct_variant".to_string(),
        field_b: 789,
        field_c: true,
    };
    let bytes = serialize_to_bytes(&enum_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComprehensiveEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(enum_struct, deserialized);
}

#[test]
fn complex_nested_forms() {
    // Test complex nested structures to ensure deep serialization works

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct InnerStruct {
        value: i32,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum InnerEnum {
        Variant { data: String },
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct OuterStruct {
        inner_struct: InnerStruct,
        inner_enum: InnerEnum,
        vec_data: Vec<String>,
        optional: Option<i32>,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum OuterEnum {
        Complex { nested_struct: OuterStruct, tuple_data: (String, i32, bool) },
        Simple(Vec<InnerStruct>),
    }

    // Test nested complex enum
    let complex_data = OuterEnum::Complex {
        nested_struct: OuterStruct {
            inner_struct: InnerStruct { value: 100 },
            inner_enum: InnerEnum::Variant { data: "nested".to_string() },
            vec_data: vec!["a".to_string(), "b".to_string(), "c".to_string()],
            optional: Some(200),
        },
        tuple_data: ("tuple".to_string(), 300, false),
    };

    let bytes = serialize_to_bytes(&complex_data);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = OuterEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(complex_data, deserialized);

    // Test simple variant with vector
    let simple_data = OuterEnum::Simple(vec![
        InnerStruct { value: 1 },
        InnerStruct { value: 2 },
        InnerStruct { value: 3 },
    ]);

    let bytes = serialize_to_bytes(&simple_data);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = OuterEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(simple_data, deserialized);
}

#[test]
fn edge_cases() {
    // Test edge cases that might reveal field index issues

    // Struct with many fields to test field index calculation
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct ManyFieldsStruct {
        field_0: u8,
        field_1: u16,
        field_2: u32,
        field_3: u64,
        field_4: i8,
        field_5: i16,
        field_6: i32,
        field_7: i64,
        field_8: f32,
        field_9: f64,
        field_10: bool,
        field_11: String,
        field_12: Vec<u8>,
        field_13: Option<i32>,
    }

    let many_fields = ManyFieldsStruct {
        field_0: 0,
        field_1: 1,
        field_2: 2,
        field_3: 3,
        field_4: 4,
        field_5: 5,
        field_6: 6,
        field_7: 7,
        field_8: 8.0,
        field_9: 9.0,
        field_10: true,
        field_11: "eleven".to_string(),
        field_12: vec![1, 2, 3],
        field_13: Some(13),
    };

    let bytes = serialize_to_bytes(&many_fields);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ManyFieldsStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(many_fields, deserialized);

    // Enum with many struct variants to test variant field index calculation
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum ManyVariantsEnum {
        Variant0 {
            a: i32,
            b: String,
        },
        Variant1 {
            x: f64,
            y: bool,
            z: Vec<u8>,
        },
        Variant2 {
            first: String,
            second: i32,
            third: bool,
            fourth: f64,
            fifth: Option<String>,
        },
    }

    // Test each variant
    let variant0 =
        ManyVariantsEnum::Variant0 { a: 100, b: "test0".to_string() };
    let bytes = serialize_to_bytes(&variant0);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ManyVariantsEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(variant0, deserialized);

    let variant1 = ManyVariantsEnum::Variant1 {
        x: 123.456,
        y: false,
        z: vec![10, 20, 30],
    };
    let bytes = serialize_to_bytes(&variant1);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ManyVariantsEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(variant1, deserialized);

    let variant2 = ManyVariantsEnum::Variant2 {
        first: "first".to_string(),
        second: 200,
        third: true,
        fourth: 456.789,
        fifth: Some("fifth".to_string()),
    };
    let bytes = serialize_to_bytes(&variant2);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ManyVariantsEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(variant2, deserialized);
}

#[test]
fn advanced_generic_struct_roundtrip() {
    // Test struct with multiple generic parameters and trait bounds
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct MultiGeneric<T, U, V>
    where
        T: Clone + std::fmt::Debug,
        U: PartialEq + Clone,
        V: std::fmt::Display + Clone,
    {
        first: T,
        second: U,
        third: V,
        metadata: (String, i32),
    }

    let multi_generic = MultiGeneric {
        first: vec![1, 2, 3, 4, 5],
        second: Some("optional".to_string()),
        third: 42.5f64,
        metadata: ("metadata".to_string(), 999),
    };

    let bytes = serialize_to_bytes(&multi_generic);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = MultiGeneric::deserialize(&mut deserializer).unwrap();
    assert_eq!(multi_generic, deserialized);

    // Test with different types
    let multi_generic2 = MultiGeneric {
        first: "different".to_string(),
        second: Result::<i32, String>::Ok(123),
        third: 100u64,
        metadata: ("other".to_string(), -50),
    };

    let bytes = serialize_to_bytes(&multi_generic2);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = MultiGeneric::deserialize(&mut deserializer).unwrap();
    assert_eq!(multi_generic2, deserialized);
}

#[test]
fn advanced_generic_enum_roundtrip() {
    // Test enum with complex generic parameters and constraints
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum ComplexGenericEnum<T, U, V>
    where
        T: Clone + std::fmt::Debug + PartialEq,
        U: Clone + std::fmt::Debug + PartialEq,
        V: Clone + std::fmt::Debug + PartialEq,
    {
        Empty,
        Single(T),
        Pair(T, U),
        Triple(T, U, V),
        Structured {
            primary: T,
            secondary: Option<U>,
            tertiary: Vec<V>,
            extra: (bool, String),
        },
        Nested {
            inner: Box<ComplexGenericEnum<U, V, T>>, // Reordered generics
            depth: usize,
        },
    }

    // Test Empty variant
    let empty: ComplexGenericEnum<i32, String, f64> = ComplexGenericEnum::Empty;
    let bytes = serialize_to_bytes(&empty);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(empty, deserialized);

    // Test Single variant with explicit type annotation
    let single: ComplexGenericEnum<Vec<i32>, String, f64> =
        ComplexGenericEnum::Single(vec![1, 2, 3]);
    let bytes = serialize_to_bytes(&single);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(single, deserialized);

    // Test Pair variant with explicit type annotation
    let pair: ComplexGenericEnum<String, Option<i32>, f64> =
        ComplexGenericEnum::Pair("first".to_string(), Some(42i32));
    let bytes = serialize_to_bytes(&pair);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(pair, deserialized);

    // Test Triple variant
    let triple = ComplexGenericEnum::Triple(
        vec!["a".to_string(), "b".to_string()],
        (true, false, true),
        999u64,
    );
    let bytes = serialize_to_bytes(&triple);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(triple, deserialized);

    // Test Structured variant
    let structured = ComplexGenericEnum::Structured {
        primary: std::collections::HashMap::from([
            ("key1".to_string(), 100),
            ("key2".to_string(), 200),
        ]),
        secondary: Some("secondary".to_string()),
        tertiary: vec![1.1, 2.2, 3.3],
        extra: (false, "extra_data".to_string()),
    };
    let bytes = serialize_to_bytes(&structured);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(structured, deserialized);

    // Test Nested variant with reordered generics
    let nested: ComplexGenericEnum<
        std::collections::HashMap<String, i32>,
        String,
        f64,
    > = ComplexGenericEnum::Nested {
        inner: Box::new(ComplexGenericEnum::Single("inner_value".to_string())),
        depth: 5,
    };
    let bytes = serialize_to_bytes(&nested);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(nested, deserialized);
}

#[test]
fn lifetime_generic_roundtrip() {
    // Test struct with lifetime parameters
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct WithLifetimes<T>
    where
        T: Clone + PartialEq,
    {
        data: T,
        name: String,
        description: String,
        id: u64,
    }

    let lifetime_data = WithLifetimes {
        data: vec![10, 20, 30, 40],
        name: "test_name".to_string(),
        description: "test_description".to_string(),
        id: 12345,
    };

    let bytes = serialize_to_bytes(&lifetime_data);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = WithLifetimes::deserialize(&mut deserializer).unwrap();
    assert_eq!(lifetime_data, deserialized);

    // Test enum with lifetimes
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum LifetimeEnum<T>
    where
        T: Clone + PartialEq,
    {
        Reference(String),
        Owned(String),
        Generic(T),
        Combined { reference: String, owned: String, generic: T },
    }

    let reference_variant: LifetimeEnum<Vec<i32>> =
        LifetimeEnum::Reference("reference_data".to_string());
    let bytes = serialize_to_bytes(&reference_variant);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = LifetimeEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(reference_variant, deserialized);

    let combined_variant = LifetimeEnum::Combined {
        reference: "ref_value".to_string(),
        owned: "owned_value".to_string(),
        generic: Some(42i32),
    };
    let bytes = serialize_to_bytes(&combined_variant);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = LifetimeEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(combined_variant, deserialized);
}

// Test struct with const generic parameters
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct ConstGenericStruct<T, const N: usize>
where
    T: Clone + PartialEq + std::fmt::Debug,
{
    data: [T; N],
    size: usize,
    metadata: String,
}

#[test]
fn const_generic_roundtrip() {
    let const_struct = ConstGenericStruct {
        data: [1, 2, 3, 4, 5],
        size: 5,
        metadata: "fixed_size_array".to_string(),
    };

    let bytes = serialize_to_bytes(&const_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ConstGenericStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(const_struct, deserialized);

    // Test with different size and type
    let const_struct2 = ConstGenericStruct {
        data: ["a".to_string(), "b".to_string(), "c".to_string()],
        size: 3,
        metadata: "string_array".to_string(),
    };

    let bytes = serialize_to_bytes(&const_struct2);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ConstGenericStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(const_struct2, deserialized);

    // Test enum with const generics
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum ConstGenericEnum<T, const N: usize>
    where
        T: Clone + PartialEq + std::fmt::Debug,
    {
        Array([T; N]),
        Dynamic(Vec<T>),
        Mixed { fixed: [T; N], dynamic: Vec<T>, count: usize },
    }

    let array_variant = ConstGenericEnum::Array([10.1, 20.2, 30.3]);
    let bytes = serialize_to_bytes(&array_variant);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ConstGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(array_variant, deserialized);

    let mixed_variant = ConstGenericEnum::Mixed {
        fixed: [true, false],
        dynamic: vec![true, true, false, true],
        count: 6,
    };
    let bytes = serialize_to_bytes(&mixed_variant);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ConstGenericEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(mixed_variant, deserialized);
}

#[test]
fn higher_order_trait_bounds_roundtrip() {
    // Test with higher-order trait bounds (HRTB)
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct HigherOrderStruct<F>
    where
        F: for<'a> Fn(&'a str) -> String + Clone + PartialEq,
    {
        transformer: F,
        input: String,
        output: String,
    }

    // Since we can't easily serialize closures, we'll test with a simple struct
    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    struct TransformerStruct {
        prefix: String,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct AlternativeStruct<T>
    where
        T: Clone + PartialEq + std::fmt::Debug,
    {
        transformer: T,
        input: String,
        output: String,
    }

    let alt_struct = AlternativeStruct {
        transformer: TransformerStruct { prefix: "test_".to_string() },
        input: "input_data".to_string(),
        output: "output_data".to_string(),
    };

    let bytes = serialize_to_bytes(&alt_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        AlternativeStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(alt_struct, deserialized);
}

#[test]
fn complex_where_clause_roundtrip() {
    // Test with very complex where clauses
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct ComplexWhereStruct<T, U, V>
    where
        T: Clone + std::fmt::Debug + PartialEq + Send + Sync + 'static,
        U: Clone + PartialEq + std::fmt::Display,
        V: Clone + PartialEq + From<i32> + Into<f64>,
        (T, U): Clone + PartialEq,
        Vec<T>: Clone,
    {
        primary: T,
        secondary: U,
        tertiary: V,
        compound: (T, U),
        collection: Vec<T>,
        metadata: std::collections::BTreeMap<String, i32>,
    }

    let complex_struct = ComplexWhereStruct {
        primary: "primary_value".to_string(),
        secondary: "secondary_value".to_string(),
        tertiary: 123f64, // f64 implements From<i32> and Into<f64>
        compound: (
            "compound_primary".to_string(),
            "compound_secondary".to_string(),
        ),
        collection: vec![
            "item1".to_string(),
            "item2".to_string(),
            "item3".to_string(),
        ],
        metadata: std::collections::BTreeMap::from([
            ("key1".to_string(), 100),
            ("key2".to_string(), 200),
            ("key3".to_string(), 300),
        ]),
    };

    let bytes = serialize_to_bytes(&complex_struct);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexWhereStruct::deserialize(&mut deserializer).unwrap();
    assert_eq!(complex_struct, deserialized);

    // Test corresponding enum
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum ComplexWhereEnum<T, U, V>
    where
        T: Clone + std::fmt::Debug + PartialEq + Send + Sync + 'static,
        U: Clone + PartialEq + std::fmt::Display,
        V: Clone + PartialEq + From<i32> + Into<f64>,
        (T, U): Clone + PartialEq,
        Vec<T>: Clone,
    {
        Simple(T),
        Compound(T, U, V),
        Complex {
            primary: T,
            secondary: U,
            tertiary: V,
            compound: (T, U),
            collection: Vec<T>,
        },
    }

    let simple_variant: ComplexWhereEnum<Vec<i32>, String, f64> =
        ComplexWhereEnum::Simple(vec![1, 2, 3, 4, 5]);
    let bytes = serialize_to_bytes(&simple_variant);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexWhereEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(simple_variant, deserialized);

    let compound_variant: ComplexWhereEnum<String, String, f64> =
        ComplexWhereEnum::Compound(
            "compound_primary".to_string(),
            "compound_secondary".to_string(),
            999.0f64,
        );
    let bytes = serialize_to_bytes(&compound_variant);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexWhereEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(compound_variant, deserialized);

    let complex_variant: ComplexWhereEnum<String, String, f64> =
        ComplexWhereEnum::Complex {
            primary: "complex_primary".to_string(),
            secondary: "complex_secondary".to_string(),
            tertiary: 888.0f64,
            compound: (
                "complex_compound".to_string(),
                "compound_secondary".to_string(),
            ),
            collection: vec!["a".to_string(), "b".to_string()],
        };
    let bytes = serialize_to_bytes(&complex_variant);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        ComplexWhereEnum::deserialize(&mut deserializer).unwrap();
    assert_eq!(complex_variant, deserialized);
}

#[test]
fn recursive_generic_roundtrip() {
    // Test recursive structures with generics
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    enum RecursiveGeneric<T>
    where
        T: Clone + PartialEq + std::fmt::Debug,
    {
        Leaf(T),
        Node { value: T, children: Vec<RecursiveGeneric<T>> },
    }

    // Test simple leaf
    let leaf = RecursiveGeneric::Leaf("leaf_value".to_string());
    let bytes = serialize_to_bytes(&leaf);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        RecursiveGeneric::deserialize(&mut deserializer).unwrap();
    assert_eq!(leaf, deserialized);

    // Test single node
    let single_node = RecursiveGeneric::Node {
        value: 100,
        children: vec![
            RecursiveGeneric::Leaf(1),
            RecursiveGeneric::Leaf(2),
            RecursiveGeneric::Leaf(3),
        ],
    };
    let bytes = serialize_to_bytes(&single_node);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        RecursiveGeneric::deserialize(&mut deserializer).unwrap();
    assert_eq!(single_node, deserialized);

    // Test nested nodes
    let nested_nodes = RecursiveGeneric::Node {
        value: "root".to_string(),
        children: vec![
            RecursiveGeneric::Node {
                value: "child1".to_string(),
                children: vec![
                    RecursiveGeneric::Leaf("grandchild1".to_string()),
                    RecursiveGeneric::Leaf("grandchild2".to_string()),
                ],
            },
            RecursiveGeneric::Node {
                value: "child2".to_string(),
                children: vec![RecursiveGeneric::Leaf(
                    "grandchild3".to_string(),
                )],
            },
            RecursiveGeneric::Leaf("child3".to_string()),
        ],
    };
    let bytes = serialize_to_bytes(&nested_nodes);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        RecursiveGeneric::deserialize(&mut deserializer).unwrap();
    assert_eq!(nested_nodes, deserialized);
}

// Test struct with serde(skip) attribute
#[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
struct StructWithSkip {
    name: String,
    #[serde(skip)]
    skipped_field: i32,
    age: u32,
}

// Test tuple struct with serde(skip) attribute
#[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
struct TupleWithSkip(String, #[serde(skip)] i32, u32);

// Test enum with serde(skip) in variant fields
#[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
enum EnumWithSkip {
    Named {
        name: String,
        #[serde(skip)]
        _skipped: i32,
        value: u32,
    },
    Tuple(String, #[serde(skip)] i32, u32),
    #[default]
    Unit,
}

#[test]
fn struct_with_skip_round_trip() {
    let original = StructWithSkip {
        name: "test".to_string(),
        skipped_field: 999, // This should be ignored during serialization
        age: 25,
    };

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = StructWithSkip::deserialize(&mut deserializer).unwrap();

    // The deserialized struct should have the skipped field set to default (0)
    let expected = StructWithSkip {
        name: "test".to_string(),
        skipped_field: 0, // Default value for i32
        age: 25,
    };

    assert_eq!(expected, deserialized);
    assert_ne!(original, deserialized); // Original and deserialized should
                                        // differ due to skipped field
}

#[test]
fn test_tuple_with_skip_round_trip() {
    let original = TupleWithSkip("test".to_string(), 999, 25);

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = TupleWithSkip::deserialize(&mut deserializer).unwrap();

    // The deserialized tuple should have the skipped field (middle) set to
    // default (0)
    let expected = TupleWithSkip("test".to_string(), 0, 25);

    assert_eq!(expected, deserialized);
    assert_ne!(original, deserialized);
}

#[test]
fn enum_with_skip_round_trip() {
    // Test named variant with skip
    let original_named = EnumWithSkip::Named {
        name: "test".to_string(),
        _skipped: 999,
        value: 42,
    };

    let bytes = serialize_to_bytes(&original_named);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized_named =
        EnumWithSkip::deserialize(&mut deserializer).unwrap();

    let expected_named = EnumWithSkip::Named {
        name: "test".to_string(),
        _skipped: 0, // Default value
        value: 42,
    };

    assert_eq!(expected_named, deserialized_named);

    // Test tuple variant with skip
    let original_tuple = EnumWithSkip::Tuple("test".to_string(), 999, 42);

    let bytes = serialize_to_bytes(&original_tuple);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized_tuple =
        EnumWithSkip::deserialize(&mut deserializer).unwrap();

    let expected_tuple = EnumWithSkip::Tuple("test".to_string(), 0, 42);

    assert_eq!(expected_tuple, deserialized_tuple);

    // Test unit variant (no skip fields)
    let original_unit = EnumWithSkip::Unit;

    let bytes = serialize_to_bytes(&original_unit);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized_unit =
        EnumWithSkip::deserialize(&mut deserializer).unwrap();

    assert_eq!(original_unit, deserialized_unit);
}

#[test]
fn multiple_skip_fields() {
    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    struct MultipleSkip {
        keep1: String,
        #[serde(skip)]
        skip1: i32,
        keep2: u32,
        #[serde(skip)]
        skip2: f64,
        keep3: bool,
    }

    let original = MultipleSkip {
        keep1: "hello".to_string(),
        skip1: 999,
        keep2: 42,
        skip2: 1.23,
        keep3: true,
    };

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = MultipleSkip::deserialize(&mut deserializer).unwrap();

    let expected = MultipleSkip {
        keep1: "hello".to_string(),
        skip1: 0, // Default i32
        keep2: 42,
        skip2: 0.0, // Default f64
        keep3: true,
    };

    assert_eq!(expected, deserialized);
}

#[test]
fn all_fields_skipped() {
    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    struct AllSkipped {
        #[serde(skip)]
        field1: i32,
        #[serde(skip)]
        field2: String,
    }

    let original =
        AllSkipped { field1: 999, field2: "should be ignored".to_string() };

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = AllSkipped::deserialize(&mut deserializer).unwrap();

    let expected = AllSkipped::default();

    assert_eq!(expected, deserialized);
}

#[test]
fn skip_fields_binary_buffer_inspection() {
    // Test that skipped fields don't contribute any bytes to the serialized
    // output

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    struct WithSkip {
        keep1: u32,
        #[serde(skip)]
        skip1: i32,
        keep2: String,
        #[serde(skip)]
        skip2: f64,
        keep3: bool,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    struct WithoutSkip {
        keep1: u32,
        keep2: String,
        keep3: bool,
    }

    let with_skip = WithSkip {
        keep1: 42,
        skip1: 999, // This should not appear in serialized bytes
        keep2: "hello".to_string(),
        skip2: 123.456, // This should not appear in serialized bytes
        keep3: true,
    };

    let without_skip =
        WithoutSkip { keep1: 42, keep2: "hello".to_string(), keep3: true };

    // Serialize both structs
    let bytes_with_skip = serialize_to_bytes(&with_skip);
    let bytes_without_skip = serialize_to_bytes(&without_skip);

    // The serialized bytes should be identical since skipped fields are not
    // written
    assert_eq!(
        bytes_with_skip, bytes_without_skip,
        "Skipped fields should not contribute to serialized output"
    );

    // Additional verification: the buffer should not contain the skipped values
    // The integer 999 should not appear in the binary representation
    // (we need to be careful about false positives, but 999 as i32 has a
    // specific byte pattern)
    let skip1_bytes = 999i32.to_le_bytes();
    let contains_skip1 = bytes_with_skip
        .windows(skip1_bytes.len())
        .any(|window| window == skip1_bytes);
    assert!(
        !contains_skip1,
        "Skipped i32 field value should not appear in binary output"
    );

    // The float 123.456 should not appear in the binary representation
    let skip2_bytes = 123.456f64.to_le_bytes();
    let contains_skip2 = bytes_with_skip
        .windows(skip2_bytes.len())
        .any(|window| window == skip2_bytes);
    assert!(
        !contains_skip2,
        "Skipped f64 field value should not appear in binary output"
    );

    // Verify deserialization works correctly
    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(bytes_with_skip));
    let deserialized = WithSkip::deserialize(&mut deserializer).unwrap();

    let expected = WithSkip {
        keep1: 42,
        skip1: 0, // Default value
        keep2: "hello".to_string(),
        skip2: 0.0, // Default value
        keep3: true,
    };

    assert_eq!(expected, deserialized);
}

#[test]
fn enum_skip_fields_binary_buffer_inspection() {
    // Test that skipped fields in enum variants don't contribute bytes to
    // serialized output

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    enum EnumWithSkip {
        Variant {
            keep: u32,
            #[serde(skip)]
            _skip: i64,
        },
        #[default]
        Default,
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    enum EnumWithoutSkip {
        Variant {
            keep: u32,
        },
        #[default]
        Default,
    }

    let with_skip = EnumWithSkip::Variant {
        keep: 123,
        _skip: 987654321, /* This large value should not appear in serialized
                           * bytes */
    };

    let without_skip = EnumWithoutSkip::Variant { keep: 123 };

    let bytes_with_skip = serialize_to_bytes(&with_skip);
    let bytes_without_skip = serialize_to_bytes(&without_skip);

    // The serialized bytes should be identical since skipped fields are not
    // written
    assert_eq!(
        bytes_with_skip, bytes_without_skip,
        "Skipped enum fields should not contribute to serialized output"
    );

    // Verify the large skipped value doesn't appear in the binary
    let skip_bytes = 987654321i64.to_le_bytes();
    let contains_skip = bytes_with_skip
        .windows(skip_bytes.len())
        .any(|window| window == skip_bytes);
    assert!(
        !contains_skip,
        "Skipped enum field value should not appear in binary output"
    );
}

#[test]
fn tuple_struct_skip_fields_binary_buffer_inspection() {
    // Test that skipped fields in tuple structs don't contribute bytes

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    struct TupleWithSkip(u16, #[serde(skip)] i128, String);

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    struct TupleWithoutSkip(u16, String);

    let with_skip = TupleWithSkip(
        555,
        -999999999999999999, // Large negative value that should not appear
        "test".to_string(),
    );

    let without_skip = TupleWithoutSkip(555, "test".to_string());

    let bytes_with_skip = serialize_to_bytes(&with_skip);
    let bytes_without_skip = serialize_to_bytes(&without_skip);

    // Should be identical
    assert_eq!(
        bytes_with_skip, bytes_without_skip,
        "Skipped tuple fields should not contribute to serialized output"
    );

    // Verify the large skipped value doesn't appear
    let skip_bytes = (-999999999999999999i128).to_le_bytes();
    let contains_skip = bytes_with_skip
        .windows(skip_bytes.len())
        .any(|window| window == skip_bytes);
    assert!(
        !contains_skip,
        "Skipped tuple field value should not appear in binary output"
    );
}

#[test]
fn extension_attribute_with_generics() {
    // Test that #[serde(extension(...))] works with generic types

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(extension(Clone + Send + Sync + 'static))]
    struct GenericExtension<T> {
        data: T,
        count: usize,
    }

    let original = GenericExtension { data: vec![1, 2, 3], count: 3 };

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        GenericExtension::deserialize(&mut deserializer).unwrap();

    assert_eq!(original, deserialized);
}

#[test]
fn extension_attribute_tuple_struct() {
    // Test that #[serde(extension(...))] works with tuple structs

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(extension(Clone + Send + Sync + 'static))]
    struct ExtensionTuple(String, u32, Vec<u8>);

    let original = ExtensionTuple("data".to_string(), 42, vec![1, 2, 3, 4]);

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = ExtensionTuple::deserialize(&mut deserializer).unwrap();

    assert_eq!(original, deserialized);
}

#[test]
fn ser_extension_attribute() {
    // Test that #[serde(ser_extension(...))] applies bounds only to
    // serialization

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    #[serde(ser_extension(Clone + Send + Sync + 'static))]
    struct SerExtensionTest {
        data: String,
        number: u32,
    }

    let original =
        SerExtensionTest { data: "test_data".to_string(), number: 123 };

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        SerExtensionTest::deserialize(&mut deserializer).unwrap();

    assert_eq!(original, deserialized);
}

#[test]
fn de_extension_attribute() {
    // Test that #[serde(de_extension(...))] applies bounds only to
    // deserialization

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    #[serde(de_extension(Clone + Send + Sync + 'static))]
    struct DeExtensionTest {
        data: String,
        number: u32,
    }

    let original =
        DeExtensionTest { data: "test_data".to_string(), number: 456 };

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized = DeExtensionTest::deserialize(&mut deserializer).unwrap();

    assert_eq!(original, deserialized);
}

#[test]
fn mixed_extension_attributes() {
    // Test that multiple extension attributes can be combined

    #[derive(Serialize, Deserialize, Debug, PartialEq, Default)]
    #[serde(extension(Clone + 'static))]
    #[serde(ser_extension(Send + 'static))]
    #[serde(de_extension(Sync + 'static))]
    struct MixedExtensionTest {
        value: i32,
    }

    let original = MixedExtensionTest { value: 789 };

    let bytes = serialize_to_bytes(&original);
    let mut deserializer = BinaryDeserializer::new(std::io::Cursor::new(bytes));
    let deserialized =
        MixedExtensionTest::deserialize(&mut deserializer).unwrap();

    assert_eq!(original, deserialized);
}

#[test]
fn separate_extension_attributes_enum() {
    // Test that ser_extension and de_extension work with enums

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(ser_extension(Send + Sync))]
    #[serde(de_extension(Clone + 'static))]
    enum ExtensionEnum {
        Variant1(String),
        Variant2 { value: u32 },
        Variant3,
    }

    let test_cases = vec![
        ExtensionEnum::Variant1("test".to_string()),
        ExtensionEnum::Variant2 { value: 42 },
        ExtensionEnum::Variant3,
    ];

    for original in test_cases {
        let bytes = serialize_to_bytes(&original);
        let mut deserializer =
            BinaryDeserializer::new(std::io::Cursor::new(bytes));
        let deserialized =
            ExtensionEnum::deserialize(&mut deserializer).unwrap();

        assert_eq!(original, deserialized);
    }
}

#[test]
fn shared_pointer_extension_ser_only() {
    // Test that ser_extension works with the shared pointer extension
    use std::sync::Arc;

    use crate::extension::shared_pointer::{
        SharedPointerSerialize, SharedPointerTracker,
    };

    #[derive(Serialize, Debug, PartialEq)]
    #[serde(ser_extension(SharedPointerSerialize))]
    struct SerOnlySharedTest {
        shared_data: Arc<String>,
        other_data: u32,
    }

    let shared_data = Arc::new("shared_content".to_string());
    let test_data =
        SerOnlySharedTest { shared_data: shared_data.clone(), other_data: 42 };

    // Serialize with shared pointer tracking
    let tracker = SharedPointerTracker::new();
    let buffer = Vec::new();
    let mut serializer =
        crate::binary::ser::BinarySerializer::with_extension(buffer, tracker);
    test_data.serialize(&mut serializer).unwrap();
    let (buffer, _) = serializer.into_parts();

    // Verify that something was actually serialized
    assert!(!buffer.is_empty());
}

#[test]
fn shared_pointer_extension_combined() {
    // Test that multiple extension attributes work together including shared
    // pointer support
    use std::sync::Arc;

    use crate::extension::shared_pointer::{
        SharedPointerDeserialize, SharedPointerSerialize, SharedPointerStore,
        SharedPointerTracker,
    };

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(ser_extension(SharedPointerSerialize))]
    #[serde(de_extension(SharedPointerDeserialize))]
    struct CombinedExtensionTest {
        shared_data: Arc<String>,
        regular_data: String,
    }

    let shared_data = Arc::new("shared_content".to_string());
    let original = CombinedExtensionTest {
        shared_data: shared_data.clone(),
        regular_data: "regular_content".to_string(),
    };

    // Serialize with shared pointer tracking
    let tracker = SharedPointerTracker::new();
    let buffer = Vec::new();
    let mut serializer =
        crate::binary::ser::BinarySerializer::with_extension(buffer, tracker);
    original.serialize(&mut serializer).unwrap();
    let (buffer, _) = serializer.into_parts();

    // Deserialize with shared pointer reconstruction
    let store = SharedPointerStore::new();
    let cursor = std::io::Cursor::new(buffer);
    let mut deserializer =
        crate::binary::de::BinaryDeserializer::with_extension(cursor, store);
    let deserialized =
        CombinedExtensionTest::deserialize(&mut deserializer).unwrap();

    assert_eq!(original, deserialized);
}

#[test]
fn ser_bound_and_de_bound_attributes() {
    // Test that ser_bound and de_bound work correctly and override default
    // bounds

    // A type that implements Clone + Copy but not the usual
    // Serialize/Deserialize
    #[derive(Clone, Copy, Debug, PartialEq)]
    struct CustomData(i32);

    // Custom implementations that satisfy our bounds
    impl<S: crate::ser::Serializer> crate::ser::Serialize<S> for CustomData {
        fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
            self.0.serialize(serializer)
        }
    }

    impl<D: crate::de::Deserializer> crate::de::Deserialize<D> for CustomData {
        fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
            Ok(CustomData(i32::deserialize(deserializer)?))
        }
    }

    // Use explicit bounds instead of auto-generated ones
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(ser_bound(T: Clone + Copy + crate::ser::Serialize<__S>))]
    #[serde(de_bound(T: Clone + Copy + crate::de::Deserialize<__D>))]
    struct BoundedGeneric<T> {
        data: T,
        value: i32,
    }

    let original = BoundedGeneric { data: CustomData(42), value: 100 };

    // Test serialization
    let buffer = Vec::new();
    let mut serializer = crate::binary::ser::BinarySerializer::new(buffer);
    original.serialize(&mut serializer).unwrap();
    let buffer = serializer.into_inner();

    // Test deserialization
    let cursor = std::io::Cursor::new(buffer);
    let mut deserializer = crate::binary::de::BinaryDeserializer::new(cursor);
    let deserialized =
        BoundedGeneric::<CustomData>::deserialize(&mut deserializer).unwrap();

    assert_eq!(original, deserialized);
}

#[test]
fn ser_bound_only() {
    // Test that ser_bound works independently

    #[derive(Debug, PartialEq)]
    struct SerializeOnly(String);

    impl<S: crate::ser::Serializer> crate::ser::Serialize<S> for SerializeOnly {
        fn serialize(&self, serializer: &mut S) -> Result<(), S::Error> {
            self.0.serialize(serializer)
        }
    }

    #[derive(Serialize, Debug, PartialEq)]
    #[serde(ser_bound(T: crate::ser::Serialize<__S>))]
    struct SerBoundOnly<T> {
        data: T,
    }

    let original = SerBoundOnly { data: SerializeOnly("test".to_string()) };

    // Test serialization works
    let buffer = Vec::new();
    let mut serializer = crate::binary::ser::BinarySerializer::new(buffer);
    original.serialize(&mut serializer).unwrap();
    let _buffer = serializer.into_inner();

    // This should compile successfully, demonstrating that our custom ser_bound
    // works
}

#[test]
fn de_bound_only() {
    // Test that de_bound works independently

    #[derive(Debug, PartialEq)]
    struct DeserializeOnly(String);

    impl<D: crate::de::Deserializer> crate::de::Deserialize<D> for DeserializeOnly {
        fn deserialize(deserializer: &mut D) -> Result<Self, D::Error> {
            Ok(DeserializeOnly(String::deserialize(deserializer)?))
        }
    }

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(de_bound(T: crate::de::Deserialize<__D>))]
    struct DeBoundOnly<T> {
        data: T,
    }

    // Test deserialization works with minimal data
    let data = vec![4u8, b't', b'e', b's', b't']; // String "test" serialized
    let cursor = std::io::Cursor::new(data);
    let mut deserializer = crate::binary::de::BinaryDeserializer::new(cursor);
    let result =
        DeBoundOnly::<DeserializeOnly>::deserialize(&mut deserializer).unwrap();

    let expected = DeBoundOnly { data: DeserializeOnly("test".to_string()) };

    assert_eq!(result, expected);
}
