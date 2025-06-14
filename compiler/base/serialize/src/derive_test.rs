//! Tests for the Serialize derive macro.

use crate::{binary::ser::BinarySerializer, Serialize};

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
