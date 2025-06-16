use crate::{
    ron::ser::{
        to_ron_string, to_ron_string_compact, to_ron_string_with_config,
        RonConfig, RonError, RonSerializer,
    },
    ser::Error,
};

#[test]
fn basic_functionality() {
    // This is a basic smoke test
    let config = RonConfig::default();
    assert!(
        matches!(config, RonConfig::Pretty(ref indent) if indent == "    ")
    );

    let error = RonError::custom("test error");
    assert!(error.to_string().contains("test error"));
}

#[test]
fn serializer_creation() {
    let mut buffer = String::new();
    let _serializer = RonSerializer::new(&mut buffer);

    let mut buffer2 = String::new();
    let config = RonConfig::Pretty("  ".to_string());
    let _serializer2 = RonSerializer::with_config(&mut buffer2, config);

    let mut buffer3 = String::new();
    let _serializer3 = RonSerializer::compact(&mut buffer3);
}

#[test]
fn primitives() {
    insta::assert_snapshot!(to_ron_string(&42).unwrap(), @"42");
    insta::assert_snapshot!(to_ron_string(&true).unwrap(), @"true");
    insta::assert_snapshot!(to_ron_string(&false).unwrap(), @"false");
    insta::assert_snapshot!(to_ron_string(&1.5).unwrap(), @"1.5");
    insta::assert_snapshot!(to_ron_string(&"hello world").unwrap(), @r#""hello world""#);
    insta::assert_snapshot!(to_ron_string(&'A').unwrap(), @"'A'");
}

#[test]
fn arrays_compact() {
    let simple = vec![1, 2, 3];
    let nested = vec![vec![1, 2], vec![3, 4, 5], vec![]];

    insta::assert_snapshot!(to_ron_string_compact(&simple).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&nested).unwrap());
}

#[test]
fn arrays_pretty() {
    let simple = vec![1, 2, 3];
    let nested = vec![vec![1, 2], vec![3, 4, 5], vec![]];

    let config = RonConfig::Pretty("    ".to_string());
    insta::assert_snapshot!(
        to_ron_string_with_config(&simple, config.clone()).unwrap()
    );
    insta::assert_snapshot!(
        to_ron_string_with_config(&nested, config.clone()).unwrap()
    );
}

#[test]
fn string_escaping() {
    insta::assert_snapshot!(to_ron_string(&"hello \"world\"").unwrap(), @r#""hello \"world\"""#);
    insta::assert_snapshot!(to_ron_string(&"line1\nline2").unwrap(), @r#""line1\nline2""#);
    insta::assert_snapshot!(to_ron_string(&"tab\there").unwrap(), @r#""tab\there""#);
    insta::assert_snapshot!(to_ron_string(&"backslash\\test").unwrap(), @r#""backslash\\test""#);
}

#[test]
fn simple_arrays_compact() {
    let empty: Vec<i32> = vec![];
    let single = vec![42];
    let multiple = vec![1, 2, 3, 4, 5];

    insta::assert_snapshot!(to_ron_string_compact(&empty).unwrap(), @"[]");
    insta::assert_snapshot!(to_ron_string_compact(&single).unwrap(), @"[42]");
    insta::assert_snapshot!(to_ron_string_compact(&multiple).unwrap(), @"[1,2,3,4,5]");
}

#[test]
fn nested_arrays_compact() {
    let nested = vec![vec![1, 2], vec![3, 4, 5], vec![]];

    insta::assert_snapshot!(to_ron_string_compact(&nested).unwrap(), @"[[1,2],[3,4,5],[]]");
}

// ========================================================================
// Tuple formatting tests
// ========================================================================

#[test]
fn tuples_compact() {
    let empty = ();
    let single = (42,);
    let pair = (1, 2);
    let triple = (1, "hello", true);
    let nested = ((1, 2), (3, 4));

    insta::assert_snapshot!(to_ron_string_compact(&empty).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&single).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&pair).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&triple).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&nested).unwrap());
}

#[test]
fn tuples_pretty() {
    let empty = ();
    let single = (42,);
    let pair = (1, 2);
    let triple = (1, "hello", true);
    let nested = ((1, 2), (3, 4));

    let config = RonConfig::Pretty("    ".to_string());
    insta::assert_snapshot!(
        to_ron_string_with_config(&empty, config.clone()).unwrap()
    );
    insta::assert_snapshot!(
        to_ron_string_with_config(&single, config.clone()).unwrap()
    );
    insta::assert_snapshot!(
        to_ron_string_with_config(&pair, config.clone()).unwrap()
    );
    insta::assert_snapshot!(
        to_ron_string_with_config(&triple, config.clone()).unwrap()
    );
    insta::assert_snapshot!(to_ron_string_with_config(&nested, config).unwrap());
}

// ========================================================================
// Tuple struct formatting tests
// ========================================================================

#[derive(pernixc_serialize_derive::Serialize)]
struct Point(i32, i32);

#[derive(pernixc_serialize_derive::Serialize)]
struct Color(u8, u8, u8, String);

#[test]
fn tuple_structs_compact() {
    let point = Point(10, 20);
    let color = Color(255, 128, 0, "orange".to_string());

    insta::assert_snapshot!(to_ron_string_compact(&point).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&color).unwrap());
}

#[test]
fn tuple_structs_pretty() {
    let point = Point(10, 20);
    let color = Color(255, 128, 0, "orange".to_string());

    let config = RonConfig::Pretty("    ".to_string());
    insta::assert_snapshot!(
        to_ron_string_with_config(&point, config.clone()).unwrap()
    );
    insta::assert_snapshot!(to_ron_string_with_config(&color, config).unwrap());
}

// ========================================================================
// Enum tuple variant formatting tests
// ========================================================================

#[derive(pernixc_serialize_derive::Serialize)]
enum Shape {
    Circle(f64),
    Rectangle(f64, f64),
    Triangle(f64, f64, f64),
}

#[test]
fn enum_tuple_variants_compact() {
    let circle = Shape::Circle(5.0);
    let rectangle = Shape::Rectangle(10.0, 20.0);
    let triangle = Shape::Triangle(3.0, 4.0, 5.0);

    insta::assert_snapshot!(to_ron_string_compact(&circle).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&rectangle).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&triangle).unwrap());
}

#[test]
fn enum_tuple_variants_pretty() {
    let circle = Shape::Circle(5.0);
    let rectangle = Shape::Rectangle(10.0, 20.0);
    let triangle = Shape::Triangle(3.0, 4.0, 5.0);

    let config = RonConfig::Pretty("    ".to_string());
    insta::assert_snapshot!(
        to_ron_string_with_config(&circle, config.clone()).unwrap()
    );
    insta::assert_snapshot!(to_ron_string_with_config(
        &rectangle,
        config.clone()
    )
    .unwrap());
    insta::assert_snapshot!(
        to_ron_string_with_config(&triangle, config).unwrap()
    );
}

// ========================================================================
// Map formatting tests
// ========================================================================

use pernixc_hash::HashMap;

#[test]
fn maps_compact() {
    let empty_map: HashMap<String, i32> = HashMap::default();
    let mut simple_map = HashMap::default();
    simple_map.insert("key1".to_string(), 1);
    simple_map.insert("key2".to_string(), 2);

    let mut nested_map = HashMap::default();
    let mut inner_map = HashMap::default();
    inner_map.insert("inner".to_string(), 42);
    nested_map.insert("outer".to_string(), inner_map);

    insta::assert_snapshot!(to_ron_string_compact(&empty_map).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&simple_map).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&nested_map).unwrap());
}

#[test]
fn maps_pretty() {
    let empty_map: HashMap<String, i32> = HashMap::default();
    let mut simple_map = HashMap::default();
    simple_map.insert("key1".to_string(), 1);
    simple_map.insert("key2".to_string(), 2);

    let mut nested_map = HashMap::default();
    let mut inner_map = HashMap::default();
    inner_map.insert("inner".to_string(), 42);
    nested_map.insert("outer".to_string(), inner_map);

    let config = RonConfig::Pretty("    ".to_string());
    insta::assert_snapshot!(to_ron_string_with_config(
        &empty_map,
        config.clone()
    )
    .unwrap());
    insta::assert_snapshot!(to_ron_string_with_config(
        &simple_map,
        config.clone()
    )
    .unwrap());
    insta::assert_snapshot!(
        to_ron_string_with_config(&nested_map, config).unwrap()
    );
}

// Regular struct with named fields formatting tests
#[derive(pernixc_serialize_derive::Serialize)]
struct Person {
    name: String,
    age: u32,
    active: bool,
}

#[derive(pernixc_serialize_derive::Serialize)]
struct ComplexData {
    id: u64,
    description: String,
    values: Vec<i32>,
    metadata: pernixc_hash::HashMap<String, String>,
}

#[test]
fn structs_compact() {
    let person = Person { name: "Alice".to_string(), age: 30, active: true };

    let complex = ComplexData {
        id: 12345,
        description: "Test data".to_string(),
        values: vec![1, 2, 3],
        metadata: {
            let mut map = pernixc_hash::HashMap::default();
            map.insert("type".to_string(), "test".to_string());
            map.insert("version".to_string(), "1.0".to_string());
            map
        },
    };

    insta::assert_snapshot!(to_ron_string_compact(&person).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&complex).unwrap());
}

#[test]
fn structs_pretty() {
    let person = Person { name: "Alice".to_string(), age: 30, active: true };

    let complex = ComplexData {
        id: 12345,
        description: "Test data".to_string(),
        values: vec![1, 2, 3],
        metadata: {
            let mut map = pernixc_hash::HashMap::default();
            map.insert("type".to_string(), "test".to_string());
            map.insert("version".to_string(), "1.0".to_string());
            map
        },
    };

    let config = RonConfig::Pretty("    ".to_string());
    insta::assert_snapshot!(
        to_ron_string_with_config(&person, config.clone()).unwrap()
    );
    insta::assert_snapshot!(
        to_ron_string_with_config(&complex, config).unwrap()
    );
}

// Enum with struct variants formatting tests
#[derive(pernixc_serialize_derive::Serialize)]
enum Message {
    Text {
        content: String,
        urgent: bool,
    },
    Image {
        url: String,
        width: u32,
        height: u32,
    },
    Multipart {
        title: String,
        parts: Vec<String>,
        metadata: pernixc_hash::HashMap<String, i32>,
    },
}

#[test]
fn enum_struct_variants_compact() {
    let text =
        Message::Text { content: "Hello, world!".to_string(), urgent: false };

    let image = Message::Image {
        url: "https://example.com/pic.jpg".to_string(),
        width: 800,
        height: 600,
    };

    let multipart = Message::Multipart {
        title: "Complex message".to_string(),
        parts: vec!["part1".to_string(), "part2".to_string()],
        metadata: {
            let mut map = pernixc_hash::HashMap::default();
            map.insert("priority".to_string(), 1);
            map.insert("retry_count".to_string(), 3);
            map
        },
    };

    insta::assert_snapshot!(to_ron_string_compact(&text).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&image).unwrap());
    insta::assert_snapshot!(to_ron_string_compact(&multipart).unwrap());
}

#[test]
fn enum_struct_variants_pretty() {
    let text =
        Message::Text { content: "Hello, world!".to_string(), urgent: false };

    let image = Message::Image {
        url: "https://example.com/pic.jpg".to_string(),
        width: 800,
        height: 600,
    };

    let multipart = Message::Multipart {
        title: "Complex message".to_string(),
        parts: vec!["part1".to_string(), "part2".to_string()],
        metadata: {
            let mut map = pernixc_hash::HashMap::default();
            map.insert("priority".to_string(), 1);
            map.insert("retry_count".to_string(), 3);
            map
        },
    };

    let config = RonConfig::Pretty("    ".to_string());
    insta::assert_snapshot!(
        to_ron_string_with_config(&text, config.clone()).unwrap()
    );
    insta::assert_snapshot!(
        to_ron_string_with_config(&image, config.clone()).unwrap()
    );
    insta::assert_snapshot!(
        to_ron_string_with_config(&multipart, config).unwrap()
    );
}
