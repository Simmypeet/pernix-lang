use super::Equivalent;
use crate::semantic::term::r#type::{Primitive, Type};

#[test]
fn default_case() {
    let first = Type::Primitive(Primitive::Int32);
    let second = Type::Primitive(Primitive::Int64);

    let mut equivalent = Equivalent::new();

    equivalent.insert(first.clone(), second.clone());

    assert_eq!(equivalent.lifetime_classes.len(), 0);
    assert_eq!(equivalent.type_classes.len(), 1);
    assert_eq!(equivalent.constant_classes.len(), 0);

    assert_eq!(equivalent.type_classes[0].len(), 2);
    assert!(equivalent.type_classes[0].contains(&first));
    assert!(equivalent.type_classes[0].contains(&second));
}

#[test]
fn duplicating_case() {
    let first = Type::Primitive(Primitive::Int32);
    let second = Type::Primitive(Primitive::Int32);

    let mut equivalent = Equivalent::new();

    equivalent.insert(first.clone(), second.clone());

    assert_eq!(equivalent.lifetime_classes.len(), 0);
    assert_eq!(equivalent.type_classes.len(), 1);
    assert_eq!(equivalent.constant_classes.len(), 0);

    assert_eq!(equivalent.type_classes[0].len(), 1);
    assert!(equivalent.type_classes[0].contains(&first));
    assert!(equivalent.type_classes[0].contains(&second));
}

#[test]
fn add_to_existing_class_case() {
    let first = Type::Primitive(Primitive::Int64);
    let second = Type::Primitive(Primitive::Int32);

    let mut equivalent = Equivalent::new();

    equivalent.insert(first.clone(), second.clone());

    assert_eq!(equivalent.lifetime_classes.len(), 0);
    assert_eq!(equivalent.type_classes.len(), 1);
    assert_eq!(equivalent.constant_classes.len(), 0);

    assert_eq!(equivalent.type_classes[0].len(), 2);
    assert!(equivalent.type_classes[0].contains(&first));
    assert!(equivalent.type_classes[0].contains(&second));

    let thrid = Type::Primitive(Primitive::Int16);

    equivalent.insert(first.clone(), thrid.clone());

    assert_eq!(equivalent.lifetime_classes.len(), 0);
    assert_eq!(equivalent.type_classes.len(), 1);
    assert_eq!(equivalent.constant_classes.len(), 0);

    assert_eq!(equivalent.type_classes[0].len(), 3);
    assert!(equivalent.type_classes[0].contains(&first));
    assert!(equivalent.type_classes[0].contains(&second));
    assert!(equivalent.type_classes[0].contains(&thrid));
}

#[test]
fn merging_case() {
    let first = Type::Primitive(Primitive::Int64);
    let second = Type::Primitive(Primitive::Int32);

    let mut equivalent = Equivalent::new();

    equivalent.insert(first.clone(), second.clone());

    assert_eq!(equivalent.lifetime_classes.len(), 0);
    assert_eq!(equivalent.type_classes.len(), 1);
    assert_eq!(equivalent.constant_classes.len(), 0);

    assert_eq!(equivalent.type_classes[0].len(), 2);
    assert!(equivalent.type_classes[0].contains(&first));
    assert!(equivalent.type_classes[0].contains(&second));

    let thrid = Type::Primitive(Primitive::Int16);
    let fourth = Type::Primitive(Primitive::Int8);

    equivalent.insert(thrid.clone(), fourth.clone());

    assert_eq!(equivalent.lifetime_classes.len(), 0);
    assert_eq!(equivalent.type_classes.len(), 2);
    assert_eq!(equivalent.constant_classes.len(), 0);

    assert_eq!(equivalent.type_classes[0].len(), 2);
    assert!(equivalent.type_classes[0].contains(&first));
    assert!(equivalent.type_classes[0].contains(&second));

    assert_eq!(equivalent.type_classes[1].len(), 2);
    assert!(equivalent.type_classes[1].contains(&thrid));
    assert!(equivalent.type_classes[1].contains(&fourth));

    equivalent.insert(first.clone(), thrid.clone());

    assert_eq!(equivalent.lifetime_classes.len(), 0);
    assert_eq!(equivalent.type_classes.len(), 1);
    assert_eq!(equivalent.constant_classes.len(), 0);

    assert_eq!(equivalent.type_classes[0].len(), 4);
    assert!(equivalent.type_classes[0].contains(&first));
    assert!(equivalent.type_classes[0].contains(&second));
    assert!(equivalent.type_classes[0].contains(&thrid));
    assert!(equivalent.type_classes[0].contains(&fourth));
}
