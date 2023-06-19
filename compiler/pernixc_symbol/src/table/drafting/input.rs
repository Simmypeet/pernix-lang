use std::collections::HashSet;

use proptest::{prelude::Arbitrary, strategy::Strategy};

use crate::Accessibility;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Enum {
    accessibility: Accessibility,
    variants: HashSet<String>,
}

fn enum_strategy() -> impl Strategy<Value = Enum> {
    (
        Accessibility::arbitrary(),
        proptest::collection::hash_set(crate::input::name(), 0..=8),
    )
        .prop_map(|(accessibility, variants)| Enum {
            accessibility,
            variants,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct GenericsParameter {
    lifetime_parameters: HashSet<String>,
    type_parameters: HashSet<String>,
}

fn generics_parameters() -> impl Strategy<Value = GenericsParameter> {
    (
        proptest::collection::hash_set(crate::input::name(), 0..=3),
        proptest::collection::hash_set(crate::input::name(), 0..=3),
    )
        .prop_map(|(lifetime_parameters, type_parameters)| GenericsParameter {
            lifetime_parameters,
            type_parameters,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Struct {
    accessibility: Accessibility,
    fields: Vec<String>,
    generics_parameters: GenericsParameter,
}

fn struct_strategy() -> impl Strategy<Value = Struct> {
    (
        Accessibility::arbitrary(),
        proptest::collection::hash_set(crate::input::name(), 0..=4),
        generics_parameters(),
    )
        .prop_map(|(accessibility, fields, generics_parameters)| Struct {
            accessibility,
            fields: fields.into_iter().collect(),
            generics_parameters,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Drafting {
    Enum(Enum),
    Struct(Struct),
}
