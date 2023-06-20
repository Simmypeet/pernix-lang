use std::collections::{HashMap, HashSet};

use proptest::{
    prelude::Arbitrary,
    prop_oneof,
    strategy::{Just, Strategy},
};

use crate::{
    table::{module::input::table_with_module_strategy, Table},
    ty, Accessibility, Generics,
};

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
struct Function {
    accessibility: Accessibility,
    parameters: Vec<String>,
    generics_parameters: GenericsParameter,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Drafting {
    Enum(Enum),
    Struct(Struct),
}

#[allow(clippy::too_many_lines)]
pub(in crate::table) fn table_with_drafting() -> impl Strategy<Value = Table> {
    let table_with_draftings = table_with_module_strategy().prop_flat_map(|table| {
        (
            proptest::collection::hash_map(
                proptest::sample::select(table.modules.ids().collect::<Vec<_>>()),
                proptest::collection::hash_map(
                    crate::input::name(),
                    prop_oneof![
                        enum_strategy().prop_map(Drafting::Enum),
                        struct_strategy().prop_map(Drafting::Struct),
                    ],
                    0..=4,
                ),
                0..=table.modules.len(),
            ),
            Just(table),
        )
    });

    table_with_draftings.prop_map(|(draftings, mut table)| {
        for (module_id, draftings_by_name) in draftings {
            let module = &table.modules[module_id];

            for (name, drafing) in draftings_by_name {
                if module.module_child_ids_by_name.contains_key(&name) {
                    continue;
                }

                match drafing {
                    Drafting::Enum(enum_drafting) => {
                        let enum_id = table.enums.push(crate::Enum {
                            name,
                            accessibility: enum_drafting.accessibility,
                            parent_module_id: module_id,
                            syntax_tree: None,
                            variant_ids_by_name: HashMap::new(),
                            variant_order: Vec::new(),
                        });

                        for (index, variant) in enum_drafting.variants.iter().enumerate() {
                            let enum_variant_id = table.enum_variants.push(crate::EnumVariant {
                                name: variant.clone(),
                                parent_enum_id: enum_id,
                                declaration_order: index,
                                syntax_tree: None,
                            });

                            let enum_symbol = &mut table.enums[enum_id];
                            enum_symbol
                                .variant_ids_by_name
                                .insert(variant.clone(), enum_variant_id);
                            enum_symbol.variant_order.push(enum_variant_id);
                        }
                    }
                    Drafting::Struct(drafting) => {
                        let struct_id = table.structs.push(crate::Struct {
                            name,
                            accessibility: drafting.accessibility,
                            parent_module_id: module_id,
                            syntax_tree: None,
                            field_ids_by_name: HashMap::new(), // to be filled later
                            generics: Generics::default(),     // to be filled later
                            field_order: Vec::new(),           // to be filled later
                        });

                        for lifetime_parameter in &drafting.generics_parameters.lifetime_parameters
                        {
                            let lifetime_parameter_id =
                                table.lifetime_parameters.push(crate::LifetimeParameter {
                                    name: lifetime_parameter.clone(),
                                    parent_genericable_id: struct_id.into(),
                                });

                            let struct_symbol = &mut table.structs[struct_id];
                            struct_symbol
                                .generics
                                .parameters
                                .lifetime_parameter_ids_by_name
                                .insert(lifetime_parameter.clone(), lifetime_parameter_id);
                            struct_symbol
                                .generics
                                .parameters
                                .lifetime_parameter_order
                                .push(lifetime_parameter_id);
                        }

                        for type_parameter in &drafting.generics_parameters.type_parameters {
                            let type_parameter_id =
                                table.type_parameters.push(crate::TypeParameter {
                                    name: type_parameter.clone(),
                                    parent_genericable_id: struct_id.into(),
                                });

                            let struct_symbol = &mut table.structs[struct_id];
                            struct_symbol
                                .generics
                                .parameters
                                .type_parameter_ids_by_name
                                .insert(type_parameter.clone(), type_parameter_id);
                            struct_symbol
                                .generics
                                .parameters
                                .type_parameter_order
                                .push(type_parameter_id);
                        }

                        for (index, field) in drafting.fields.into_iter().enumerate() {
                            let field_id = table.fields.push(crate::Field {
                                name: field.clone(),
                                accessibility: drafting.accessibility,
                                parent_struct_id: struct_id,
                                syntax_tree: None,
                                declaration_order: index,
                                ty: ty::Type::PrimitiveType(ty::PrimitiveType::Void),
                            });

                            let struct_symbol = &mut table.structs[struct_id];

                            struct_symbol.field_ids_by_name.insert(field, field_id);
                            struct_symbol.field_order.push(field_id);
                        }
                    }
                }
            }
        }

        table
    })
}
