use std::collections::{HashMap, HashSet};

use pernixc_system::arena;
use proptest::{
    prelude::Arbitrary,
    prop_oneof,
    strategy::{Just, Strategy},
};

use crate::{
    table::{module::input::table_with_module_strategy, Table},
    ty, Accessibility, GenericableID, Generics, Module,
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
struct GenericParameters {
    lifetime_parameters: HashSet<String>,
    type_parameters: HashSet<String>,
}

fn generic_parameters() -> impl Strategy<Value = GenericParameters> {
    (
        proptest::collection::hash_set(crate::input::name(), 0..=3),
        proptest::collection::hash_set(crate::input::name(), 0..=3),
    )
        .prop_map(|(lifetime_parameters, type_parameters)| GenericParameters {
            lifetime_parameters,
            type_parameters,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Struct {
    accessibility: Accessibility,
    fields: Vec<String>,
    generic_parameters: GenericParameters,
}

fn struct_strategy() -> impl Strategy<Value = Struct> {
    (
        Accessibility::arbitrary(),
        proptest::collection::hash_set(crate::input::name(), 0..=4),
        generic_parameters(),
    )
        .prop_map(|(accessibility, fields, generic_parameters)| Struct {
            accessibility,
            fields: fields.into_iter().collect(),
            generic_parameters,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Function {
    accessibility: Accessibility,
    parameters: Vec<(String, bool)>,
    generic_parameters: GenericParameters,
}

fn function_strategy() -> impl Strategy<Value = Function> {
    (
        Accessibility::arbitrary(),
        proptest::collection::hash_map(crate::input::name(), proptest::bool::ANY, 0..=4),
        generic_parameters(),
    )
        .prop_map(|(accessibility, parameters, generic_parameters)| Function {
            accessibility,
            parameters: parameters.into_iter().collect(),
            generic_parameters,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Drafting {
    Enum(Enum),
    Struct(Struct),
    Function(Function),
}

impl Table {
    fn apply_generic_parameters(
        &mut self,
        parent_genericable_id: GenericableID,
        drafting: &GenericParameters,
        generic_parameters: &mut crate::GenericParameters,
    ) {
        for lifetime_parameter in &drafting.lifetime_parameters {
            let lifetime_parameter_id = self.lifetime_parameters.push(crate::LifetimeParameter {
                name: lifetime_parameter.clone(),
                parent_genericable_id,
            });

            generic_parameters
                .lifetime_parameter_ids_by_name
                .insert(lifetime_parameter.clone(), lifetime_parameter_id);
            generic_parameters
                .lifetime_parameter_order
                .push(lifetime_parameter_id);
        }

        for type_parameter in &drafting.type_parameters {
            let type_parameter_id = self.type_parameters.push(crate::TypeParameter {
                name: type_parameter.clone(),
                parent_genericable_id,
            });

            generic_parameters
                .type_parameter_ids_by_name
                .insert(type_parameter.clone(), type_parameter_id);
            generic_parameters
                .type_parameter_order
                .push(type_parameter_id);
        }
    }

    fn apply_function_drafting(
        &mut self,
        parent_module_id: arena::ID<Module>,
        name: String,
        drafting: Function,
    ) {
        let function_id = self.functions.push(crate::Function {
            function_signature: crate::FunctionSignature {
                name,
                parameter_ids_by_name: HashMap::new(), // to be filled later
                parameter_order: Vec::new(),           // to be filled later
                return_type: ty::Type::PrimitiveType(ty::PrimitiveType::Void),
                syntax_tree: None,
                generics: Generics::default(), // to be filled later
            },
            parent_module_id,
            syntax_tree: None,
            accessibility: drafting.accessibility,
        });

        let mut generic_parameters = crate::GenericParameters::default();
        self.apply_generic_parameters(
            function_id.into(),
            &drafting.generic_parameters,
            &mut generic_parameters,
        );
        self.functions[function_id].generics.parameters = generic_parameters;

        for (index, parameter) in drafting.parameters.into_iter().enumerate() {
            let parameter_id = self.parameters.push(crate::Parameter {
                name: parameter.0.clone(),
                parameter_parent_id: function_id.into(),
                declaration_order: index,
                ty: ty::Type::PrimitiveType(ty::PrimitiveType::Void),
                syntax_tree: None,
                is_mutable: parameter.1,
            });

            let function_symbol = &mut self.functions[function_id];

            function_symbol
                .parameter_ids_by_name
                .insert(parameter.0, parameter_id);
            function_symbol.parameter_order.push(parameter_id);
        }
    }

    fn apply_struct_drafting(
        &mut self,
        parent_module_id: arena::ID<Module>,
        name: String,
        drafting: Struct,
    ) {
        let struct_id = self.structs.push(crate::Struct {
            name,
            accessibility: drafting.accessibility,
            parent_module_id,
            syntax_tree: None,
            field_ids_by_name: HashMap::new(), // to be filled later
            generics: Generics::default(),     // to be filled later
            field_order: Vec::new(),           // to be filled later
        });

        let mut generic_parameters = crate::GenericParameters::default();
        self.apply_generic_parameters(
            struct_id.into(),
            &drafting.generic_parameters,
            &mut generic_parameters,
        );
        self.structs[struct_id].generics.parameters = generic_parameters;

        for (index, field) in drafting.fields.into_iter().enumerate() {
            let field_id = self.fields.push(crate::Field {
                name: field.clone(),
                accessibility: drafting.accessibility,
                parent_struct_id: struct_id,
                syntax_tree: None,
                declaration_order: index,
                ty: ty::Type::PrimitiveType(ty::PrimitiveType::Void),
            });

            let struct_symbol = &mut self.structs[struct_id];

            struct_symbol
                .field_ids_by_name
                .insert(field.clone(), field_id);
            struct_symbol.field_order.push(field_id);
        }
    }

    fn apply_enum_drafting(
        &mut self,
        parent_module_id: arena::ID<Module>,
        name: String,
        drafting: &Enum,
    ) {
        let enum_id = self.enums.push(crate::Enum {
            name,
            accessibility: drafting.accessibility,
            parent_module_id,
            syntax_tree: None,
            variant_ids_by_name: HashMap::new(),
            variant_order: Vec::new(),
        });

        for (index, variant) in drafting.variants.iter().enumerate() {
            let enum_variant_id = self.enum_variants.push(crate::EnumVariant {
                name: variant.clone(),
                parent_enum_id: enum_id,
                declaration_order: index,
                syntax_tree: None,
            });

            let enum_symbol = &mut self.enums[enum_id];
            enum_symbol
                .variant_ids_by_name
                .insert(variant.clone(), enum_variant_id);
            enum_symbol.variant_order.push(enum_variant_id);
        }
    }
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
                        function_strategy().prop_map(Drafting::Function),
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
            for (name, drafing) in draftings_by_name {
                let module = &table.modules[module_id];
                if module.module_child_ids_by_name.contains_key(&name) {
                    continue;
                }

                match drafing {
                    Drafting::Enum(drafting) => {
                        table.apply_enum_drafting(module_id, name, &drafting);
                    }
                    Drafting::Function(drafting) => {
                        table.apply_function_drafting(module_id, name, drafting);
                    }
                    Drafting::Struct(drafting) => {
                        table.apply_struct_drafting(module_id, name, drafting);
                    }
                }
            }
        }

        table
    })
}
