use std::collections::{HashMap, HashSet};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_system::arena;
use proptest::{
    prelude::Arbitrary,
    prop_oneof,
    strategy::{Just, Strategy},
};

use crate::{
    table::{module::input::table_with_module_strategy, Table},
    ty, Accessibility, GenericableID, Generics, Module, ModuleChildID,
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct TraitFunction {
    parameters: Vec<(String, bool)>,
    generic_parameters: GenericParameters,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TraitType {
    generic_parameters: GenericParameters,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
enum TraitMember {
    Type(TraitType),
    Function(TraitFunction),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Trait {
    accessibility: Accessibility,
    generic_parameters: GenericParameters,
    trait_members_by_name: HashMap<String, TraitMember>,
}

fn trait_type_strategy(
    parent_generic_parameters: GenericParameters,
) -> impl Strategy<Value = TraitType> {
    generic_parameters_strategy(Some(parent_generic_parameters))
        .prop_map(|generic_parameters| TraitType { generic_parameters })
}

fn trait_function_strategy(
    parent_generic_parameters: GenericParameters,
) -> impl Strategy<Value = TraitFunction> {
    (
        proptest::collection::hash_map(crate::input::name(), proptest::bool::ANY, 0..=4),
        generic_parameters_strategy(Some(parent_generic_parameters)),
    )
        .prop_map(|(parameters, generic_parameters)| TraitFunction {
            parameters: parameters
                .into_iter()
                .map(|(name, is_mutable)| (name, is_mutable))
                .collect(),
            generic_parameters,
        })
}

fn trait_strategy() -> impl Strategy<Value = Trait> {
    let empty_trait = (
        Accessibility::arbitrary(),
        generic_parameters_strategy(None),
    )
        .prop_map(|(accessibility, generic_parameters)| Trait {
            accessibility,
            generic_parameters,
            trait_members_by_name: HashMap::new(),
        });

    empty_trait
        .prop_flat_map(|empty_trait| {
            (
                proptest::collection::hash_map(
                    crate::input::name(),
                    prop_oneof![
                        trait_function_strategy(empty_trait.generic_parameters.clone())
                            .prop_map(TraitMember::Function),
                        trait_type_strategy(empty_trait.generic_parameters.clone())
                            .prop_map(TraitMember::Type)
                    ],
                    0..=4,
                ),
                Just(empty_trait),
            )
        })
        .prop_map(|(trait_members_by_name, mut empty_trait)| {
            empty_trait.trait_members_by_name = trait_members_by_name;
            empty_trait
        })
}

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

fn generic_parameters_strategy(
    parent: Option<GenericParameters>,
) -> impl Strategy<Value = GenericParameters> {
    (
        proptest::collection::hash_set(crate::input::name(), 0..=3),
        proptest::collection::hash_set(crate::input::name(), 0..=3),
    )
        .prop_map(
            move |(lifetime_parameters, type_parameters)| GenericParameters {
                lifetime_parameters: if let Some(parent) = parent.clone() {
                    lifetime_parameters
                        .into_iter()
                        .filter(|name| !parent.lifetime_parameters.contains(name))
                        .collect()
                } else {
                    lifetime_parameters
                },
                type_parameters: if let Some(parent) = parent.clone() {
                    type_parameters
                        .into_iter()
                        .filter(|name| !parent.type_parameters.contains(name))
                        .collect()
                } else {
                    type_parameters
                },
            },
        )
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
        generic_parameters_strategy(None),
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
        generic_parameters_strategy(None),
    )
        .prop_map(|(accessibility, parameters, generic_parameters)| Function {
            accessibility,
            parameters: parameters.into_iter().collect(),
            generic_parameters,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Type {
    accessibility: Accessibility,
    generic_parameters: GenericParameters,
}

fn type_strategy() -> impl Strategy<Value = Type> {
    (
        Accessibility::arbitrary(),
        generic_parameters_strategy(None),
    )
        .prop_map(|(accessibility, generic_parameters)| Type {
            accessibility,
            generic_parameters,
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Drafting {
    Enum(Enum),
    Struct(Struct),
    Function(Function),
    Type(Type),
    Trait(Trait),
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
    ) -> ModuleChildID {
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

        function_id.into()
    }

    fn apply_struct_drafting(
        &mut self,
        parent_module_id: arena::ID<Module>,
        name: String,
        drafting: Struct,
    ) -> ModuleChildID {
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

        struct_id.into()
    }

    fn apply_type_drafting(
        &mut self,
        parent_module_id: arena::ID<Module>,
        name: String,
        drafting: &Type,
    ) -> ModuleChildID {
        let type_id = self.types.push(crate::Type {
            name,
            accessibility: drafting.accessibility,
            parent_module_id,
            alias: ty::Type::PrimitiveType(ty::PrimitiveType::Void),
            generic_parameters: crate::GenericParameters::default(), // to be filled later
            syntax_tree: None,
        });

        let mut generic_parameters = crate::GenericParameters::default();
        self.apply_generic_parameters(
            type_id.into(),
            &drafting.generic_parameters,
            &mut generic_parameters,
        );
        self.types[type_id].generic_parameters = generic_parameters;

        type_id.into()
    }

    fn apply_trait_drafting(
        &mut self,
        parent_module_id: arena::ID<Module>,
        name: String,
        drafting: Trait,
    ) -> ModuleChildID {
        let trait_id = self.traits.push(crate::Trait {
            name,
            parent_module_id,
            generics: Generics::default(), // to be filled later
            implements: Vec::new(),        // to be filled later
            syntax_tree: None,
            accessibility: drafting.accessibility,
            trait_member_ids_by_name: HashMap::new(), // to be filled later
        });

        let mut generic_parameters = crate::GenericParameters::default();
        self.apply_generic_parameters(
            trait_id.into(),
            &drafting.generic_parameters,
            &mut generic_parameters,
        );
        self.traits[trait_id].generics.parameters = generic_parameters;

        for (name, trait_member) in drafting.trait_members_by_name {
            let trait_member_id = match trait_member {
                TraitMember::Type(trait_type) => {
                    let trait_type_id = self.trait_types.push(crate::TraitType {
                        name: name.clone(),
                        generic_parameters: crate::GenericParameters::default(), /* to be filled
                                                                                  * later */
                        parent_trait_id: trait_id,
                        syntax_tree: None,
                    });

                    let mut generic_parameters = crate::GenericParameters::default();
                    self.apply_generic_parameters(
                        trait_type_id.into(),
                        &trait_type.generic_parameters,
                        &mut generic_parameters,
                    );
                    self.trait_types[trait_type_id].generic_parameters = generic_parameters;

                    trait_type_id.into()
                }
                TraitMember::Function(trait_function) => {
                    let trait_function_id = self.trait_functions.push(crate::TraitFunction {
                        function_signature: crate::FunctionSignature {
                            name: name.clone(),
                            parameter_ids_by_name: HashMap::new(), // to be filled later
                            parameter_order: Vec::new(),           // to be filled later
                            return_type: ty::Type::PrimitiveType(ty::PrimitiveType::Void),
                            syntax_tree: None,
                            generics: Generics::default(), // to be fiiled later
                        },
                        parent_trait_id: trait_id,
                    });

                    let mut generic_parameters = crate::GenericParameters::default();
                    self.apply_generic_parameters(
                        trait_function_id.into(),
                        &trait_function.generic_parameters,
                        &mut generic_parameters,
                    );
                    self.trait_functions[trait_function_id].generics.parameters =
                        generic_parameters;

                    for (index, (name, is_mutable)) in
                        trait_function.parameters.into_iter().enumerate()
                    {
                        let parameter_id = self.parameters.push(crate::Parameter {
                            name: name.clone(),
                            is_mutable,
                            syntax_tree: None,
                            parameter_parent_id: trait_function_id.into(),
                            declaration_order: index,
                            ty: ty::Type::PrimitiveType(ty::PrimitiveType::Void),
                        });

                        let function_signature =
                            &mut self.trait_functions[trait_function_id].function_signature;
                        function_signature
                            .parameter_ids_by_name
                            .insert(name.clone(), parameter_id);
                        function_signature.parameter_order.push(parameter_id);
                    }

                    trait_function_id.into()
                }
            };

            self.traits[trait_id]
                .trait_member_ids_by_name
                .insert(name.clone(), trait_member_id);
        }

        trait_id.into()
    }

    fn apply_enum_drafting(
        &mut self,
        parent_module_id: arena::ID<Module>,
        name: String,
        drafting: &Enum,
    ) -> ModuleChildID {
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

        enum_id.into()
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
                        type_strategy().prop_map(Drafting::Type),
                        trait_strategy().prop_map(Drafting::Trait),
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

                let module_child_id = match drafing {
                    Drafting::Enum(drafting) => {
                        table.apply_enum_drafting(module_id, name.clone(), &drafting)
                    }
                    Drafting::Function(drafting) => {
                        table.apply_function_drafting(module_id, name.clone(), drafting)
                    }
                    Drafting::Struct(drafting) => {
                        table.apply_struct_drafting(module_id, name.clone(), drafting)
                    }
                    Drafting::Type(drafting) => {
                        table.apply_type_drafting(module_id, name.clone(), &drafting)
                    }
                    Drafting::Trait(drafting) => {
                        table.apply_trait_drafting(module_id, name.clone(), drafting)
                    }
                };

                table.modules[module_id]
                    .module_child_ids_by_name
                    .insert(name, module_child_id);
            }
        }

        table
    })
}
