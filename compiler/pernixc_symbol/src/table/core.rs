use std::collections::HashMap;

use super::Table;
use crate::{
    ty::{PrimitiveType, ReferenceQualifier, Type},
    Accessibility, Generics, ModuleID, Trait, TraitFunction, TypeParameter,
};

impl Table {
    // Creates a table with core-language symbols
    pub(super) fn create_core_symbols(&mut self) {
        // core module id
        let core_module_id = self.modules.insert(crate::Module {
            name: "@core".to_string(),
            accessibility: Accessibility::Public,
            parent_module_id: None,
            child_ids_by_name: HashMap::new(),
            usings: Vec::new(),
        });

        // add to the root
        self.root_module_ids_by_target_name
            .insert("@core".to_string(), core_module_id);

        self.create_drop_trait(core_module_id);
        self.create_copy_trait(core_module_id);
    }

    fn create_copy_trait(&mut self, core_module_id: ModuleID) {
        // Drop trait
        let (trait_id, type_parameter_id) = {
            let trait_id = self.traits.insert(Trait {
                name: "@Copy".to_string(),
                parent_module_id: core_module_id,
                generics: Generics::default(),
                implements: Vec::new(),
                trait_member_ids_by_name: HashMap::new(), // to be filled later
            });

            // A type parameter for the type being dropped
            let type_parameter_id = self.type_parameters.insert(TypeParameter {
                name: "T".to_string(),
                parent_genericable_id: trait_id.into(),
            });

            let trait_sym = self.traits.get_mut(trait_id).unwrap();

            trait_sym
                .generics
                .generic_parameters
                .type_parameter_order
                .push(type_parameter_id);

            trait_sym
                .generics
                .generic_parameters
                .type_parameter_ids_by_name
                .insert("T".to_string(), type_parameter_id);

            (trait_id, type_parameter_id)
        };

        // create a function for trait
        let trait_function_id = {
            let trait_function_id = self.trait_functions.insert(TraitFunction {
                function_signature: crate::FunctionSignature {
                    name: "copy".to_string(),
                    parameter_ids_by_name: HashMap::new(), // to be filled later
                    parameter_order: Vec::new(),           // to be filled later
                    return_type: Type::TypeParameter(type_parameter_id),
                    generics: Generics::default(),
                    syntax_tree: None,
                },
                parent_trait_id: trait_id,
            });

            let parameter_id = self.parameters.insert(crate::Parameter {
                name: "this".to_string(),
                parent_parameter_id: trait_function_id.into(),
                declaration_order: 0,
                ty: Type::ReferenceType(crate::ty::ReferenceType {
                    operand: Box::new(Type::TypeParameter(type_parameter_id)),
                    qualifier: None,
                    lifetime: None,
                }),
                is_mutable: false,
                syntax_tree: None,
            });

            let trait_function = self.trait_functions.get_mut(trait_function_id).unwrap();

            trait_function
                .parameter_ids_by_name
                .insert("this".to_string(), parameter_id);

            trait_function.parameter_order.push(parameter_id);

            trait_function_id
        };

        let trait_sym = self.traits.get_mut(trait_id).unwrap();

        trait_sym
            .trait_member_ids_by_name
            .insert("drop".to_string(), trait_function_id.into());
    }

    fn create_drop_trait(&mut self, core_module_id: ModuleID) {
        // Drop trait
        let (trait_id, type_parameter_id) = {
            let trait_id = self.traits.insert(Trait {
                name: "@Drop".to_string(),
                parent_module_id: core_module_id,
                generics: Generics::default(),
                implements: Vec::new(),
                trait_member_ids_by_name: HashMap::new(), // to be filled later
            });

            // A type parameter for the type being dropped
            let type_parameter_id = self.type_parameters.insert(TypeParameter {
                name: "T".to_string(),
                parent_genericable_id: trait_id.into(),
            });

            let trait_sym = self.traits.get_mut(trait_id).unwrap();

            trait_sym
                .generics
                .generic_parameters
                .type_parameter_order
                .push(type_parameter_id);

            trait_sym
                .generics
                .generic_parameters
                .type_parameter_ids_by_name
                .insert("T".to_string(), type_parameter_id);

            (trait_id, type_parameter_id)
        };

        // create a function for trait
        let trait_function_id = {
            let trait_function_id = self.trait_functions.insert(TraitFunction {
                function_signature: crate::FunctionSignature {
                    name: "drop".to_string(),
                    parameter_ids_by_name: HashMap::new(), // to be filled later
                    parameter_order: Vec::new(),           // to be filled later
                    return_type: Type::PrimitiveType(PrimitiveType::Void),
                    generics: Generics::default(),
                    syntax_tree: None,
                },
                parent_trait_id: trait_id,
            });

            let parameter_id = self.parameters.insert(crate::Parameter {
                name: "this".to_string(),
                parent_parameter_id: trait_function_id.into(),
                declaration_order: 0,
                ty: Type::ReferenceType(crate::ty::ReferenceType {
                    operand: Box::new(Type::TypeParameter(type_parameter_id)),
                    qualifier: Some(ReferenceQualifier::Restrict),
                    lifetime: None,
                }),
                is_mutable: false,
                syntax_tree: None,
            });

            let trait_function = self.trait_functions.get_mut(trait_function_id).unwrap();

            trait_function
                .parameter_ids_by_name
                .insert("this".to_string(), parameter_id);

            trait_function.parameter_order.push(parameter_id);

            trait_function_id
        };

        let trait_sym = self.traits.get_mut(trait_id).unwrap();

        trait_sym
            .trait_member_ids_by_name
            .insert("drop".to_string(), trait_function_id.into());
    }
}
