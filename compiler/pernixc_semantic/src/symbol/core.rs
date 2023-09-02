use std::collections::HashMap;

use pernixc_system::arena::{self, Arena};

use super::{
    Accessibility, GenericItemRef, GenericParameters, Lifetime, Module, Table, Trait,
    TraitFunction, TypeParameter, TypeParameterRef, WhereClause, ID,
};
use crate::{
    pattern::Irrefutable,
    ty::{self, ReferenceQualifier},
};

impl Table {
    pub(super) fn create_core_module(&mut self) {
        // core module
        let core_module_name = "@core".to_string();
        let core_module = self.modules.push(super::Module {
            accessibility: Accessibility::Public,
            name: core_module_name.to_string(),
            parent_module_id: None,
            children_ids_by_name: HashMap::new(),
            usings: HashMap::new(),
            syntax_tree: None,
        });
        self.target_root_module_ids_by_name
            .insert(core_module_name, core_module);

        self.create_copy_trait(core_module);
        self.create_drop_trait(core_module);
        self.create_into_trait(core_module);
    }

    fn create_core_trait<const N: usize>(
        &mut self,
        core_module: arena::ID<Module>,
        trait_name: String,
        type_parameter_names: [String; N],
        initializer: impl FnOnce(&mut arena::Symbol<Trait>, [arena::ID<TypeParameter>; N]),
    ) {
        let trait_id = self.traits.push(Trait {
            accessibility: Accessibility::Public,
            name: trait_name.clone(),
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            associated_ids_by_name: HashMap::new(),
            constants: Arena::default(),
            types: Arena::default(),
            functions: Arena::default(),
            implements: Arena::default(),
            negative_implements: Arena::default(),
            syntax_tree: None,
            parent_module_id: core_module,
        });
        let triat_symbol = &mut self.traits[trait_id];

        let mut type_parameter_ids: [arena::ID<TypeParameter>; N] = [arena::ID::default(); N];

        for (index, type_parameter_name) in type_parameter_names.into_iter().enumerate() {
            let type_parameter_id = triat_symbol
                .generic_parameters
                .types
                .insert(type_parameter_name.clone(), TypeParameter {
                    name: type_parameter_name,
                    span: None,
                })
                .expect("should have no name duplication in `type_parameter_names`");

            type_parameter_ids[index] = type_parameter_id;
        }

        initializer(triat_symbol, type_parameter_ids);

        // add to the core module
        self.modules[core_module]
            .children_ids_by_name
            .insert(trait_name, ID::Trait(trait_id));
    }

    fn create_trait_function(
        trait_symbol: &mut arena::Symbol<Trait>,
        function_name: String,
        initializer: impl FnOnce(&mut arena::Symbol<TraitFunction>),
    ) {
        let parent_trait_id = trait_symbol.id();
        let trait_function_id = trait_symbol.functions.push(TraitFunction {
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            name: function_name.clone(),
            parameters: Arena::new(),
            return_type: ty::Type::default(),
            parent_trait_id,
            syntax_tree: None,
        });

        initializer(&mut trait_symbol.functions[trait_function_id]);

        trait_symbol.associated_ids_by_name.insert(
            function_name,
            super::TraitAssociatedID::Function(trait_function_id),
        );
    }

    fn create_into_trait(&mut self, core_module: arena::ID<Module>) {
        self.create_core_trait(
            core_module,
            "@Into".to_string(),
            ["T".to_string(), "To".to_string()],
            |into_trait, type_parameters| {
                let into_trait_id = into_trait.id();
                Self::create_trait_function(into_trait, "into".to_string(), |into_function| {
                    // update the return type
                    //
                    // : To
                    into_function.return_type = ty::Type::Parameter(TypeParameterRef {
                        id: type_parameters[1],
                        generic_item_ref: GenericItemRef::Trait(into_trait_id),
                    });

                    // update the parameter
                    //
                    // T
                    into_function.parameters.push(super::Parameter {
                        pattern: Irrefutable::Discard,
                        ty: ty::Type::Parameter(TypeParameterRef {
                            id: type_parameters[0],
                            generic_item_ref: GenericItemRef::Trait(into_trait_id),
                        }),
                    });
                });
            },
        );
    }

    fn create_drop_trait(&mut self, core_module: arena::ID<Module>) {
        self.create_core_trait(
            core_module,
            "@Drop".to_string(),
            ["T".to_string()],
            |drop_trait, type_parameters| {
                let drop_trait_id = drop_trait.id();
                Self::create_trait_function(drop_trait, "drop".to_string(), |drop_function| {
                    // &restrict T
                    drop_function.parameters.push(super::Parameter {
                        pattern: Irrefutable::Discard,
                        ty: ty::Type::Reference(ty::Reference {
                            qualifier: Some(ReferenceQualifier::Restrict),
                            lifetime: Lifetime::ElidedLifetime(super::ElidedLifetime {
                                generic_item_ref: GenericItemRef::Trait(drop_trait_id),
                            }),
                            ty: Box::new(ty::Type::Parameter(TypeParameterRef {
                                id: type_parameters[0],
                                generic_item_ref: GenericItemRef::Trait(drop_trait_id),
                            })),
                        }),
                    });
                });
            },
        );
    }

    fn create_copy_trait(&mut self, core_module: arena::ID<Module>) {
        self.create_core_trait(
            core_module,
            "@Copy".to_string(),
            ["T".to_string()],
            |copy_trait, type_parameters| {
                let copy_trait_id = copy_trait.id();
                Self::create_trait_function(copy_trait, "copy".to_string(), |copy_function| {
                    // &T
                    copy_function.parameters.push(super::Parameter {
                        pattern: Irrefutable::Discard,
                        ty: ty::Type::Reference(ty::Reference {
                            qualifier: None,
                            lifetime: Lifetime::ElidedLifetime(super::ElidedLifetime {
                                generic_item_ref: GenericItemRef::Trait(copy_trait_id),
                            }),
                            ty: Box::new(ty::Type::Parameter(TypeParameterRef {
                                id: type_parameters[0],
                                generic_item_ref: GenericItemRef::Trait(copy_trait_id),
                            })),
                        }),
                    });

                    // update return type
                    //
                    // T
                    copy_function.return_type = ty::Type::Parameter(TypeParameterRef {
                        id: type_parameters[0],
                        generic_item_ref: GenericItemRef::Trait(copy_trait_id),
                    });
                });
            },
        );
    }
}
