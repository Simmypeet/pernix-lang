use std::collections::HashMap;

use super::{
    state::{DraftedSymbolRef, DraftedSymbolSyntax},
    Module, Table,
};
use crate::{
    pattern,
    symbol::{
        Accessibility, GenericItemRef, GenericParameters, LocalParameterRef,
        LocalTraitAssociatedRef, LocalTraitFunctionRef, LocalTypeParameterRef, ModuleMemberRef,
        ModuleRef, Parameter, Trait, TraitFunction, TraitFunctionRef, TraitRef, TypeParameter,
        TypeParameterRef, WhereClause,
    },
    ty::{self, ElidedLifetime},
};

impl Table {
    pub(super) fn create_core_module(&mut self) {
        // core module
        let core_module_name = "@core".to_string();
        let core_module_index = {
            let module_ref = ModuleRef(self.modules.len());
            self.modules.push(Module {
                name: "@core".to_string(),
                module_ref,
                accessibility: Accessibility::Public,
                parent_module_ref: None,
                module_member_refs_by_name: HashMap::new(),
                usings: HashMap::new(),
                span: None,
            });

            module_ref
        };

        self.target_root_module_indices_by_name
            .insert(core_module_name, core_module_index);

        self.create_copy_trait(core_module_index);
        self.create_drop_trait(core_module_index);
        self.create_into_trait(core_module_index);
        self.create_assign_trait(core_module_index);
        self.create_assign_restrict_trait(core_module_index);

        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Trait(self.builtin_copy_trait_index),
            DraftedSymbolSyntax::Trait {
                trait_syntax: None,
                implements_syntax: Vec::new(),
            },
        );
        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Trait(self.builtin_drop_trait_index),
            DraftedSymbolSyntax::Trait {
                trait_syntax: None,
                implements_syntax: Vec::new(),
            },
        );
        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Trait(self.builtin_into_trait_index),
            DraftedSymbolSyntax::Trait {
                trait_syntax: None,
                implements_syntax: Vec::new(),
            },
        );
        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Trait(self.builtin_assign_trait_index),
            DraftedSymbolSyntax::Trait {
                trait_syntax: None,
                implements_syntax: Vec::new(),
            },
        );
        self.state_mananger.add_drafted_symbol(
            DraftedSymbolRef::Trait(self.builtin_assign_restrict_trait_index),
            DraftedSymbolSyntax::Trait {
                trait_syntax: None,
                implements_syntax: Vec::new(),
            },
        );
    }

    fn create_core_trait<const N: usize>(
        &mut self,
        core_module_ref: ModuleRef,
        trait_name: String,
        type_parameter_names: [String; N],
        initializer: impl FnOnce(&mut Trait, [TypeParameterRef; N]),
    ) -> TraitRef {
        let trait_ref = TraitRef(self.traits.len());
        self.traits.push(Trait {
            name: trait_name.clone(),
            trait_ref,
            accessibility: Accessibility::Public,
            parent_module_ref: core_module_ref,
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            functions: Vec::default(),
            types: Vec::default(),
            constants: Vec::default(),
            associated_refs_by_name: HashMap::default(),
            span: None,
            implements: Vec::new(),
            negative_implements: Vec::new(),
        });
        let triat_symbol = &mut self.traits[trait_ref.0];

        let mut type_parameter_refs: [TypeParameterRef; N] = [TypeParameterRef {
            generic_item_ref: GenericItemRef::Trait(trait_ref),
            local_ref: LocalTypeParameterRef(0),
        }; N];

        // add type parameters
        for (index, type_parameter_name) in type_parameter_names.into_iter().enumerate() {
            triat_symbol
                .generic_parameters
                .types
                .insert(type_parameter_name.clone(), TypeParameter {
                    name: type_parameter_name,
                    span: None,
                    local_type_parameter_ref: LocalTypeParameterRef(index),
                    parent_generic_item_ref: GenericItemRef::Trait(trait_ref),
                })
                .expect("should have no name duplication in `type_parameter_names`");

            type_parameter_refs[index].local_ref = LocalTypeParameterRef(index);
        }

        initializer(triat_symbol, type_parameter_refs);

        // add to the core module
        self.modules[core_module_ref.0]
            .module_member_refs_by_name
            .insert(trait_name, ModuleMemberRef::Trait(trait_ref));

        trait_ref
    }

    fn create_trait_function(
        trait_symbol: &mut Trait,
        function_name: String,
        initializer: impl FnOnce(&mut TraitFunction),
    ) {
        let parent_trait_ref = trait_symbol.trait_ref;
        let trait_function_ref = LocalTraitFunctionRef(trait_symbol.functions.len());
        trait_symbol.functions.push(TraitFunction {
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            name: function_name.clone(),
            return_type: ty::Type::Tuple(ty::Tuple {
                elements: Vec::new(),
            }),
            parameters: Vec::new(),
            span: None,
            trait_function_ref,
            parent_trait_ref,
        });

        initializer(&mut trait_symbol.functions[trait_function_ref.0]);

        assert!(trait_symbol
            .associated_refs_by_name
            .insert(
                function_name,
                LocalTraitAssociatedRef::Function(trait_function_ref),
            )
            .is_none());
    }

    fn create_assign_restrict_trait(&mut self, core_module_ref: ModuleRef) {
        self.builtin_assign_restrict_trait_index = self.create_core_trait(
            core_module_ref,
            "@AssignRestrict".to_string(),
            ["T".to_string()],
            |assign_trait, type_parameters| {
                Self::create_trait_function(
                    assign_trait,
                    "assignRestrict".to_string(),
                    |assign_function| {
                        // update the return type NONE
                        assign_function.return_type = ty::Type::unit();

                        assign_function.parameters.push(Parameter {
                            ty: ty::Type::Reference(ty::Reference {
                                qualifier: ty::Qualifier::Restrict,
                                lifetime: ty::Lifetime::Elided(ElidedLifetime {
                                    generic_item_ref: GenericItemRef::TraitFunction(
                                        TraitFunctionRef {
                                            trait_ref: assign_function.parent_trait_ref,
                                            local_ref: assign_function.trait_function_ref,
                                        },
                                    ),
                                }),
                                operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                            }),
                            pattern: pattern::Irrefutable::Discard,
                            parameter_ref: LocalParameterRef(0),
                            parent_ref: TraitFunctionRef {
                                trait_ref: assign_function.parent_trait_ref,
                                local_ref: assign_function.trait_function_ref,
                            },
                        });
                    },
                );
            },
        );
    }

    fn create_assign_trait(&mut self, core_module_ref: ModuleRef) {
        self.builtin_assign_trait_index = self.create_core_trait(
            core_module_ref,
            "@Assign".to_string(),
            ["T".to_string()],
            |assign_trait, type_parameters| {
                Self::create_trait_function(
                    assign_trait,
                    "assign".to_string(),
                    |assign_function| {
                        // update the return type NONE
                        assign_function.return_type = ty::Type::unit();

                        assign_function.parameters.push(Parameter {
                            ty: ty::Type::Reference(ty::Reference {
                                qualifier: ty::Qualifier::Mutable,
                                lifetime: ty::Lifetime::Elided(ElidedLifetime {
                                    generic_item_ref: GenericItemRef::TraitFunction(
                                        TraitFunctionRef {
                                            trait_ref: assign_function.parent_trait_ref,
                                            local_ref: assign_function.trait_function_ref,
                                        },
                                    ),
                                }),
                                operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                            }),
                            pattern: pattern::Irrefutable::Discard,
                            parameter_ref: LocalParameterRef(0),
                            parent_ref: TraitFunctionRef {
                                trait_ref: assign_function.parent_trait_ref,
                                local_ref: assign_function.trait_function_ref,
                            },
                        });
                    },
                );
            },
        );
    }

    fn create_into_trait(&mut self, core_module_ref: ModuleRef) {
        self.builtin_into_trait_index = self.create_core_trait(
            core_module_ref,
            "@Into".to_string(),
            ["T".to_string(), "To".to_string()],
            |into_trait, type_parameters| {
                Self::create_trait_function(into_trait, "into".to_string(), |into_function| {
                    // update the return type
                    //
                    // : To
                    into_function.return_type = ty::Type::Parameter(type_parameters[1]);

                    // update the parameter
                    //
                    // T
                    into_function.parameters.push(Parameter {
                        parameter_ref: LocalParameterRef(0),
                        pattern: pattern::Irrefutable::Discard,
                        ty: ty::Type::Parameter(type_parameters[0]),
                        parent_ref: TraitFunctionRef {
                            trait_ref: into_function.parent_trait_ref,
                            local_ref: into_function.trait_function_ref,
                        },
                    });
                });
            },
        );
    }

    fn create_drop_trait(&mut self, core_module_ref: ModuleRef) {
        self.builtin_drop_trait_index = self.create_core_trait(
            core_module_ref,
            "@Drop".to_string(),
            ["T".to_string()],
            |drop_trait, type_parameters| {
                Self::create_trait_function(drop_trait, "drop".to_string(), |drop_function| {
                    // &restrict T
                    drop_function.parameters.push(Parameter {
                        parameter_ref: LocalParameterRef(0),
                        pattern: pattern::Irrefutable::Discard,
                        ty: ty::Type::Reference(ty::Reference {
                            qualifier: ty::Qualifier::Restrict,
                            lifetime: ty::Lifetime::Elided(ElidedLifetime {
                                generic_item_ref: GenericItemRef::TraitFunction(TraitFunctionRef {
                                    trait_ref: drop_function.parent_trait_ref,
                                    local_ref: drop_function.trait_function_ref,
                                }),
                            }),
                            operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                        }),
                        parent_ref: TraitFunctionRef {
                            trait_ref: drop_function.parent_trait_ref,
                            local_ref: drop_function.trait_function_ref,
                        },
                    });
                });
            },
        );
    }

    fn create_copy_trait(&mut self, core_module_ref: ModuleRef) {
        self.builtin_copy_trait_index = self.create_core_trait(
            core_module_ref,
            "@Copy".to_string(),
            ["T".to_string()],
            |copy_trait, type_parameters| {
                Self::create_trait_function(copy_trait, "copy".to_string(), |copy_function| {
                    // &T
                    copy_function.parameters.push(Parameter {
                        parameter_ref: LocalParameterRef(0),
                        pattern: pattern::Irrefutable::Discard,
                        ty: ty::Type::Reference(ty::Reference {
                            qualifier: ty::Qualifier::Immutable,
                            lifetime: ty::Lifetime::Elided(ElidedLifetime {
                                generic_item_ref: GenericItemRef::TraitFunction(TraitFunctionRef {
                                    trait_ref: copy_function.parent_trait_ref,
                                    local_ref: copy_function.trait_function_ref,
                                }),
                            }),
                            operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                        }),
                        parent_ref: TraitFunctionRef {
                            trait_ref: copy_function.parent_trait_ref,
                            local_ref: copy_function.trait_function_ref,
                        },
                    });

                    // update return type
                    //
                    // T
                    copy_function.return_type = ty::Type::Parameter(type_parameters[0]);
                });
            },
        );
    }
}
