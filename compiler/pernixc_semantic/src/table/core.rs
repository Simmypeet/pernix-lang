use std::collections::HashMap;

use super::{
    state::{DraftedSymbolRef, DraftedSymbolSyntax},
    Module, Table,
};
use crate::{
    pattern,
    symbol::{
        Accessibility, GenericItemRef, GenericParameterRef, GenericParameters, Index,
        ModuleMemberRef, Parameter, Trait, TraitAssociatedRef, TraitFunction, TypeParameter,
        WhereClause,
    },
    ty,
};

impl Table {
    pub(super) fn create_core_module(&mut self) {
        // core module
        let core_module_name = "@core".to_string();
        let core_module_index = {
            let index = self.modules.len();
            self.modules.push(Module {
                name: "@core".to_string(),
                index,
                accessibility: Accessibility::Public,
                parent_module_index: None,
                module_member_refs_by_name: HashMap::new(),
                usings: HashMap::new(),
                span: None,
            });
            index
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
        core_module_index: usize,
        trait_name: String,
        type_parameter_names: [String; N],
        initializer: impl FnOnce(&mut Trait, [GenericParameterRef; N]),
    ) -> Index {
        let trait_index = self.traits.len();
        self.traits.push(Trait {
            name: trait_name.clone(),
            index: trait_index,
            accessibility: Accessibility::Public,
            parent_module_index: core_module_index,
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            functions: Vec::default(),
            types: Vec::default(),
            constants: Vec::default(),
            associated_ids_by_name: HashMap::default(),
            span: None,
            implements: Vec::new(),
            negative_implements: Vec::new(),
        });
        let triat_symbol = &mut self.traits[trait_index];

        let mut type_parameter_refs: [GenericParameterRef; N] = [GenericParameterRef {
            generic_item_ref: GenericItemRef::Trait(trait_index),
            index: 0,
        }; N];

        // add type parameters
        for (index, type_parameter_name) in type_parameter_names.into_iter().enumerate() {
            triat_symbol
                .generic_parameters
                .types
                .insert(type_parameter_name.clone(), TypeParameter {
                    name: type_parameter_name,
                    span: None,
                    index,
                    parent_generic_item_ref: GenericItemRef::Trait(trait_index),
                })
                .expect("should have no name duplication in `type_parameter_names`");

            type_parameter_refs[index].index = index;
        }

        initializer(triat_symbol, type_parameter_refs);

        // add to the core module
        self.modules[core_module_index]
            .module_member_refs_by_name
            .insert(trait_name, ModuleMemberRef::Trait(trait_index));

        trait_index
    }

    fn create_trait_function(
        trait_symbol: &mut Trait,
        function_name: String,
        initializer: impl FnOnce(&mut TraitFunction),
    ) {
        let parent_trait_index = trait_symbol.index;
        let trait_function_index = trait_symbol.functions.len();
        trait_symbol.functions.push(TraitFunction {
            generic_parameters: GenericParameters::default(),
            where_clause: WhereClause::default(),
            name: function_name.clone(),
            return_type: ty::Type::Tuple(ty::Tuple {
                elements: Vec::new(),
            }),
            index: trait_function_index,
            parent_trait_index,
            parameters: Vec::new(),
            span: None,
        });

        initializer(&mut trait_symbol.functions[trait_function_index]);

        assert!(trait_symbol
            .associated_ids_by_name
            .insert(
                function_name,
                TraitAssociatedRef::Function(trait_function_index),
            )
            .is_none());
    }

    fn create_assign_restrict_trait(&mut self, core_module_index: usize) {
        self.builtin_assign_restrict_trait_index = self.create_core_trait(
            core_module_index,
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
                            index: 0,
                            parent_function_index: assign_function.index,
                            ty: ty::Type::Reference(ty::Reference {
                                qualifier: ty::Qualifier::Restrict,
                                lifetime: ty::Lifetime::Elided(GenericItemRef::Function(
                                    assign_function.index,
                                )),
                                operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                            }),
                            pattern: pattern::Irrefutable::Discard,
                        });
                    },
                );
            },
        );
    }

    fn create_assign_trait(&mut self, core_module_index: usize) {
        self.builtin_assign_trait_index = self.create_core_trait(
            core_module_index,
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
                            index: 0,
                            parent_function_index: assign_function.index,
                            ty: ty::Type::Reference(ty::Reference {
                                qualifier: ty::Qualifier::Mutable,
                                lifetime: ty::Lifetime::Elided(GenericItemRef::Function(
                                    assign_function.index,
                                )),
                                operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                            }),
                            pattern: pattern::Irrefutable::Discard,
                        });
                    },
                );
            },
        );
    }

    fn create_into_trait(&mut self, core_module_index: usize) {
        self.builtin_into_trait_index = self.create_core_trait(
            core_module_index,
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
                        pattern: pattern::Irrefutable::Discard,
                        index: 0,
                        ty: ty::Type::Parameter(type_parameters[0]),
                        parent_function_index: into_function.index,
                    });
                });
            },
        );
    }

    fn create_drop_trait(&mut self, core_module_index: usize) {
        self.builtin_drop_trait_index = self.create_core_trait(
            core_module_index,
            "@Drop".to_string(),
            ["T".to_string()],
            |drop_trait, type_parameters| {
                Self::create_trait_function(drop_trait, "drop".to_string(), |drop_function| {
                    // &restrict T
                    drop_function.parameters.push(Parameter {
                        pattern: pattern::Irrefutable::Discard,
                        ty: ty::Type::Reference(ty::Reference {
                            qualifier: ty::Qualifier::Restrict,
                            lifetime: ty::Lifetime::Elided(GenericItemRef::Function(
                                drop_function.index,
                            )),
                            operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                        }),
                        index: 0,
                        parent_function_index: drop_function.index,
                    });
                });
            },
        );
    }

    fn create_copy_trait(&mut self, core_module_index: usize) {
        self.builtin_copy_trait_index = self.create_core_trait(
            core_module_index,
            "@Copy".to_string(),
            ["T".to_string()],
            |copy_trait, type_parameters| {
                Self::create_trait_function(copy_trait, "copy".to_string(), |copy_function| {
                    // &T
                    copy_function.parameters.push(Parameter {
                        pattern: pattern::Irrefutable::Discard,
                        ty: ty::Type::Reference(ty::Reference {
                            qualifier: ty::Qualifier::Immutable,
                            lifetime: ty::Lifetime::Elided(GenericItemRef::Function(
                                copy_function.index,
                            )),
                            operand: Box::new(ty::Type::Parameter(type_parameters[0])),
                        }),
                        index: 0,
                        parent_function_index: copy_function.index,
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

/*
public trait @Copy<T> {
    function copy(this: &T): T;
}

public trait @Drop<T> {
    function drop(this: &restrict T);
}

public trait @Into<T, To> {
    function into(this: T): To;
}

public trait @Assign<T> {
    function assign(this: &mutable T, value: T);
}

public trait @AssignRestrict<T> {
    function assignRestrict(this: &restrict T, value: T);
}
*/
