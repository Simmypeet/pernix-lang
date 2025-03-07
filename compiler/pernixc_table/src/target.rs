use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::{Hash, Hasher},
    time::SystemTime,
};

use pernixc_handler::Handler;
use pernixc_lexical::token::Identifier;
use pernixc_source_file::SourceElement;
use pernixc_syntax::{
    state_machine::parse::Passable,
    syntax_tree::{
        self, item::function::ParameterKind, target::ModuleTree, ConnectedList,
        QualifiedIdentifierRoot,
    },
};

use super::{
    diagnostic::{
        FoundImplementationMemberOnMarker, InvalidSymbolInImplementation,
        MismatchedTraitMemberAndImplementationMember,
        SymbolIsMoreAccessibleThanParent, TraitMemberKind,
        UnexpectedAdtImplementationMember, UnimplementedTraitMembers,
        UnknownTraitImplementationMember,
    },
    resolution::diagnostic::{NoGenericArgumentsRequired, ThisNotFound},
    GlobalID, Representation, TargetID, ID,
};
use crate::{
    component::{
        self, syntax_tree as syntax_tree_component, Accessibility, Extern,
        ExternC, Implemented, Implements, Import, LocationSpan, Member, Name,
        Parent, PositiveTraitImplementation, SymbolKind, TraitImplementation,
        Using, VariantDeclarationOrder,
    },
    diagnostic::{
        AccessModifierIsNotAllowedInTraitImplementation, ConflictingUsing,
        Diagnostic, ExpectAccessModifier, ExpectModule,
        ExpectedImplementationWithBodyForAdt, InvalidConstImplementation,
        InvalidFinalImplementation, ItemRedifinition,
        NonFinalMarkerImplementation, TargetRootInImportIsNotAllowedwithFrom,
        UnknownExternCallingConvention, VariadicArgumentsAreNotAllowed,
        VariadicArgumentsMustBeTrailing,
    },
    resolution::diagnostic::{SymbolIsNotAccessible, SymbolNotFound},
    Target,
};

/// Errors that can occur when adding a target to the representation.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
pub enum AddTargetError {
    /// The target name is already in use.
    DuplicateTargetName(String),

    /// The `linked_targets` ids contain an unknown target.
    UnknownTargetLink(TargetID),
}

impl Representation {
    /// Adds a target syntax tree to the compilation target.
    ///
    /// The function will analyse the syntax tree and create the symbols and
    /// their input components in the table.
    ///
    /// # Errors
    ///
    /// See [`AddTargetError`] for possible errors.
    pub fn add_compilation_target(
        &mut self,
        name: String,
        linked_targets: impl IntoIterator<Item = TargetID>,
        target_syntax: syntax_tree::target::Target,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<TargetID, AddTargetError> {
        // make sure every target links to the core target
        let mut linked_targets_vec =
            std::iter::once(TargetID::CORE).collect::<HashSet<_>>();

        for linked_target in linked_targets {
            if !self.targets_by_id.contains_key(&linked_target)
                && linked_target != TargetID::CORE
            {
                return Err(AddTargetError::UnknownTargetLink(linked_target));
            }

            linked_targets_vec.insert(linked_target);
        }

        // check if the target name is unique
        if name == "core" || self.targets_by_name.contains_key(&name) {
            return Err(AddTargetError::DuplicateTargetName(name));
        }

        let mut instant = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_nanos();

        let mut target_id = {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            name.hash(&mut hasher);
            instant.hash(&mut hasher);
            TargetID(hasher.finish())
        };

        // keep generating target IDs until we find a unique one
        while self.targets_by_id.contains_key(&target_id)
            || target_id == TargetID::CORE
        {
            instant += 1;
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            name.hash(&mut hasher);
            instant.hash(&mut hasher);
            target_id = TargetID(hasher.finish());
        }

        let target =
            Target { generated_ids: 1, linked_targets: linked_targets_vec };

        // add target information
        assert!(self.targets_by_id.insert(target_id, target).is_none());
        assert!(self.targets_by_name.insert(name.clone(), target_id).is_none());

        let mut usings_by_module_id = HashMap::new();
        let mut implementations_by_module_id = HashMap::new();

        // create the root module
        self.create_module(
            target_id,
            name,
            target_syntax.module_tree,
            None,
            &mut usings_by_module_id,
            &mut implementations_by_module_id,
            handler,
        );

        for (defined_in_module_id, using) in usings_by_module_id
            .into_iter()
            .flat_map(|x| x.1.into_iter().map(move |y| (x.0, y)))
        {
            self.insert_imports(defined_in_module_id, &using, handler);
        }

        for (defined_in_module_id, implementation) in
            implementations_by_module_id
                .into_iter()
                .flat_map(|x| x.1.into_iter().map(move |y| (x.0, y)))
        {
            self.insert_implements(
                defined_in_module_id,
                implementation,
                handler,
            );
        }

        Ok(target_id)
    }

    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn insert_implementation(
        &mut self,
        implemented_id: GlobalID,
        defined_in_module_id: GlobalID,
        symbol_kind: SymbolKind,
        implementation_signature: syntax_tree::item::implements::Signature,
        where_clause_syn: Option<syntax_tree::item::where_clause::WhereClause>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        let new_symbol_id = GlobalID::new(
            defined_in_module_id.target_id,
            self.targets_by_id
                .get_mut(&defined_in_module_id.target_id)
                .unwrap()
                .generate_id(),
        );

        assert!(self.storage.add_component(new_symbol_id, LocationSpan {
            span: Some(implementation_signature.qualified_identifier.span())
        }));

        let name = self.storage.get::<Name>(implemented_id).unwrap().0.clone();

        assert!(self.storage.add_component(new_symbol_id, Name(name)));
        assert!(self.storage.add_component(new_symbol_id, Parent {
            parent: Some(defined_in_module_id.id)
        },));
        assert!(self.storage.add_component(new_symbol_id, symbol_kind));

        if symbol_kind.has_member() {
            assert!(self
                .storage
                .add_component(new_symbol_id, Member::default()));
        }
        assert!(self.add_component(new_symbol_id, Implements(implemented_id)));
        assert!(self
            .storage
            .get_mut::<Implemented>(implemented_id)
            .unwrap()
            .0
            .insert(new_symbol_id));

        assert!(self.storage.add_component(
            new_symbol_id,
            syntax_tree_component::GenericParameters(
                implementation_signature.generic_parameters
            )
        ));
        assert!(self.storage.add_component(
            new_symbol_id,
            syntax_tree_component::WhereClause(where_clause_syn)
        ));

        match symbol_kind {
            SymbolKind::PositiveTraitImplementation => {
                assert!(self.storage.add_component(
                    new_symbol_id,
                    PositiveTraitImplementation {
                        is_const: implementation_signature
                            .const_keyword
                            .is_some(),
                    },
                ));
                assert!(self.storage.add_component(
                    new_symbol_id,
                    TraitImplementation {
                        is_final: implementation_signature
                            .final_keyword
                            .is_some()
                    }
                ));
            }

            SymbolKind::NegativeTraitImplementation => {
                assert!(self.storage.add_component(
                    new_symbol_id,
                    TraitImplementation {
                        is_final: implementation_signature
                            .final_keyword
                            .is_some(),
                    }
                ));

                if let Some(const_keyword) =
                    &implementation_signature.const_keyword
                {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span.clone(),
                    }));
                }
            }

            SymbolKind::AdtImplementation => {
                if let Some(final_keyword) =
                    &implementation_signature.final_keyword
                {
                    handler.receive(Box::new(InvalidFinalImplementation {
                        span: final_keyword.span.clone(),
                    }));
                }

                if let Some(const_keyword) =
                    &implementation_signature.const_keyword
                {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span.clone(),
                    }));
                }
            }

            SymbolKind::NegativeMarkerImplementation
            | SymbolKind::PositiveMarkerImplementation => {
                if implementation_signature.final_keyword.is_none() {
                    handler.receive(Box::new(NonFinalMarkerImplementation {
                        span: implementation_signature
                            .qualified_identifier
                            .span(),
                    }));
                }

                if let Some(const_keyword) =
                    &implementation_signature.const_keyword
                {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span.clone(),
                    }));
                }
            }

            _ => panic!("invalid {symbol_kind:?}"),
        }

        // insert qualified identifier
        assert!(self.storage.add_component(
            new_symbol_id,
            syntax_tree_component::ImplementationQualifiedIdentifier(
                implementation_signature.qualified_identifier
            )
        ));

        new_symbol_id
    }

    #[allow(clippy::too_many_lines)]
    fn create_adt_implementation(
        &mut self,
        implementation: syntax_tree::item::implements::Implements,
        declared_in: GlobalID,
        adt_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let implementation_body = match implementation.body {
            syntax_tree::item::implements::Body::Negative(_) => {
                handler.receive(Box::new(
                    ExpectedImplementationWithBodyForAdt {
                        invalid_implementation_span: implementation
                            .signature
                            .span(),
                    },
                ));

                return;
            }
            syntax_tree::item::implements::Body::Positive(body) => body,
        };

        // the implementation id
        let adt_implementation_id = self.insert_implementation(
            adt_id,
            declared_in,
            SymbolKind::AdtImplementation,
            implementation.signature,
            implementation_body.where_clause,
            handler,
        );

        for member in implementation_body
            .members
            .into_iter()
            .filter_map(Passable::into_option)
        {
            match member {
                syntax_tree::item::implements::Member::Constant(syn) => {
                    handler.receive(Box::new(
                        UnexpectedAdtImplementationMember {
                            unexpected_member_span: syn.signature.span(),
                        },
                    ));
                }
                syntax_tree::item::implements::Member::Type(syn) => {
                    handler.receive(Box::new(
                        UnexpectedAdtImplementationMember {
                            unexpected_member_span: syn.signature.span(),
                        },
                    ));
                }
                syntax_tree::item::implements::Member::Function(syn) => {
                    let function_id = if let Some(existing_id) = self
                        .get_member_of(
                            adt_id,
                            syn.signature.signature.identifier.span.str(),
                        ) {
                        let in_implementation = self
                            .get::<Member>(adt_implementation_id)
                            .contains_key(
                                syn.signature.signature.identifier.span.str(),
                            );

                        let new_id = self.insert_member(
                            adt_implementation_id,
                            &syn.signature.signature.identifier,
                            SymbolKind::AdtImplementationFunction,
                            Some(syn.access_modifier.as_ref().map_or_else(
                                || {
                                    handler.receive(Box::new(
                                        ExpectAccessModifier {
                                            span: syn.signature.span(),
                                        },
                                    ));

                                    Accessibility::Public
                                },
                                |acc| {
                                    self.create_accessibility(
                                        adt_implementation_id,
                                        acc,
                                    )
                                    .unwrap()
                                },
                            )),
                            syn.signature.signature.generic_parameters,
                            syn.body.where_clause,
                            handler,
                        );

                        if !in_implementation {
                            handler.receive(Box::new(ItemRedifinition {
                                existing_id,
                                new_id,
                                in_id: adt_id,
                            }));
                        }

                        new_id
                    } else {
                        self.insert_member(
                            adt_implementation_id,
                            &syn.signature.signature.identifier,
                            SymbolKind::AdtImplementationFunction,
                            Some(syn.access_modifier.as_ref().map_or_else(
                                || {
                                    handler.receive(Box::new(
                                        ExpectAccessModifier {
                                            span: syn.signature.span(),
                                        },
                                    ));

                                    Accessibility::Public
                                },
                                |acc| {
                                    self.create_accessibility(
                                        adt_implementation_id,
                                        acc,
                                    )
                                    .unwrap()
                                },
                            )),
                            syn.signature.signature.generic_parameters,
                            syn.body.where_clause,
                            handler,
                        )
                    };

                    // add the parameters
                    Self::validate_parameters_syntax(
                        &syn.signature.signature.parameters,
                        false,
                        handler,
                    );
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters: syn.signature.signature.parameters,
                            return_type: syn.signature.signature.return_type
                        }
                    ));

                    // add the body
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionBody {
                            statements: syn.body.members
                        }
                    ));
                }
            };
        }
    }

    fn create_marker_implementation(
        &mut self,
        implementation: syntax_tree::item::implements::Implements,
        declared_in: GlobalID,
        marker_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let (symbol_kind, where_clause) = match implementation.body {
            syntax_tree::item::implements::Body::Negative(negative) => (
                SymbolKind::NegativeMarkerImplementation,
                negative.trailing_where_clause.map(|x| x.where_clause),
            ),

            syntax_tree::item::implements::Body::Positive(body) => {
                for x in body.members.iter().filter_map(|x| x.as_option()) {
                    handler
                        .receive(Box::new(FoundImplementationMemberOnMarker {
                        implementation_member_span: match x {
                            syntax_tree::item::implements::Member::Constant(
                                member_template,
                            ) => member_template.signature.identifier.span(),
                            syntax_tree::item::implements::Member::Function(
                                member_template,
                            ) => member_template
                                .signature
                                .signature
                                .identifier
                                .span(),
                            syntax_tree::item::implements::Member::Type(
                                member_template,
                            ) => member_template.signature.identifier.span(),
                        },
                    }));
                }

                (SymbolKind::PositiveMarkerImplementation, body.where_clause)
            }
        };

        self.insert_implementation(
            marker_id,
            declared_in,
            symbol_kind,
            implementation.signature,
            where_clause,
            handler,
        );
    }

    fn create_trait_implementation(
        &mut self,
        implementation: syntax_tree::item::implements::Implements,
        declared_in: GlobalID,
        trait_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let (symbol_kind, body, where_clause) = match implementation.body {
            syntax_tree::item::implements::Body::Negative(body) => (
                SymbolKind::NegativeTraitImplementation,
                None,
                body.trailing_where_clause.map(|x| x.where_clause),
            ),
            syntax_tree::item::implements::Body::Positive(body) => (
                SymbolKind::PositiveTraitImplementation,
                Some(body.members),
                body.where_clause,
            ),
        };

        let implementation_id = self.insert_implementation(
            trait_id,
            declared_in,
            symbol_kind,
            implementation.signature,
            where_clause,
            handler,
        );

        if let Some(body) = body {
            self.insert_positive_trait_implementation_members(
                implementation_id,
                trait_id,
                body,
                handler,
            );
        }
    }

    #[allow(clippy::too_many_lines)]
    fn insert_positive_trait_implementation_members(
        &mut self,
        implementation_id: GlobalID,
        trait_id: GlobalID,
        members: Vec<Passable<syntax_tree::item::implements::Member>>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        for member in members.into_iter().filter_map(Passable::into_option) {
            let access_modifier = match &member {
                syntax_tree::item::implements::Member::Constant(
                    member_template,
                ) => member_template.access_modifier.as_ref(),
                syntax_tree::item::implements::Member::Function(
                    member_template,
                ) => member_template.access_modifier.as_ref(),
                syntax_tree::item::implements::Member::Type(
                    member_template,
                ) => member_template.access_modifier.as_ref(),
            };

            if let Some(access_modifier) = access_modifier {
                handler.receive(Box::new(
                    AccessModifierIsNotAllowedInTraitImplementation {
                        access_modifier_span: access_modifier.span(),
                    },
                ));
            }

            let trait_implementation_member_id = match member {
                syntax_tree::item::implements::Member::Type(syn) => {
                    let ty_id = self.insert_member(
                        implementation_id,
                        &syn.signature.identifier,
                        SymbolKind::TraitImplementationType,
                        None,
                        syn.signature.generic_parameters,
                        syn.body.trailing_where_clause.map(|x| x.where_clause),
                        handler,
                    );

                    assert!(self.storage.add_component(
                        ty_id,
                        syntax_tree_component::TypeAlias(syn.body.r#type)
                    ));

                    ty_id
                }

                syntax_tree::item::implements::Member::Function(syn) => {
                    let function_id = self.insert_member(
                        implementation_id,
                        &syn.signature.signature.identifier,
                        SymbolKind::TraitImplementationFunction,
                        None,
                        syn.signature.signature.generic_parameters,
                        syn.body.where_clause,
                        handler,
                    );

                    // add the parameters
                    Self::validate_parameters_syntax(
                        &syn.signature.signature.parameters,
                        false,
                        handler,
                    );
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters: syn.signature.signature.parameters,
                            return_type: syn.signature.signature.return_type
                        }
                    ));

                    // add the body
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionBody {
                            statements: syn.body.members
                        }
                    ));

                    function_id
                }

                syntax_tree::item::implements::Member::Constant(syn) => {
                    let constant_id = self.insert_member(
                        implementation_id,
                        &syn.signature.identifier,
                        SymbolKind::TraitImplementationConstant,
                        None,
                        syn.signature.generic_parameters,
                        syn.body.trailing_where_clause.map(|x| x.where_clause),
                        handler,
                    );

                    assert!(self
                        .storage
                        .add_component(constant_id, syn.signature.r#type));
                    assert!(self
                        .storage
                        .add_component(constant_id, syn.body.expression));

                    constant_id
                }
            };

            // find the cooresponding trait member id
            let Some(trait_member_id) = self
                .storage
                .get::<Member>(trait_id)
                .unwrap()
                .get(
                    self.storage
                        .get::<Name>(trait_implementation_member_id)
                        .unwrap()
                        .as_str(),
                )
                .copied()
            else {
                handler.receive(Box::new(UnknownTraitImplementationMember {
                    identifier_span: self
                        .get::<LocationSpan>(trait_implementation_member_id)
                        .span
                        .clone()
                        .unwrap(),
                    trait_id,
                }));
                continue;
            };

            let trait_member_id =
                GlobalID::new(trait_id.target_id, trait_member_id);

            let trait_member_kind = *self.get::<SymbolKind>(trait_member_id);
            let trait_implementation_member_kind =
                *self.get::<SymbolKind>(trait_implementation_member_id);

            match (trait_member_kind, trait_implementation_member_kind) {
                (
                    SymbolKind::TraitType,
                    SymbolKind::TraitImplementationType,
                )
                | (
                    SymbolKind::TraitFunction,
                    SymbolKind::TraitImplementationFunction,
                )
                | (
                    SymbolKind::TraitConstant,
                    SymbolKind::TraitImplementationConstant,
                ) => { /* do nothing, correct implementation */ }

                (_, kind) => handler.receive(Box::new(
                    MismatchedTraitMemberAndImplementationMember {
                        trait_member_id,
                        found_kind: match kind {
                            SymbolKind::TraitImplementationType => {
                                TraitMemberKind::Type
                            }
                            SymbolKind::TraitImplementationFunction => {
                                TraitMemberKind::Function
                            }
                            SymbolKind::TraitImplementationConstant => {
                                TraitMemberKind::Constant
                            }
                            _ => unreachable!(),
                        },

                        implementation_member_identifer_span: self
                            .get::<LocationSpan>(trait_implementation_member_id)
                            .span
                            .clone()
                            .unwrap(),
                    },
                )),
            }
        }

        let trait_members = self.storage.get::<Member>(trait_id).unwrap();
        let trait_implementation_members =
            self.storage.get::<Member>(implementation_id).unwrap();

        // check if there are any missing members
        let unimplemented_trait_member_ids = trait_members
            .iter()
            .filter_map(|(name, id)| {
                (!trait_implementation_members.contains_key(name))
                    .then_some(GlobalID::new(trait_id.target_id, *id))
            })
            .collect::<HashSet<_>>();

        drop(trait_members);
        drop(trait_implementation_members);

        if !unimplemented_trait_member_ids.is_empty() {
            handler.receive(Box::new(UnimplementedTraitMembers {
                unimplemented_trait_member_ids,
                implementation_id,
            }));
        }
    }

    #[allow(clippy::too_many_lines)]
    fn insert_implements(
        &mut self,
        defined_in_module_id: GlobalID,
        imple: pernixc_syntax::syntax_tree::item::implements::Implements,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let mut current_id: GlobalID = match &imple
            .signature
            .qualified_identifier
            .root
        {
            QualifiedIdentifierRoot::Target(_) => {
                GlobalID::new(defined_in_module_id.target_id, ID::ROOT_MODULE)
            }
            QualifiedIdentifierRoot::This(keyword) => {
                handler
                    .receive(Box::new(ThisNotFound { span: keyword.span() }));
                return;
            }
            QualifiedIdentifierRoot::GenericIdentifier(generic_identifier) => {
                let Some(id) = self
                    .get_member_of(
                        defined_in_module_id,
                        generic_identifier.identifier.span.str(),
                    )
                    .or_else(|| {
                        self.storage
                            .get::<Import>(defined_in_module_id)
                            .unwrap()
                            .get(generic_identifier.identifier.span.str())
                            .map(|x| x.id)
                    })
                else {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_item_id: Some(defined_in_module_id),
                        resolution_span: generic_identifier
                            .identifier
                            .span
                            .clone(),
                    }));
                    return;
                };

                id
            }
        };

        if !imple.signature.qualified_identifier.rest.is_empty() {
            if *self.storage.get::<SymbolKind>(current_id).unwrap()
                != SymbolKind::Module
            {
                handler.receive(Box::new(ExpectModule {
                    module_path: imple
                        .signature
                        .qualified_identifier
                        .root
                        .span(),
                    found_id: current_id,
                }));
                return;
            }

            if let Some(gen_args) = imple
                .signature
                .qualified_identifier
                .root
                .as_generic_identifier()
                .and_then(|x| x.generic_arguments.as_ref())
            {
                handler.receive(Box::new(NoGenericArgumentsRequired {
                    global_id: current_id,
                    generic_argument_span: gen_args.span(),
                }));
            }
        }

        for (index, (_, generic_identifier)) in
            imple.signature.qualified_identifier.rest.iter().enumerate()
        {
            let Some(next_id) = self.get_member_of(
                current_id,
                generic_identifier.identifier.span.str(),
            ) else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(current_id),
                    resolution_span: generic_identifier.identifier.span.clone(),
                }));
                return;
            };

            // non-fatal error, no need to return early
            if !self.symbol_accessible(defined_in_module_id, next_id) {
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site: defined_in_module_id,
                    referred: next_id,
                    referred_span: generic_identifier.identifier.span.clone(),
                }));
            }

            if index == imple.signature.qualified_identifier.rest.len() - 1 {
                current_id = next_id;
            } else {
                if *self.storage.get::<SymbolKind>(next_id).unwrap()
                    != SymbolKind::Module
                {
                    handler.receive(Box::new(ExpectModule {
                        module_path: generic_identifier.identifier.span.clone(),
                        found_id: next_id,
                    }));
                    return;
                }

                if let Some(gen_args) =
                    generic_identifier.generic_arguments.as_ref()
                {
                    handler.receive(Box::new(NoGenericArgumentsRequired {
                        global_id: next_id,
                        generic_argument_span: gen_args.span(),
                    }));
                }
            }
        }

        let current_kind = *self.storage.get::<SymbolKind>(current_id).unwrap();

        match current_kind {
            SymbolKind::Trait => {
                self.create_trait_implementation(
                    imple,
                    defined_in_module_id,
                    current_id,
                    handler,
                );
            }

            SymbolKind::Marker => {
                self.create_marker_implementation(
                    imple,
                    defined_in_module_id,
                    current_id,
                    handler,
                );
            }

            SymbolKind::Enum | SymbolKind::Struct => {
                self.create_adt_implementation(
                    imple,
                    defined_in_module_id,
                    current_id,
                    handler,
                );
            }

            _ => {
                handler.receive(Box::new(InvalidSymbolInImplementation {
                    invalid_item_id: current_id,
                    qualified_identifier_span: imple
                        .signature
                        .qualified_identifier
                        .span(),
                }));
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn insert_imports(
        &self,
        defined_in_module_id: GlobalID,
        import: &pernixc_syntax::syntax_tree::item::module::Import,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        assert_eq!(
            *self.storage.get::<SymbolKind>(defined_in_module_id).unwrap(),
            SymbolKind::Module
        );

        let start_from = if let Some(from) = &import.from {
            let Some(from_id) = self.resolve_simple_path(
                &from.simple_path,
                defined_in_module_id,
                true,
                handler,
            ) else {
                return;
            };

            // must be module
            if *self.storage.get::<SymbolKind>(from_id).unwrap()
                != SymbolKind::Module
            {
                handler.receive(Box::new(ExpectModule {
                    module_path: from.simple_path.span(),
                    found_id: from_id,
                }));

                return;
            }

            Some(from_id)
        } else {
            None
        };

        'item: for import_item in
            import.items.iter().flat_map(ConnectedList::elements)
        {
            // if start from , then all the import items can't have `target` as
            // it root path
            if import_item.simple_path.root.is_target() && import.from.is_some()
            {
                handler.receive(Box::new(
                    TargetRootInImportIsNotAllowedwithFrom {
                        target_root_span: import_item.simple_path.root.span(),
                    },
                ));
                continue;
            }

            let mut start = match start_from {
                Some(current) => {
                    let Some(id) = self
                        .storage
                        .get::<Member>(current)
                        .unwrap()
                        .get(
                            import_item
                                .simple_path
                                .root
                                .as_identifier()
                                .unwrap()
                                .span
                                .str(),
                        )
                        .copied()
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: Some(current),
                            resolution_span: import_item
                                .simple_path
                                .root
                                .span(),
                        }));
                        continue;
                    };

                    let result = GlobalID::new(current.target_id, id);
                    if !self.symbol_accessible(defined_in_module_id, result) {
                        handler.receive(Box::new(SymbolIsNotAccessible {
                            referring_site: defined_in_module_id,
                            referred: GlobalID::new(current.target_id, id),
                            referred_span: import_item.simple_path.root.span(),
                        }));
                    }

                    result
                }
                None => match &import_item.simple_path.root {
                    syntax_tree::SimplePathRoot::Target(_) => GlobalID::new(
                        defined_in_module_id.target_id,
                        ID::ROOT_MODULE,
                    ),
                    syntax_tree::SimplePathRoot::Identifier(identifier) => {
                        let Some(id) = self
                            .targets_by_name
                            .get(identifier.span.str())
                            .copied()
                            .filter(|x| {
                                self.targets_by_id
                                    .get(&defined_in_module_id.target_id)
                                    .is_some_and(|y| {
                                        y.linked_targets.contains(x)
                                    })
                                    || x == &defined_in_module_id.target_id
                            })
                        else {
                            handler.receive(Box::new(SymbolNotFound {
                                searched_item_id: None,
                                resolution_span: identifier.span.clone(),
                            }));
                            continue;
                        };

                        GlobalID::new(id, ID::ROOT_MODULE)
                    }
                },
            };

            for rest in &import_item.simple_path.rest {
                let Some(next) = self
                    .storage
                    .get::<Member>(start)
                    .unwrap()
                    .get(rest.1.span.str())
                    .copied()
                else {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_item_id: Some(start),
                        resolution_span: rest.1.span.clone(),
                    }));
                    continue 'item;
                };

                let next_id = GlobalID::new(start.target_id, next);

                if !self.symbol_accessible(defined_in_module_id, next_id) {
                    handler.receive(Box::new(SymbolIsNotAccessible {
                        referring_site: defined_in_module_id,
                        referred: next_id,
                        referred_span: rest.1.span.clone(),
                    }));
                }

                start = next_id;
            }

            let name = import_item.alias.as_ref().map_or_else(
                || self.storage.get::<Name>(start).unwrap().0.clone(),
                |x| x.identifier.span.str().to_owned(),
            );

            // check if there's existing symbol right now
            let existing = self
                .storage
                .get::<Member>(defined_in_module_id)
                .unwrap()
                .get(&name)
                .map(|x| {
                    self.get::<LocationSpan>(GlobalID::new(
                        defined_in_module_id.target_id,
                        *x,
                    ))
                    .span
                    .clone()
                })
                .or_else(|| {
                    self.storage
                        .get::<Import>(defined_in_module_id)
                        .unwrap()
                        .get(&name)
                        .map(|x| Some(x.span.clone()))
                });

            if let Some(existing) = existing {
                handler.receive(Box::new(ConflictingUsing {
                    using_span: import_item.alias.as_ref().map_or_else(
                        || import_item.simple_path.span(),
                        SourceElement::span,
                    ),
                    name: name.clone(),
                    module_id: defined_in_module_id,
                    conflicting_span: existing,
                }));
            } else {
                self.storage
                    .get_mut::<Import>(defined_in_module_id)
                    .unwrap()
                    .insert(name, Using {
                        id: start,
                        span: import_item.alias.as_ref().map_or_else(
                            || import_item.simple_path.span(),
                            SourceElement::span,
                        ),
                    });
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn insert_member(
        &mut self,
        parent_id: GlobalID,
        name: &Identifier,
        symbol_kind: SymbolKind,
        accessibility: Option<Accessibility>,
        generic_parameters_syn: Option<
            syntax_tree::item::generic_parameter::GenericParameters,
        >,
        where_clause_syn: Option<syntax_tree::item::where_clause::WhereClause>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        let new_symbol_id = GlobalID::new(
            parent_id.target_id,
            self.targets_by_id
                .get_mut(&parent_id.target_id)
                .unwrap()
                .generate_id(),
        );

        assert!(self.storage.add_component(new_symbol_id, LocationSpan {
            span: Some(name.span.clone())
        }));
        assert!(self
            .storage
            .add_component(new_symbol_id, Name(name.span.str().to_owned())));
        assert!(self.storage.add_component(new_symbol_id, Parent {
            parent: Some(parent_id.id)
        }));
        assert!(self.storage.add_component(new_symbol_id, symbol_kind));

        if let Some(accessibility) = accessibility {
            assert!(self.storage.add_component(new_symbol_id, accessibility));
        }

        if symbol_kind.has_member() {
            assert!(self
                .storage
                .add_component(new_symbol_id, Member::default()));
        }

        if symbol_kind.has_generic_parameters() {
            assert!(self.storage.add_component(
                new_symbol_id,
                syntax_tree_component::GenericParameters(
                    generic_parameters_syn
                )
            ));
        }

        if symbol_kind.has_where_clause() {
            assert!(self.storage.add_component(
                new_symbol_id,
                syntax_tree_component::WhereClause(where_clause_syn)
            ));
        }

        let mut parent = self.storage.get_mut::<Member>(parent_id).unwrap();

        match parent.entry(name.span.str().to_owned()) {
            Entry::Occupied(entry) => {
                handler.receive(Box::new(ItemRedifinition {
                    existing_id: GlobalID::new(
                        parent_id.target_id,
                        *entry.get(),
                    ),
                    new_id: new_symbol_id,
                    in_id: parent_id,
                }));
            }
            Entry::Vacant(entry) => {
                entry.insert(new_symbol_id.id);
            }
        }

        new_symbol_id
    }

    fn create_enum(
        &mut self,
        syntax_tree: syntax_tree::item::r#enum::Enum,
        parent_module_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        let enum_id = self.insert_member(
            parent_module_id,
            &syntax_tree.signature.identifier,
            SymbolKind::Enum,
            Some(
                self.create_accessibility(
                    parent_module_id,
                    &syntax_tree.access_modifier,
                )
                .unwrap(),
            ),
            syntax_tree.signature.generic_parameters,
            syntax_tree.body.where_clause,
            handler,
        );

        assert!(self.storage.add_component(enum_id, Implemented::default()));

        for (index, variant) in syntax_tree
            .body
            .members
            .into_iter()
            .filter_map(Passable::into_option)
            .enumerate()
        {
            let variant_id = self.insert_member(
                enum_id,
                &variant.identifier,
                SymbolKind::Variant,
                None,
                None,
                None,
                handler,
            );

            assert!(self.storage.add_component(
                variant_id,
                syntax_tree_component::Variant {
                    variant_association: variant.association
                }
            ));

            assert!(self
                .storage
                .add_component(variant_id, VariantDeclarationOrder {
                    order: index
                }));
        }

        enum_id
    }

    #[allow(clippy::too_many_lines)]
    fn create_trait(
        &mut self,
        syntax_tree: syntax_tree::item::r#trait::Trait,
        parent_module_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        let trait_id = self.insert_member(
            parent_module_id,
            &syntax_tree.signature.identifier,
            SymbolKind::Trait,
            Some(
                self.create_accessibility(
                    parent_module_id,
                    &syntax_tree.access_modifier,
                )
                .unwrap(),
            ),
            syntax_tree.signature.generic_parameters,
            syntax_tree.body.where_clause,
            handler,
        );

        assert!(self.storage.add_component(trait_id, Implemented::default()));

        for item in syntax_tree
            .body
            .members
            .into_iter()
            .filter_map(Passable::into_option)
        {
            let member_id = match item {
                syntax_tree::item::r#trait::Member::Function(
                    trait_function,
                ) => {
                    let trait_function_id = self.insert_member(
                        trait_id,
                        &trait_function.signature.identifier,
                        SymbolKind::TraitFunction,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &trait_function.access_modifier,
                            )
                            .unwrap(),
                        ),
                        trait_function.signature.generic_parameters,
                        trait_function
                            .trailing_where_clause
                            .map(|x| x.where_clause),
                        handler,
                    );

                    // add the parameters
                    Self::validate_parameters_syntax(
                        &trait_function.signature.parameters,
                        false,
                        handler,
                    );
                    assert!(self.storage.add_component(
                        trait_function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters: trait_function.signature.parameters,
                            return_type: trait_function.signature.return_type
                        }
                    ));

                    trait_function_id
                }

                syntax_tree::item::r#trait::Member::Type(trait_type) => self
                    .insert_member(
                        trait_id,
                        &trait_type.signature.identifier,
                        SymbolKind::TraitType,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &trait_type.access_modifier,
                            )
                            .unwrap(),
                        ),
                        trait_type.signature.generic_parameters,
                        trait_type
                            .trailing_where_clause
                            .map(|x| x.where_clause),
                        handler,
                    ),

                syntax_tree::item::r#trait::Member::Constant(
                    trait_constant,
                ) => {
                    let trait_constant_id = self.insert_member(
                        trait_id,
                        &trait_constant.signature.identifier,
                        SymbolKind::TraitConstant,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &trait_constant.access_modifier,
                            )
                            .unwrap(),
                        ),
                        trait_constant.signature.generic_parameters,
                        trait_constant
                            .trailing_where_clause
                            .map(|x| x.where_clause),
                        handler,
                    );

                    assert!(self.storage.add_component(
                        trait_constant_id,
                        trait_constant.signature.r#type
                    ));

                    trait_constant_id
                }
            };

            let member_accessibility =
                self.storage.get::<Accessibility>(member_id).unwrap();

            if self
                .accessibility_hierarchy_relationship(
                    parent_module_id.target_id,
                    *member_accessibility,
                    *self.storage.get(trait_id).unwrap(),
                )
                .unwrap()
                == component::HierarchyRelationship::Parent
            {
                handler.receive(Box::new(SymbolIsMoreAccessibleThanParent {
                    symbol_id: member_id,
                    parent_id: trait_id,
                }));
            }
        }

        trait_id
    }

    fn validate_parameters_syntax(
        parameters: &syntax_tree::item::function::Parameters,
        can_have_var_ars: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let count =
            parameters.connected_list.as_ref().map_or(0, |x| x.rest.len() + 1);

        for (index, param) in parameters
            .connected_list
            .iter()
            .flat_map(ConnectedList::elements)
            .enumerate()
        {
            // not-allowed
            if !can_have_var_ars && param.is_var_args() {
                handler.receive(Box::new(VariadicArgumentsAreNotAllowed {
                    span: param.span(),
                }));
            }

            // not-trailing
            if param.is_var_args() && index != (count - 1) {
                handler.receive(Box::new(VariadicArgumentsMustBeTrailing {
                    span: param.span(),
                }));
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        clippy::cognitive_complexity,
        clippy::too_many_arguments
    )]
    fn create_module(
        &mut self,
        target_id: TargetID,
        name: String,
        syntax_tree: syntax_tree::target::ModuleTree,
        parent_module_id: Option<GlobalID>,
        imports_by_module_id: &mut HashMap<
            GlobalID,
            Vec<syntax_tree::item::module::Import>,
        >,
        implements_by_module_id: &mut HashMap<
            GlobalID,
            Vec<syntax_tree::item::implements::Implements>,
        >,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        // create the module id that we will operate on
        let module_id = GlobalID::new(
            target_id,
            if parent_module_id.is_some() {
                self.targets_by_id.get_mut(&target_id).unwrap().generate_id()
            } else {
                ID::ROOT_MODULE
            },
        );

        // has the parent module id

        assert!(self.storage.add_component(module_id, Parent {
            parent: parent_module_id.map(|x| x.id)
        }));

        if let Some(parent_module_id) = parent_module_id {
            assert!(self.storage.add_component(
                module_id,
                syntax_tree.signature.as_ref().map_or(
                    Accessibility::Public,
                    |x| {
                        self.create_accessibility(
                            parent_module_id,
                            &x.access_modifier,
                        )
                        .unwrap()
                    }
                )
            ));
            assert!(self
                .storage
                .get_mut::<Member>(parent_module_id)
                .unwrap()
                .insert(name.clone(), module_id.id)
                .is_none());
        } else {
            assert!(self
                .storage
                .add_component(module_id, Accessibility::Public));
        }

        assert!(self.storage.add_component(module_id, Import::default()));
        assert!(self.storage.add_component(module_id, SymbolKind::Module));
        assert!(self.storage.add_component(module_id, Member::default()));
        assert!(self.storage.add_component(module_id, Name(name)));

        let ModuleTree { signature, module_content, submodules_by_name } =
            syntax_tree;

        assert!(self.storage.add_component(module_id, LocationSpan {
            span: signature.map(|x| x.signature.identifier.span),
        }));

        // recursively create the submodules, redifinitions are handled by the
        // target parsing logic already
        for (name, submodule) in submodules_by_name {
            self.create_module(
                target_id,
                name.clone(),
                submodule,
                Some(module_id),
                imports_by_module_id,
                implements_by_module_id,
                handler,
            );
        }

        for member in module_content.members {
            let Passable::SyntaxTree(member) = member else {
                continue;
            };

            match member {
                syntax_tree::item::module::Member::Trait(syn) => {
                    self.create_trait(syn, module_id, handler);
                }

                syntax_tree::item::module::Member::Function(syn) => {
                    let function_id = self.insert_member(
                        module_id,
                        &syn.signature.identifier,
                        SymbolKind::Function,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &syn.access_modifier,
                            )
                            .unwrap(),
                        ),
                        syn.signature.generic_parameters,
                        syn.body.where_clause,
                        handler,
                    );

                    // add the signature
                    Self::validate_parameters_syntax(
                        &syn.signature.parameters,
                        false,
                        handler,
                    );

                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters: syn.signature.parameters,
                            return_type: syn.signature.return_type,
                        }
                    ));

                    // add the body
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionBody {
                            statements: syn.body.members,
                        }
                    ));
                }

                syntax_tree::item::module::Member::Type(syn) => {
                    let ty_id = self.insert_member(
                        module_id,
                        &syn.signature.identifier,
                        SymbolKind::Type,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &syn.access_modifier,
                            )
                            .unwrap(),
                        ),
                        syn.signature.generic_parameters,
                        syn.body.trailing_where_clause.map(|x| x.where_clause),
                        handler,
                    );

                    assert!(self.storage.add_component(
                        ty_id,
                        syntax_tree_component::TypeAlias(syn.body.r#type)
                    ));
                }

                syntax_tree::item::module::Member::Struct(syn) => {
                    let struct_id = self.insert_member(
                        module_id,
                        &syn.signature.identifier,
                        SymbolKind::Struct,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &syn.access_modifier,
                            )
                            .unwrap(),
                        ),
                        syn.signature.generic_parameters,
                        syn.body.where_clause,
                        handler,
                    );

                    assert!(self
                        .storage
                        .add_component(struct_id, Implemented::default()));
                    assert!(self.storage.add_component(
                        struct_id,
                        syntax_tree_component::Fields {
                            fields: syn.body.members
                        }
                    ));
                }

                syntax_tree::item::module::Member::Implements(
                    implementation,
                ) => {
                    implements_by_module_id
                        .entry(module_id)
                        .or_default()
                        .push(implementation);
                }

                syntax_tree::item::module::Member::Enum(syn) => {
                    self.create_enum(syn, module_id, handler);
                }

                syntax_tree::item::module::Member::Module(_) => {
                    unreachable!("should've been extracted out")
                }

                syntax_tree::item::module::Member::Constant(constant) => {
                    let constant_id = self.insert_member(
                        module_id,
                        &constant.signature.identifier,
                        SymbolKind::Constant,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &constant.access_modifier,
                            )
                            .unwrap(),
                        ),
                        constant.signature.generic_parameters,
                        constant
                            .body
                            .trailing_where_clause
                            .map(|x| x.where_clause),
                        handler,
                    );

                    assert!(self
                        .storage
                        .add_component(constant_id, constant.signature.r#type));
                    assert!(self
                        .storage
                        .add_component(constant_id, constant.body.expression));
                }

                syntax_tree::item::module::Member::Marker(marker) => {
                    let marker_id = self.insert_member(
                        module_id,
                        &marker.signature.identifier,
                        SymbolKind::Marker,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &marker.access_modifier,
                            )
                            .unwrap(),
                        ),
                        marker.signature.generic_parameters,
                        marker.trailing_where_clause.map(|x| x.where_clause),
                        handler,
                    );

                    assert!(self
                        .storage
                        .add_component(marker_id, Implemented::default()));
                }

                syntax_tree::item::module::Member::Extern(syn) => {
                    // get the calling convention of this extern
                    if syn.convention.value.as_deref() != Some("C") {
                        handler.receive(Box::new(
                            UnknownExternCallingConvention {
                                span: syn.convention.span.clone(),
                            },
                        ));
                    }

                    for function in syn
                        .functions
                        .into_iter()
                        .filter_map(Passable::into_option)
                    {
                        let calling_convention =
                            match syn.convention.value.as_deref() {
                                Some("C") => Extern::C(ExternC {
                                    var_args: function
                                        .signature
                                        .parameters
                                        .connected_list
                                        .iter()
                                        .flat_map(ConnectedList::elements)
                                        .last()
                                        .is_some_and(
                                            ParameterKind::is_var_args,
                                        ),
                                }),
                                _ => Extern::Unknown,
                            };

                        let function_id = self.insert_member(
                            module_id,
                            &function.signature.identifier,
                            SymbolKind::ExternFunction,
                            Some(
                                self.create_accessibility(
                                    module_id,
                                    &function.access_modifier,
                                )
                                .unwrap(),
                            ),
                            function.signature.generic_parameters,
                            function
                                .trailing_where_clause
                                .map(|x| x.where_clause),
                            handler,
                        );

                        Self::validate_parameters_syntax(
                            &function.signature.parameters,
                            matches!(calling_convention, Extern::C(..)),
                            handler,
                        );

                        // add the parameters
                        assert!(self.storage.add_component(
                            function_id,
                            syntax_tree_component::FunctionSignature {
                                parameters: function.signature.parameters,
                                return_type: function.signature.return_type,
                            }
                        ));

                        // add the extern calling convention
                        assert!(self
                            .storage
                            .add_component(function_id, calling_convention));
                    }
                }

                syntax_tree::item::module::Member::Import(using) => {
                    imports_by_module_id
                        .entry(module_id)
                        .or_default()
                        .push(using);
                }
            }
        }

        module_id
    }
}

#[cfg(test)]
mod test;
