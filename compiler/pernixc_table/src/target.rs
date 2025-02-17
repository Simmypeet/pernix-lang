use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::{Hash, Hasher},
    time::SystemTime,
};

use pernixc_handler::Handler;
use pernixc_lexical::token::Identifier;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{
    self,
    item::{
        ImplementationKind, ImplementationMember, ParameterKind, UsingKind,
    },
    ConnectedList, QualifiedIdentifierRoot,
};

use super::{
    diagnostic::{
        FoundEmptyImplementationOnTrait, FoundImplementationWithBodyOnMarker,
        InvalidSymbolInImplementation,
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
        ConflictingUsing, Diagnostic, ExpectModule,
        ExpectedImplementationWithBodyForAdt, InvalidConstImplementation,
        InvalidFinalImplementation, ItemRedifinition,
        NonFinalMarkerImplementation, UnknownExternCallingConvention,
        VariadicArgumentsAreNotAllowed, VariadicArgumentsMustBeTrailing,
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
            target_syntax.dissolve().0,
            None,
            &mut usings_by_module_id,
            &mut implementations_by_module_id,
            handler,
        );

        for (defined_in_module_id, using) in usings_by_module_id
            .into_iter()
            .flat_map(|x| x.1.into_iter().map(move |y| (x.0, y)))
        {
            self.insert_usings(defined_in_module_id, &using, handler);
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
        implementation_signature: syntax_tree::item::ImplementationSignature,
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
            span: Some(implementation_signature.qualified_identifier().span())
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

        let (
            final_keword,
            _,
            generic_parameters,
            const_keyword,
            qualified_identifier,
            where_clause,
        ) = implementation_signature.dissolve();

        assert!(self.storage.add_component(
            new_symbol_id,
            syntax_tree_component::GenericParameters(generic_parameters)
        ));
        assert!(self.storage.add_component(
            new_symbol_id,
            syntax_tree_component::WhereClause(where_clause)
        ));

        match symbol_kind {
            SymbolKind::PositiveTraitImplementation => {
                assert!(self.storage.add_component(
                    new_symbol_id,
                    PositiveTraitImplementation {
                        is_const: const_keyword.is_some(),
                    },
                ));
                assert!(self
                    .storage
                    .add_component(new_symbol_id, TraitImplementation {
                        is_final: final_keword.is_some(),
                    }));
            }

            SymbolKind::NegativeTraitImplementation => {
                assert!(self
                    .storage
                    .add_component(new_symbol_id, TraitImplementation {
                        is_final: final_keword.is_some(),
                    }));

                if let Some(const_keyword) = const_keyword {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span,
                    }));
                }
            }

            SymbolKind::AdtImplementation => {
                if let Some(final_keyword) = final_keword {
                    handler.receive(Box::new(InvalidFinalImplementation {
                        span: final_keyword.span,
                    }));
                }

                if let Some(const_keyword) = const_keyword {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span,
                    }));
                }
            }

            SymbolKind::NegativeMarkerImplementation
            | SymbolKind::PositiveMarkerImplementation => {
                if final_keword.is_none() {
                    handler.receive(Box::new(NonFinalMarkerImplementation {
                        span: qualified_identifier.span(),
                    }));
                }

                if let Some(const_keyword) = const_keyword {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span,
                    }));
                }
            }

            _ => panic!("invalid {symbol_kind:?}"),
        }

        // insert qualified identifier
        assert!(self.storage.add_component(
            new_symbol_id,
            syntax_tree_component::ImplementationQualifiedIdentifier(
                qualified_identifier
            )
        ));

        new_symbol_id
    }

    #[allow(clippy::too_many_lines)]
    fn create_adt_implementation(
        &mut self,
        implementation: syntax_tree::item::Implementation,
        declared_in: GlobalID,
        adt_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let (signature, kind) = implementation.dissolve();
        let implementation_body = match kind {
            ImplementationKind::Empty(_) | ImplementationKind::Negative(_) => {
                handler.receive(Box::new(
                    ExpectedImplementationWithBodyForAdt {
                        invalid_implementation_span: signature.span(),
                    },
                ));

                return;
            }
            ImplementationKind::Positive(body) => body,
        };
        let (_, members, _) = implementation_body.dissolve();

        // the implementation id
        let adt_implementation_id = self.insert_implementation(
            adt_id,
            declared_in,
            SymbolKind::AdtImplementation,
            signature,
            handler,
        );

        for member in members {
            match member {
                ImplementationMember::Constant(syn) => {
                    handler.receive(Box::new(
                        UnexpectedAdtImplementationMember {
                            unexpected_member_span: syn.signature().span(),
                        },
                    ));
                }
                ImplementationMember::Type(syn) => {
                    handler.receive(Box::new(
                        UnexpectedAdtImplementationMember {
                            unexpected_member_span: syn.signature().span(),
                        },
                    ));
                }
                ImplementationMember::Function(syn) => {
                    let (access_modifier, _, signature, body) = syn.dissolve();
                    let (
                        _,
                        ident,
                        generic_parameters,
                        parameters,
                        return_type,
                        where_clause,
                    ) = signature.dissolve();

                    let function_id = if let Some(existing_id) =
                        self.get_member_of(adt_id, ident.span.str())
                    {
                        let in_implementation = self
                            .get::<Member>(adt_implementation_id)
                            .contains_key(ident.span.str());

                        let new_id = self.insert_member(
                            adt_implementation_id,
                            &ident,
                            SymbolKind::AdtImplementationFunction,
                            Some(
                                self.create_accessibility(
                                    adt_implementation_id,
                                    &access_modifier,
                                )
                                .unwrap(),
                            ),
                            generic_parameters,
                            where_clause,
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
                            &ident,
                            SymbolKind::AdtImplementationFunction,
                            Some(
                                self.create_accessibility(
                                    adt_implementation_id,
                                    &access_modifier,
                                )
                                .unwrap(),
                            ),
                            generic_parameters,
                            where_clause,
                            handler,
                        )
                    };

                    // add the parameters
                    Self::validate_parameters_syntax(
                        &parameters,
                        false,
                        handler,
                    );
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters,
                            return_type
                        }
                    ));

                    // add the body
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionBody {
                            statements: body
                        }
                    ));
                }
            };
        }
    }

    fn create_marker_implementation(
        &mut self,
        implementation: syntax_tree::item::Implementation,
        declared_in: GlobalID,
        marker_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let (signature, kind) = implementation.dissolve();

        let symbol_kind = match &kind {
            ImplementationKind::Negative(..) => {
                SymbolKind::NegativeMarkerImplementation
            }
            ImplementationKind::Positive(_) => {
                handler.receive(Box::new(
                    FoundImplementationWithBodyOnMarker {
                        implementation_span: signature
                            .qualified_identifier()
                            .span(),
                    },
                ));

                return;
            }

            ImplementationKind::Empty(_) => {
                SymbolKind::PositiveMarkerImplementation
            }
        };

        self.insert_implementation(
            marker_id,
            declared_in,
            symbol_kind,
            signature,
            handler,
        );
    }

    fn create_trait_implementation(
        &mut self,
        implementation: syntax_tree::item::Implementation,
        declared_in: GlobalID,
        trait_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let (signature, kind) = implementation.dissolve();

        let symbol_kind = match &kind {
            ImplementationKind::Negative(..) => {
                SymbolKind::NegativeTraitImplementation
            }
            ImplementationKind::Positive(_) => {
                SymbolKind::PositiveTraitImplementation
            }

            ImplementationKind::Empty(_) => {
                handler.receive(Box::new(FoundEmptyImplementationOnTrait {
                    empty_implementation_signature_span: signature.span(),
                }));
                return;
            }
        };

        let implementation_id = self.insert_implementation(
            trait_id,
            declared_in,
            symbol_kind,
            signature,
            handler,
        );

        if let ImplementationKind::Positive(body) = kind {
            self.insert_positive_trait_implementation_members(
                implementation_id,
                trait_id,
                body.dissolve().1,
                handler,
            );
        }
    }

    #[allow(clippy::too_many_lines)]
    fn insert_positive_trait_implementation_members(
        &mut self,
        implementation_id: GlobalID,
        trait_id: GlobalID,
        members: Vec<syntax_tree::item::ImplementationMember>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        for member in members {
            let trait_implementation_member_id = match member {
                ImplementationMember::Type(syn) => {
                    let (_, signature, definition, _) = syn.dissolve();
                    let (_, ident, generic_parameters) = signature.dissolve();
                    let (_, ty, where_clause) = definition.dissolve();

                    let ty_id = self.insert_member(
                        implementation_id,
                        &ident,
                        SymbolKind::TraitImplementationType,
                        None,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(
                        ty_id,
                        syntax_tree_component::TypeAlias(ty)
                    ));

                    ty_id
                }
                ImplementationMember::Function(syn) => {
                    let (_, _, signature, body) = syn.dissolve();
                    let (
                        _,
                        ident,
                        generic_parameters,
                        parameters,
                        return_type,
                        where_clause,
                    ) = signature.dissolve();

                    let function_id = self.insert_member(
                        implementation_id,
                        &ident,
                        SymbolKind::TraitImplementationFunction,
                        None,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    // add the parameters
                    Self::validate_parameters_syntax(
                        &parameters,
                        false,
                        handler,
                    );
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters,
                            return_type
                        }
                    ));

                    // add the body
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionBody {
                            statements: body
                        }
                    ));

                    function_id
                }
                ImplementationMember::Constant(syn) => {
                    let (_, signature, definition) = syn.dissolve();
                    let (_, ident, generic_parameters, _, ty) =
                        signature.dissolve();
                    let (_, expression, where_caluse, _) =
                        definition.dissolve();

                    let constant_id = self.insert_member(
                        implementation_id,
                        &ident,
                        SymbolKind::TraitImplementationConstant,
                        None,
                        generic_parameters,
                        where_caluse,
                        handler,
                    );

                    assert!(self.storage.add_component(constant_id, ty));
                    assert!(self
                        .storage
                        .add_component(constant_id, expression));

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
        implementation: pernixc_syntax::syntax_tree::item::Implementation,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let mut current_id: GlobalID = match implementation
            .signature()
            .qualified_identifier()
            .root()
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
                        generic_identifier.identifier().span.str(),
                    )
                    .or_else(|| {
                        self.storage
                            .get::<Import>(defined_in_module_id)
                            .unwrap()
                            .get(generic_identifier.identifier().span.str())
                            .map(|x| x.id)
                    })
                else {
                    handler.receive(Box::new(SymbolNotFound {
                        searched_item_id: Some(defined_in_module_id),
                        resolution_span: generic_identifier
                            .identifier()
                            .span
                            .clone(),
                    }));
                    return;
                };

                id
            }
        };

        if !implementation.signature().qualified_identifier().rest().is_empty()
        {
            if *self.storage.get::<SymbolKind>(current_id).unwrap()
                != SymbolKind::Module
            {
                handler.receive(Box::new(ExpectModule {
                    module_path: implementation
                        .signature()
                        .qualified_identifier()
                        .root()
                        .span(),
                    found_id: current_id,
                }));
                return;
            }

            if let Some(gen_args) = implementation
                .signature()
                .qualified_identifier()
                .root()
                .as_generic_identifier()
                .and_then(|x| x.generic_arguments().as_ref())
            {
                handler.receive(Box::new(NoGenericArgumentsRequired {
                    global_id: current_id,
                    generic_argument_span: gen_args.span(),
                }));
            }
        }

        for (index, (_, generic_identifier)) in implementation
            .signature()
            .qualified_identifier()
            .rest()
            .iter()
            .enumerate()
        {
            let Some(next_id) = self.get_member_of(
                current_id,
                generic_identifier.identifier().span.str(),
            ) else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(current_id),
                    resolution_span: generic_identifier
                        .identifier()
                        .span
                        .clone(),
                }));
                return;
            };

            // non-fatal error, no need to return early
            if !self.symbol_accessible(defined_in_module_id, next_id) {
                handler.receive(Box::new(SymbolIsNotAccessible {
                    referring_site: defined_in_module_id,
                    referred: next_id,
                    referred_span: generic_identifier.identifier().span.clone(),
                }));
            }

            if index
                == implementation
                    .signature()
                    .qualified_identifier()
                    .rest()
                    .len()
                    - 1
            {
                current_id = next_id;
            } else {
                if *self.storage.get::<SymbolKind>(next_id).unwrap()
                    != SymbolKind::Module
                {
                    handler.receive(Box::new(ExpectModule {
                        module_path: generic_identifier
                            .identifier()
                            .span
                            .clone(),
                        found_id: next_id,
                    }));
                    return;
                }

                if let Some(gen_args) =
                    generic_identifier.generic_arguments().as_ref()
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
                    implementation,
                    defined_in_module_id,
                    current_id,
                    handler,
                );
            }

            SymbolKind::Marker => {
                self.create_marker_implementation(
                    implementation,
                    defined_in_module_id,
                    current_id,
                    handler,
                );
            }

            SymbolKind::Enum | SymbolKind::Struct => {
                self.create_adt_implementation(
                    implementation,
                    defined_in_module_id,
                    current_id,
                    handler,
                );
            }

            _ => {
                handler.receive(Box::new(InvalidSymbolInImplementation {
                    invalid_item_id: current_id,
                    qualified_identifier_span: implementation
                        .signature()
                        .qualified_identifier()
                        .span(),
                }));
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn insert_usings(
        &self,
        defined_in_module_id: GlobalID,
        using: &pernixc_syntax::syntax_tree::item::Using,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        assert_eq!(
            *self.storage.get::<SymbolKind>(defined_in_module_id).unwrap(),
            SymbolKind::Module
        );

        match using.kind() {
            UsingKind::From(from) => {
                let Some(from_id) = self.resolve_simple_path(
                    from.from().simple_path(),
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
                        module_path: from.from().simple_path().span(),
                        found_id: from_id,
                    }));

                    return;
                }

                for import in from
                    .imports()
                    .connected_list()
                    .as_ref()
                    .into_iter()
                    .flat_map(ConnectedList::elements)
                {
                    let Some(imported_id) = self
                        .storage
                        .get::<Member>(from_id)
                        .unwrap()
                        .get(import.identifier().span.str())
                        .copied()
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: Some(from_id),
                            resolution_span: import.identifier().span.clone(),
                        }));
                        continue;
                    };
                    let imported_global_id =
                        GlobalID::new(from_id.target_id, imported_id);

                    // check if the symbol is accessible
                    if !self.symbol_accessible(
                        defined_in_module_id,
                        imported_global_id,
                    ) {
                        handler.receive(Box::new(SymbolIsNotAccessible {
                            referring_site: defined_in_module_id,
                            referred: imported_global_id,
                            referred_span: import.identifier().span.clone(),
                        }));
                    }

                    let name = import.alias().as_ref().map_or_else(
                        || {
                            self.storage
                                .get::<Name>(imported_global_id)
                                .unwrap()
                                .0
                                .clone()
                        },
                        |x| x.identifier().span.str().to_owned(),
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
                            using_span: import.alias().as_ref().map_or_else(
                                || import.identifier().span(),
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
                                id: GlobalID::new(
                                    from_id.target_id,
                                    imported_id,
                                ),
                                span: import.alias().as_ref().map_or_else(
                                    || import.identifier().span(),
                                    SourceElement::span,
                                ),
                            });
                    }
                }
            }

            UsingKind::One(one) => {
                let Some(using_module_id) = self.resolve_simple_path(
                    one.simple_path(),
                    defined_in_module_id,
                    true,
                    handler,
                ) else {
                    return;
                };

                if *self.storage.get::<SymbolKind>(using_module_id).unwrap()
                    != SymbolKind::Module
                {
                    handler.receive(Box::new(ExpectModule {
                        module_path: one.simple_path().span(),
                        found_id: using_module_id,
                    }));

                    return;
                }

                let name = one.alias().as_ref().map_or_else(
                    || {
                        self.storage
                            .get::<Name>(using_module_id)
                            .unwrap()
                            .0
                            .clone()
                    },
                    |x| x.identifier().span.str().to_owned(),
                );

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
                        self.get::<Import>(defined_in_module_id)
                            .get(&name)
                            .map(|x| Some(x.span.clone()))
                    });

                if let Some(existing) = existing {
                    handler.receive(Box::new(ConflictingUsing {
                        using_span: one.alias().as_ref().map_or_else(
                            || one.simple_path().span(),
                            SourceElement::span,
                        ),
                        name,
                        module_id: defined_in_module_id,
                        conflicting_span: existing,
                    }));
                } else {
                    self.storage
                        .get_mut::<Import>(defined_in_module_id)
                        .unwrap()
                        .insert(name, Using {
                            id: using_module_id,
                            span: one.alias().as_ref().map_or_else(
                                || one.simple_path().span(),
                                SourceElement::span,
                            ),
                        });
                }
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
        generic_parameters_syn: Option<syntax_tree::item::GenericParameters>,
        where_clause_syn: Option<syntax_tree::item::WhereClause>,
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
        syntax_tree: syntax_tree::item::Enum,
        parent_module_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, ident, generic_parameters, where_clause) = signature.dissolve();

        let enum_id = self.insert_member(
            parent_module_id,
            &ident,
            SymbolKind::Enum,
            Some(
                self.create_accessibility(parent_module_id, &access_modifier)
                    .unwrap(),
            ),
            generic_parameters,
            where_clause,
            handler,
        );

        assert!(self.storage.add_component(enum_id, Implemented::default()));

        for (index, variant) in body
            .dissolve()
            .1
            .into_iter()
            .flat_map(ConnectedList::into_elements)
            .enumerate()
        {
            let (ident, association) = variant.dissolve();

            let variant_id = self.insert_member(
                enum_id,
                &ident,
                SymbolKind::Variant,
                None,
                None,
                None,
                handler,
            );

            assert!(self.storage.add_component(
                variant_id,
                syntax_tree_component::Variant {
                    variant_association: association
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
        syntax_tree: syntax_tree::item::Trait,
        parent_module_id: GlobalID,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        let (access_modifier, signature, content) = syntax_tree.dissolve();
        let (_, ident, generic_parameters, where_clause) = signature.dissolve();

        let trait_id = self.insert_member(
            parent_module_id,
            &ident,
            SymbolKind::Trait,
            Some(
                self.create_accessibility(parent_module_id, &access_modifier)
                    .unwrap(),
            ),
            generic_parameters,
            where_clause,
            handler,
        );

        assert!(self.storage.add_component(trait_id, Implemented::default()));

        for item in content.dissolve().1 {
            let member_id = match item {
                syntax_tree::item::TraitMember::Function(trait_function) => {
                    let (access_modifier, signature, _) =
                        trait_function.dissolve();

                    let (
                        _,
                        ident,
                        generic_parameters_syn,
                        parameters,
                        return_type,
                        where_clause_syn,
                    ) = signature.dissolve();

                    let trait_function_id = self.insert_member(
                        trait_id,
                        &ident,
                        SymbolKind::TraitFunction,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters_syn,
                        where_clause_syn,
                        handler,
                    );

                    // add the parameters
                    Self::validate_parameters_syntax(
                        &parameters,
                        false,
                        handler,
                    );
                    assert!(self.storage.add_component(
                        trait_function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters,
                            return_type
                        }
                    ));

                    trait_function_id
                }
                syntax_tree::item::TraitMember::Type(trait_type) => {
                    let (access_modifier, signature, where_clause, _) =
                        trait_type.dissolve();

                    let (_, ident, generic_parameters) = signature.dissolve();

                    self.insert_member(
                        trait_id,
                        &ident,
                        SymbolKind::TraitType,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters,
                        where_clause,
                        handler,
                    )
                }
                syntax_tree::item::TraitMember::Constant(trait_constant) => {
                    let (access_modifier, signature, where_clause, _) =
                        trait_constant.dissolve();
                    let (_, ident, generic_parameters, _, ty) =
                        signature.dissolve();

                    let trait_constant_id = self.insert_member(
                        trait_id,
                        &ident,
                        SymbolKind::TraitConstant,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(trait_constant_id, ty));

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
        parameters: &syntax_tree::item::Parameters,
        can_have_var_ars: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        let count = parameters
            .connected_list()
            .as_ref()
            .map_or(0, |x| x.rest().len() + 1);

        for (index, param) in parameters
            .connected_list()
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
        usings_by_module_id: &mut HashMap<
            GlobalID,
            Vec<syntax_tree::item::Using>,
        >,
        implementations_by_module_id: &mut HashMap<
            GlobalID,
            Vec<syntax_tree::item::Implementation>,
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
                syntax_tree.signature().as_ref().map_or(
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

        let (signature, content, submodule_by_name) = syntax_tree.dissolve();
        assert!(self.storage.add_component(module_id, LocationSpan {
            span: signature.map(|x| x.signature.identifier().span.clone()),
        }));
        let (usings, items) = content.dissolve();

        usings_by_module_id.entry(module_id).or_default().extend(usings);

        // recursively create the submodules, redifinitions are handled by the
        // target parsing logic already
        for (name, submodule) in submodule_by_name {
            self.create_module(
                target_id,
                name.clone(),
                submodule,
                Some(module_id),
                usings_by_module_id,
                implementations_by_module_id,
                handler,
            );
        }

        for item in items {
            match item {
                syntax_tree::item::Item::Trait(syn) => {
                    self.create_trait(syn, module_id, handler);
                }
                syntax_tree::item::Item::Function(syn) => {
                    let (access_modifier, _, signature, body) = syn.dissolve();
                    let (
                        _,
                        ident,
                        generic_parameters,
                        parameters,
                        return_type,
                        where_clause,
                    ) = signature.dissolve();

                    let function_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Function,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    // add the signature
                    Self::validate_parameters_syntax(
                        &parameters,
                        false,
                        handler,
                    );

                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionSignature {
                            parameters,
                            return_type
                        }
                    ));

                    // add the body
                    assert!(self.storage.add_component(
                        function_id,
                        syntax_tree_component::FunctionBody {
                            statements: body,
                        }
                    ));
                }
                syntax_tree::item::Item::Type(syn) => {
                    let (access_modifier, signature, definition, _) =
                        syn.dissolve();
                    let (_, ident, generic_parameters) = signature.dissolve();
                    let (_, ty, where_clause) = definition.dissolve();

                    let ty_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Type,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(
                        ty_id,
                        syntax_tree_component::TypeAlias(ty)
                    ));
                }
                syntax_tree::item::Item::Struct(syn) => {
                    let (access_modifier, signature, body) = syn.dissolve();
                    let (_, ident, generic_parameters, where_clause) =
                        signature.dissolve();

                    let struct_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Struct,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self
                        .storage
                        .add_component(struct_id, Implemented::default()));
                    assert!(self.storage.add_component(
                        struct_id,
                        syntax_tree_component::Fields { fields: body }
                    ));
                }
                syntax_tree::item::Item::Implementation(implementation) => {
                    implementations_by_module_id
                        .entry(module_id)
                        .or_default()
                        .push(implementation);
                }
                syntax_tree::item::Item::Enum(syn) => {
                    self.create_enum(syn, module_id, handler);
                }
                syntax_tree::item::Item::Module(_) => {
                    unreachable!("should've been extracted out")
                }
                syntax_tree::item::Item::Constant(constant) => {
                    let (access_modifier, signature, definition) =
                        constant.dissolve();
                    let (_, ident, generic_parameters, _, ty) =
                        signature.dissolve();
                    let (_, expression, where_caluse, _) =
                        definition.dissolve();

                    let constant_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Constant,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters,
                        where_caluse,
                        handler,
                    );

                    assert!(self.storage.add_component(constant_id, ty));
                    assert!(self
                        .storage
                        .add_component(constant_id, expression));
                }
                syntax_tree::item::Item::Marker(marker) => {
                    let (access_modifier, signature, _) = marker.dissolve();
                    let (_, ident, generic_parameters, where_clause) =
                        signature.dissolve();

                    let marker_id = self.insert_member(
                        module_id,
                        &ident,
                        SymbolKind::Marker,
                        Some(
                            self.create_accessibility(
                                module_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self
                        .storage
                        .add_component(marker_id, Implemented::default()));
                }
                syntax_tree::item::Item::Extern(syn) => {
                    let (_, calling_convention, functions) = syn.dissolve();

                    // get the calling convention of this extern
                    if calling_convention.value.as_deref() != Some("C") {
                        handler.receive(Box::new(
                            UnknownExternCallingConvention {
                                span: calling_convention.span.clone(),
                            },
                        ));
                    }

                    for function in functions.dissolve().1 {
                        let (access_modifier, signature, _) =
                            function.dissolve();

                        let (
                            _,
                            ident,
                            generic_parameters,
                            parameters,
                            return_type,
                            where_clause,
                        ) = signature.dissolve();

                        let calling_convention = match calling_convention
                            .value
                            .as_deref()
                        {
                            Some("C") => Extern::C(ExternC {
                                var_args: parameters
                                    .connected_list()
                                    .iter()
                                    .flat_map(ConnectedList::elements)
                                    .last()
                                    .is_some_and(ParameterKind::is_var_args),
                            }),
                            _ => Extern::Unknown,
                        };

                        let function_id = self.insert_member(
                            module_id,
                            &ident,
                            SymbolKind::ExternFunction,
                            Some(
                                self.create_accessibility(
                                    module_id,
                                    &access_modifier,
                                )
                                .unwrap(),
                            ),
                            generic_parameters,
                            where_clause,
                            handler,
                        );

                        Self::validate_parameters_syntax(
                            &parameters,
                            matches!(calling_convention, Extern::C(..)),
                            handler,
                        );

                        // add the parameters
                        assert!(self.storage.add_component(
                            function_id,
                            syntax_tree_component::FunctionSignature {
                                parameters,
                                return_type
                            }
                        ));

                        // add the extern calling convention
                        assert!(self
                            .storage
                            .add_component(function_id, calling_convention));
                    }
                }
            }
        }

        module_id
    }
}

#[cfg(test)]
mod test;
