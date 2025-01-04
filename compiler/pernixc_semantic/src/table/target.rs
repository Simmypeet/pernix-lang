use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::{Hash, Hasher},
    time::SystemTime,
};

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    self,
    item::{ImplementationKind, ImplementationMember, UsingKind},
    ConnectedList, QualifiedIdentifierRoot,
};

use super::{
    diagnostic::{
        FoundEmptyImplementationOnTrait, InvalidSymbolInImplementation,
        MismatchedTraitMemberAndImplementationMember,
        SymbolIsMoreAccessibleThanParent, TraitMemberKind,
        UnimplementedTraitMembers, UnknownTraitImplementationMember,
    },
    resolution::diagnostic::{NoGenericArgumentsRequired, ThisNotFound},
    GlobalID, Representation, TargetID, ID,
};
use crate::{
    component::{
        self, Accessibility, ConstTraitImplementation, Extern,
        FinalImplementation, Implemented, Import, LocationSpan, Member, Name,
        Parent, SymbolKind, Using,
    },
    diagnostic::Diagnostic,
    table::{
        diagnostic::{
            ConflictingUsing, ExpectModule, InvalidConstImplementation,
            InvalidFinalImplementation, ItemRedifinition,
            NonFinalMarkerImplementation, UnknownExternCallingConvention,
        },
        resolution::diagnostic::{SymbolIsNotAccessible, SymbolNotFound},
        Target,
    },
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
    /// Adds a target to the representation.
    ///
    /// # Errors
    ///
    /// See [`AddTargetError`] for possible errors.
    pub fn add_target(
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
            TargetID(hasher.finish() as usize)
        };

        // keep generating target IDs until we find a unique one
        while self.targets_by_id.contains_key(&target_id)
            || target_id == TargetID::CORE
        {
            instant += 1;
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            name.hash(&mut hasher);
            instant.hash(&mut hasher);
            target_id = TargetID(hasher.finish() as usize);
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
            self.insert_usings(defined_in_module_id, using, handler);
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

    fn insert_implementation(
        &mut self,
        implemented_id: GlobalID,
        defined_in_module_id: GlobalID,
        symbol_kind: SymbolKind,
        implementation_signature: syntax_tree::item::ImplementationSignature,
        has_member: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> GlobalID {
        let new_symbol_id = GlobalID::new(
            defined_in_module_id.target_id,
            self.targets_by_id
                .get_mut(&defined_in_module_id.target_id)
                .unwrap()
                .generate_id(),
        );

        assert!(self.storage.add_component(
            new_symbol_id,
            LocationSpan(
                implementation_signature.qualified_identifier().span()
            )
        ));

        let name =
            self.get_component::<Name>(implemented_id).unwrap().0.clone();

        assert!(self.storage.add_component(new_symbol_id, Name(name)));
        assert!(self
            .storage
            .add_component(new_symbol_id, Parent(defined_in_module_id.id)));
        assert!(self.storage.add_component(new_symbol_id, symbol_kind));

        if has_member {
            assert!(self
                .storage
                .add_component(new_symbol_id, Member::default()));
        }

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

        if let Some(generic_parameters_syn) = generic_parameters {
            assert!(self
                .storage
                .add_component(new_symbol_id, generic_parameters_syn,));
        }
        if let Some(where_clause_syn) = where_clause {
            assert!(self
                .storage
                .add_component(new_symbol_id, where_clause_syn,));
        }

        match symbol_kind {
            SymbolKind::PositiveTraitImplementation => {
                if final_keword.is_some() {
                    assert!(self
                        .storage
                        .add_component(new_symbol_id, FinalImplementation));
                }

                if const_keyword.is_some() {
                    assert!(self.storage.add_component(
                        new_symbol_id,
                        ConstTraitImplementation
                    ));
                }
            }

            SymbolKind::NegativeTraitImplementation => {
                if final_keword.is_some() {
                    assert!(self
                        .storage
                        .add_component(new_symbol_id, FinalImplementation));
                }

                if let Some(const_keyword) = const_keyword {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span.clone(),
                    }));
                }
            }

            SymbolKind::AdtImplementation => {
                if let Some(final_keyword) = final_keword {
                    handler.receive(Box::new(InvalidFinalImplementation {
                        span: final_keyword.span.clone(),
                    }));
                }

                if let Some(const_keyword) = const_keyword {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span.clone(),
                    }));
                }
            }

            SymbolKind::PositiveMarkerImplementation => {
                if final_keword.is_none() {
                    handler.receive(Box::new(NonFinalMarkerImplementation {
                        span: qualified_identifier.span(),
                    }));
                }

                if let Some(const_keyword) = const_keyword {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span.clone(),
                    }));
                }
            }

            SymbolKind::NegativeMarkerImplementation => {
                if final_keword.is_none() {
                    handler.receive(Box::new(NonFinalMarkerImplementation {
                        span: qualified_identifier.span(),
                    }));
                }

                if let Some(const_keyword) = const_keyword {
                    handler.receive(Box::new(InvalidConstImplementation {
                        span: const_keyword.span.clone(),
                    }));
                }
            }

            _ => panic!("invalid {symbol_kind:?}"),
        }

        // insert qualified identifier
        assert!(self
            .storage
            .add_component(new_symbol_id, qualified_identifier));

        new_symbol_id
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
            if symbol_kind == SymbolKind::PositiveTraitImplementation {
                true
            } else {
                false
            },
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
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(ty_id, ty));

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
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    // add the parameters
                    assert!(self
                        .storage
                        .add_component(function_id, parameters));

                    // add the return type
                    if let Some(return_type) = return_type {
                        assert!(self
                            .storage
                            .add_component(function_id, return_type));
                    }

                    // add the body
                    assert!(self.storage.add_component(function_id, body));

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
                        false,
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
                .get_component::<Member>(trait_id)
                .unwrap()
                .get(
                    self.get_component::<Name>(trait_implementation_member_id)
                        .unwrap()
                        .as_str(),
                )
                .copied()
            else {
                handler.receive(Box::new(UnknownTraitImplementationMember {
                    identifier_span: self
                        .get_component::<LocationSpan>(
                            trait_implementation_member_id,
                        )
                        .unwrap()
                        .0
                        .clone(),
                    trait_id,
                }));
                continue;
            };

            let trait_member_id =
                GlobalID::new(trait_id.target_id, trait_member_id.into());

            let trait_member_kind =
                *self.get_component::<SymbolKind>(trait_member_id).unwrap();
            let trait_implementation_member_kind = *self
                .get_component::<SymbolKind>(trait_implementation_member_id)
                .unwrap();

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
                            .get_component::<LocationSpan>(
                                trait_implementation_member_id,
                            )
                            .unwrap()
                            .0
                            .clone(),
                    },
                )),
            }
        }

        let trait_members = self.get_component::<Member>(trait_id).unwrap();
        let trait_implementation_members =
            self.get_component::<Member>(implementation_id).unwrap();

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
                    .ok()
                    .or_else(|| {
                        self.get_component::<Import>(defined_in_module_id)
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
            if *self.get_component::<SymbolKind>(current_id).unwrap()
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
            let Ok(next_id) = self.get_member_of(
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
            if !self.symbol_accessible(defined_in_module_id, next_id).unwrap() {
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
                if *self.get_component::<SymbolKind>(next_id).unwrap()
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

        let current_kind =
            *self.get_component::<SymbolKind>(current_id).unwrap();

        match current_kind {
            SymbolKind::Trait => {
                self.create_trait_implementation(
                    implementation,
                    defined_in_module_id,
                    current_id,
                    handler,
                );
            }

            SymbolKind::Enum | SymbolKind::Struct => {}

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

    fn insert_usings(
        &mut self,
        defined_in_module_id: GlobalID,
        using: pernixc_syntax::syntax_tree::item::Using,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        assert_eq!(
            *self.get_component::<SymbolKind>(defined_in_module_id).unwrap(),
            SymbolKind::Module
        );

        match using.kind() {
            UsingKind::From(from) => {
                let Ok(from_id) = self.resolve_simple_path(
                    from.from().simple_path(),
                    defined_in_module_id,
                    true,
                    handler,
                ) else {
                    return;
                };

                // must be module
                if *self.get_component::<SymbolKind>(from_id).unwrap()
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
                        .get_component::<Member>(from_id)
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
                        GlobalID::new(from_id.target_id, imported_id.into());

                    // check if the symbol is accessible
                    if !self
                        .symbol_accessible(
                            defined_in_module_id,
                            imported_global_id,
                        )
                        .unwrap()
                    {
                        handler.receive(Box::new(SymbolIsNotAccessible {
                            referring_site: defined_in_module_id,
                            referred: imported_global_id,
                            referred_span: import.identifier().span.clone(),
                        }));
                    }

                    let name = import.alias().as_ref().map_or_else(
                        || {
                            self.get_component::<Name>(imported_global_id)
                                .unwrap()
                                .0
                                .clone()
                        },
                        |x| x.identifier().span.str().to_owned(),
                    );

                    // check if there's existing symbol right now
                    let existing = self
                        .get_component::<Member>(defined_in_module_id)
                        .unwrap()
                        .get(&name)
                        .map(|x| {
                            self.get_component::<LocationSpan>(GlobalID::new(
                                defined_in_module_id.target_id,
                                *x,
                            ))
                            .map(|x| x.0.clone())
                        })
                        .or_else(|| {
                            self.get_component::<Import>(defined_in_module_id)
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
                                    imported_id.into(),
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
                let Ok(using_module_id) = self.resolve_simple_path(
                    one.simple_path(),
                    defined_in_module_id,
                    true,
                    handler,
                ) else {
                    return;
                };

                if *self.get_component::<SymbolKind>(using_module_id).unwrap()
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
                        self.get_component::<Name>(using_module_id)
                            .unwrap()
                            .0
                            .clone()
                    },
                    |x| x.identifier().span.str().to_owned(),
                );

                let existing = self
                    .get_component::<Member>(defined_in_module_id)
                    .unwrap()
                    .get(&name)
                    .map(|x| {
                        self.get_component::<LocationSpan>(GlobalID::new(
                            defined_in_module_id.target_id,
                            *x,
                        ))
                        .map(|x| x.0.clone())
                    })
                    .or_else(|| {
                        self.get_component::<Import>(defined_in_module_id)
                            .unwrap()
                            .get(&name)
                            .map(|x| Some(x.span.clone()))
                    });

                if let Some(existing) = existing {
                    handler.receive(Box::new(ConflictingUsing {
                        using_span: one.alias().as_ref().map_or_else(
                            || one.simple_path().span(),
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

    fn insert_member(
        &mut self,
        parent_id: GlobalID,
        name: &Identifier,
        symbol_kind: SymbolKind,
        accessibility: Option<Accessibility>,
        has_member: bool,
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

        assert!(self
            .storage
            .add_component(new_symbol_id, LocationSpan(name.span.clone())));
        assert!(self
            .storage
            .add_component(new_symbol_id, Name(name.span.str().to_owned())));
        assert!(self
            .storage
            .add_component(new_symbol_id, Parent(parent_id.id)));
        assert!(self.storage.add_component(new_symbol_id, symbol_kind));

        if let Some(accessibility) = accessibility {
            assert!(self.storage.add_component(new_symbol_id, accessibility));
        }

        if has_member {
            assert!(self
                .storage
                .add_component(new_symbol_id, Member::default()));
        }

        if let Some(generic_parameters_syn) = generic_parameters_syn {
            assert!(self
                .storage
                .add_component(new_symbol_id, generic_parameters_syn,));
        }
        if let Some(where_clause_syn) = where_clause_syn {
            assert!(self
                .storage
                .add_component(new_symbol_id, where_clause_syn,));
        }

        let mut parent = self.storage.get_mut::<Member>(parent_id).unwrap();

        match parent.entry(name.span.str().to_owned()) {
            Entry::Occupied(entry) => {
                handler.receive(Box::new(ItemRedifinition {
                    existing_id: *entry.get(),
                    new_id: new_symbol_id.id,
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
            true,
            generic_parameters,
            where_clause,
            handler,
        );

        assert!(self.storage.add_component(enum_id, Implemented::default()));

        for variant in
            body.dissolve().1.into_iter().flat_map(ConnectedList::into_elements)
        {
            let (ident, association) = variant.dissolve();

            let variant_id = self.insert_member(
                enum_id,
                &ident,
                SymbolKind::Variant,
                None,
                false,
                None,
                None,
                handler,
            );

            if let Some(association) = association {
                assert!(self
                    .storage
                    .add_component(variant_id, association.dissolve().1));
            }
        }

        enum_id
    }

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
            true,
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
                        false,
                        generic_parameters_syn,
                        where_clause_syn,
                        handler,
                    );

                    // add the parameters
                    assert!(self
                        .storage
                        .add_component(trait_function_id, parameters));

                    // add the return type
                    if let Some(return_type) = return_type {
                        assert!(self
                            .storage
                            .add_component(trait_function_id, return_type));
                    }

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
                        false,
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
                        SymbolKind::Constant,
                        Some(
                            self.create_accessibility(
                                trait_id,
                                &access_modifier,
                            )
                            .unwrap(),
                        ),
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(trait_constant_id, ty));

                    trait_constant_id
                }
            };

            let member_accessibility =
                self.get_component::<Accessibility>(member_id).unwrap();

            if self
                .accessibility_hierarchy_relationship(
                    parent_module_id.target_id,
                    *member_accessibility,
                    *self.get_component(trait_id).unwrap(),
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
        if let Some(parent_module_id) = parent_module_id {
            assert!(self
                .storage
                .add_component(module_id, Parent(parent_module_id.id)));
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
        assert!(self.storage.add_component(module_id, Name(name.clone())));

        let (signature, content, submodule_by_name) = syntax_tree.dissolve();
        if let Some(signature) = signature {
            assert!(self.storage.add_component(
                module_id,
                LocationSpan(signature.signature.identifier().span.clone())
            ));
        }
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
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    // add the parameters
                    assert!(self
                        .storage
                        .add_component(function_id, parameters));

                    // add the return type
                    if let Some(return_type) = return_type {
                        assert!(self
                            .storage
                            .add_component(function_id, return_type));
                    }

                    // add the body
                    assert!(self.storage.add_component(function_id, body));
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
                        false,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self.storage.add_component(ty_id, ty));
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
                        true,
                        generic_parameters,
                        where_clause,
                        handler,
                    );

                    assert!(self
                        .storage
                        .add_component(struct_id, Implemented::default()));
                    assert!(self.storage.add_component(struct_id, body));
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
                        false,
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
                        false,
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
                    let calling_convention =
                        if calling_convention.value.as_deref() == Some("C") {
                            Extern::C
                        } else {
                            handler.receive(Box::new(
                                UnknownExternCallingConvention {
                                    span: calling_convention.span.clone(),
                                },
                            ));

                            Extern::Unknown
                        };

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
                            false,
                            generic_parameters,
                            where_clause,
                            handler,
                        );

                        // add the parameters
                        assert!(self
                            .storage
                            .add_component(function_id, parameters));

                        // add the return type
                        if let Some(return_type) = return_type {
                            assert!(self
                                .storage
                                .add_component(function_id, return_type));
                        }

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
