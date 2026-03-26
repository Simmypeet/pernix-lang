use std::{collections::hash_map, sync::Arc};

use pernixc_handler::Storage;
use pernixc_hash::{FxDashMap, FxHashMap, FxHashSet};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_source_file::SourceFile;
use pernixc_symbol::{
    SymbolID, accessibility::Accessibility, calculate_qualified_name_id,
    get_target_root_module_id, kind::Kind, linkage, member::Member,
};
use pernixc_syntax::AccessModifier;
use pernixc_target::TargetID;
use qbice::storage::intern::Interned;

use crate::{
    diagnostic::{Diagnostic, ItemRedefinition},
    table::{ExternalSubmodule, Table},
};

/// A builder struct for constructing the final [`super::Table`].
pub struct Builder {
    engine: TrackedEngine,
    storage: Storage<Diagnostic>,
    target_id: TargetID,

    kinds: FxDashMap<SymbolID, Kind>,
    names: FxDashMap<SymbolID, Interned<str>>,
    spans: FxDashMap<SymbolID, Option<RelativeSpan>>,
    members: FxDashMap<SymbolID, Interned<Member>>,
    accessibilities: FxDashMap<SymbolID, Accessibility<SymbolID>>,

    implements_access_modifier_syntaxes:
        FxDashMap<SymbolID, Option<pernixc_syntax::AccessModifier>>,

    external_submodules: FxDashMap<SymbolID, Interned<ExternalSubmodule>>,

    generic_parameter_syntaxes: FxDashMap<
        SymbolID,
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,
    where_clause_syntaxes: FxDashMap<
        SymbolID,
        Option<pernixc_syntax::item::where_clause::Predicates>,
    >,
    type_alias_syntaxes:
        FxDashMap<SymbolID, Option<pernixc_syntax::r#type::Type>>,
    constant_type_annotation_syntaxes:
        FxDashMap<SymbolID, Option<pernixc_syntax::r#type::Type>>,
    constant_expression_syntaxes:
        FxDashMap<SymbolID, Option<pernixc_syntax::expression::Expression>>,
    function_signature_syntaxes: FxDashMap<
        SymbolID,
        (
            Option<pernixc_syntax::item::function::Parameters>,
            Option<pernixc_syntax::item::function::ReturnType>,
        ),
    >,
    function_linkages: FxDashMap<SymbolID, linkage::Linkage>,
    fields_syntaxes: FxDashMap<
        SymbolID,
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,
    variant_associated_type_syntaxes:
        FxDashMap<SymbolID, Option<pernixc_syntax::r#type::Type>>,
    variant_declaration_orders: FxDashMap<SymbolID, usize>,
    import_syntaxes:
        FxDashMap<SymbolID, Interned<[pernixc_syntax::item::module::Import]>>,
    implements_qualified_identifier_syntaxes:
        FxDashMap<SymbolID, pernixc_syntax::QualifiedIdentifier>,
    final_keywords: FxDashMap<SymbolID, Option<pernixc_syntax::Keyword>>,

    instance_trait_ref_syntaxes:
        FxDashMap<SymbolID, Option<pernixc_syntax::TraitRef>>,
    function_body_syntaxes: FxDashMap<
        SymbolID,
        Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    >,

    function_effect_annotation_syntaxes: FxDashMap<
        SymbolID,
        Option<pernixc_syntax::item::function::EffectAnnotation>,
    >,

    instance_associated_value_syntaxes:
        FxDashMap<SymbolID, Option<pernixc_syntax::InstanceValue>>,

    function_unsafe_keywords:
        FxDashMap<SymbolID, Option<pernixc_syntax::Keyword>>,

    scope_spans: FxDashMap<SymbolID, Option<RelativeSpan>>,
    external_instances: FxDashMap<SymbolID, bool>,

    token_tree: Option<Interned<pernixc_lexical::tree::Tree>>,
    source_file: Option<SourceFile>,

    is_root: bool,
}

impl Builder {
    /// Creates a new [`Builder`].
    pub fn new(
        engine: TrackedEngine,
        storage: Storage<Diagnostic>,
        source_file: Option<SourceFile>,
        token_tree: Option<Interned<pernixc_lexical::tree::Tree>>,
        target_id: TargetID,
        is_root: bool,
    ) -> Self {
        Self {
            engine,
            storage,
            target_id,
            kinds: FxDashMap::default(),
            names: FxDashMap::default(),
            spans: FxDashMap::default(),
            members: FxDashMap::default(),
            accessibilities: FxDashMap::default(),
            implements_access_modifier_syntaxes: FxDashMap::default(),
            external_submodules: FxDashMap::default(),
            generic_parameter_syntaxes: FxDashMap::default(),
            where_clause_syntaxes: FxDashMap::default(),
            type_alias_syntaxes: FxDashMap::default(),
            constant_type_annotation_syntaxes: FxDashMap::default(),
            constant_expression_syntaxes: FxDashMap::default(),
            function_signature_syntaxes: FxDashMap::default(),
            function_linkages: FxDashMap::default(),
            fields_syntaxes: FxDashMap::default(),
            variant_associated_type_syntaxes: FxDashMap::default(),
            variant_declaration_orders: FxDashMap::default(),
            import_syntaxes: FxDashMap::default(),
            implements_qualified_identifier_syntaxes: FxDashMap::default(),
            final_keywords: FxDashMap::default(),
            function_body_syntaxes: FxDashMap::default(),
            function_effect_annotation_syntaxes: FxDashMap::default(),
            instance_associated_value_syntaxes: FxDashMap::default(),
            external_instances: FxDashMap::default(),
            scope_spans: FxDashMap::default(),
            instance_trait_ref_syntaxes: FxDashMap::default(),
            function_unsafe_keywords: FxDashMap::default(),
            token_tree,
            source_file,
            is_root,
        }
    }

    /// Retrieves the tracked engine.
    #[must_use]
    pub const fn engine(&self) -> &TrackedEngine { &self.engine }

    /// Retrieves the target ID.
    #[must_use]
    pub const fn target_id(&self) -> TargetID { self.target_id }

    /// Finiailizes the builder into an immutable [`super::Table`].
    #[must_use]
    pub fn into_table(self, module_id: SymbolID) -> Interned<super::Table> {
        self.engine.intern(Table {
            kinds: self.engine.intern(self.kinds.into_iter().collect()),
            names: self.engine.intern(self.names.into_iter().collect()),
            spans: self.engine.intern(self.spans.into_iter().collect()),
            members: self.engine.intern(self.members.into_iter().collect()),
            accessibilities: self
                .engine
                .intern(self.accessibilities.into_iter().collect()),
            implements_access_modifier_syntaxes: self.engine.intern(
                self.implements_access_modifier_syntaxes.into_iter().collect(),
            ),

            // syntax extractions
            generic_parameter_syntaxes: self
                .engine
                .intern(self.generic_parameter_syntaxes.into_iter().collect()),
            where_clause_syntaxes: self
                .engine
                .intern(self.where_clause_syntaxes.into_iter().collect()),
            type_alias_syntaxes: self
                .engine
                .intern(self.type_alias_syntaxes.into_iter().collect()),
            constant_type_annotation_syntaxes: self.engine.intern(
                self.constant_type_annotation_syntaxes.into_iter().collect(),
            ),
            constant_expression_syntaxes: self.engine.intern(
                self.constant_expression_syntaxes.into_iter().collect(),
            ),
            function_signature_syntaxes: self
                .engine
                .intern(self.function_signature_syntaxes.into_iter().collect()),
            fields_syntaxes: self
                .engine
                .intern(self.fields_syntaxes.into_iter().collect()),
            variant_associated_type_syntaxes: self.engine.intern(
                self.variant_associated_type_syntaxes.into_iter().collect(),
            ),
            variant_declaration_orders: self
                .engine
                .intern(self.variant_declaration_orders.into_iter().collect()),
            import_syntaxes: self
                .engine
                .intern(self.import_syntaxes.into_iter().collect()),
            implements_qualified_identifier_syntaxes: self.engine.intern(
                self.implements_qualified_identifier_syntaxes
                    .into_iter()
                    .collect(),
            ),
            final_keywords: self
                .engine
                .intern(self.final_keywords.into_iter().collect()),
            function_body_syntaxes: self
                .engine
                .intern(self.function_body_syntaxes.into_iter().collect()),
            function_linkages: self
                .engine
                .intern(self.function_linkages.into_iter().collect()),
            function_effect_annotation_syntaxes: self.engine.intern(
                self.function_effect_annotation_syntaxes.into_iter().collect(),
            ),
            instance_associated_value_syntaxes: self.engine.intern(
                self.instance_associated_value_syntaxes.into_iter().collect(),
            ),
            instance_trait_ref_syntaxes: self
                .engine
                .intern(self.instance_trait_ref_syntaxes.into_iter().collect()),
            function_unsafe_keywords: self
                .engine
                .intern(self.function_unsafe_keywords.into_iter().collect()),
            scope_spans: self
                .engine
                .intern(self.scope_spans.into_iter().collect()),
            external_instances: self
                .engine
                .intern(self.external_instances.into_iter().collect()),

            external_submodules: self
                .engine
                .intern(self.external_submodules.into_iter().collect()),
            diagnostics: self.engine.intern_unsized({
                let mut storage = self.storage.into_vec();
                storage.sort();
                storage
            }),
            module_id,
        })
    }

    /// Creates an accessibility from an access modifier.
    pub async fn create_accessibility(
        &self,
        current_module_scope: SymbolID,
        access_modifier: Option<&AccessModifier>,
    ) -> Accessibility<SymbolID> {
        match access_modifier {
            Some(pernixc_syntax::AccessModifier::Private(_)) => {
                Accessibility::Scoped(current_module_scope)
            }
            Some(pernixc_syntax::AccessModifier::Internal(_)) => {
                Accessibility::Scoped(
                    self.engine.get_target_root_module_id(self.target_id).await,
                )
            }
            Some(pernixc_syntax::AccessModifier::Public(_)) | None => {
                Accessibility::Public
            }
        }
    }

    /// Adds a diagnostic to the builder.
    pub fn add_diagnostic(&self, diagnostic: Diagnostic) {
        self.storage.as_vec_mut().push(diagnostic);
    }

    /// Determines whether the table being built is defined at the root level.
    #[must_use]
    pub const fn is_root(&self) -> bool { self.is_root }

    /// Creates a name for the implements qualified identifier.
    #[must_use]
    pub fn implements_qualified_identifier_name(
        &self,
        qualified_identifier_span: &RelativeSpan,
    ) -> Interned<str> {
        let source_file = self.source_file.as_ref().unwrap();
        let token_tree = self.token_tree.as_ref().unwrap();

        self.engine.intern_unsized(format!(
            "[implements {}]",
            &source_file.content()[qualified_identifier_span
                .to_absolute_span(source_file, token_tree)
                .range()]
        ))
    }
}

impl Builder {
    /// Inserts a kind into the builder.
    pub fn insert_kind(&self, id: pernixc_symbol::SymbolID, kind: Kind) {
        assert!(self.kinds.insert(id, kind).is_none());
    }

    /// Inserts a member into the builder from the [`MemberBuilder`].
    pub fn insert_member_from_builder(
        &self,
        id: pernixc_symbol::SymbolID,
        member: MemberBuilder,
    ) {
        assert!(
            self.members
                .insert(
                    id,
                    self.engine.intern(Member {
                        member_ids_by_name: member.member_ids_by_name,
                        unnameds: member.unnameds,
                    }),
                )
                .is_none()
        );

        self.storage.as_vec_mut().extend(
            member
                .redefinition_errors
                .into_iter()
                .map(Diagnostic::ItemRedefinition),
        );
    }

    pub fn insert_member(
        &self,
        id: pernixc_symbol::SymbolID,
        member: Interned<Member>,
    ) {
        assert!(self.members.insert(id, member).is_none());
    }

    /// Inserts a name into the builder.
    pub fn insert_name(
        &self,
        id: pernixc_symbol::SymbolID,
        name: Interned<str>,
    ) {
        assert!(self.names.insert(id, name).is_none());
    }

    /// Inserts an identifier used as a name and span into the builder.
    pub fn insert_name_identifier(
        &self,
        id: pernixc_symbol::SymbolID,
        identifier: &pernixc_syntax::Identifier,
    ) {
        assert!(self.names.insert(id, identifier.kind.0.clone()).is_none());
        assert!(self.spans.insert(id, Some(identifier.span)).is_none());
    }

    /// Inserts a span into the builder.
    pub fn insert_span(
        &self,
        id: pernixc_symbol::SymbolID,
        span: Option<RelativeSpan>,
    ) {
        assert!(self.spans.insert(id, span).is_none());
    }

    /// Inserts an accessibility into the builder.
    pub fn insert_accessibility(
        &self,
        id: pernixc_symbol::SymbolID,
        accessibility: Accessibility<pernixc_symbol::SymbolID>,
    ) {
        assert!(self.accessibilities.insert(id, accessibility).is_none());
    }

    pub fn insert_instance_associated_value(
        &self,
        id: pernixc_symbol::SymbolID,
        qualified_identifier: Option<pernixc_syntax::InstanceValue>,
    ) {
        assert!(
            self.instance_associated_value_syntaxes
                .insert(id, qualified_identifier)
                .is_none()
        );
    }

    pub fn insert_instance_trait_ref(
        &self,
        id: pernixc_symbol::SymbolID,
        qualified_identifier: Option<pernixc_syntax::TraitRef>,
    ) {
        assert!(
            self.instance_trait_ref_syntaxes
                .insert(id, qualified_identifier)
                .is_none()
        );
    }

    pub fn insert_external_instance(
        &self,
        id: pernixc_symbol::SymbolID,
        is_external: bool,
    ) {
        assert!(self.external_instances.insert(id, is_external).is_none());
    }

    /// Inserts an accessibility into the builder from an access modifier.
    pub async fn insert_accessibility_by_access_modifier(
        &self,
        id: pernixc_symbol::SymbolID,
        current_module_scope_id: pernixc_symbol::SymbolID,
        access_modifier: Option<&pernixc_syntax::AccessModifier>,
    ) {
        let accessibility = self
            .create_accessibility(current_module_scope_id, access_modifier)
            .await;

        assert!(self.accessibilities.insert(id, accessibility).is_none());
    }

    pub fn insert_generic_parameters_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        generic_parameters: Option<
            pernixc_syntax::item::generic_parameters::GenericParameters,
        >,
    ) {
        assert!(
            self.generic_parameter_syntaxes
                .insert(id, generic_parameters)
                .is_none()
        );
    }

    pub fn insert_where_clause_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        where_clause: Option<pernixc_syntax::item::where_clause::Predicates>,
    ) {
        assert!(self.where_clause_syntaxes.insert(id, where_clause).is_none());
    }

    pub fn insert_final_keyword(
        &self,
        id: pernixc_symbol::SymbolID,
        final_keyword: Option<pernixc_syntax::Keyword>,
    ) {
        assert!(self.final_keywords.insert(id, final_keyword).is_none());
    }

    pub fn insert_implements_qualified_identifier_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        qualified_identifier: pernixc_syntax::QualifiedIdentifier,
    ) {
        assert!(
            self.implements_qualified_identifier_syntaxes
                .insert(id, qualified_identifier)
                .is_none()
        );
    }

    pub fn insert_implements_access_modifier_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        access_modifier: Option<pernixc_syntax::AccessModifier>,
    ) {
        assert!(
            self.implements_access_modifier_syntaxes
                .insert(id, access_modifier)
                .is_none()
        );
    }

    pub fn insert_function_signature_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        parameters: Option<pernixc_syntax::item::function::Parameters>,
        return_type: Option<pernixc_syntax::item::function::ReturnType>,
    ) {
        assert!(
            self.function_signature_syntaxes
                .insert(id, (parameters, return_type))
                .is_none()
        );
    }

    pub fn insert_function_body_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        body: Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    ) {
        assert!(self.function_body_syntaxes.insert(id, body).is_none());
    }

    pub fn insert_function_effect_annotation_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        effect_annotation: Option<
            pernixc_syntax::item::function::EffectAnnotation,
        >,
    ) {
        assert!(
            self.function_effect_annotation_syntaxes
                .insert(id, effect_annotation)
                .is_none()
        );
    }

    pub fn insert_function_unsafe_keyword(
        &self,
        id: pernixc_symbol::SymbolID,
        unsafe_keyword: Option<pernixc_syntax::Keyword>,
    ) {
        assert!(
            self.function_unsafe_keywords.insert(id, unsafe_keyword).is_none()
        );
    }

    pub fn insert_type_alias_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        type_alias: Option<pernixc_syntax::r#type::Type>,
    ) {
        assert!(self.type_alias_syntaxes.insert(id, type_alias).is_none());
    }

    pub fn insert_constant_type_annotation_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        type_annotation: Option<pernixc_syntax::r#type::Type>,
    ) {
        assert!(
            self.constant_type_annotation_syntaxes
                .insert(id, type_annotation)
                .is_none()
        );
    }

    pub fn insert_constant_expression_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        expression: Option<pernixc_syntax::expression::Expression>,
    ) {
        assert!(
            self.constant_expression_syntaxes.insert(id, expression).is_none()
        );
    }

    pub fn insert_struct_field_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        fields: Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    ) {
        assert!(self.fields_syntaxes.insert(id, fields).is_none());
    }

    pub fn insert_variant_declaration_order(
        &self,
        id: pernixc_symbol::SymbolID,
        order: usize,
    ) {
        assert!(self.variant_declaration_orders.insert(id, order).is_none());
    }

    pub fn insert_variant_associated_type_syntax(
        &self,
        id: pernixc_symbol::SymbolID,
        associated_type: Option<pernixc_syntax::r#type::Type>,
    ) {
        assert!(
            self.variant_associated_type_syntaxes
                .insert(id, associated_type)
                .is_none()
        );
    }

    pub fn insert_function_linkage(
        &self,
        id: pernixc_symbol::SymbolID,
        linkage: linkage::Linkage,
    ) {
        assert!(self.function_linkages.insert(id, linkage).is_none());
    }

    pub fn insert_scope_span(
        &self,
        id: pernixc_symbol::SymbolID,
        span: RelativeSpan,
    ) {
        assert!(self.scope_spans.insert(id, Some(span)).is_none());
    }

    pub fn insert_maybe_scope_span(
        &self,
        id: pernixc_symbol::SymbolID,
        span: Option<RelativeSpan>,
    ) {
        assert!(self.scope_spans.insert(id, span).is_none());
    }

    /// Inserts imports into the builder.
    pub fn insert_imports(
        &self,
        id: pernixc_symbol::SymbolID,
        imports: Interned<[pernixc_syntax::item::module::Import]>,
    ) {
        assert!(self.import_syntaxes.insert(id, imports).is_none());
    }

    /// Inserts an external submodule into the builder.
    pub fn insert_external_submodule(
        &self,
        id: pernixc_symbol::SymbolID,
        external_submodule: Interned<ExternalSubmodule>,
    ) {
        assert!(
            self.external_submodules.insert(id, external_submodule).is_none()
        );
    }
}

/// A builder for building [`pernixc_symbol::member::Member`] instances.
pub struct MemberBuilder {
    symbol_id: SymbolID,
    symbol_qualified_name: Arc<[Interned<str>]>,
    target_id: TargetID,

    member_ids_by_name: FxHashMap<Interned<str>, SymbolID>,
    name_occurrences: FxHashMap<Interned<str>, usize>,
    unnameds: FxHashSet<SymbolID>,

    redefinition_errors: FxHashSet<ItemRedefinition>,
}

impl MemberBuilder {
    /// Creates a new [`MemberBuilder`].
    ///
    /// # Parameters
    ///
    /// - `symbol_id`: The symbol ID of the member being built.
    /// - `symbol_qualified_name`: The qualified name of the symbol being built.
    /// - `target_id`: The target ID of the member being built.
    pub fn new(
        symbol_id: SymbolID,
        symbol_qualified_name: Arc<[Interned<str>]>,
        target_id: TargetID,
    ) -> Self {
        assert!(!symbol_qualified_name.is_empty());

        Self {
            symbol_id,
            symbol_qualified_name,
            target_id,

            member_ids_by_name: FxHashMap::default(),
            name_occurrences: FxHashMap::default(),
            unnameds: FxHashSet::default(),

            redefinition_errors: FxHashSet::default(),
        }
    }

    /// Explicitly inserts an unnamed member ID.
    pub fn insert_unnameds(&mut self, id: SymbolID) {
        assert!(self.unnameds.insert(id));
    }

    /// Retrieves the symbol ID of the member being built.
    #[must_use]
    pub const fn current_symbol_id(&self) -> SymbolID { self.symbol_id }

    /// Retrieves the last name in the qualified name.
    pub fn last_name(&self) -> &Interned<str> {
        self.symbol_qualified_name
            .last()
            .expect("symbol qualified name is empty")
    }

    /// Extends the qualified name sequence with a new name.
    pub fn extend_qualified_name_sequence(
        &mut self,
        name: Interned<str>,
    ) -> Arc<[Interned<str>]> {
        self.symbol_qualified_name
            .iter()
            .cloned()
            .chain(std::iter::once(name))
            .collect::<Arc<[_]>>()
    }

    /// Adds a member to the member builder
    pub async fn add_member(
        &mut self,
        identifier: pernixc_syntax::Identifier,
        engine: &TrackedEngine,
    ) -> SymbolID {
        let occurrences =
            self.name_occurrences.entry(identifier.kind.0.clone()).or_default();

        let current_count = *occurrences;
        *occurrences += 1;

        let new_member_id = engine
            .calculate_qualified_name_id(
                self.symbol_qualified_name
                    .iter()
                    .map(AsRef::as_ref)
                    .chain(std::iter::once(identifier.kind.0.as_ref())),
                self.target_id,
                Some(self.symbol_id),
                current_count,
            )
            .await;

        match self.member_ids_by_name.entry(identifier.kind.0) {
            hash_map::Entry::Occupied(occupied_entry) => {
                self.redefinition_errors.insert(ItemRedefinition {
                    existing_id: self
                        .target_id
                        .make_global(*occupied_entry.get()),
                    redefinition_span: identifier.span,
                    in_id: self.target_id.make_global(self.symbol_id),
                });

                self.unnameds.insert(new_member_id);
            }
            hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(new_member_id);
            }
        }

        new_member_id
    }

    /// Adds a member to the member builder without adding it to the `unnameds`
    /// set in case of redefinition.
    pub async fn add_member_no_unnamed(
        &mut self,
        identifier: pernixc_syntax::Identifier,
        engine: &TrackedEngine,
    ) -> Option<pernixc_symbol::SymbolID> {
        let occurrences =
            self.name_occurrences.entry(identifier.kind.0.clone()).or_default();

        let current_count = *occurrences;
        *occurrences += 1;

        let new_member_id = engine
            .calculate_qualified_name_id(
                self.symbol_qualified_name
                    .iter()
                    .map(AsRef::as_ref)
                    .chain(std::iter::once(identifier.kind.0.as_ref())),
                self.target_id,
                Some(self.symbol_id),
                current_count,
            )
            .await;

        match self.member_ids_by_name.entry(identifier.kind.0) {
            hash_map::Entry::Occupied(occupied_entry) => {
                self.redefinition_errors.insert(ItemRedefinition {
                    existing_id: self
                        .target_id
                        .make_global(*occupied_entry.get()),
                    redefinition_span: identifier.span,
                    in_id: self.target_id.make_global(self.symbol_id),
                });

                None
            }
            hash_map::Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(new_member_id);

                Some(new_member_id)
            }
        }
    }
}
