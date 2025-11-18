use std::{collections::hash_map, sync::Arc};

use bon::Builder;
use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_handler::Storage;
use pernixc_hash::{DashMap, HashMap, HashSet};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_source_file::SourceFile;
use pernixc_symbol::{
    accessibility::Accessibility, calculate_qualified_name_id,
    get_target_root_module_id, kind::Kind, linkage, member::Member, ID,
};
use pernixc_syntax::AccessModifier;
use pernixc_target::TargetID;

use crate::{
    diagnostic::{Diagnostic, ItemRedefinition},
    table::{ExternalSubmodule, Table},
};

/// A builder struct for constructing the final [`super::Table`].
pub struct Builder {
    engine: TrackedEngine,
    storage: Storage<Diagnostic>,
    target_id: TargetID,

    kinds: DashMap<ID, Kind>,
    names: DashMap<ID, SharedStr>,
    spans: DashMap<ID, Option<RelativeSpan>>,
    members: DashMap<ID, Arc<Member>>,
    accessibilities: DashMap<ID, Accessibility<ID>>,

    implements_access_modifier_syntaxes:
        DashMap<ID, Option<pernixc_syntax::AccessModifier>>,

    external_submodules: DashMap<ID, Arc<ExternalSubmodule>>,

    generic_parameter_syntaxes: DashMap<
        ID,
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,
    where_clause_syntaxes:
        DashMap<ID, Option<pernixc_syntax::item::where_clause::Predicates>>,
    type_alias_syntaxes: DashMap<ID, Option<pernixc_syntax::r#type::Type>>,
    constant_type_annotation_syntaxes:
        DashMap<ID, Option<pernixc_syntax::r#type::Type>>,
    constant_expression_syntaxes:
        DashMap<ID, Option<pernixc_syntax::expression::Expression>>,
    function_signature_syntaxes: DashMap<
        ID,
        (
            Option<pernixc_syntax::item::function::Parameters>,
            Option<pernixc_syntax::item::function::ReturnType>,
        ),
    >,
    function_linkages: DashMap<ID, linkage::Linkage>,
    fields_syntaxes: DashMap<
        ID,
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,
    variant_associated_type_syntaxes:
        DashMap<ID, Option<pernixc_syntax::r#type::Type>>,
    variant_declaration_orders: DashMap<ID, usize>,
    import_syntaxes: DashMap<ID, Arc<[pernixc_syntax::item::module::Import]>>,
    implements_qualified_identifier_syntaxes:
        DashMap<ID, pernixc_syntax::QualifiedIdentifier>,
    final_keywords: DashMap<ID, Option<pernixc_syntax::Keyword>>,

    function_body_syntaxes: DashMap<
        ID,
        Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    >,

    function_effect_annotation_syntaxes:
        DashMap<ID, Option<pernixc_syntax::item::function::EffectAnnotation>>,

    function_unsafe_keywords: DashMap<ID, Option<pernixc_syntax::Keyword>>,

    token_tree: Option<Arc<pernixc_lexical::tree::Tree>>,
    source_file: Option<Arc<SourceFile>>,

    is_root: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Naming {
    Identifier(pernixc_syntax::Identifier),
    Implements(pernixc_syntax::QualifiedIdentifier),
}

#[derive(Debug, Clone, PartialEq, Eq, Builder)]
#[allow(clippy::option_option)]
pub struct Entry {
    pub naming: Naming,
    pub kind: Kind,

    pub member: Option<Arc<Member>>,

    pub accessibility: Option<Option<pernixc_syntax::AccessModifier>>,

    pub generic_parameters_syntax: Option<
        Option<pernixc_syntax::item::generic_parameters::GenericParameters>,
    >,

    pub where_clause_syntax:
        Option<Option<pernixc_syntax::item::where_clause::Predicates>>,

    pub type_alias_syntax: Option<Option<pernixc_syntax::r#type::Type>>,

    pub constant_type_annotation_syntax:
        Option<Option<pernixc_syntax::r#type::Type>>,

    pub constant_expression_syntax:
        Option<Option<pernixc_syntax::expression::Expression>>,

    pub function_signature_syntax: Option<(
        Option<pernixc_syntax::item::function::Parameters>,
        Option<pernixc_syntax::item::function::ReturnType>,
    )>,

    pub fields_syntax: Option<
        Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    >,

    pub variant_associated_type_syntax:
        Option<Option<pernixc_syntax::r#type::Type>>,

    pub variant_declaration_order: Option<usize>,

    pub function_body_syntax: Option<
        Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    >,

    pub final_keyword: Option<Option<pernixc_syntax::Keyword>>,

    pub function_effect_annotation_syntax:
        Option<Option<pernixc_syntax::item::function::EffectAnnotation>>,

    pub function_unsafe_keyword: Option<Option<pernixc_syntax::Keyword>>,

    pub function_linkage: Option<linkage::Linkage>,
}

impl Builder {
    /// Creates a new [`Builder`].
    pub fn new(
        engine: TrackedEngine,
        storage: Storage<Diagnostic>,
        source_file: Option<Arc<SourceFile>>,
        token_tree: Option<Arc<pernixc_lexical::tree::Tree>>,
        target_id: TargetID,
        is_root: bool,
    ) -> Self {
        Self {
            engine,
            storage,
            target_id,
            kinds: DashMap::default(),
            names: DashMap::default(),
            spans: DashMap::default(),
            members: DashMap::default(),
            accessibilities: DashMap::default(),
            implements_access_modifier_syntaxes: DashMap::default(),
            external_submodules: DashMap::default(),
            generic_parameter_syntaxes: DashMap::default(),
            where_clause_syntaxes: DashMap::default(),
            type_alias_syntaxes: DashMap::default(),
            constant_type_annotation_syntaxes: DashMap::default(),
            constant_expression_syntaxes: DashMap::default(),
            function_signature_syntaxes: DashMap::default(),
            function_linkages: DashMap::default(),
            fields_syntaxes: DashMap::default(),
            variant_associated_type_syntaxes: DashMap::default(),
            variant_declaration_orders: DashMap::default(),
            import_syntaxes: DashMap::default(),
            implements_qualified_identifier_syntaxes: DashMap::default(),
            final_keywords: DashMap::default(),
            function_body_syntaxes: DashMap::default(),
            function_effect_annotation_syntaxes: DashMap::default(),
            function_unsafe_keywords: DashMap::default(),
            token_tree,
            source_file,
            is_root,
        }
    }

    /// Retrieves the tracked engine.
    #[must_use]
    pub fn engine(&self) -> &TrackedEngine { &self.engine }

    /// Retrieves the target ID.
    #[must_use]
    pub fn target_id(&self) -> TargetID { self.target_id }

    /// Finiailizes the builder into an immutable [`super::Table`].
    #[must_use]
    pub fn into_table(self) -> Arc<super::Table> {
        Arc::new(Table {
            kinds: Arc::new(self.kinds.into_read_only()),
            names: Arc::new(self.names.into_read_only()),
            spans: Arc::new(self.spans.into_read_only()),
            members: Arc::new(self.members.into_read_only()),
            accessibilities: Arc::new(self.accessibilities.into_read_only()),
            implements_access_modifier_syntaxes: Arc::new(
                self.implements_access_modifier_syntaxes.into_read_only(),
            ),

            // syntax extractions
            generic_parameter_syntaxes: Arc::new(
                self.generic_parameter_syntaxes.into_read_only(),
            ),
            where_clause_syntaxes: Arc::new(
                self.where_clause_syntaxes.into_read_only(),
            ),
            type_alias_syntaxes: Arc::new(
                self.type_alias_syntaxes.into_read_only(),
            ),
            constant_type_annotation_syntaxes: Arc::new(
                self.constant_type_annotation_syntaxes.into_read_only(),
            ),
            constant_expression_syntaxes: Arc::new(
                self.constant_expression_syntaxes.into_read_only(),
            ),
            function_signature_syntaxes: Arc::new(
                self.function_signature_syntaxes.into_read_only(),
            ),
            fields_syntaxes: Arc::new(self.fields_syntaxes.into_read_only()),
            variant_associated_type_syntaxes: Arc::new(
                self.variant_associated_type_syntaxes.into_read_only(),
            ),
            variant_declaration_orders: Arc::new(
                self.variant_declaration_orders.into_read_only(),
            ),
            import_syntaxes: Arc::new(self.import_syntaxes.into_read_only()),
            implements_qualified_identifier_syntaxes: Arc::new(
                self.implements_qualified_identifier_syntaxes.into_read_only(),
            ),
            final_keywords: Arc::new(self.final_keywords.into_read_only()),
            function_body_syntaxes: Arc::new(
                self.function_body_syntaxes.into_read_only(),
            ),
            function_linkages: Arc::new(
                self.function_linkages.into_read_only(),
            ),
            function_effect_annotation_syntaxes: Arc::new(
                self.function_effect_annotation_syntaxes.into_read_only(),
            ),
            function_unsafe_keywords: Arc::new(
                self.function_unsafe_keywords.into_read_only(),
            ),

            external_submodules: Arc::new(
                self.external_submodules.into_read_only(),
            ),
            diagnostics: Arc::new(
                self.storage.into_vec().into_iter().collect(),
            ),
        })
    }

    /// Creates an accessibility from an access modifier.
    pub async fn create_accessibility(
        &self,
        current_module_scope: ID,
        access_modifier: Option<&AccessModifier>,
    ) -> Accessibility<ID> {
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
    pub fn is_root(&self) -> bool { self.is_root }
}

impl Builder {
    /// Inserts a kind into the builder.
    pub fn insert_kind(&self, id: pernixc_symbol::ID, kind: Kind) {
        assert!(self.kinds.insert(id, kind).is_none());
    }

    /// Inserts a member into the builder from the [`MemberBuilder`].
    pub fn insert_member(&self, id: pernixc_symbol::ID, member: MemberBuilder) {
        assert!(self
            .members
            .insert(
                id,
                Arc::new(Member {
                    member_ids_by_name: member.member_ids_by_name,
                    unnameds: member.unnameds,
                }),
            )
            .is_none());

        self.storage.as_vec_mut().extend(
            member
                .redefinition_errors
                .into_iter()
                .map(Diagnostic::ItemRedefinition),
        );
    }

    /// Inserts a name into the builder.
    pub fn insert_name(&self, id: pernixc_symbol::ID, name: SharedStr) {
        assert!(self.names.insert(id, name).is_none());
    }

    /// Inserts an identifier used as a name and span into the builder.
    pub fn insert_name_identifier(
        &self,
        id: pernixc_symbol::ID,
        identifier: &pernixc_syntax::Identifier,
    ) {
        assert!(self.names.insert(id, identifier.kind.0.clone()).is_none());
        assert!(self.spans.insert(id, Some(identifier.span)).is_none());
    }

    /// Inserts a span into the builder.
    pub fn insert_span(
        &self,
        id: pernixc_symbol::ID,
        span: Option<RelativeSpan>,
    ) {
        assert!(self.spans.insert(id, span).is_none());
    }

    /// Inserts an accessibility into the builder.
    pub fn insert_accessibility(
        &self,
        id: pernixc_symbol::ID,
        accessibility: Accessibility<pernixc_symbol::ID>,
    ) {
        assert!(self.accessibilities.insert(id, accessibility).is_none());
    }

    /// Inserts an accessibility into the builder from an access modifier.
    pub async fn insert_accessibility_by_access_modifier(
        &self,
        id: pernixc_symbol::ID,
        current_module_scope_id: pernixc_symbol::ID,
        access_modifier: Option<&pernixc_syntax::AccessModifier>,
    ) {
        let accessibility = self
            .create_accessibility(current_module_scope_id, access_modifier)
            .await;

        assert!(self.accessibilities.insert(id, accessibility).is_none());
    }

    pub fn insert_generic_parameters_syntax(
        &self,
        id: pernixc_symbol::ID,
        generic_parameters: Option<
            pernixc_syntax::item::generic_parameters::GenericParameters,
        >,
    ) {
        assert!(self
            .generic_parameter_syntaxes
            .insert(id, generic_parameters)
            .is_none());
    }

    pub fn insert_where_clause_syntax(
        &self,
        id: pernixc_symbol::ID,
        where_clause: Option<pernixc_syntax::item::where_clause::Predicates>,
    ) {
        assert!(self.where_clause_syntaxes.insert(id, where_clause).is_none());
    }

    pub fn insert_function_signature_syntax(
        &self,
        id: pernixc_symbol::ID,
        parameters: Option<pernixc_syntax::item::function::Parameters>,
        return_type: Option<pernixc_syntax::item::function::ReturnType>,
    ) {
        assert!(self
            .function_signature_syntaxes
            .insert(id, (parameters, return_type))
            .is_none());
    }

    pub fn insert_function_body_syntax(
        &self,
        id: pernixc_symbol::ID,
        body: Option<
            pernixc_syntax::item::Members<pernixc_syntax::statement::Statement>,
        >,
    ) {
        assert!(self.function_body_syntaxes.insert(id, body).is_none());
    }

    pub fn insert_function_effect_annotation_syntax(
        &self,
        id: pernixc_symbol::ID,
        effect_annotation: Option<
            pernixc_syntax::item::function::EffectAnnotation,
        >,
    ) {
        assert!(self
            .function_effect_annotation_syntaxes
            .insert(id, effect_annotation)
            .is_none());
    }

    pub fn insert_function_unsafe_keyword(
        &self,
        id: pernixc_symbol::ID,
        unsafe_keyword: Option<pernixc_syntax::Keyword>,
    ) {
        assert!(self
            .function_unsafe_keywords
            .insert(id, unsafe_keyword)
            .is_none());
    }

    pub fn insert_type_alias_syntax(
        &self,
        id: pernixc_symbol::ID,
        type_alias: Option<pernixc_syntax::r#type::Type>,
    ) {
        assert!(self.type_alias_syntaxes.insert(id, type_alias).is_none());
    }

    pub fn insert_constant_type_annotation_syntax(
        &self,
        id: pernixc_symbol::ID,
        type_annotation: Option<pernixc_syntax::r#type::Type>,
    ) {
        assert!(self
            .constant_type_annotation_syntaxes
            .insert(id, type_annotation)
            .is_none());
    }

    pub fn insert_constant_expression_syntax(
        &self,
        id: pernixc_symbol::ID,
        expression: Option<pernixc_syntax::expression::Expression>,
    ) {
        assert!(self
            .constant_expression_syntaxes
            .insert(id, expression)
            .is_none());
    }

    pub fn insert_struct_field_syntax(
        &self,
        id: pernixc_symbol::ID,
        fields: Option<
            pernixc_syntax::item::Body<pernixc_syntax::item::r#struct::Field>,
        >,
    ) {
        assert!(self.fields_syntaxes.insert(id, fields).is_none());
    }

    pub fn insert_variant_declaration_order(
        &self,
        id: pernixc_symbol::ID,
        order: usize,
    ) {
        assert!(self.variant_declaration_orders.insert(id, order).is_none());
    }

    pub fn insert_variant_associated_type_syntax(
        &self,
        id: pernixc_symbol::ID,
        associated_type: Option<pernixc_syntax::r#type::Type>,
    ) {
        assert!(self
            .variant_associated_type_syntaxes
            .insert(id, associated_type)
            .is_none());
    }

    pub fn insert_function_linkage(
        &self,
        id: pernixc_symbol::ID,
        linkage: linkage::Linkage,
    ) {
        assert!(self.function_linkages.insert(id, linkage).is_none());
    }

    /// Inserts imports into the builder.
    pub fn insert_imports(
        &self,
        id: pernixc_symbol::ID,
        imports: Arc<[pernixc_syntax::item::module::Import]>,
    ) {
        assert!(self.import_syntaxes.insert(id, imports).is_none());
    }

    /// Inserts an external submodule into the builder.
    pub fn insert_external_submodule(
        &self,
        id: pernixc_symbol::ID,
        external_submodule: Arc<ExternalSubmodule>,
    ) {
        assert!(self
            .external_submodules
            .insert(id, external_submodule)
            .is_none());
    }
}

/// A builder for building [`pernixc_symbol::member::Member`] instances.
pub struct MemberBuilder {
    symbol_id: ID,
    symbol_qualified_name: Arc<[SharedStr]>,
    target_id: TargetID,

    member_ids_by_name: HashMap<SharedStr, ID>,
    name_occurrences: HashMap<SharedStr, usize>,
    unnameds: HashSet<ID>,

    redefinition_errors: HashSet<ItemRedefinition>,
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
        symbol_id: ID,
        symbol_qualified_name: Arc<[SharedStr]>,
        target_id: TargetID,
    ) -> Self {
        assert!(!symbol_qualified_name.is_empty());

        Self {
            symbol_id,
            symbol_qualified_name,
            target_id,

            member_ids_by_name: HashMap::default(),
            name_occurrences: HashMap::default(),
            unnameds: HashSet::default(),

            redefinition_errors: HashSet::default(),
        }
    }

    /// Retrieves the symbol ID of the member being built.
    #[must_use]
    pub fn current_symbol_id(&self) -> ID { self.symbol_id }

    /// Retrieves the last name in the qualified name.
    pub fn last_name(&self) -> &SharedStr {
        self.symbol_qualified_name
            .last()
            .expect("symbol qualified name is empty")
    }

    /// Extends the qualified name sequence with a new name.
    pub fn extend_qualified_name_sequence(
        &mut self,
        name: SharedStr,
    ) -> Arc<[SharedStr]> {
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
    ) -> ID {
        let occurrences =
            self.name_occurrences.entry(identifier.kind.0.clone()).or_default();

        let current_count = *occurrences;
        *occurrences += 1;

        let new_member_id = engine
            .calculate_qualified_name_id(
                self.symbol_qualified_name
                    .iter()
                    .map(flexstr::FlexStr::as_str)
                    .chain(std::iter::once(identifier.kind.0.as_str())),
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
    ) -> Option<pernixc_symbol::ID> {
        let occurrences =
            self.name_occurrences.entry(identifier.kind.0.clone()).or_default();

        let current_count = *occurrences;
        *occurrences += 1;

        let new_member_id = engine
            .calculate_qualified_name_id(
                self.symbol_qualified_name
                    .iter()
                    .map(flexstr::FlexStr::as_str)
                    .chain(std::iter::once(identifier.kind.0.as_str())),
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

    /// Retrieves a member ID by name.
    #[must_use]
    pub fn get_member<Q: std::hash::Hash + std::cmp::Eq>(
        &self,
        name: &Q,
    ) -> Option<&ID>
    where
        SharedStr: std::borrow::Borrow<Q>,
    {
        self.member_ids_by_name.get(name)
    }
}
