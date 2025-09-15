use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_hash::HashSet;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::fields::{get_fields, Field};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    accessibility::accessibility_description, kind::get_kind,
    name::get_qualified_name, source_map::to_absolute_span,
};
use pernixc_target::Global;

use crate::diagnostic_enum;

diagnostic_enum! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        StableHash,
        Serialize,
        Deserialize,
    )]
    pub enum Diagnostic {
        ExpectedStructSymbol(ExpectedStructSymbol),
        FieldNotFound(FieldNotFound),
        FieldIsNotAccessible(FieldIsNotAccessible),
        DuplicatedFieldInitialization(DuplicatedFieldInitialization),
        UninitializedFields(UninitializedFields),
    }
}

/// The struct type was expected but the found type is different.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ExpectedStructSymbol {
    /// The span of the qualified identifier.
    pub span: RelativeSpan,

    /// The resolved symbol that wasn't the struct type.
    pub id: Global<pernixc_symbol::ID>,
}

impl Report for ExpectedStructSymbol {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;
        let qualified_name = engine.get_qualified_name(self.id).await;
        let kind = engine.get_kind(self.id).await;

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message("expected struct symbol for struct expression")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "expected struct type, found {} '{qualified_name}'",
                        kind.kind_str()
                    ))
                    .span(span)
                    .build(),
            )
            .build())
    }
}

/// Field with given name was not found in the struct.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct FieldNotFound {
    /// The span to the identifier that refers to non-existent field.
    pub identifier_span: RelativeSpan,

    /// The struct ID where the field is not found.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The name of the field that was not found.
    pub field_name: SharedStr,
}

impl Report for FieldNotFound {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let identifier_span =
            engine.to_absolute_span(&self.identifier_span).await;
        let struct_name = engine.get_qualified_name(self.struct_id).await;

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message("field not found in struct")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "field '{field_name}' not found in struct \
                         '{struct_name}'",
                        field_name = self.field_name
                    ))
                    .span(identifier_span)
                    .build(),
            )
            .build())
    }
}

/// The field is not accessible in the given scope.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct FieldIsNotAccessible {
    /// The ID of the field that is not accessible.
    pub field_id: ID<Field>,

    /// The struct in which the field is not accessible.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The ID to the scope where the field is not accessible.
    pub referring_site: Global<pernixc_symbol::ID>,

    /// The span where the field is referred from.
    pub referring_identifier_span: RelativeSpan,
}

impl Report for FieldIsNotAccessible {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let identifier_span =
            engine.to_absolute_span(&self.referring_identifier_span).await;
        let struct_name = engine.get_qualified_name(self.struct_id).await;
        let referring_site_name =
            engine.get_qualified_name(self.referring_site).await;

        // Get the fields to access the field name
        let fields = engine.get_fields(self.struct_id).await.unwrap();
        let field = &fields.fields[self.field_id];

        // Get the field's accessibility description for the help message
        let field_accessibility_description = engine
            .accessibility_description(
                field.accessibility.into_global(self.struct_id.target_id),
            )
            .await;

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message("field is not accessible")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "field '{}' of struct '{}' is not accessible from '{}'",
                        field.name, struct_name, referring_site_name
                    ))
                    .span(identifier_span)
                    .build(),
            )
            .help_message(format!(
                "field '{}' is {}",
                field.name, field_accessibility_description
            ))
            .build())
    }
}

/// The field is initialized more than once.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct DuplicatedFieldInitialization {
    /// The ID of the field that is initialized more than once.
    pub field_id: ID<Field>,

    /// The ID of the struct where the field is initialized.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The span of the first initialization.
    pub prior_initialization_span: RelativeSpan,

    /// The span of the duplicate initialization.
    pub duplicate_initialization_span: RelativeSpan,
}

impl Report for DuplicatedFieldInitialization {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let duplicate_span =
            engine.to_absolute_span(&self.duplicate_initialization_span).await;
        let prior_span =
            engine.to_absolute_span(&self.prior_initialization_span).await;
        let struct_name = engine.get_qualified_name(self.struct_id).await;

        // Get the field name
        let fields = engine.get_fields(self.struct_id).await.unwrap();
        let field = &fields.fields[self.field_id];

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message("field is initialized multiple times")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "field '{}' of struct '{}' is initialized here again",
                        field.name, struct_name
                    ))
                    .span(duplicate_span)
                    .build(),
            )
            .related(vec![Highlight::builder()
                .message("field was first initialized here".to_string())
                .span(prior_span)
                .build()])
            .build())
    }
}

/// The struct expression contains uninitialized fields.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct UninitializedFields {
    /// The ID of the struct where the fields are uninitialized.
    pub struct_id: Global<pernixc_symbol::ID>,

    /// The set of uninitialized fields.
    pub uninitialized_fields: HashSet<ID<Field>>,

    /// The span of the struct expression.
    pub struct_expression_span: RelativeSpan,
}

impl Report for UninitializedFields {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let struct_span =
            engine.to_absolute_span(&self.struct_expression_span).await;
        let struct_name = engine.get_qualified_name(self.struct_id).await;

        // Get the field names
        let fields = engine.get_fields(self.struct_id).await.unwrap();
        let uninitialized_field_names: Vec<String> = self
            .uninitialized_fields
            .iter()
            .map(|field_id| fields.fields[*field_id].name.to_string())
            .collect();

        let field_list = if uninitialized_field_names.len() == 1 {
            format!("field '{}'", uninitialized_field_names[0])
        } else if uninitialized_field_names.len() == 2 {
            format!(
                "fields '{}' and '{}'",
                uninitialized_field_names[0], uninitialized_field_names[1]
            )
        } else {
            let (last, rest) = uninitialized_field_names.split_last().unwrap();
            format!(
                "fields {} and '{}'",
                rest.iter()
                    .map(|name| format!("'{name}'"))
                    .collect::<Vec<_>>()
                    .join(", "),
                last
            )
        };

        let verb =
            if uninitialized_field_names.len() == 1 { "is" } else { "are" };

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message("struct has uninitialized fields")
            .primary_highlight(
                Highlight::builder()
                    .message(format!(
                        "{field_list} of struct '{struct_name}' {verb} not \
                         initialized"
                    ))
                    .span(struct_span)
                    .build(),
            )
            .build())
    }
}
