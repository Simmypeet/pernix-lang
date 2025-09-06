use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
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
        PartialOrd,
        Ord,
        Hash,
        StableHash,
        Serialize,
        Deserialize,
    )]
    pub enum Diagnostic {
        ExpectedStructSymbol(ExpectedStructSymbol),
        FieldNotFound(FieldNotFound),
        FieldIsNotAccessible(FieldIsNotAccessible),
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

impl Report<&TrackedEngine> for ExpectedStructSymbol {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let span = engine.to_absolute_span(&self.span).await;
        let qualified_name = engine.get_qualified_name(self.id).await;
        let kind = engine.get_kind(self.id).await;

        pernixc_diagnostic::Diagnostic::builder()
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
            .build()
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

impl Report<&TrackedEngine> for FieldNotFound {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let identifier_span =
            engine.to_absolute_span(&self.identifier_span).await;
        let struct_name = engine.get_qualified_name(self.struct_id).await;

        pernixc_diagnostic::Diagnostic::builder()
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
            .build()
    }
}

/// The field is not accessible in the given scope.
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

impl Report<&TrackedEngine> for FieldIsNotAccessible {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
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

        pernixc_diagnostic::Diagnostic::builder()
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
            .build()
    }
}
