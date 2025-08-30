use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_symbol::accessibility::Accessibility;
use pernixc_target::TargetID;
use pernixc_term::fields::Field;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Identifiable,
    StableHash,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(clippy::large_enum_variant)]
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    TypeSystem(pernixc_type_system::diagnostic::Diagnostic),
    FieldRedefinition(FieldRedefinition),
    FieldMoreAccessibleThanStruct(FieldMoreAccessibleThanStruct),
}

impl pernixc_diagnostic::Report<&TrackedEngine> for Diagnostic {
    type Location = ByteIndex;

    async fn report(
        &self,
        parameter: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        match self {
            Self::Resolution(diagnostic) => diagnostic.report(parameter).await,
            Self::TypeSystem(diagnostic) => diagnostic.report(parameter).await,
            Self::FieldRedefinition(diagnostic) => {
                diagnostic.report(parameter).await
            }
            Self::FieldMoreAccessibleThanStruct(diagnostic) => {
                diagnostic.report(parameter).await
            }
        }
    }
}

/// A field with the same name is already defined in the struct.
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
pub struct FieldRedefinition {
    /// The name of the field that is being redefined.
    pub field_name: String,

    /// The span of the redefinition.
    pub redefinition_span: RelativeSpan,

    /// The span of the original definition.
    pub original_span: Option<RelativeSpan>,

    /// The ID of the struct containing the field.
    pub struct_id: pernixc_symbol::ID,

    /// The target ID.
    pub target_id: TargetID,
}

impl pernixc_diagnostic::Report<&TrackedEngine> for FieldRedefinition {
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<ByteIndex> {
        use pernixc_diagnostic::{Highlight, Severity};
        use pernixc_symbol::{
            name::get_qualified_name, source_map::to_absolute_span,
        };

        let struct_name = engine
            .get_qualified_name(self.target_id.make_global(self.struct_id))
            .await;

        let primary_highlight = Some(Highlight::new(
            engine.to_absolute_span(&self.redefinition_span).await,
            Some(format!("field `{}` redefined here", self.field_name)),
        ));

        let related = if let Some(original_span) = &self.original_span {
            vec![Highlight::new(
                engine.to_absolute_span(original_span).await,
                Some("previous definition here".to_string()),
            )]
        } else {
            Vec::new()
        };

        pernixc_diagnostic::Diagnostic {
            primary_highlight,
            message: format!(
                "field `{}` is already defined in struct `{}`",
                self.field_name, struct_name
            ),
            severity: Severity::Error,
            help_message: Some(
                "consider using a different field name".to_string(),
            ),
            related,
        }
    }
}

/// A field is more accessible than the struct containing it.
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
pub struct FieldMoreAccessibleThanStruct {
    /// The field that is more accessible.
    pub field: Field,

    /// The accessibility of the struct.
    pub struct_accessibility: Accessibility<pernixc_symbol::ID>,

    /// The ID of the struct.
    pub struct_id: pernixc_symbol::ID,

    /// The target ID.
    pub target_id: TargetID,
}

impl pernixc_diagnostic::Report<&TrackedEngine>
    for FieldMoreAccessibleThanStruct
{
    type Location = ByteIndex;

    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<ByteIndex> {
        use pernixc_diagnostic::{Highlight, Severity};
        use pernixc_symbol::{
            accessibility::accessibility_description, name::get_qualified_name,
            source_map::to_absolute_span,
        };

        let struct_name = engine
            .get_qualified_name(self.target_id.make_global(self.struct_id))
            .await;

        let field_accessibility_description = engine
            .accessibility_description(
                self.field.accessibility.into_global(self.target_id),
            )
            .await;

        let struct_accessibility_description = engine
            .accessibility_description(
                self.struct_accessibility.into_global(self.target_id),
            )
            .await;

        let primary_highlight = if let Some(span) = &self.field.span {
            Some(Highlight::new(
                engine.to_absolute_span(span).await,
                Some(format!(
                    "field `{}` is {} but struct is {}",
                    self.field.name,
                    field_accessibility_description,
                    struct_accessibility_description
                )),
            ))
        } else {
            None
        };

        pernixc_diagnostic::Diagnostic {
            primary_highlight,
            message: format!(
                "field `{}` is more accessible than struct `{}`",
                self.field.name, struct_name
            ),
            severity: Severity::Error,
            help_message: Some(format!(
                "consider making the field {} or making the struct {}",
                struct_accessibility_description,
                field_accessibility_description
            )),
            related: Vec::new(),
        }
    }
}
