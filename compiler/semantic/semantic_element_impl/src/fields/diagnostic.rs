use flexstr::SharedStr;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_symbol::accessibility::Accessibility;

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
    pub field_name: SharedStr,

    /// The span of the redefinition.
    pub redefinition_span: RelativeSpan,

    /// The span of the original definition.
    pub original_span: Option<RelativeSpan>,

    /// The ID of the struct containing the field.
    pub struct_id: pernixc_target::Global<pernixc_symbol::ID>,
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

        let struct_name = engine.get_qualified_name(self.struct_id).await;

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
    /// The span to the field that is more accessible.
    pub field_span: RelativeSpan,

    /// The accessibility of the field.
    pub field_accessibility: Accessibility<pernixc_symbol::ID>,

    /// The name of the field.
    pub field_name: SharedStr,

    /// The accessibility of the struct.
    pub struct_accessibility: Accessibility<pernixc_symbol::ID>,

    /// The ID of the struct.
    pub struct_id: pernixc_target::Global<pernixc_symbol::ID>,
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

        let struct_name = engine.get_qualified_name(self.struct_id).await;

        let field_accessibility_description = engine
            .accessibility_description(
                self.field_accessibility.into_global(self.struct_id.target_id),
            )
            .await;

        let struct_accessibility_description = engine
            .accessibility_description(
                self.struct_accessibility.into_global(self.struct_id.target_id),
            )
            .await;

        let primary_highlight = {
            Some(Highlight::new(
                engine.to_absolute_span(&self.field_span).await,
                Some(format!(
                    "field `{}` is {} but struct is {}",
                    self.field_name,
                    field_accessibility_description,
                    struct_accessibility_description
                )),
            ))
        };

        pernixc_diagnostic::Diagnostic {
            primary_highlight,
            message: format!(
                "field `{}` is more accessible than struct `{}`",
                self.field_name, struct_name
            ),
            severity: Severity::Error,
            help_message: Some(format!(
                "consider making the field {struct_accessibility_description} \
                 or making the struct {field_accessibility_description}"
            )),
            related: Vec::new(),
        }
    }
}
