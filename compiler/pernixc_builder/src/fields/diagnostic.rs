//! Contains the diagnostics related to building the fields of a struct.

use pernixc_arena::ID;
use pernixc_component::fields::{Field, Fields};
use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{diagnostic::ReportError, GlobalID, Table};

/// The field with the same name already exists in the struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldDuplication {
    /// The struct ID where the field is duplicated.
    pub struct_id: GlobalID,

    /// The ID of the existing field.
    pub field_id: ID<Field>,

    /// The span of the redeclaration.
    pub redeclaration_span: Span,
}

impl Report<&Table> for FieldDuplication {
    type Error = ReportError;

    fn report(&self, table: &Table) -> Result<Diagnostic, Self::Error> {
        let struct_qualified_name =
            table.get_qualified_name(self.struct_id).ok_or(ReportError)?;
        let fields =
            table.query::<Fields>(self.struct_id).map_err(|_| ReportError)?;
        let field_sym = fields.fields.get(self.field_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.redeclaration_span.clone(),
            message: format!(
                "the field `{}` is already defined in the struct \
                 `{struct_qualified_name}`",
                field_sym.name
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: field_sym.span.clone().ok_or(ReportError)?,
                message: "is first defined here".to_string(),
            }],
        })
    }
}
