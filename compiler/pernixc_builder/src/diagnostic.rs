//! Contains the general diagnostic information related to the symbol building.

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{
    component::Accessibility, diagnostic::ReportError, DisplayObject,
    GlobalAccessibility, GlobalID, Table,
};

/// An entity was exposed to the public interface but it's accessability is less
/// permissive than the public interface.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PrivateEntityLeakedToPublicInterface<T> {
    /// The entity that was leaked.
    pub entity: T,

    /// The overall accessibility of the entity.
    pub entity_overall_accessibility: GlobalAccessibility,

    /// The span where the entity was leaked.
    pub leaked_span: Span,

    /// The ID of the public interface that contains the leaked entity.
    pub public_interface_id: GlobalID,
}

fn accessibility_description(
    table: &Table,
    accessibility: GlobalAccessibility,
) -> Result<String, ReportError> {
    match accessibility {
        GlobalAccessibility::Public => Ok("publicly accessible".to_owned()),
        GlobalAccessibility::Scoped(module_id) => {
            let module_qualified_name =
                table.get_qualified_name(module_id).ok_or(ReportError)?;

            Ok(format!("accessible in `{module_qualified_name}`"))
        }
    }
}

impl<T: pernixc_table::Display> Report<&Table>
    for PrivateEntityLeakedToPublicInterface<T>
{
    type Error = ReportError;

    fn report(&self, table: &Table) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.leaked_span.clone(),
            message: format!(
                "`{}` is {} but it was declared in a `{}` interface, which is \
                 {}",
                DisplayObject { display: &self.entity, table },
                accessibility_description(
                    table,
                    self.entity_overall_accessibility
                )?,
                table
                    .get_qualified_name(self.public_interface_id)
                    .ok_or(ReportError)?,
                accessibility_description(
                    table,
                    match table
                        .get_accessibility(self.public_interface_id)
                        .ok_or(ReportError)?
                    {
                        Accessibility::Public => GlobalAccessibility::Public,
                        Accessibility::Scoped(id) =>
                            GlobalAccessibility::Scoped(GlobalID::new(
                                self.public_interface_id.target_id,
                                id
                            )),
                    }
                )?,
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}
