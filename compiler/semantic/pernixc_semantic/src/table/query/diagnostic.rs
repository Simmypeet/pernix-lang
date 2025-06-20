//! Contains the definition of [`CyclicDependency`] error.

use pernixc_diagnostic::Report;
use pernixc_log::Severity;

use super::Record;
use crate::{component::input::LocationSpan, table::Table};

/// A cyclic dependency between symbols detected during the query.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error(
    "a cylic dependency detected while building a component for the \
     particular symbol"
)]
pub struct CyclicDependency {
    /// The stack of records that caused the cyclic dependency.
    pub records_stack: Vec<Record>,
}

impl Report<&Table> for CyclicDependency {
    fn report(&self, parameter: &Table) -> pernixc_diagnostic::Diagnostic {
        pernixc_diagnostic::Diagnostic {
            span: parameter
                .get::<LocationSpan>(self.records_stack[0].global_id)
                .span
                .clone()
                .unwrap(),
            message: format!(
                "cyclic dependency while building {}.",
                self.records_stack
                    .iter()
                    .map(|x| format!(
                        "{} for `{}`",
                        x.name,
                        parameter.get_qualified_name(x.global_id)
                    ))
                    .collect::<Vec<_>>()
                    .join(" -> ")
            ),
            severity: Severity::Error,
            help_message: Some(format!(
                "required by `{}`",
                self.records_stack.first().unwrap().name
            )),
            related: self
                .records_stack
                .iter()
                .skip(1)
                .map(|x| pernixc_diagnostic::Related {
                    span: parameter
                        .get::<LocationSpan>(x.global_id)
                        .span
                        .clone()
                        .unwrap(),
                    message: format!("required by `{}`", x.name),
                })
                .collect::<Vec<_>>(),
        }
    }
}
