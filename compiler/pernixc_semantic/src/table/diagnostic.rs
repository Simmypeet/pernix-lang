//! Contains the diagnostic related to the table.

use pernixc_base::{
    diagnostic::{Diagnostic, Related, Report},
    log::Severity,
    source_file::Span,
};

use super::{GlobalID, Representation, Table, TargetID, ID};
use crate::{
    component::{Accessibility, LocationSpan, Name, SymbolKind},
    diagnostic::ReportError,
};

/// The item symbol with the same name already exists in the given scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemRedifinition {
    /// The ID of the existing symbol.
    pub existing_id: ID,

    /// The ID of the new symbol.
    pub new_id: ID,

    /// The scope in which the duplication occurred.
    pub in_id: GlobalID,
}

impl Report<&Table> for ItemRedifinition {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table,
    ) -> Result<pernixc_base::diagnostic::Diagnostic, Self::Error> {
        let existing_symbol_span = table
            .get_component::<LocationSpan>(GlobalID::new(
                self.in_id.target_id,
                self.existing_id,
            ))
            .ok_or(ReportError)?;
        let new_symbol_span = table
            .get_component::<LocationSpan>(GlobalID::new(
                self.in_id.target_id,
                self.new_id,
            ))
            .ok_or(ReportError)?;
        let existing_symbol_name = table
            .get_component::<Name>(GlobalID::new(
                self.in_id.target_id,
                self.existing_id,
            ))
            .ok_or(ReportError)?;
        let in_name =
            table.get_qualified_name(self.in_id).ok_or(ReportError)?;

        Ok(Diagnostic {
            span: new_symbol_span.0.clone(),
            message: format!(
                "the symbol `{}` is already defined in `{in_name}`",
                existing_symbol_name.as_str(),
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: existing_symbol_span.0.clone(),
                message: "previously defined here".to_string(),
            }],
        })
    }
}

/// The symbol is more accessible than the parent symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolIsMoreAccessibleThanParent {
    /// The ID of the symbol that is more accessible than the parent symbol.
    pub symbol_id: GlobalID,

    /// The ID of the parent symbol.
    pub parent_id: GlobalID,
}

impl Representation {
    fn accessibility_description(
        &self,
        target_id: TargetID,
        accessibility: Accessibility,
    ) -> Result<String, ReportError> {
        match accessibility {
            Accessibility::Public => Ok("publicly accessible".to_owned()),
            Accessibility::Scoped(module_id) => {
                let module_qualified_name = self
                    .get_qualified_name(GlobalID::new(target_id, module_id))
                    .ok_or(ReportError)?;

                Ok(format!("accessible in `{module_qualified_name}`"))
            }
        }
    }
}

impl Report<&Table> for SymbolIsMoreAccessibleThanParent {
    type Error = ReportError;

    fn report(
        &self,
        table: &Table,
    ) -> Result<pernixc_base::diagnostic::Diagnostic, Self::Error> {
        let (Some(symbol_name), Some(parent_qualified_name)) = (
            table.get_component::<Name>(self.symbol_id),
            table.get_qualified_name(self.parent_id),
        ) else {
            return Err(ReportError);
        };

        let (Some(symbol_accessibility), Some(parent_accessibility)) = (
            table.get_accessibility(self.symbol_id),
            table.get_accessibility(self.parent_id),
        ) else {
            return Err(ReportError);
        };

        let (Some(symbol_span), Some(parent_span)) = (
            table.get_component::<LocationSpan>(self.symbol_id),
            table.get_component::<LocationSpan>(self.parent_id),
        ) else {
            return Err(ReportError);
        };

        let symbol_accessibility_description = table
            .accessibility_description(
                self.symbol_id.target_id,
                symbol_accessibility,
            )?;

        let parent_accessibility_description = table
            .accessibility_description(
                self.parent_id.target_id,
                parent_accessibility,
            )?;

        Ok(Diagnostic {
            span: symbol_span.0.clone(),
            message: format!(
                "the symbol `{}` in `{parent_qualified_name}` is more \
                 accessible than the parent symbol",
                symbol_name.as_str()
            ),
            severity: Severity::Error,
            help_message: Some(format!(
                "the symbol `{}` is {symbol_accessibility_description}",
                symbol_name.as_str()
            )),
            related: vec![Related {
                span: parent_span.0.clone(),
                message: format!(
                    "the parent symbol is {parent_accessibility_description}",
                ),
            }],
        })
    }
}

/// The calling convention is an `extern` block is invalid.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownExternCallingConvention {
    /// The span of the extern calling convention.
    pub span: Span,
}

impl Report<&Table> for UnknownExternCallingConvention {
    type Error = ReportError;

    fn report(&self, _: &Table) -> Result<Diagnostic, Self::Error> {
        Ok(Diagnostic {
            span: self.span.clone(),
            message: format!(
                "unknown calling convention `{}` in `extern`",
                self.span.str()
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// Expected a module in the module path, but found other kind of symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpectModule {
    /// The module path that was expected to be a module.
    pub module_path: Span,

    /// The ID of the symbol that was found instead of a module.
    pub found_id: GlobalID,
}

impl Report<&Table> for ExpectModule {
    type Error = ReportError;

    fn report(&self, table: &Table) -> Result<Diagnostic, Self::Error> {
        let found_symbol_qualified_name =
            table.get_qualified_name(self.found_id).ok_or(ReportError)?;
        let kind = table.get_component::<SymbolKind>(self.found_id).unwrap();

        Ok(Diagnostic {
            span: self.module_path.clone(),
            message: format!(
                "expected a module in the module path, but found `{} {}`",
                kind.kind_str(),
                found_symbol_qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}

/// The name is already exists in the given module.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictingUsing {
    /// The span of the using statement.
    pub using_span: Span,

    /// The name that conflicts with the existing name in the module.
    pub name: String,

    /// The module where the name is already defined.
    pub module_id: GlobalID,

    /// The span of the conflicting name.
    ///
    /// This can either be the span to the declared symbol or the previous
    /// using that uses the given name.
    pub conflicting_span: Option<Span>,
}

impl Report<&Table> for ConflictingUsing {
    type Error = ReportError;

    fn report(&self, table: &Table) -> Result<Diagnostic, Self::Error> {
        let module_qualified_name = table
            .get_qualified_name(self.module_id.into())
            .ok_or(ReportError)?;

        Ok(Diagnostic {
            span: self.using_span.clone(),
            message: format!(
                "the using `{name}` conflicts with the existing name in the \
                 module `{module_qualified_name}`",
                name = self.name,
            ),
            severity: Severity::Error,
            help_message: None,
            related: self
                .conflicting_span
                .as_ref()
                .map(|span| {
                    vec![Related {
                        span: span.clone(),
                        message: "conflicting symbol defined here".to_string(),
                    }]
                })
                .unwrap_or_default(),
        })
    }
}
