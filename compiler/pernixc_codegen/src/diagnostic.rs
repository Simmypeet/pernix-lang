//! Contains the diagnostics used in the code generation process.

use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_log::Severity;
use pernixc_semantic::{component::LocationSpan, GlobalID, Table};

/// The `main` symbol is reserved from the main function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MainIsNotAFunction {
    /// The ID of the main function.
    pub main_function_id: GlobalID,
}

impl Report<&Table> for MainIsNotAFunction {
    fn report(&self, table: &Table) -> Diagnostic {
        let span = table
            .get::<LocationSpan>(self.main_function_id)
            .span
            .clone()
            .unwrap();

        Diagnostic {
            span,
            message: "`main` is reserved for functions only".to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The main function must have the signature `function(): int32`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidMainFunctionSignature {
    /// The ID of the main function.
    pub main_function_id: GlobalID,
}

impl Report<&Table> for InvalidMainFunctionSignature {
    fn report(&self, table: &Table) -> Diagnostic {
        let span = table
            .get::<LocationSpan>(self.main_function_id)
            .span
            .clone()
            .unwrap();

        Diagnostic {
            span,
            message: "main function must have the signature `function()`"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The main  function is not allowed to have generic parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParametersAreNotAllowedInMainFunction {
    /// The ID of the main function
    pub main_function_id: GlobalID,
}

impl Report<&Table> for GenericParametersAreNotAllowedInMainFunction {
    fn report(&self, table: &Table) -> Diagnostic {
        let span = table
            .get::<LocationSpan>(self.main_function_id)
            .span
            .clone()
            .unwrap();

        Diagnostic {
            span,
            message: "generic parameters are not allowed in main function"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The where clause predicates are not allowed in the main function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhereClausePredicatesAreNotAllowedInMainFunction {
    /// The ID of the main function
    pub main_function_id: GlobalID,
}

impl Report<&Table> for WhereClausePredicatesAreNotAllowedInMainFunction {
    fn report(&self, table: &Table) -> Diagnostic {
        let span = table
            .get::<LocationSpan>(self.main_function_id)
            .span
            .clone()
            .unwrap();

        Diagnostic {
            span,
            message: "where clause predicates are not allowed in main function"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
