//! Contains the dianostic types related to the borrow check analysis.

use enum_as_inner::EnumAsInner;
use pernixc_diagnostic::{Diagnostic, Related, Report};
use pernixc_log::Severity;
use pernixc_source_file::Span;
use pernixc_table::{DisplayObject, Table};

use crate::UniversalRegion;

/// An enumeration of what part of the code uses the invalidated
/// borrows/variables.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Usage {
    /// The invalidated borrows are later used within the body of local
    /// function/scope.
    Local(Span),

    /// The invalidated borrows might be later used by the univseral regions
    /// (the caller of the function).
    ByUniversalRegions(Vec<UniversalRegion>),

    /// The invalidated borrows are used in the drop implementation.
    Drop,
}

/// The value is moved out from the variabl while it is borrowed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MovedOutWhileBorrowed {
    /// The span of the borrow.
    pub borrow_span: Span,

    /// The span of the borrow usage.
    pub usage: Usage,

    /// The span where the value is moved out
    pub moved_out_span: Span,
}

impl Report<&Table> for MovedOutWhileBorrowed {
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.moved_out_span.clone(),
            message: "the value is moved out from the variable while it is \
                      borrowed"
                .to_string(),
            severity: Severity::Error,
            help_message: match &self.usage {
                Usage::Local { .. } => None,
                Usage::ByUniversalRegions(vec) => Some(format!(
                        "lifetime(s) {} can access the borrow later",
                        vec.iter()
                            .map(|x| DisplayObject { display: x, table }
                                .to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                Usage::Drop => Some(
                    "the borrow is used in the drop implementation".to_string(),
                ),
            },
            related: std::iter::once(Related {
                span: self.borrow_span.clone(),
                message: "the borrow starts here".to_string(),
            })
            .chain(self.usage.as_local().map(|span| {
                let in_loop = &self.borrow_span == span;

                Related {
                    span: span.clone(),
                    message: if in_loop {
                        "the borrow is used later in the next iteration"
                            .to_string()
                    } else {
                        "the borrow is used here".to_string()
                    },
                }
            }))
            .collect(),
        }
    }
}

/// The variable doesn't live long enough to outlives the given lifetime.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDoesNotLiveLongEnough {
    /// The span where the borrows occurred
    pub borrow_span: Span,

    /// The span of the variable declaration.
    pub variable_span: Span,

    /// The usage of the borrow that points to the goes out of scope variable.
    pub usage: Usage,
}

impl Report<&Table> for VariableDoesNotLiveLongEnough {
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.variable_span.clone(),
            message: "the variable doesn't live long enough".to_string(),
            severity: Severity::Error,
            help_message: match &self.usage {
                Usage::Local { .. } => None,
                Usage::ByUniversalRegions(vec) => Some(format!(
                        "lifetime(s) {} can access the borrow later",
                        vec.iter()
                            .map(|x| DisplayObject { display: x, table }
                                .to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                Usage::Drop => Some(
                    "the borrow is used in the drop implementation".to_string(),
                ),
            },
            related: std::iter::once(Related {
                span: self.borrow_span.clone(),
                message: "the borrow starts here".to_string(),
            })
            .chain(self.usage.as_local().map(|span| Related {
                span: span.clone(),
                message: "the borrow is used here".to_string(),
            }))
            .collect(),
        }
    }
}

/// The mutable access is done while the value is immutably borrowed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MutablyAccessWhileImmutablyBorrowed {
    /// The span of the mutable access.
    pub mutable_access_span: Span,

    /// The span of the prior borrow.
    pub immutable_borrow_span: Option<Span>,

    /// The usage span of the prior borrow.
    pub usage: Usage,
}

impl Report<&Table> for MutablyAccessWhileImmutablyBorrowed {
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.mutable_access_span.clone(),
            message: format!(
                "mutable access to `{}` is done while it is borrowed",
                self.mutable_access_span.str()
            ),
            severity: Severity::Error,
            help_message: match &self.usage {
                Usage::Local { .. } => None,
                Usage::ByUniversalRegions(vec) => Some(format!(
                        "lifetime(s) {} can access the borrow later",
                        vec.iter()
                            .map(|x| DisplayObject { display: x, table }
                                .to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                Usage::Drop => Some(
                    "the borrow is used in the drop implementation".to_string(),
                ),
            },
            related: self
                .immutable_borrow_span
                .as_ref()
                .map(|x| Related {
                    span: x.clone(),
                    message: "the borrow starts here".to_string(),
                })
                .into_iter()
                .chain(self.usage.as_local().map(|span| {
                    let in_loop =
                        self.immutable_borrow_span.as_ref() == Some(span);

                    Related {
                        span: span.clone(),
                        message: if in_loop {
                            "the borrow is used later in the next iteration"
                                .to_string()
                        } else {
                            "the borrow is used here".to_string()
                        },
                    }
                }))
                .collect(),
        }
    }
}

/// The access is done while the value is mutably borrowed.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AccessWhileMutablyBorrowed {
    /// The span of the access.
    pub access_span: Span,

    /// The span of the prior borrow.
    pub mutable_borrow_span: Option<Span>,

    /// The usage span of the prior borrow.
    pub borrow_usage: Usage,
}

impl Report<&Table> for AccessWhileMutablyBorrowed {
    fn report(&self, table: &Table) -> Diagnostic {
        Diagnostic {
            span: self.access_span.clone(),
            message: format!(
                "access to `{}` is done while it is mutably borrowed",
                self.access_span.str()
            ),
            severity: Severity::Error,
            help_message: match &self.borrow_usage {
                Usage::Local { .. } => None,
                Usage::ByUniversalRegions(vec) => Some(format!(
                        "lifetime(s) {} can access the borrow later",
                        vec.iter()
                            .map(|x| DisplayObject { display: x, table }
                                .to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )),
                Usage::Drop => Some(
                    "the borrow is used in the drop implementation".to_string(),
                ),
            },
            related: self
                .mutable_borrow_span
                .as_ref()
                .map(|x| Related {
                    span: x.clone(),
                    message: "the mutable borrow starts here".to_string(),
                })
                .into_iter()
                .chain(self.borrow_usage.as_local().map(|span| {
                    let in_loop =
                        self.mutable_borrow_span.as_ref() == Some(span);

                    Related {
                        span: span.clone(),
                        message: if in_loop {
                            "the borrow is used later in the next iteration"
                                .to_string()
                        } else {
                            "the borrow is used here".to_string()
                        },
                    }
                }))
                .collect(),
        }
    }
}
