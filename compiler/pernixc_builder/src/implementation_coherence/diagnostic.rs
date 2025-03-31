//! Diagnostics for the implementation coherence check.

use pernixc_arena::ID;
use pernixc_diagnostic::{Related, Report};
use pernixc_log::Severity;
use pernixc_semantic::{
    component::{
        derived::{
            generic_parameters::{
                ConstantParameterID, GenericKind, GenericParameter,
                GenericParameters,
            },
            implementation::Implementation,
        },
        input::{Implements, LocationSpan, SymbolKind},
    },
    table::{DisplayObject, GlobalID, MemberID, Table},
    term::{predicate::Predicate, Default},
};
use pernixc_source_file::Span;

/// Generic parameter is unused in the implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnusedGenericParameterInImplementation<T> {
    /// The ID of the generic parameter that is unused.
    pub generic_parameter_id: MemberID<ID<T>>,
}

impl<T: GenericParameter> Report<&Table>
    for UnusedGenericParameterInImplementation<T>
{
    fn report(&self, parameter: &Table) -> pernixc_diagnostic::Diagnostic {
        let generic_parameters = parameter
            .query::<GenericParameters>(self.generic_parameter_id.parent)
            .unwrap();

        let arena = T::get_generic_parameters_arena(&generic_parameters);

        let generic_parameter = &arena[self.generic_parameter_id.id];

        let span = T::span(generic_parameter).cloned().unwrap();
        let name = T::name(generic_parameter);
        let kind = match T::kind() {
            GenericKind::Type => "type parameter",
            GenericKind::Lifetime => "lifetime parameter",
            GenericKind::Constant => "constant parameter",
        };

        pernixc_diagnostic::Diagnostic {
            span,
            message: format!("unused {kind} `{name}` in the implementation"),
            severity: pernixc_log::Severity::Error,
            help_message: None,
            related: vec![],
        }
    }
}

/// Attempted to implement a foreign struct or enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ImplementedForeignAdt {
    /// The ID of the implementation.
    pub adt_implementation_id: GlobalID,
}

impl Report<&Table> for ImplementedForeignAdt {
    fn report(&self, parameter: &Table) -> pernixc_diagnostic::Diagnostic {
        let implementation_span =
            parameter.get::<LocationSpan>(self.adt_implementation_id);
        let implemented_id =
            parameter.get::<Implements>(self.adt_implementation_id).0;

        let symbol_kind = parameter.get::<SymbolKind>(implemented_id);
        let kind_str = symbol_kind.kind_str();
        let qualified_name = parameter.get_qualified_name(implemented_id);

        pernixc_diagnostic::Diagnostic {
            span: implementation_span.span.clone().unwrap(),
            message: format!(
                "{kind_str} `{qualified_name}` is defined in another target; \
                 `implements` to foreign {kind_str} is not allowed",
            ),
            severity: pernixc_log::Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// Implementing a foreign trait/marker but doesn't contain any struct or enum
/// defined in the current target in the implementation arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OrphanRuleViolation {
    /// The ID of the trait/marker implementation.
    pub implementation_id: GlobalID,
}

impl Report<&Table> for OrphanRuleViolation {
    fn report(&self, parameter: &Table) -> pernixc_diagnostic::Diagnostic {
        let implementation_span =
            parameter.get::<LocationSpan>(self.implementation_id);

        let implemented_id =
            parameter.get::<Implements>(self.implementation_id).0;

        let symbol_kind = parameter.get::<SymbolKind>(implemented_id);
        let kind_str = symbol_kind.kind_str();
        let qualified_name = parameter.get_qualified_name(implemented_id);

        pernixc_diagnostic::Diagnostic {
            span: implementation_span.span.clone().unwrap(),
            message: format!(
                "can't implement a foreign {kind_str} `{qualified_name}` \
                 without any struct or enum defined in the current target in \
                 the implementation arguments"
            ),
            severity: pernixc_log::Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The implementation is final but it is overriden by another
/// implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FinalImplementationCannotBeOverriden {
    /// The ID of the final implementation that is overriden.
    pub final_implementation_id: GlobalID,

    /// The ID of the implementation that overrides the final implementation.
    pub overriden_implementation_id: GlobalID,
}

impl Report<&Table> for FinalImplementationCannotBeOverriden {
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let location_span = table
            .get::<LocationSpan>(self.overriden_implementation_id)
            .span
            .clone()
            .unwrap();

        let override_impl = table
            .query::<Implementation>(self.overriden_implementation_id)
            .unwrap();
        let final_impl = table
            .query::<Implementation>(self.final_implementation_id)
            .unwrap();
        let final_qualified_name =
            table.get_qualified_name(self.final_implementation_id);
        let override_qualified_name =
            table.get_qualified_name(self.overriden_implementation_id);

        let final_impl_location =
            table.get::<LocationSpan>(self.final_implementation_id);

        pernixc_diagnostic::Diagnostic {
            span: location_span,
            message: format!(
                "the implementation `{}{}` overrides the implementation \
                 `{}{}`, which is final",
                override_qualified_name,
                DisplayObject {
                    table,
                    display: &override_impl.generic_arguments
                },
                final_qualified_name,
                DisplayObject { table, display: &final_impl.generic_arguments },
            ),
            severity: Severity::Error,
            help_message: None,
            related: final_impl_location
                .span
                .as_ref()
                .map(|span| Related {
                    span: span.clone(),
                    message: "the final implementation is defined here"
                        .to_string(),
                })
                .into_iter()
                .collect::<Vec<_>>(),
        }
    }
}

/// Two implementations have the same specialty order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AmbiguousImplementation {
    /// The ID of the first implementation.
    pub first_implementation_id: GlobalID,

    /// The ID of the second implementation.
    pub second_implementation_id: GlobalID,
}

impl Report<&Table> for AmbiguousImplementation {
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let name = table.get_qualified_name(self.first_implementation_id);
        let first_span = table
            .get::<LocationSpan>(self.first_implementation_id)
            .span
            .clone()
            .unwrap();
        let second_span =
            table.get::<LocationSpan>(self.second_implementation_id);

        pernixc_diagnostic::Diagnostic {
            span: first_span,
            message: format!(
                "the implementations of the `{name}` are ambiguous",
            ),
            severity: Severity::Error,
            help_message: None,
            related: second_span
                .span
                .as_ref()
                .map(|x| Related {
                    span: x.clone(),
                    message: "the other implementation is defined here"
                        .to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The trait member and the implementation member have different number of
/// generic parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedGenericParameterCountInImplementation {
    /// The ID of the implementation member
    pub implementation_member_id: GlobalID,

    /// The ID of the trait member
    pub trait_member_id: GlobalID,

    /// Expected count of generic parameters
    pub expected_count: usize,

    /// Number of generic parameters declared in the implementation
    pub declared_count: usize,

    /// The kind of the generic parameter that has mismatched count
    pub generic_kind: GenericKind,
}

impl Report<&Table> for MismatchedGenericParameterCountInImplementation {
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let generic_kind = match self.generic_kind {
            GenericKind::Type => "type",
            GenericKind::Lifetime => "lifetime",
            GenericKind::Constant => "constant",
        };

        pernixc_diagnostic::Diagnostic {
            span: table
                .get::<LocationSpan>(self.implementation_member_id)
                .span
                .clone()
                .unwrap(),
            message: format!(
                "the implementation member has {} {generic_kind} parameters, \
                 but the trait member has {}",
                self.declared_count, self.expected_count
            ),
            severity: Severity::Error,
            help_message: None,
            related: table
                .get::<LocationSpan>(self.trait_member_id)
                .span
                .as_ref()
                .map(|x| Related {
                    span: x.clone(),
                    message: "the trait member is defined here".to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The type of the constant parameter in the implementation doesn't match the
/// type of the constant parameter in the trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MismatchedImplementationConstantTypeParameter {
    /// The constant parameter ID of the implementation member
    pub implementation_member_constant_parameter_id: ConstantParameterID,

    /// The constant parameter ID of the trait member
    pub trait_member_constant_parameter_id: ConstantParameterID,
}

impl Report<&Table> for MismatchedImplementationConstantTypeParameter {
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let impl_member_span = table
            .query::<GenericParameters>(
                self.implementation_member_constant_parameter_id.parent,
            )
            .unwrap()
            .constants()[self.implementation_member_constant_parameter_id.id]
            .span
            .clone()
            .unwrap();

        let trait_member_span = table
            .query::<GenericParameters>(
                self.trait_member_constant_parameter_id.parent,
            )
            .unwrap()
            .constants()[self.trait_member_constant_parameter_id.id]
            .span
            .clone();

        pernixc_diagnostic::Diagnostic {
            span: impl_member_span,
            message: "the type of the constant parameter in the \
                      implementation doesn't match the type of the constant \
                      parameter in the trait"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: trait_member_span
                .map(|x| Related {
                    span: x,
                    message: "the constant parameter in the trait is declared \
                              here"
                        .to_string(),
                })
                .into_iter()
                .collect(),
        }
    }
}

/// The trait implementation member contains extraneous predicate--a predicate
/// that is not defined in the trait member.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExtraneousImplementationMemberPredicate {
    /// The ID of the trait implementation member containing the extraneous
    /// predicate.
    pub trait_implementation_member_id: GlobalID,

    /// The extraneous predicate.
    pub predicate: Predicate<Default>,

    /// The declaration span of the extraneous predicate.
    pub predicate_span: Span,
}

impl Report<&Table> for ExtraneousImplementationMemberPredicate {
    fn report(&self, table: &Table) -> pernixc_diagnostic::Diagnostic {
        let trait_implementation_symbol_span = table
            .get::<LocationSpan>(self.trait_implementation_member_id)
            .span
            .clone()
            .unwrap();

        let qualified_name =
            table.get_qualified_name(self.trait_implementation_member_id);

        pernixc_diagnostic::Diagnostic {
            span: trait_implementation_symbol_span,
            message: format!(
                "the trait implementation member `{}` contains an extraneous \
                 predicate `{}` -- a predicate that is not defined in the \
                 trait member",
                qualified_name,
                DisplayObject { display: &self.predicate, table }
            ),
            severity: Severity::Error,
            help_message: None,
            related: vec![Related {
                span: self.predicate_span.clone(),
                message: "the extraneous predicate is declared here"
                    .to_string(),
            }],
        }
    }
}
