use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::{RelativeLocation, RelativeSpan};
use pernixc_query::TrackedEngine;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_symbol::{name::get_qualified_name, MemberID};
use pernixc_term::generic_parameters::{
    get_generic_parameters, ConstantParameter, GenericKind, GenericParameter,
    LifetimeParameter, TypeParameter,
};

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
pub enum Diagnostic {
    Resolution(pernixc_resolution::diagnostic::Diagnostic),
    MisorderedGenericParameter(MisorderedGenericParameter),
    DefaultGenericParameterMustBeTrailing(
        DefaultGenericParameterMustBeTrailing,
    ),
    LifetimeParameterRedefinition(
        GenericParameterRedefinition<LifetimeParameter>,
    ),
    TypeParameterRedefinition(GenericParameterRedefinition<TypeParameter>),
    ConstantParameterRedefinition(
        GenericParameterRedefinition<ConstantParameter>,
    ),
}

/// The generic parameter was declared in the wrong order.
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
pub struct MisorderedGenericParameter {
    /// The kind of the mis-ordered generic parameter.
    pub generic_kind: GenericKind,

    /// The span of the generic parameter.
    pub generic_parameter_span: RelativeSpan,
}

impl Report<()> for MisorderedGenericParameter {
    type Location = RelativeLocation;

    async fn report(
        &self,
        (): (),
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                self.generic_parameter_span,
                match self.generic_kind {
                    GenericKind::Type => Some(
                        "can't be declared after constant parameters"
                            .to_string(),
                    ),
                    GenericKind::Lifetime => Some(
                        "can't be declared after type or constant parameters"
                            .to_string(),
                    ),
                    GenericKind::Constant => None,
                },
            )),
            message: "the generic parameter was declared in the wrong order"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The default generic parameter must be trailing.
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
pub struct DefaultGenericParameterMustBeTrailing {
    /// The span of the generic parameter.
    pub invalid_generic_default_parameter_span: RelativeSpan,
}

impl Report<()> for DefaultGenericParameterMustBeTrailing {
    type Location = RelativeLocation;

    async fn report(
        &self,
        (): (),
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                self.invalid_generic_default_parameter_span,
                None,
            )),
            message: "the default generic parameter must be trailing"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}

/// The generic parameter with the same name already exists in the given scope.
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
pub struct GenericParameterRedefinition<T> {
    /// The ID of the existing generic parameter.
    pub existing_generic_parameter_id: MemberID<pernixc_arena::ID<T>>,

    /// The ID of the new generic parameter.
    pub duplicating_generic_parameter_span: RelativeSpan,
}

impl<T: GenericParameter> Report<&TrackedEngine>
    for GenericParameterRedefinition<T>
{
    type Location = RelativeLocation;

    async fn report(
        &self,
        table: &TrackedEngine,
    ) -> pernixc_diagnostic::Diagnostic<Self::Location> {
        let qualified_name = table
            .get_qualified_name(self.existing_generic_parameter_id.parent_id)
            .await;
        let generic_parameters = table
            .get_generic_parameters(
                self.existing_generic_parameter_id.parent_id,
            )
            .await
            .unwrap();

        let generic_parameter =
            T::get_generic_parameters_arena(&generic_parameters)
                .get(self.existing_generic_parameter_id.id)
                .unwrap();

        pernixc_diagnostic::Diagnostic {
            primary_highlight: Some(Highlight::new(
                self.duplicating_generic_parameter_span,
                Some("redefinition here".to_string()),
            )),
            message: format!(
                "generic parameter `{}` is already defined in the `{}`",
                generic_parameter.name(),
                qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: generic_parameter
                .span()
                .map(|x| {
                    Highlight::new(
                        *x,
                        Some("previously defined here".to_string()),
                    )
                })
                .into_iter()
                .collect(),
        }
    }
}
