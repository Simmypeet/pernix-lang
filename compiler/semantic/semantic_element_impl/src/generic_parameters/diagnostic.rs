use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{TrackedEngine, runtime::executor};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_symbol::{
    MemberID, name::get_qualified_name, parent::get_parent_global,
    source_map::to_absolute_span, span::get_span,
};
use pernixc_target::Global;
use pernixc_term::generic_parameters::{
    ConstantParameter, GenericKind, GenericParameter, LifetimeParameter,
    TypeParameter, get_generic_parameters,
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
    EffectOperationCanNotHaveTypeOrConstantParameters(
        EffectOperationCanNotHaveTypeOrConstantParameters,
    ),
}

impl Report for Diagnostic {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        match self {
            Self::Resolution(diagnostic) => diagnostic.report(engine).await,
            Self::MisorderedGenericParameter(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::DefaultGenericParameterMustBeTrailing(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::LifetimeParameterRedefinition(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::TypeParameterRedefinition(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::ConstantParameterRedefinition(diagnostic) => {
                diagnostic.report(engine).await
            }
            Self::EffectOperationCanNotHaveTypeOrConstantParameters(
                diagnostic,
            ) => diagnostic.report(engine).await,
        }
    }
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

impl Report for MisorderedGenericParameter {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine.to_absolute_span(&self.generic_parameter_span).await,
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
        })
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

impl Report for DefaultGenericParameterMustBeTrailing {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine
                    .to_absolute_span(
                        &self.invalid_generic_default_parameter_span,
                    )
                    .await,
                None,
            )),
            message: "the default generic parameter must be trailing"
                .to_string(),
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
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

impl<T: GenericParameter> Report for GenericParameterRedefinition<T> {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let qualified_name = engine
            .get_qualified_name(self.existing_generic_parameter_id.parent_id)
            .await;
        let generic_parameters = engine
            .get_generic_parameters(
                self.existing_generic_parameter_id.parent_id,
            )
            .await
            .unwrap();

        let generic_parameter =
            T::get_generic_parameters_arena(&generic_parameters)
                .get(self.existing_generic_parameter_id.id)
                .unwrap();

        Ok(pernixc_diagnostic::Rendered {
            primary_highlight: Some(Highlight::new(
                engine
                    .to_absolute_span(&self.duplicating_generic_parameter_span)
                    .await,
                Some("redefinition here".to_string()),
            )),
            message: format!(
                "generic parameter `{}` is already defined in the `{}`",
                generic_parameter.name(),
                qualified_name
            ),
            severity: Severity::Error,
            help_message: None,
            related: if let Some(span) = generic_parameter.span() {
                vec![Highlight::new(
                    engine.to_absolute_span(span).await,
                    Some("previously defined here".to_string()),
                )]
            } else {
                Vec::new()
            },
        })
    }
}

/// The effect operation cannot have type or constant parameters.
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
pub struct EffectOperationCanNotHaveTypeOrConstantParameters {
    /// The spans of the type or constant parameters.
    pub type_or_constant_parameter_span: Vec<RelativeSpan>,

    /// The span of the effect operation.
    pub effect_operation_id: Global<pernixc_symbol::ID>,
}

impl Report for EffectOperationCanNotHaveTypeOrConstantParameters {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let span = engine
            .to_absolute_span(
                &engine.get_span(self.effect_operation_id).await.unwrap(),
            )
            .await;

        let parent_effect_id =
            engine.get_parent_global(self.effect_operation_id).await.unwrap();

        let parent_effect_id_span = engine
            .to_absolute_span(&engine.get_span(parent_effect_id).await.unwrap())
            .await;

        let parent_qualified_name =
            engine.get_qualified_name(parent_effect_id).await;

        let qualified_name =
            engine.get_qualified_name(self.effect_operation_id).await;

        let mut relateds =
            Vec::with_capacity(self.type_or_constant_parameter_span.len() + 1);

        for type_or_constant_parameter_span in
            &self.type_or_constant_parameter_span
        {
            let absolute_span =
                engine.to_absolute_span(type_or_constant_parameter_span).await;

            relateds.push(
                Highlight::builder()
                    .span(absolute_span)
                    .message("defined here")
                    .build(),
            );
        }

        relateds.push(
            Highlight::builder()
                .span(parent_effect_id_span)
                .message(format!(
                    "or consider moving generic parameters to the effect \
                     `{parent_qualified_name}` symbol instead",
                ))
                .build(),
        );

        Ok(pernixc_diagnostic::Rendered::builder()
            .message(
                "effect operation symbol cannot have type or constant \
                 parameters",
            )
            .primary_highlight(
                Highlight::builder()
                    .span(span)
                    .message(format!(
                        "`{qualified_name}` is an effect operation and cannot \
                         have type or constant parameters",
                    ))
                    .build(),
            )
            .related(relateds)
            .severity(pernixc_diagnostic::Severity::Error)
            .help_message(
                "only lifetime parameters are allowed on effect operations; \
                 consider moving type or constant parameters to the parent \
                 effect symbol instead",
            )
            .build())
    }
}
