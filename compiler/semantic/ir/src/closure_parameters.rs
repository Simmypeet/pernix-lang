//! Contains the definition of [`ClosureParameters`].

use derive_more::Index;
use pernixc_arena::{Arena, ID, OrderedArena};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::parameter::Parameters;
use pernixc_term::{instantiation::Instantiation, r#type::Type};
use qbice::{Decode, Encode, StableHash};

use crate::transform;

/// Represents a parameter taken by a closure.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct ClosureParameter {
    /// The type of the parameter.
    pub r#type: Type,

    /// The span of the parameter.
    pub span: RelativeSpan,
}

/// Represents a collection of parameters taken by a closure.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, StableHash, Encode, Decode, Index,
)]
pub struct ClosureParameters(OrderedArena<ClosureParameter>);

impl ClosureParameters {
    /// Returns an iterator over the IDs of the parameters.
    #[must_use]
    pub fn ids(
        &self,
    ) -> impl ExactSizeIterator<Item = ID<ClosureParameter>> + '_ {
        self.0.ids()
    }
}

impl ClosureParameters {
    /// Creates closure parameters from original parameters and an
    /// instantiation.
    ///
    /// The parameters will be taken up to the number of spans provided by
    /// `span_iterator`. Since each parameter must have a span, the number of
    /// spans provided must be at least equal to the number of parameters.
    #[must_use]
    pub fn from_original_parameters_and_instantiation(
        parameters: &Parameters,
        inst: &Instantiation,
        span_iterator: impl Iterator<Item = RelativeSpan>,
    ) -> Self {
        let mut closure_parameters = Self::default();

        for ((_, parameter), span) in
            parameters.parameters_as_order().zip(span_iterator)
        {
            let closure_parameter = ClosureParameter {
                r#type: inst.clone_and_instantiate(&parameter.r#type),
                span,
            };

            closure_parameters.0.insert(closure_parameter);
        }

        closure_parameters
    }

    /// Returns an iterator over the parameters in order it was declared.
    #[must_use]
    pub fn parameters_as_order(
        &self,
    ) -> impl ExactSizeIterator<Item = (ID<ClosureParameter>, &ClosureParameter)>
    {
        self.0.iter()
    }

    /// Returns the declaration order of the parameter.
    #[must_use]
    pub fn get_parameter_declaration_order(
        &self,
        parameter_id: ID<ClosureParameter>,
    ) -> usize {
        self.0.id_index(parameter_id).unwrap()
    }
}

impl transform::Element for ClosureParameters {
    async fn transform<
        T: transform::Transformer<pernixc_term::lifetime::Lifetime>
            + transform::Transformer<Type>
            + transform::Transformer<pernixc_term::constant::Constant>,
    >(
        &mut self,
        transformer: &mut T,
        _engine: &pernixc_qbice::TrackedEngine,
    ) {
        for (_, parameter) in self.0.iter_mut_unordered() {
            transformer
                .transform(
                    &mut parameter.r#type,
                    transform::TypeTermSource::ClosureParameter,
                    parameter.span,
                )
                .await;
        }
    }
}

/// A collection of all closure parameters used in a function.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Encode,
    Decode,
    derive_more::Index,
    derive_more::IndexMut,
)]
pub struct ClosureParametersMap {
    #[index]
    #[index_mut]
    arena: Arena<ClosureParameters>,
}

impl ClosureParametersMap {
    /// Creates a new empty [`ClosureParametersMap`].
    #[must_use]
    pub fn new() -> Self { Self { arena: Arena::new() } }

    /// Inserts new closure parameters into the map and returns its ID.
    #[must_use]
    pub fn insert(
        &mut self,
        closure_parameters: ClosureParameters,
    ) -> ID<ClosureParameters> {
        self.arena.insert(closure_parameters)
    }
}

impl transform::Element for ClosureParametersMap {
    async fn transform<
        T: transform::Transformer<pernixc_term::lifetime::Lifetime>
            + transform::Transformer<Type>
            + transform::Transformer<pernixc_term::constant::Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &pernixc_qbice::TrackedEngine,
    ) {
        for (_, closure_parameters) in &mut self.arena {
            closure_parameters.transform(transformer, engine).await;
        }
    }
}
