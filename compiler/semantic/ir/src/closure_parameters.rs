//! Contains the definition of [`ClosureParameters`].

use derive_more::Index;
use pernixc_arena::{OrderedArena, ID};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::parameter::Parameters;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{instantiation::Instantiation, r#type::Type};

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
    Serialize,
    Deserialize,
)]
pub struct ClosureParameter {
    /// The type of the parameter.
    pub r#type: Type,

    /// The span of the parameter.
    pub span: Option<RelativeSpan>,
}

/// Represents a collection of parameters taken by a closure.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Serialize,
    Deserialize,
    Index,
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
    #[must_use]
    pub fn from_original_parameters_and_instantiation(
        parameters: &Parameters,
        inst: &Instantiation,
    ) -> Self {
        let mut closure_parameters = Self::default();

        for (_, parameter) in parameters.parameters_as_order() {
            let closure_parameter = ClosureParameter {
                r#type: inst.clone_and_instantiate(&parameter.r#type),
                span: parameter.span,
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
        _engine: &pernixc_query::TrackedEngine,
    ) -> Result<(), pernixc_query::runtime::executor::CyclicError> {
        for (_, parameter) in self.0.iter_mut_unordered() {
            transformer
                .transform(
                    &mut parameter.r#type,
                    transform::TypeTermSource::ClosureParameter,
                    parameter.span,
                )
                .await?;
        }

        Ok(())
    }
}
