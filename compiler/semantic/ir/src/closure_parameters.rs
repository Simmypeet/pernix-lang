//! Contains the definition of [`ClosureParameters`].

use derive_more::Index;
use pernixc_arena::{Arena, ID, OrderedArena};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::parameter::Parameters;
use pernixc_term::{instantiation::Instantiation, r#type::Type};
use qbice::{Decode, Encode, StableHash};

use crate::resolution_visitor::{
    self, Abort, Resolution, ResolutionMut, ResolutionVisitor,
};

macro_rules! visit_closure_parameters {
    (
        $iterable:expr,
        $visitor:expr,
        $visit_method:ident,
        $resolution_ctor:ident,
        $type_ref:ident
    ) => {{
        for (_, parameter) in $iterable {
            $visitor
                .$visit_method(
                    $resolution_ctor::Type($type_ref!(parameter.r#type)),
                    parameter.span,
                )
                .await?;
        }
        Ok(())
    }};
}

macro_rules! visit_closure_parameter_maps {
    ($iterable:expr, $visitor:expr, $accept_method:ident) => {{
        for (_, closure_parameters) in $iterable {
            closure_parameters.$accept_method($visitor).await?;
        }
        Ok(())
    }};
}

macro_rules! immut_ref {
    ($field:expr) => {
        &$field
    };
}

macro_rules! mut_ref {
    ($field:expr) => {
        &mut $field
    };
}

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

impl resolution_visitor::MutableResolutionVisitable for ClosureParameters {
    async fn accept_mut<T: resolution_visitor::MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_closure_parameters!(
            self.0.iter_mut_unordered(),
            visitor,
            visit_mut,
            ResolutionMut,
            mut_ref
        )
    }
}

impl resolution_visitor::ResolutionVisitable for ClosureParameters {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_closure_parameters!(
            self.0.iter(),
            visitor,
            visit,
            Resolution,
            immut_ref
        )
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

impl resolution_visitor::MutableResolutionVisitable for ClosureParametersMap {
    async fn accept_mut<T: resolution_visitor::MutableResolutionVisitor>(
        &mut self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_closure_parameter_maps!(&mut self.arena, visitor, accept_mut)
    }
}

impl resolution_visitor::ResolutionVisitable for ClosureParametersMap {
    async fn accept<T: ResolutionVisitor>(
        &self,
        visitor: &mut T,
    ) -> Result<(), Abort> {
        visit_closure_parameter_maps!(&self.arena, visitor, accept)
    }
}
