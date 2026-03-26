//! Contains the definition of a [`Tuple`] register assignment.

use std::ops::Deref;

use pernixc_arena::ID;
use pernixc_term::{predicate::Predicate, r#type::Type};
use pernixc_type_system::{
    OverflowError, Succeeded, UnrecoverableError, constraints::Constraints,
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    Values,
    resolution_visitor::{Abort, MutableResolutionVisitor, ResolutionVisitor},
    value::{TypeOf, Value, register::Register},
};

macro_rules! visit_tuple_literals {
    ($elements:expr, $literal_accessor:ident, $visitor:expr, $accept_method:ident) => {{
        for element in $elements.filter_map(|x| x.value.$literal_accessor()) {
            element.$accept_method($visitor).await?;
        }
        Ok(())
    }};
}

/// Represents a tuple of values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct TupleElement {
    /// The value of the tuple element.
    pub value: Value,

    /// Whether the tuple element is unpacked.
    pub is_unpacked: bool,
}

/// Represents a tuple of values.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
)]
pub struct Tuple {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement>,
}

impl Tuple {
    /// Returns the list of registers that are used in the tuple.
    #[must_use]
    pub fn get_used_registers(&self) -> Vec<ID<Register>> {
        self.elements
            .iter()
            .filter_map(|x| x.value.as_register().copied())
            .collect()
    }
}

impl Tuple {
    /// Performs well-formedness checking on this tuple construction.
    ///
    /// It primarily searches for unpacking elements and checks whether their
    /// type satisfies the `tuple` bound.
    pub async fn wf_check<N, D>(
        &self,
        environment: &crate::value::Environment<'_, N>,
        values: &crate::value::Values,
        handler: &dyn pernixc_handler::Handler<D>,
    ) -> Result<Constraints, UnrecoverableError>
    where
        N: pernixc_type_system::normalizer::Normalizer,
        D: pernixc_diagnostic::Report
            + From<pernixc_type_system::diagnostic::Diagnostic>,
    {
        let mut lifetime_constraints = Constraints::new();
        for element in self.elements.iter().filter(|x| x.is_unpacked) {
            let ty = values
                .type_of(&element.value, environment)
                .await
                .map_err(|x| {
                    x.report_as_type_calculating_overflow(
                        *values.span_of_value(&element.value),
                        &handler,
                    )
                })?;

            let predicate = Predicate::TupleType(
                pernixc_term::predicate::Tuple(ty.result.clone()),
            );

            lifetime_constraints.extend(
                environment
                    .type_environment
                    .predicate_satisfied(
                        predicate,
                        values.span_of_value(&element.value),
                        None,
                        &handler,
                    )
                    .await?,
            );
        }

        Ok(lifetime_constraints)
    }
}

impl crate::visitor::Element for Tuple {
    fn accept(&self, visitor: &mut impl crate::visitor::Visitor) {
        for element in &self.elements {
            visitor.visit_value(std::borrow::Cow::Borrowed(&element.value));
        }
    }
}

pub(super) async fn transform_tuple<T: MutableResolutionVisitor>(
    tuple: &mut Tuple,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_tuple_literals!(
        tuple.elements.iter_mut(),
        as_literal_mut,
        visitor,
        accept_mut
    )
}

pub(super) async fn inspect_tuple<T: ResolutionVisitor>(
    tuple: &Tuple,
    visitor: &mut T,
) -> Result<(), Abort> {
    visit_tuple_literals!(tuple.elements.iter(), as_literal, visitor, accept)
}

impl TypeOf<&Tuple> for Values {
    async fn type_of<N: pernixc_type_system::normalizer::Normalizer>(
        &self,
        value: &Tuple,
        environment: &crate::value::Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, OverflowError> {
        let mut constraints = Constraints::new();
        let mut elements = Vec::new();

        for element in &value.elements {
            let Succeeded { result: ty, constraints: new_constraint } =
                Box::pin(self.type_of(&element.value, environment)).await?;

            constraints.extend(new_constraint);

            if element.is_unpacked {
                match ty {
                    Type::Tuple(ty) => elements.extend(ty.into_elements()),
                    ty => elements
                        .push(pernixc_term::tuple::Element::new_unpacked(ty)),
                }
            } else {
                elements.push(pernixc_term::tuple::Element::new_regular(ty));
            }
        }

        let tuple_ty = Type::new_tuple(elements);
        let mut simplified_ty = environment
            .type_environment
            .simplify(tuple_ty)
            .await?
            .deref()
            .clone();
        simplified_ty.constraints.extend(constraints);
        Ok(simplified_ty)
    }
}
