use std::{collections::HashSet, convert::Infallible};

use pernixc_base::handler::Handler;

use crate::{
    error::TypeAnnotationRequired,
    ir::{
        self,
        representation::binding::{
            infer::{self, ConstraintModel, NoConstraint},
            HandlerWrapper,
        },
        Erased, Transform,
    },
    symbol::table::{self, Table},
    type_system::{
        sub_term::TermLocation,
        term::{
            self,
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Type},
            Error, Never, Term,
        },
        visitor::{self, MutableRecursive, RecursiveIterator},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ReplaceInference;

impl MutableRecursive<Lifetime<ConstraintModel>> for ReplaceInference {
    fn visit(
        &mut self,
        _: &mut Lifetime<ConstraintModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}
impl MutableRecursive<Type<ConstraintModel>> for ReplaceInference {
    fn visit(
        &mut self,
        term: &mut Type<ConstraintModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Type::Error(Error);
        }

        true
    }
}
impl MutableRecursive<Constant<ConstraintModel>> for ReplaceInference {
    fn visit(
        &mut self,
        term: &mut Constant<ConstraintModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Constant::Error(Error);
        }

        true
    }
}

impl TryFrom<NoConstraint> for Erased {
    type Error = Infallible;

    fn try_from(_: NoConstraint) -> Result<Self, Self::Error> { Ok(Self) }
}

impl TryFrom<NoConstraint> for Never {
    type Error = Infallible;

    fn try_from(_: NoConstraint) -> Result<Self, Self::Error> {
        panic!("NoConstraint should never be converted to Never")
    }
}

impl TryFrom<r#type::Constraint> for Never {
    type Error = Infallible;

    fn try_from(_: r#type::Constraint) -> Result<Self, Self::Error> {
        panic!("Constraint should never be converted to Never")
    }
}

#[derive(Clone, Copy)]
pub struct Transformer<'a> {
    inference_context: &'a infer::Context,
    handler: &'a HandlerWrapper<'a>,
    should_report: bool,
}

impl Transform<Lifetime<infer::Model>> for Transformer<'_> {
    type Target = ir::Model;

    fn transform(
        &mut self,
        term: Lifetime<infer::Model>,
    ) -> Lifetime<ir::Model> {
        match term {
            Lifetime::Static => Lifetime::Static,
            Lifetime::Parameter(member_id) => Lifetime::Parameter(member_id),
            Lifetime::Inference(_) => Lifetime::Inference(Erased),
            Lifetime::Forall(forall) => Lifetime::Forall(forall),
            Lifetime::Error(error) => Lifetime::Error(error),
        }
    }
}

impl Transform<Constant<infer::Model>> for Transformer<'_> {
    type Target = ir::Model;

    fn transform(
        &mut self,
        term: Constant<infer::Model>,
    ) -> Constant<ir::Model> {
        let mut constant = self
            .inference_context
            .transform_constant_into_constraint_model(term)
            .unwrap();
        let mut replace_inference = ReplaceInference;

        visitor::accept_recursive_mut(&mut constant, &mut replace_inference);

        Constant::try_from_other_model(constant)
            .expect("all inference should've been replaced")
    }
}

impl Transform<Type<infer::Model>> for Transformer<'_> {
    type Target = ir::Model;

    fn inspect_and_transform(
        &mut self,
        term: Type<infer::Model>,
        span: Option<pernixc_base::source_file::Span>,
    ) -> Type<ir::Model> {
        let mut ty = self
            .inference_context
            .transform_type_into_constraint_model(term)
            .unwrap();
        let found_inference = RecursiveIterator::new(&ty).any(|x| match x.0 {
            term::Kind::Lifetime(_) => false,
            term::Kind::Type(a) => a.is_inference(),
            term::Kind::Constant(a) => a.is_inference(),
        });

        if let Some(span) = span {
            if found_inference && self.should_report {
                self.handler.receive(Box::new(TypeAnnotationRequired {
                    span,
                    r#type: ty.clone(),
                }));
            }
        }

        let mut replace_inference = ReplaceInference;

        visitor::accept_recursive_mut(&mut ty, &mut replace_inference);

        Type::try_from_other_model(ty)
            .expect("all inference should've been replaced")
    }

    fn transform(&mut self, term: Type<infer::Model>) -> Type<ir::Model> {
        let mut ty = self
            .inference_context
            .transform_type_into_constraint_model(term)
            .unwrap();

        let mut replace_inference = ReplaceInference;

        visitor::accept_recursive_mut(&mut ty, &mut replace_inference);

        Type::try_from_other_model(ty)
            .expect("all inference should've been replaced")
    }
}

pub fn transform_inference(
    mut original: ir::Representation<infer::Model>,
    inference_context: &infer::Context,
    table: &Table<impl table::State>,
    handler: &HandlerWrapper,
) -> ir::Representation<ir::Model> {
    original.control_flow_graph.remove_unerachable_blocks();

    let used_allocas = original
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .filter_map(|x| x.as_alloca_declaration().map(|x| x.id))
        .collect::<HashSet<_>>();

    original.values.allocas.retain(|id, _| used_allocas.contains(&id));

    let used_registers = original
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .filter_map(|x| x.as_register_assignment().map(|x| x.id))
        .collect::<HashSet<_>>();

    original.values.registers.retain(|id, _| used_registers.contains(&id));

    let mut result = ir::Representation::<ir::Model>::default();

    let mut transformer =
        Transformer { inference_context, handler, should_report: false };

    result.values.allocas =
        original.values.allocas.map(|x| x.transform_model(&mut transformer));

    transformer.should_report = true;

    result.control_flow_graph =
        original.control_flow_graph.transform_model(&mut transformer);
    result.values.registers = original
        .values
        .registers
        .map(|x| x.transform_model(&mut transformer, table));
    result.scope_tree = original.scope_tree;

    result
}

#[cfg(test)]
mod test;
