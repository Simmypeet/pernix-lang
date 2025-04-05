use std::{collections::HashSet, option::Option};

use pernixc_arena::{Key, ID};
use pernixc_handler::Handler;
use pernixc_semantic::{
    component::derived::ir::{
        model::{self, Erased, Transform},
        Representation,
    },
    diagnostic::Diagnostic,
    table::Table,
    term::{
        self,
        constant::Constant,
        lifetime::Lifetime,
        r#type::Type,
        sub_term::TermLocation,
        visitor::{self, MutableRecursive, RecursiveIterator},
        ModelOf,
    },
};
use pernixc_source_file::GlobalSpan;

use crate::{diagnostic::TypeAnnotationRequired, infer, Abort};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ReplaceInference;

impl MutableRecursive<Lifetime<model::Constrained>> for ReplaceInference {
    fn visit(
        &mut self,
        _: &mut Lifetime<model::Constrained>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}
impl MutableRecursive<Type<model::Constrained>> for ReplaceInference {
    fn visit(
        &mut self,
        term: &mut Type<model::Constrained>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Type::Error(term::Error);
        }

        true
    }
}
impl MutableRecursive<Constant<model::Constrained>> for ReplaceInference {
    fn visit(
        &mut self,
        term: &mut Constant<model::Constrained>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            *term = Constant::Error(term::Error);
        }

        true
    }
}

#[derive(Clone, Copy)]
pub struct Transformer<'a> {
    inference_context: &'a infer::Context,
    table: &'a Table,
    handler: &'a dyn Handler<Box<dyn Diagnostic>>,
    should_report: bool,
}

impl Transform<Lifetime<infer::Model>> for Transformer<'_> {
    type Target = model::Model;
    type Error = Abort;

    fn transform(
        &mut self,
        term: Lifetime<infer::Model>,
        _: Option<&GlobalSpan>,
    ) -> Result<Lifetime<model::Model>, Abort> {
        Ok(match term {
            Lifetime::Static => Lifetime::Static,
            Lifetime::Parameter(member_id) => Lifetime::Parameter(member_id),
            Lifetime::Elided(elided_id) => Lifetime::Elided(elided_id),
            Lifetime::Inference(_) => Lifetime::Inference(Erased),
            Lifetime::Forall(forall) => Lifetime::Forall(forall),
            Lifetime::Error(error) => Lifetime::Error(error),
        })
    }

    fn inspect(
        &mut self,
        _: &Lifetime<infer::Model>,
        _: Option<&GlobalSpan>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl Transform<Constant<infer::Model>> for Transformer<'_> {
    type Target = model::Model;
    type Error = Abort;

    fn inspect(
        &mut self,
        _: &Constant<infer::Model>,
        _: Option<&GlobalSpan>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Constant<infer::Model>,
        span: Option<&GlobalSpan>,
    ) -> Result<Constant<model::Model>, Abort> {
        let mut constant =
            self.inference_context.transform_constant_into_constraint_model(
                term,
                span.cloned().unwrap(),
                self.table,
                self.handler,
            )?;

        let mut replace_inference = ReplaceInference;

        visitor::accept_recursive_mut(&mut constant, &mut replace_inference);

        Ok(Constant::try_from_other_model(constant)
            .expect("all inference should've been replaced"))
    }
}

impl Transform<Type<infer::Model>> for Transformer<'_> {
    type Target = model::Model;
    type Error = Abort;

    fn inspect(
        &mut self,
        term: &Type<infer::Model>,
        span: Option<&GlobalSpan>,
    ) -> Result<(), Self::Error> {
        let ty = self.inference_context.transform_type_into_constraint_model(
            term.clone(),
            span.cloned().unwrap(),
            self.table,
            self.handler,
        )?;

        let found_inference = RecursiveIterator::new(&ty).any(|x| match x.0 {
            term::Kind::Lifetime(_) => false,
            term::Kind::Type(a) => a.is_inference(),
            term::Kind::Constant(a) => a.is_inference(),
        });

        if found_inference && self.should_report {
            self.handler.receive(Box::new(TypeAnnotationRequired {
                span: span.cloned().unwrap(),
                r#type: ty,
            }));
        }

        Ok(())
    }

    fn transform(
        &mut self,
        term: Type<infer::Model>,
        span: Option<&GlobalSpan>,
    ) -> Result<Type<model::Model>, Abort> {
        let mut ty =
            self.inference_context.transform_type_into_constraint_model(
                term,
                span.cloned().unwrap(),
                self.table,
                self.handler,
            )?;

        let mut replace_inference = ReplaceInference;

        visitor::accept_recursive_mut(&mut ty, &mut replace_inference);

        Ok(Type::try_from_other_model(ty)
            .expect("all inference should've been replaced"))
    }
}

pub fn transform_inference(
    mut original: Representation<infer::Model>,
    inference_context: &infer::Context,
    table: &Table,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Representation<model::Model>, Abort> {
    original.control_flow_graph.remove_unerachable_blocks();

    let used_registers = original
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .filter_map(|x| x.as_register_assignment().map(|x| x.id))
        .collect::<HashSet<_>>();

    original.values.registers.retain(|id, _| used_registers.contains(&id));

    let mut result = Representation::<model::Model>::default();

    let mut transformer =
        Transformer { inference_context, handler, should_report: false, table };

    result.values.allocas = original
        .values
        .allocas
        .into_iter()
        .map(|(id, x)| {
            Ok((
                ID::from_index(id.into_index()),
                x.transform_model(&mut transformer)?,
            ))
        })
        .collect::<Result<_, Abort>>()?;

    transformer.should_report = true;

    result.control_flow_graph =
        original.control_flow_graph.transform_model(&mut transformer)?;
    result.values.registers = original
        .values
        .registers
        .into_iter()
        .map(|(id, x)| {
            Ok((
                ID::from_index(id.into_index()),
                x.transform_model(&mut transformer, table)?,
            ))
        })
        .collect::<Result<_, Abort>>()?;

    result.scope_tree = original.scope_tree;

    Ok(result)
}

#[cfg(test)]
mod test;
