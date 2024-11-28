use std::convert::Infallible;

use pernixc_base::{handler::Handler, source_file::Span};

use crate::{
    arena::{Key, ID},
    error::TypeAnnotationRequired,
    ir::{
        self,
        alloca::Alloca,
        representation::binding::{
            infer::{self, ConstraintModel, NoConstraint},
            HandlerWrapper,
        },
        value::literal::{self, Literal},
        Erased,
    },
    type_system::{
        model::Transform,
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

    fn try_from(_: NoConstraint) -> Result<Self, Self::Error> { Ok(Erased) }
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
}

impl Transform<Type<infer::Model>> for Transformer<'_> {
    type Target = ir::Model;

    fn transform(
        &mut self,
        term: Type<infer::Model>,
        span: Option<Span>,
    ) -> <Type<infer::Model> as Term>::Rebind<Self::Target> {
        let mut ty =
            self.inference_context.into_constraint_model(term).unwrap();
        let found_inference = RecursiveIterator::new(&ty).any(|x| match x.0 {
            term::Kind::Lifetime(a) => a.is_inference(),
            term::Kind::Type(a) => a.is_inference(),
            term::Kind::Constant(a) => a.is_inference(),
        });

        if let Some(span) = span {
            if found_inference {
                self.handler.receive(Box::new(TypeAnnotationRequired { span }));
            }
        }

        visitor::accept_recursive_mut(&mut ty, &mut ReplaceInference);

        Type::try_from_other_model(ty)
            .expect("all inference should've been replaced")
    }
}

pub fn transform_inference(
    mut original: ir::Representation<infer::Model>,
    inference_context: &infer::Context,
    handler: &HandlerWrapper,
) -> ir::Representation<ir::Model> {
    let mut transformed = ir::Representation::<ir::Model>::default();

    // should always be equal, ID(0)
    assert_eq!(
        transformed.control_flow_graph.entry_block_id().into_index(),
        original.control_flow_graph.entry_block_id().into_index()
    );

    let mut transformer = Transformer { inference_context, handler };

    // transform the allocas
    let used_allocas = original
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .flat_map(|x| x.as_alloca_declaration().map(|x| x.id))
        .collect::<Vec<_>>();

    for alloca_id in used_allocas {
        let removed_alloca = original.allocas.remove(alloca_id).unwrap();
        transformed.allocas.insert_with_id(
            ID::from_index(alloca_id.into_index()),
            removed_alloca.transform_model(&mut transformer),
        );
    }

    // transform the blocks
    transform_block(
        &mut original,
        &mut transformed,
        inference_context,
        0, // start from the entry block
        handler,
    );

    transformed
}
