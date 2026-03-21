//! Contains the logic related to transforming the IR from [`IRModel`] to
//! [`BorrowModel`] and vice versa.

use pernixc_ir::{
    IR,
    resolution_visitor::{
        Abort, MutableResolutionVisitable, MutableResolutionVisitor,
        ResolutionMut,
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{
    constant::Constant,
    instance::Instance,
    lifetime::Lifetime,
    r#type::Type,
    visitor::{self, MutableRecursive},
};

use crate::local_region_generator::LocalRegionGenerator;

#[derive(Debug, Clone, Copy)]
pub struct ToBorrowTransformer {
    generator: LocalRegionGenerator,
}

impl MutableRecursive<Lifetime> for ToBorrowTransformer {
    fn visit(
        &mut self,
        term: &mut Lifetime,
        _: impl Iterator<Item = pernixc_term::sub_term::TermLocation>,
    ) -> bool {
        transform_lifetime(term, &mut self.generator);
        true
    }
}

impl MutableRecursive<Type> for ToBorrowTransformer {
    fn visit(
        &mut self,
        _: &mut Type,
        _: impl Iterator<Item = pernixc_term::sub_term::TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant> for ToBorrowTransformer {
    fn visit(
        &mut self,
        _: &mut Constant,
        _: impl Iterator<Item = pernixc_term::sub_term::TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Instance> for ToBorrowTransformer {
    fn visit(
        &mut self,
        _: &mut Instance,
        _: impl Iterator<Item = pernixc_term::sub_term::TermLocation>,
    ) -> bool {
        true
    }
}

fn transform_lifetime(
    lt: &mut Lifetime,
    region_gen: &mut LocalRegionGenerator,
) {
    match lt {
        Lifetime::Inference(_) => {
            panic!("should have no prior inference lifetime")
        }
        Lifetime::Error(_)
        | Lifetime::Parameter(_)
        | Lifetime::Elided(_)
        | Lifetime::Forall(_)
        | Lifetime::Static => {}
        Lifetime::Erased => {
            *lt = Lifetime::Inference(region_gen.next());
        }
    }
}

impl MutableResolutionVisitor for ToBorrowTransformer {
    async fn visit_mut(
        &mut self,
        mut resolution: ResolutionMut<'_>,
        _span: RelativeSpan,
    ) -> Result<(), Abort> {
        for term in resolution.iter_all_term_mut() {
            match term {
                pernixc_term::TermMut::Constant(constant) => {
                    visitor::accept_recursive_mut(constant, self);
                }
                pernixc_term::TermMut::Lifetime(lifetime) => {
                    transform_lifetime(lifetime, &mut self.generator);
                }
                pernixc_term::TermMut::Type(ty) => {
                    visitor::accept_recursive_mut(ty, self);
                }
                pernixc_term::TermMut::Instance(instance) => {
                    visitor::accept_recursive_mut(instance, self);
                }
            }
        }
        Ok(())
    }
}

pub(super) async fn transform_to_inference(
    ir: &mut IR,
) -> LocalRegionGenerator {
    let mut transformer =
        ToBorrowTransformer { generator: LocalRegionGenerator::new() };

    let _ = ir.accept_mut(&mut transformer).await;

    transformer.generator
}
