//! Contains the logic related to transforming the IR from [`IRModel`] to
//! [`BorrowModel`] and vice versa.

use pernixc_ir::{
    FunctionIR,
    resolution_visitor::{Abort, MutableResolutionVisitor, ResolutionMut},
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::{
    constant::Constant,
    instance::Instance,
    lifetime::Lifetime,
    r#type::Type,
    visitor::{self, MutableRecursive},
};

use crate::local_region_generator::{
    ClosureLifetimeGenerator, LocalRegionGenerator,
};

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    Local(LocalRegionGenerator),
    Closure(ClosureLifetimeGenerator),
}

#[derive(Debug, Clone, Copy)]
pub struct ToBorrowTransformer {
    generator: Mode,
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

fn transform_lifetime(lt: &mut Lifetime, region_gen: &mut Mode) {
    match region_gen {
        Mode::Local(local_region_generator) => match lt {
            Lifetime::Error(_)
            | Lifetime::Closure(_)
            | Lifetime::Inference(_) => {
                unreachable!("unexpected lifetime in closure context: {lt:?}")
            }

            // leave forall as is
            Lifetime::Elided(_)
            | Lifetime::Static
            | Lifetime::Parameter(_)
            | Lifetime::Forall(_) => {}

            Lifetime::Erased => {
                *lt = Lifetime::Inference(local_region_generator.next());
            }
        },

        Mode::Closure(closure_lifetime_generator) => match lt {
            Lifetime::Error(_)
            | Lifetime::Closure(_)
            | Lifetime::Inference(_) => {
                unreachable!("unexpected lifetime in closure context: {lt:?}")
            }

            // leave forall as is
            Lifetime::Forall(_) => {}

            Lifetime::Elided(_)
            | Lifetime::Parameter(_)
            | Lifetime::Static
            | Lifetime::Erased => {
                *lt = Lifetime::Closure(closure_lifetime_generator.next());
            }
        },
    }

    match lt {
        Lifetime::Closure(_) | Lifetime::Inference(_) => {
            panic!("should have no prior inference lifetime")
        }

        Lifetime::Error(_)
        | Lifetime::Parameter(_)
        | Lifetime::Elided(_)
        | Lifetime::Forall(_)
        | Lifetime::Static => {}

        Lifetime::Erased => {
            *lt = match region_gen {
                Mode::Local(local_region_generator) => {
                    Lifetime::Inference(local_region_generator.next())
                }
                Mode::Closure(closure_lifetime_generator) => {
                    Lifetime::Closure(closure_lifetime_generator.next())
                }
            }
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

pub(super) async fn transform_to_inference(ir: &mut FunctionIR) {
    let mut transformer = ToBorrowTransformer {
        generator: Mode::Closure(ClosureLifetimeGenerator::new()),
    };

    ir.accept_visitor_for_captures_and_handling_scopes(&mut transformer)
        .await
        .expect("should've no errors");

    transformer.generator = Mode::Local(LocalRegionGenerator::new());

    ir.accept_visitor_for_ir(&mut transformer)
        .await
        .expect("should've no errors");
}
