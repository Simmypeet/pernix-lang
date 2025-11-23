//! Contains the logic related to transforming the IR from [`IRModel`] to
//! [`BorrowModel`] and vice versa.

use pernixc_ir::{
    IR,
    transform::{Element, Transformable, Transformer},
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_term::{
    constant::Constant,
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

impl Transformer<Lifetime> for ToBorrowTransformer {
    async fn transform(
        &mut self,
        term: &mut Lifetime,
        _: <Lifetime as Transformable>::Source,
        _: Option<RelativeSpan>,
    ) -> Result<(), CyclicError> {
        transform_lifetime(term, &mut self.generator);

        Ok(())
    }
}

impl Transformer<Type> for ToBorrowTransformer {
    async fn transform(
        &mut self,
        term: &mut Type,
        _: <Type as Transformable>::Source,
        _: Option<RelativeSpan>,
    ) -> Result<(), CyclicError> {
        visitor::accept_recursive_mut(term, self);

        Ok(())
    }
}

impl Transformer<Constant> for ToBorrowTransformer {
    async fn transform(
        &mut self,
        term: &mut Constant,
        _: <Constant as Transformable>::Source,
        _: Option<RelativeSpan>,
    ) -> Result<(), CyclicError> {
        visitor::accept_recursive_mut(term, self);

        Ok(())
    }
}

pub(super) async fn transform_to_inference(
    ir: &mut IR,
    engine: &TrackedEngine,
) -> Result<LocalRegionGenerator, CyclicError> {
    let mut transformer =
        ToBorrowTransformer { generator: LocalRegionGenerator::new() };

    ir.transform(&mut transformer, engine).await?;

    Ok(transformer.generator)
}
