//! Contains the logic related to transforming the IR from [`ir::Model`] to
//! [`borrow::Model`] and vice versa.

use std::convert::Infallible;

use super::local_region_generator::LocalRegionGenerator;
use crate::{
    arena::{Key, ID},
    ir::{
        self,
        representation::borrow::{self, LocalRegion, Model as BorrowModel},
        value::register::Assignment,
        Erased, Transform,
    },
    symbol::table::{self, Table},
    type_system::{
        sub_term::TermLocation,
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor::{self, MutableRecursive},
    },
};

#[derive(Debug, PartialEq, Eq)]
struct ToBorrowRecursiveVisitor<'a> {
    generator: &'a mut LocalRegionGenerator,
}

impl<'a> MutableRecursive<Lifetime<borrow::Model>>
    for ToBorrowRecursiveVisitor<'a>
{
    fn visit(
        &mut self,
        term: &mut Lifetime<borrow::Model>,
        _: impl Iterator<Item = crate::type_system::sub_term::TermLocation>,
    ) -> bool {
        if let Lifetime::Inference(id) = term {
            *id = self.generator.next();
        }

        true
    }
}

impl<'a> MutableRecursive<Type<borrow::Model>>
    for ToBorrowRecursiveVisitor<'a>
{
    fn visit(
        &mut self,
        _: &mut Type<borrow::Model>,
        _: impl Iterator<Item = crate::type_system::sub_term::TermLocation>,
    ) -> bool {
        true
    }
}

impl<'a> MutableRecursive<Constant<borrow::Model>>
    for ToBorrowRecursiveVisitor<'a>
{
    fn visit(
        &mut self,
        _: &mut Constant<borrow::Model>,
        _: impl Iterator<Item = crate::type_system::sub_term::TermLocation>,
    ) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToBorrowTransformer {
    generator: LocalRegionGenerator,
}

impl From<Erased> for ID<LocalRegion> {
    fn from(Erased: Erased) -> Self {
        // use default value first, will be replaced later
        Self::from_index(0)
    }
}

impl From<ID<LocalRegion>> for Erased {
    fn from(_: ID<LocalRegion>) -> Self { Self }
}

impl Transform<Lifetime<ir::Model>> for ToBorrowTransformer {
    type Target = borrow::Model;

    type Error = Infallible;
    fn inspect(
        &mut self,
        _: &Lifetime<ir::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Lifetime<ir::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<Lifetime<borrow::Model>, Self::Error> {
        Ok(match term {
            Lifetime::Static => Lifetime::Static,
            Lifetime::Parameter(param) => Lifetime::Parameter(param),
            Lifetime::Inference(Erased) => {
                Lifetime::Inference(self.generator.next())
            }
            Lifetime::Forall(_) => unreachable!(),
            Lifetime::Error(error) => Lifetime::Error(error),
        })
    }
}

impl Transform<Type<ir::Model>> for ToBorrowTransformer {
    type Target = borrow::Model;

    type Error = Infallible;

    fn inspect(
        &mut self,
        _: &Type<ir::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Type<ir::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<Type<borrow::Model>, Self::Error> {
        let mut ty: Type<borrow::Model> = Type::from_other_model(term);
        let mut visitor =
            ToBorrowRecursiveVisitor { generator: &mut self.generator };

        visitor::accept_recursive_mut(&mut ty, &mut visitor);

        Ok(ty)
    }
}

impl Transform<Constant<ir::Model>> for ToBorrowTransformer {
    type Target = borrow::Model;

    type Error = Infallible;

    fn inspect(
        &mut self,
        _: &Constant<ir::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Constant<ir::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<Constant<borrow::Model>, Self::Error> {
        // constant should've no lifetime, but here we are doing it anyway
        let mut constant = Constant::from_other_model(term);
        let mut visitor =
            ToBorrowRecursiveVisitor { generator: &mut self.generator };

        visitor::accept_recursive_mut(&mut constant, &mut visitor);

        Ok(constant)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToIRTransofmer;

impl Transform<Lifetime<borrow::Model>> for ToIRTransofmer {
    type Target = ir::Model;

    type Error = Infallible;

    fn inspect(
        &mut self,
        _: &Lifetime<borrow::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Lifetime<borrow::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<Lifetime<ir::Model>, Self::Error> {
        Ok(Lifetime::from_other_model(term))
    }
}

impl Transform<Type<borrow::Model>> for ToIRTransofmer {
    type Target = ir::Model;

    type Error = Infallible;

    fn inspect(
        &mut self,
        _: &Type<borrow::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Type<borrow::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<Type<ir::Model>, Self::Error> {
        Ok(Type::from_other_model(term))
    }
}

impl Transform<Constant<borrow::Model>> for ToIRTransofmer {
    type Target = ir::Model;

    type Error = Infallible;

    fn inspect(
        &mut self,
        _: &Constant<borrow::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Constant<borrow::Model>,
        _: pernixc_base::source_file::Span,
    ) -> Result<Constant<ir::Model>, Self::Error> {
        Ok(Constant::from_other_model(term))
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ReplaceWithFreshInference<'a> {
    generator: &'a mut LocalRegionGenerator,
}

impl MutableRecursive<Lifetime<BorrowModel>> for ReplaceWithFreshInference<'_> {
    fn visit(
        &mut self,
        term: &mut Lifetime<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_inference() {
            return true;
        }

        *term = Lifetime::Inference(self.generator.next());

        true
    }
}

impl MutableRecursive<Type<BorrowModel>> for ReplaceWithFreshInference<'_> {
    fn visit(
        &mut self,
        _: &mut Type<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant<BorrowModel>> for ReplaceWithFreshInference<'_> {
    fn visit(
        &mut self,
        _: &mut Constant<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl ir::Representation<BorrowModel> {
    fn replace_with_fresh_lifetimes(
        &mut self,
        origins: &mut LocalRegionGenerator,
    ) {
        let mut visitor = ReplaceWithFreshInference { generator: origins };

        // replace the lifetime in allocas
        for alloca in self.values.allocas.items_mut() {
            visitor::accept_recursive_mut(&mut alloca.r#type, &mut visitor);
        }

        // replace the lifetime in registers
        for register in self.values.registers.items_mut() {
            // these assignments merge multiple lifetimes, therefore we create
            // a new lifetime for each of them
            match &mut register.assignment {
                Assignment::Phi(phi) => {
                    visitor::accept_recursive_mut(
                        &mut phi.r#type,
                        &mut visitor,
                    );
                }

                Assignment::Array(array) => {
                    visitor::accept_recursive_mut(
                        &mut array.element_type,
                        &mut visitor,
                    );
                }

                Assignment::Struct(structure) => {
                    for lifetime in &mut structure.generic_arguments.lifetimes {
                        visitor::accept_recursive_mut(lifetime, &mut visitor);
                    }

                    for ty in &mut structure.generic_arguments.types {
                        visitor::accept_recursive_mut(ty, &mut visitor);
                    }

                    for con in &mut structure.generic_arguments.constants {
                        visitor::accept_recursive_mut(con, &mut visitor);
                    }
                }

                Assignment::Variant(variant) => {
                    for lifetime in &mut variant.generic_arguments.lifetimes {
                        visitor::accept_recursive_mut(lifetime, &mut visitor);
                    }

                    for ty in &mut variant.generic_arguments.types {
                        visitor::accept_recursive_mut(ty, &mut visitor);
                    }

                    for con in &mut variant.generic_arguments.constants {
                        visitor::accept_recursive_mut(con, &mut visitor);
                    }
                }

                _ => {}
            }
        }
    }
}

pub(super) fn transform_to_borrow_model(
    ir: ir::Representation<ir::Model>,
    table: &Table<impl table::State>,
) -> (ir::Representation<borrow::Model>, LocalRegionGenerator) {
    let mut transformer =
        ToBorrowTransformer { generator: LocalRegionGenerator::new() };

    let mut transformed_ir = ir.transform_model(&mut transformer, table).unwrap();
    transformed_ir.replace_with_fresh_lifetimes(&mut transformer.generator);

    (transformed_ir, transformer.generator)
}
