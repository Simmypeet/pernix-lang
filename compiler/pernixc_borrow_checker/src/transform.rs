//! Contains the logic related to transforming the IR from [`IRModel`] to
//! [`BorrowModel`] and vice versa.

use pernixc_abort::Abort;
use pernixc_arena::{Key, ID};
use pernixc_semantic::{
    component::derived::ir::{
        model::{Erased, Model as IRModel, Transform},
        value::register::Assignment,
        Representation,
    },
    table::Table,
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::Type,
        sub_term::TermLocation,
        visitor::{self, MutableRecursive},
        ModelOf,
    },
};
use pernixc_source_file::Span;

use crate::{
    local_region_generator::LocalRegionGenerator, LocalRegionID,
    Model as BorrowModel,
};

#[derive(Debug, PartialEq, Eq)]
struct ToBorrowRecursiveVisitor<'a> {
    generator: &'a mut LocalRegionGenerator,
}

impl MutableRecursive<Lifetime<BorrowModel>> for ToBorrowRecursiveVisitor<'_> {
    fn visit(
        &mut self,
        term: &mut Lifetime<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if let Lifetime::Inference(id) = term {
            *id = self.generator.next();
        }

        true
    }
}

impl MutableRecursive<Type<BorrowModel>> for ToBorrowRecursiveVisitor<'_> {
    fn visit(
        &mut self,
        _: &mut Type<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant<BorrowModel>> for ToBorrowRecursiveVisitor<'_> {
    fn visit(
        &mut self,
        _: &mut Constant<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ToBorrowTransformer {
    generator: LocalRegionGenerator,
}

impl From<Erased> for LocalRegionID {
    fn from(Erased: Erased) -> Self {
        // use default value first, will be replaced later
        Self(ID::from_index(0))
    }
}

impl From<LocalRegionID> for Erased {
    fn from(_: LocalRegionID) -> Self { Self }
}

impl Transform<Lifetime<IRModel>> for ToBorrowTransformer {
    type Target = BorrowModel;

    type Error = Abort;

    fn inspect(
        &mut self,
        _: &Lifetime<IRModel>,
        _: Option<&Span>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Lifetime<IRModel>,
        _: Option<&Span>,
    ) -> Result<Lifetime<BorrowModel>, Self::Error> {
        Ok(match term {
            Lifetime::Static => Lifetime::Static,
            Lifetime::Parameter(param) => Lifetime::Parameter(param),
            Lifetime::Inference(Erased) => {
                Lifetime::Inference(self.generator.next())
            }
            Lifetime::Forall(_) => unreachable!(),
            Lifetime::Error(error) => Lifetime::Error(error),
            Lifetime::Elided(member_id) => Lifetime::Elided(member_id),
        })
    }
}

impl Transform<Type<IRModel>> for ToBorrowTransformer {
    type Target = BorrowModel;

    type Error = Abort;

    fn inspect(
        &mut self,
        _: &Type<IRModel>,
        _: Option<&Span>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Type<IRModel>,
        _: Option<&Span>,
    ) -> Result<Type<BorrowModel>, Self::Error> {
        let mut ty: Type<BorrowModel> = Type::from_other_model(term);
        let mut visitor =
            ToBorrowRecursiveVisitor { generator: &mut self.generator };

        visitor::accept_recursive_mut(&mut ty, &mut visitor);

        Ok(ty)
    }
}

impl Transform<Constant<IRModel>> for ToBorrowTransformer {
    type Target = BorrowModel;

    type Error = Abort;

    fn inspect(
        &mut self,
        _: &Constant<IRModel>,
        _: Option<&Span>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Constant<IRModel>,
        _: Option<&Span>,
    ) -> Result<Constant<BorrowModel>, Self::Error> {
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

impl Transform<Lifetime<BorrowModel>> for ToIRTransofmer {
    type Target = IRModel;

    type Error = Abort;

    fn inspect(
        &mut self,
        _: &Lifetime<BorrowModel>,
        _: Option<&Span>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Lifetime<BorrowModel>,
        _: Option<&Span>,
    ) -> Result<Lifetime<IRModel>, Self::Error> {
        Ok(Lifetime::from_other_model(term))
    }
}

impl Transform<Type<BorrowModel>> for ToIRTransofmer {
    type Target = IRModel;

    type Error = Abort;

    fn inspect(
        &mut self,
        _: &Type<BorrowModel>,
        _: Option<&Span>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Type<BorrowModel>,
        _: Option<&Span>,
    ) -> Result<Type<IRModel>, Self::Error> {
        Ok(Type::from_other_model(term))
    }
}

impl Transform<Constant<BorrowModel>> for ToIRTransofmer {
    type Target = IRModel;

    type Error = Abort;

    fn inspect(
        &mut self,
        _: &Constant<BorrowModel>,
        _: Option<&Span>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn transform(
        &mut self,
        term: Constant<BorrowModel>,
        _: Option<&Span>,
    ) -> Result<Constant<IRModel>, Self::Error> {
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

fn replace_with_fresh_lifetimes(
    ir: &mut Representation<BorrowModel>,
    origins: &mut LocalRegionGenerator,
) {
    let mut visitor = ReplaceWithFreshInference { generator: origins };

    // replace the lifetime in allocas
    for alloca in ir.values.allocas.items_mut() {
        visitor::accept_recursive_mut(&mut alloca.r#type, &mut visitor);
    }

    // replace the lifetime in registers
    for register in ir.values.registers.items_mut() {
        // these assignments merge multiple lifetimes, therefore we create
        // a new lifetime for each of them
        match &mut register.assignment {
            Assignment::Phi(phi) => {
                visitor::accept_recursive_mut(&mut phi.r#type, &mut visitor);
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

pub(super) fn transform_to_borrow_model(
    ir: Representation<IRModel>,
    table: &Table,
) -> (Representation<BorrowModel>, LocalRegionGenerator) {
    let mut transformer =
        ToBorrowTransformer { generator: LocalRegionGenerator::new() };

    let mut transformed_ir =
        ir.transform_model(&mut transformer, table).unwrap();

    replace_with_fresh_lifetimes(
        &mut transformed_ir,
        &mut transformer.generator,
    );

    (transformed_ir, transformer.generator)
}
