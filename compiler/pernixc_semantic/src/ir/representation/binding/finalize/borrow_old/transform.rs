use std::convert::Infallible;

use crate::{
    arena::{Arena, Key, ID},
    ir::{
        self,
        representation::{borrow, borrow::Origin, Values},
        Erased, Transform,
    },
    symbol::table::{self, Table},
    type_system::{
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor::{self, MutableRecursive},
    },
};

#[derive(Debug, PartialEq, Eq)]
struct RecursiveVisitor<'a> {
    arena: &'a mut Arena<Origin>,
}

impl<'a> MutableRecursive<Lifetime<borrow::Model>> for RecursiveVisitor<'a> {
    fn visit(
        &mut self,
        term: &mut Lifetime<borrow::Model>,
        _: impl Iterator<Item = crate::type_system::sub_term::TermLocation>,
    ) -> bool {
        if let Lifetime::Inference(id) = term {
            *id = self.arena.insert(Origin::default());
        }

        true
    }
}

impl<'a> MutableRecursive<Type<borrow::Model>> for RecursiveVisitor<'a> {
    fn visit(
        &mut self,
        _: &mut Type<borrow::Model>,
        _: impl Iterator<Item = crate::type_system::sub_term::TermLocation>,
    ) -> bool {
        true
    }
}

impl<'a> MutableRecursive<Constant<borrow::Model>> for RecursiveVisitor<'a> {
    fn visit(
        &mut self,
        _: &mut Constant<borrow::Model>,
        _: impl Iterator<Item = crate::type_system::sub_term::TermLocation>,
    ) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Transformer {
    arena: Arena<Origin>,
}

impl From<Erased> for ID<Origin> {
    fn from(Erased: Erased) -> Self {
        // use default value first, will be replaced later
        ID::from_index(0)
    }
}

impl Transform<Lifetime<ir::Model>> for Transformer {
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
                Lifetime::Inference(self.arena.insert(Origin::default()))
            }
            Lifetime::Forall(_) => unreachable!(),
            Lifetime::Error(error) => Lifetime::Error(error),
        })
    }
}

impl Transform<Type<ir::Model>> for Transformer {
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
        let mut visitor = RecursiveVisitor { arena: &mut self.arena };

        visitor::accept_recursive_mut(&mut ty, &mut visitor);

        Ok(ty)
    }
}

impl Transform<Constant<ir::Model>> for Transformer {
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
        let mut visitor = RecursiveVisitor { arena: &mut self.arena };

        visitor::accept_recursive_mut(&mut constant, &mut visitor);

        Ok(constant)
    }
}

pub(super) fn transform<'a>(
    ir: ir::Representation<ir::Model>,
    table: &Table<impl table::State>,
) -> (ir::Representation<borrow::Model>, Arena<Origin>) {
    let mut transformer = Transformer { arena: Arena::default() };

    (
        ir::Representation {
            values: Values {
                registers: ir
                    .values
                    .registers
                    .into_iter()
                    .map(|(key, register)| {
                        (
                            ID::from_index(key.into_index()),
                            register
                                .transform_model(&mut transformer, table)
                                .unwrap(),
                        )
                    })
                    .collect(),
                allocas: ir
                    .values
                    .allocas
                    .into_iter()
                    .map(|(key, alloca)| {
                        (
                            ID::from_index(key.into_index()),
                            alloca.transform_model(&mut transformer).unwrap(),
                        )
                    })
                    .collect(),
            },
            control_flow_graph: {
                ir.control_flow_graph.transform_model(&mut transformer).unwrap()
            },
            scope_tree: ir.scope_tree,
        },
        transformer.arena,
    )
}
