use std::{clone::Clone, fmt::Debug};

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_type_system::term::Term;

pub trait Constraint: Debug + Clone {
    type Term;

    fn satisfies(&self, term: &Self::Term) -> bool;
    fn combine(&self, another: &Self) -> Option<Self>
    where
        Self: Sized;
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum Inference<C: Constraint> {
    Known(C::Term),
    Inferring(ID<C>),
}

pub struct Table {}
