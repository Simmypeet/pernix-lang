use std::marker::PhantomData;

use super::{
    model::Model,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    visitor,
};

struct Visitor<S: Model> {
    is_errorneous: bool,
    _phantom: PhantomData<S>,
}

impl<S: Model> visitor::Visitor for Visitor<S> {
    type Model = S;

    fn visit_type(&mut self, ty: &Type<Self::Model>) -> bool {
        if ty.is_error() {
            self.is_errorneous = true;
            return false;
        }
        true
    }

    fn visit_lifetime(&mut self, ty: &Lifetime<Self::Model>) -> bool {
        if ty.is_error() {
            self.is_errorneous = true;
            return false;
        }

        true
    }

    fn visit_constant(&mut self, ty: &Constant<Self::Model>) -> bool {
        if ty.is_error() {
            self.is_errorneous = true;
            return false;
        }

        true
    }
}

pub(super) fn errorneous<T: Term>(term: &T) -> bool {
    let mut visitor = Visitor {
        is_errorneous: false,
        _phantom: PhantomData,
    };

    term.accept(&mut visitor, false);

    visitor.is_errorneous
}

#[cfg(test)]
mod tests;
