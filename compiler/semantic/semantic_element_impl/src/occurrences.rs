use pernixc_handler::Handler;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_target::Global;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};
use qbice::{Decode, Encode, Identifiable, StableHash};

/// Collects all the terms and symbols resolved during building a particular
/// query.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    StableHash,
    Encode,
    Decode,
    Identifiable,
)]
#[allow(missing_docs)]
pub struct Occurrences {
    pub types: Vec<(Type, pernixc_syntax::r#type::Type)>,
    pub lifetimes: Vec<(Lifetime, pernixc_syntax::Lifetime)>,
    pub constants: Vec<(Constant, pernixc_syntax::expression::Expression)>,

    pub resolutions: Vec<(Resolution, RelativeSpan)>,

    pub unpacked_types: Vec<(Type, pernixc_syntax::r#type::Unpackable)>,
    pub unpacked_constants:
        Vec<(Constant, pernixc_syntax::expression::Expression)>,

    pub constant_types: Vec<(Type, pernixc_syntax::r#type::Type)>,
}

impl pernixc_resolution::Observer for Occurrences {
    fn on_resolution_resolved(
        &mut self,
        _: &TrackedEngine,
        _: Global<pernixc_symbol::ID>,
        resolution: &Resolution,
        span: &RelativeSpan,
        _: &dyn Handler<pernixc_resolution::diagnostic::Diagnostic>,
    ) {
        self.resolutions.push((resolution.clone(), *span));
    }

    fn on_type_resolved(
        &mut self,
        _: &TrackedEngine,
        _: Global<pernixc_symbol::ID>,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Type,
        _: &dyn Handler<pernixc_resolution::diagnostic::Diagnostic>,
    ) {
        self.types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_lifetime_resolved(
        &mut self,
        _: &TrackedEngine,
        _: Global<pernixc_symbol::ID>,
        lifetime: &Lifetime,
        syntax_tree: &pernixc_syntax::Lifetime,
        _: &dyn Handler<pernixc_resolution::diagnostic::Diagnostic>,
    ) {
        self.lifetimes.push((lifetime.clone(), syntax_tree.clone()));
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &TrackedEngine,
        _: Global<pernixc_symbol::ID>,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
        _: &dyn Handler<pernixc_resolution::diagnostic::Diagnostic>,
    ) {
        self.constants.push((constant.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_type_resolved(
        &mut self,
        _: &TrackedEngine,
        _: Global<pernixc_symbol::ID>,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Unpackable,
        _: &dyn Handler<pernixc_resolution::diagnostic::Diagnostic>,
    ) {
        self.unpacked_types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        _: &TrackedEngine,
        _: Global<pernixc_symbol::ID>,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
        _: &dyn Handler<pernixc_resolution::diagnostic::Diagnostic>,
    ) {
        self.unpacked_constants.push((constant.clone(), syntax_tree.clone()));
    }
}
