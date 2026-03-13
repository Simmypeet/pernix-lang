use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::qualified_identifier::Resolution;
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
        resolution: &Resolution,
        span: &RelativeSpan,
    ) {
        self.resolutions.push((resolution.clone(), *span));
    }

    fn on_type_resolved(
        &mut self,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Type,
    ) {
        self.types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_lifetime_resolved(
        &mut self,
        lifetime: &Lifetime,
        syntax_tree: &pernixc_syntax::Lifetime,
    ) {
        self.lifetimes.push((lifetime.clone(), syntax_tree.clone()));
    }

    fn on_constant_arguments_resolved(
        &mut self,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
    ) {
        self.constants.push((constant.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_type_resolved(
        &mut self,
        ty: &Type,
        syntax_tree: &pernixc_syntax::r#type::Unpackable,
    ) {
        self.unpacked_types.push((ty.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        constant: &Constant,
        syntax_tree: &pernixc_syntax::expression::Expression,
    ) {
        self.unpacked_constants.push((constant.clone(), syntax_tree.clone()));
    }
}
