use pernixc_qbice::Interner;
use qbice::storage::intern::Interned;

use crate::{
    generic_parameters::GenericParameterID,
    instantiation::Instantiation,
    r#type::{
        Type,
        rewrite::{RewriteContext, TypeRewriter, rewrite_type},
    },
};

struct InstantiationRewriter<'a> {
    instantiation: &'a Instantiation,
}

impl TypeRewriter for InstantiationRewriter<'_> {
    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _: RewriteContext,
    ) -> Option<Interned<Type>> {
        self.instantiation.get_instantiated_type(id).cloned()
    }
}

impl Instantiation {
    pub fn instantiate(
        &self,
        ty: &Interned<Type>,
        interner: &impl Interner,
    ) -> Interned<Type> {
        rewrite_type(
            ty,
            &mut InstantiationRewriter { instantiation: self },
            interner,
        )
    }
}
