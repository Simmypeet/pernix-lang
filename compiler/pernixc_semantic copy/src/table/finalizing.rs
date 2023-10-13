use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{
    state::{DraftedSymbolRef, DraftedSymbolSyntax},
    Table,
};
use crate::{
    error::{self, CyclicDependency},
    symbol::{GenericItemRef, StructRef},
};

mod generic;

impl Table {
    #[must_use]
    pub(super) fn finalize_symbol_if_required(
        &mut self,
        drafted_symbol_ref: DraftedSymbolRef,
        handler: &dyn Handler<error::Error>,
    ) -> bool {
        if self
            .state_mananger
            .get_item_syntax_mut(drafted_symbol_ref)
            .is_none()
        {
            return true;
        }

        let syntax = match self.state_mananger.mark_as_constructing(drafted_symbol_ref) {
            Ok(syntax) => syntax,

            Err(symbol_refs) => {
                handler.receive(error::Error::CyclicDependency(CyclicDependency {
                    participant_refs: symbol_refs.into_iter().map(Into::into).collect(),
                }));
                return false;
            }
        };

        match (drafted_symbol_ref, syntax) {
            (
                DraftedSymbolRef::Struct(struct_index),
                DraftedSymbolSyntax::Struct { struct_syntax },
            ) => self.finalize_struct(struct_index, struct_syntax, handler),

            _ => unreachable!("invalid symbol type"),
        }

        // The finalize function must mark the symbol as finalized by themselves.
    }

    pub(super) fn finalize_struct(
        &mut self,
        struct_ref: StructRef,
        syntax: syntax_tree::item::Struct,
        handler: &dyn Handler<error::Error>,
    ) -> bool {
        let (.., signature, body) = syntax.dissolve();
        let (.., identifier, generic_parameters, where_clause) = signature.dissolve();

        self.finalize_generic(
            GenericItemRef::Struct(struct_ref),
            generic_parameters,
            where_clause,
            handler,
        );

        self.state_mananger
            .mark_as_done(DraftedSymbolRef::Struct(struct_ref));

        true
    }
}
