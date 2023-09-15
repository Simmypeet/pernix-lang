use pernixc_syntax::syntax_tree;
use pernixc_system::diagnostic::Handler;

use super::{
    state::{DraftedSymbolRef, DraftedSymbolSyntax},
    Table,
};
use crate::{
    error::{self, CyclicDependency},
    symbol::{GenericItemRef, GlobalItem, Index},
};

mod generic;

impl Table {
    #[must_use]
    pub(super) fn finalize_symbol_if_required(
        &mut self,
        drafted_symbol_ref: DraftedSymbolRef,
        handler: &impl Handler<error::Error>,
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
                    participant_spans: symbol_refs
                        .into_iter()
                        .filter_map(|x| self.get_global_item(x.into()).and_then(GlobalItem::span))
                        .collect(),
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
        struct_index: Index,
        syntax: syntax_tree::item::Struct,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        let (.., signature, body) = syntax.dissolve();
        let (.., identifier, generic_parameters, where_clause) = signature.dissolve();

        self.finalize_generic(
            GenericItemRef::Struct(struct_index),
            generic_parameters,
            where_clause,
            handler,
        );

        self.state_mananger
            .mark_as_done(DraftedSymbolRef::Struct(struct_index));

        true
    }
}
