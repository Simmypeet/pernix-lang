use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::marker;
use crate::{
    arena::ID,
    error::{self, NonFinalMarkerImplementation},
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::Finalize, utility::occurrences::Occurrences,
                    Finalizer,
                },
                RwLockContainer,
            },
            Building, Table,
        },
        NegativeMarkerImplementation,
    },
};

/// Generic parameters are built
pub const GENERIC_PARAMETER_STATE: usize = 0;

/// The where clause of the trait implementation is built.
pub const WHERE_CLAUSE_STATE: usize = 1;

/// The generic arguments of the implementation are built.
pub const ARGUMENT_STATE: usize = 2;

/// Bounds check are performed
pub const CHECK_STATE: usize = 3;

impl Finalize for NegativeMarkerImplementation {
    type SyntaxTree = syntax_tree::item::ImplementationSignature;
    const FINAL_STATE: usize = CHECK_STATE;
    type Data = (Occurrences, Occurrences, Occurrences);

    fn finalize(
        table: &Table<Building<RwLockContainer, Finalizer>>,
        symbol_id: ID<Self>,
        state_flag: usize,
        syntax_tree: &Self::SyntaxTree,
        (
            generic_parameter_occurrences,
            where_clause_occurrences,
            argument_occurrences,
        ): &mut Self::Data,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        table.build_implementation(
            symbol_id,
            state_flag,
            GENERIC_PARAMETER_STATE,
            WHERE_CLAUSE_STATE,
            ARGUMENT_STATE,
            CHECK_STATE,
            syntax_tree.generic_parameters().as_ref(),
            syntax_tree.where_clause().as_ref(),
            syntax_tree.qualified_identifier(),
            generic_parameter_occurrences,
            where_clause_occurrences,
            argument_occurrences,
            marker::GENERIC_PARAMETER_STATE,
            marker::WHERE_CLAUSE_STATE,
            handler,
        );

        if state_flag == CHECK_STATE {
            // marker implementation must always be final
            if syntax_tree.final_keyword().is_none() {
                handler.receive(Box::new(NonFinalMarkerImplementation {
                    implementation_span: syntax_tree
                        .implements_keyword()
                        .span
                        .join(&syntax_tree.qualified_identifier().span())
                        .unwrap(),
                }));
            }
        }
    }
}
