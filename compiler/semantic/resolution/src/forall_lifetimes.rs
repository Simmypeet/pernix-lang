//! Contains the definition of [`create_forall_lifetimes`].

use std::{collections::hash_map::Entry, hash::BuildHasher};

use flexstr::SharedStr;
use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_term::lifetime::{self, Forall, Lifetime};

use crate::diagnostic::ForallLifetimeRedefinition;

/// Creates the forall lifetimes from the given syntax tree and inserts them
/// into the given namespace.
pub fn create_forall_lifetimes(
    namespace: &mut std::collections::HashMap<
        SharedStr,
        Lifetime,
        impl BuildHasher,
    >,
    syntax_tree: &pernixc_syntax::predicate::HigherRankedLifetimes,
    handler: &dyn Handler<ForallLifetimeRedefinition>,
) {
    for syn in syntax_tree
        .lifetimes()
        .into_iter()
        .flat_map(|x| x.lifetimes().collect::<Vec<_>>())
    {
        let Some(identifier) = syn.identifier() else {
            continue;
        };

        match namespace.entry(identifier.kind.0.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(Lifetime::Forall(Forall::Named(
                    lifetime::NamedForall::new(
                        identifier.span,
                        identifier.kind.0,
                    ),
                )));
            }
            Entry::Occupied(_) => {
                handler.receive(ForallLifetimeRedefinition {
                    redefinition_span: syn.span(),
                });
            }
        }
    }
}
