//! Crate responsible for declaring symbols from the syntax tree.

use std::{
    hash::{Hash as _, Hasher as _},
    sync::{mpsc::RecvTimeoutError, Arc},
};

use dashmap::DashMap;
use flexstr::SharedStr;
use parking_lot::RwLock;
use pernixc_hash::{HashMap, HashSet};
use pernixc_module_tree::{get_module_tree, ModuleTree};
use pernixc_query::{TrackedEngine, Value};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_syntax::AccessModifier;
use pernixc_target::TargetID;

use crate::{accessibility::Accessibility, kind::Kind};

pub mod accessibility;
pub mod kind;
pub mod member;
pub mod name;

/// Represents a unique identifier for the symbols in the compilation target.
/// This ID is only unique within the context of a single target. If wants to
/// use identifier across multiple targets, it should be combined with the
/// [`Global`]
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct ID(pub u64);

impl ID {
    /// The constant symbol ID that is fixed to zero for every target. It
    /// represents the root module of the target.
    pub const ROOT_MODULE: Self = Self(0);
}

#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct Entry {
    pub kind: kind::Kind,
    pub name: SharedStr,
    pub members: Option<Arc<member::Member>>,
    pub accessibility: Option<accessibility::Accessibility<ID>>,
}

/// The final result of building the symbol table. It contains all the
/// symbols that were declared in the compilation target. The symbols are
/// stored in the form of [`Entry`]s, which are indexed by their unique
/// [`ID`].
#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize, Value,
)]
#[value(Arc<SymbolTable>)]
#[id(TargetID)]
pub struct SymbolTable {
    pub entries_by_id: HashMap<ID, Entry>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Executor;

impl pernixc_query::runtime::executor::Executor<Key> for Executor {
    fn execute(
        &self,
        engine: &pernixc_query::TrackedEngine,
        key: &Key,
    ) -> Result<Arc<SymbolTable>, pernixc_query::runtime::executor::CyclicError>
    {
        let module_tree = match engine.get_module_tree(key.0) {
            Ok(module_tree) => module_tree,

            Err(_) => {
                // creates an empty symbol table
                return Ok(Arc::new(SymbolTable {
                    entries_by_id: HashMap::default(),
                }));
            }
        };

        todo!()
    }
}

struct Context<'a> {
    engine: &'a TrackedEngine<'a>,
    entries: &'a DashMap<ID, Entry>,
    imports_by_global_id: &'a pernixc_hash::DashMap<
        ID,
        Vec<pernixc_syntax::item::module::Import>,
    >,
}

impl Context<'_> {
    fn create_module(
        &self,
        name: SharedStr,
        syntax_tree: ModuleTree,
        parent_module_id: Option<ID>,
        parent_names: &[SharedStr],
    ) -> ID {
        // the id that will be assigned to the module
        let mut current_module_names = parent_names.to_vec();
        current_module_names.push(name.clone());

        let entry = Entry {
            kind: Kind::Module,
            name: name.clone(),
            members: None,
            accessibility: Some(match syntax_tree.access_modifier {
                Some(AccessModifier::Internal(_)) => {
                    Accessibility::Scoped(ID::ROOT_MODULE)
                }

                Some(AccessModifier::Private(_)) => Accessibility::Scoped(
                    parent_module_id.unwrap_or(ID::ROOT_MODULE),
                ),

                None | Some(AccessModifier::Public(_)) => Accessibility::Public,
            }),
        };

        let current_module_id = if let Some(id) = parent_module_id {
            let id = generate_id(
                parent_names.iter().map(flexstr::FlexStr::as_str),
                &name,
                self.entries,
                entry,
            );

            id
        } else {
            let id = ID::ROOT_MODULE;
            assert!(self.entries.insert(id, entry).is_none());
            id
        };

        // make sure atleast has an empty import list
        self.imports_by_global_id.entry(current_module_id).or_default();
    }
}

fn generate_id<'a>(
    parent_names: impl IntoIterator<Item = &'a str>,
    this_name: &str,
    generated_ids: &DashMap<ID, Entry>,
    entry: Entry,
) -> ID {
    let mut hasher = siphasher::sip::SipHasher24::default();

    for name in parent_names {
        // hash the name of the symbol
        name.hash(&mut hasher);
    }
    this_name.hash(&mut hasher);

    // encode attempts to the hasher
    let mut attempt = 0;
    loop {
        let mut attempt_hasher = hasher;
        attempt.hash(&mut attempt_hasher);

        let id = ID(attempt_hasher.finish());

        match (generated_ids.entry(id), id == ID::ROOT_MODULE) {
            (dashmap::Entry::Vacant(_), true)
            | (dashmap::Entry::Occupied(_), _) => {
                attempt += 1;
            }

            (dashmap::Entry::Vacant(vacant_entry), false) => {
                // insert the entry into the map
                vacant_entry.insert(entry.clone());

                return id;
            }
        }
    }
}
