//! Contains codes related to finalizing the table.
//!
//! Finalizing symbols occurs when the symbol is being used i.e.resolved, in the code. The symbol
//! must be finalized before it can be used in order to to make it semantically valid. We must
//! also check for cyclic dependency while finalizing the symbol.

use std::sync::Arc;

use parking_lot::Mutex;
use paste::paste;
use pernixc_base::{diagnostic::Handler, source_file::Span};
use pernixc_syntax::syntax_tree;

use super::{
    resolution::{Checking, Config},
    state::{self, Constructing, State},
    IndexMut, Table,
};
use crate::{
    arena::ID,
    entity::{constant, r#type, region::Region},
    error::{self, CyclicDependency},
    symbol::{Constant, Enum, Function, GenericID, GlobalID, Struct, Symbolic, Trait, Type},
    table::state::ConstructingLock,
};

/// A struct which composed of the required checking and the span where the checking occurs.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct CheckingWithSpan {
    checking: Checking<Symbolic>,
    span: Span,
}

/// Is a struct implementing [`Config`] trait that stores the checking and the span where the
/// checking occurs.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Storage<'a> {
    checkings: &'a mut Vec<CheckingWithSpan>,
}

impl Config<Symbolic> for Storage<'_> {
    fn region_arguments_placeholder(&mut self) -> Option<Region<Symbolic>> { None }

    fn type_arguments_placeholder(&mut self) -> Option<r#type::Type<Symbolic>> { None }

    fn constant_arguments_placeholder(&mut self) -> Option<constant::Constant<Symbolic>> { None }

    fn check(&mut self, checking: Checking<Symbolic>, span: Span) {
        self.checkings.push(CheckingWithSpan { checking, span })
    }
}

impl Table {
    fn create_generic_parameter(
        &self,
        generic_symbol: GenericID,
        syntax_tree: Option<syntax_tree::item::GenericParameters>,
        config: &mut dyn Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_struct(
        &self,
        struct_id: ID<Struct>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Struct,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_enum(
        &self,
        enum_id: ID<Enum>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Enum,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_constant(
        &self,
        constant_id: ID<Constant>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Constant,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_type(
        &self,
        type_id: ID<Type>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Type,
        handler: &dyn Handler<error::Error>,
    ) {
        let mut checking_with_spans = Vec::new();
        let mut storage = Storage {
            checkings: &mut checking_with_spans,
        };

        let Ok(ty) = self.resolve_type(
            syntax_tree.definition().ty(),
            type_id.into(),
            &mut storage,
            handler,
        ) else {
            return;
        };

        self.get_mut(type_id).unwrap().r#type = ty;
    }

    fn finalize_function(
        &self,
        function_id: ID<Function>,
        constructing_lock: ConstructingLock,
        syntax_tree: syntax_tree::item::Function,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    fn finalize_trait(
        &self,
        trait_id: ID<Trait>,
        constructing_lock: ConstructingLock,
        syntax_tree: state::Trait,
        handler: &dyn Handler<error::Error>,
    ) {
    }

    /// Returns `false` if found a cyclic dependency while finalizing.
    #[must_use]
    #[allow(clippy::significant_drop_tightening, clippy::too_many_lines)]
    pub(super) fn finalize(
        &self,
        global_id: GlobalID,
        referring_site: Option<GlobalID>,
        handler: &dyn Handler<error::Error>,
    ) -> bool {
        /*
        This is one of the most complicated part of the compiler.
        Hopefully, it's correct because I'm not sure about it.
         */
        macro_rules! remove_dependency {
            ($state_manager:ident) => {
                paste! {
                    if let Some(referring_site) = referring_site {
                        $state_manager
                            .with_upgraded(|x| x.dependencies.remove(&referring_site));
                    }
                }
            };
        }

        macro_rules! match_finalize {
            ($state_manager:ident, $($kind:ident),*) => {
                match global_id {
                    $(
                        GlobalID::$kind(global_id) => {paste! {
                            if $state_manager
                                .[<states_by_ $kind:lower _id>]
                                .get(&global_id)
                                .is_none() {
                                remove_dependency!($state_manager);
                                return true;
                            }

                            let result = $state_manager.with_upgraded(|state_manager| {
                                let Some(state) = state_manager
                                    .[<states_by_ $kind:lower _id>]
                                    .get_mut(&global_id)
                                else {
                                    panic!("should've exited above");
                                };

                                match state {
                                    drafted @ State::Drafted(..) => {
                                        let mut inner_drafted = None;
                                        let waiting_lock = Arc::new(Mutex::new(()));

                                        take_mut::take(drafted, |drafted| {
                                            inner_drafted = Some(drafted.into_drafted().unwrap());

                                            State::Constructing(Constructing {
                                                waiting_lock: waiting_lock.clone(),
                                            })
                                        });


                                        Ok((waiting_lock, inner_drafted.unwrap()))
                                    }
                                    State::Constructing(constructing) =>
                                        Err(constructing
                                            .waiting_lock
                                            .clone()),
                                }
                            });

                            // preven deadlock
                            drop($state_manager);

                            match result {
                                Ok((waiting_lock, inner_drafted)) => {
                                    let constructing_lock = ConstructingLock::new(
                                        global_id.into(),
                                        waiting_lock.lock(),
                                        self,
                                    );


                                    self.[<finalize_ $kind:lower>](
                                        global_id,
                                        constructing_lock,
                                        inner_drafted,
                                        handler
                                    );

                                    // remove dependency again
                                    if let Some(referring_site) = referring_site {
                                        self
                                            .state_manager
                                            .write()
                                            .dependencies.
                                            remove(&referring_site);
                                    }

                                    return true;
                                }
                                Err(constructing) => {
                                    constructing
                                }
                            }
                        }},
                    )*
                    _ => {
                        remove_dependency!($state_manager);
                        return true
                    },
                }
            };
        }

        let mut state_manager = self.state_manager.upgradable_read();

        if let Some(referring_site) = referring_site {
            // dependency cyclic check starts here
            let mut dependency_stack = vec![global_id];
            let mut current_node = global_id;

            while let Some(dependant) = state_manager.dependencies.get(&current_node).copied() {
                dependency_stack.push(dependant);

                // cyclic dependency found
                if dependant == referring_site {
                    for participant in &dependency_stack {
                        state_manager.with_upgraded(|x| x.dependencies.remove(participant));
                    }

                    handler.receive(error::Error::CyclicDependency(CyclicDependency {
                        participants: dependency_stack,
                    }));

                    return false;
                }

                current_node = dependant;
            }

            // no cyclic dependency, add it to the dependency graph
            state_manager.with_upgraded(|x| x.dependencies.insert(referring_site, global_id));
        }

        let waiting_lock =
            match_finalize!(state_manager, Enum, Struct, Constant, Type, Function, Trait);

        drop(waiting_lock.lock());

        if let Some(referring_site) = referring_site {
            self.state_manager
                .write()
                .dependencies
                .remove(&referring_site);
        }

        true
    }
}
