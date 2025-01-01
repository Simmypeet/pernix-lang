//! Contains the input components for the symbol table.

use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
};

use accessibility::Accessibility;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_lexical::token::Identifier;
use pernixc_syntax::syntax_tree::{
    item::{Item, UsingKind},
    target::ModuleTree,
    AccessModifier, ConnectedList, SimplePath, SimplePathRoot,
};
use serde::{Deserialize, Serialize};
use syntax_tree::FunctionKind;

use crate::{
    arena::ID,
    error::{
        self, ConflictingUsing, ExpectModule, ItemRedifinition,
        SymbolIsMoreAccessibleThanParent, SymbolNotFound,
    },
    symbol::{
        Constant, Enum, Function, Global, HierarchyRelationship, ItemID,
        Marker, Module, Struct, TargetID, Trait, TraitMemberID, Type,
    },
};

pub mod accessibility;
pub mod implemented;
pub mod implements;
pub mod import;
pub mod member;
pub mod parent;
pub mod syntax_tree;

/// A tag struct used for signifying that the input component is required for
/// the symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct Required;

/// A tag struct used for signifying that the input component is optional for
/// the symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct Optional;

/// A trait used for retrieving the input components forr a particular symbol.
trait Input<T> {
    /// If true the component must be present in the map for the symbol.
    type Requirement;

    /// The ID type used for accessing the input componenet.
    type ID: From<ID<Self>> + Eq + Hash + 'static;

    fn get_map(representation: &Map) -> &HashMap<Global<Self::ID>, T>;

    fn get_map_mut(
        representation: &mut Map,
    ) -> &mut HashMap<Global<Self::ID>, T>;
}

/// Contains the input components information required for building the full
/// table.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Map {
    symbol_names: HashMap<Global<ItemID>, String>,
    root_module_ids_by_target_id: HashMap<TargetID, ID<Module>>,
    target_ids_by_name: HashMap<String, TargetID>,

    /// The syntax tree map won't be serialized
    #[serde(skip)]
    syntax_tree: syntax_tree::Map,
    accessibility: accessibility::Map,
    parent: parent::Map,
    member: member::Map,
    import: import::Map,
    implements: implements::Map,
    implemented: implemented::Map,
}

impl Map {
    /// Gets the input component of a particular symbol.
    #[allow(private_bounds)]
    pub fn get_input<C, T: Input<C, Requirement = Required>>(
        &self,
        global_id: Global<ID<T>>,
    ) -> &C {
        T::get_map(self)
            .get(&global_id.map(Into::into))
            .unwrap_or_else(|| panic!("{global_id:?} not found"))
    }

    /// Gets the mutable input component of a particular symbol.
    fn get_input_mut<C, T: Input<C, Requirement = Required>>(
        &mut self,
        global_id: Global<ID<T>>,
    ) -> &mut C {
        T::get_map_mut(self)
            .get_mut(&global_id.map(Into::into))
            .unwrap_or_else(|| panic!("{global_id:?} not found"))
    }

    /// Inserts the input component of a particular symbol.
    ///
    /// # Panics
    ///
    /// If the global ID is already used.
    fn insert_input<C, T: Input<C>>(
        &mut self,
        global_id: Global<ID<T>>,
        component: C,
    ) {
        assert!(T::get_map_mut(self)
            .insert(global_id.map(Into::into), component)
            .is_none());
    }

    /// Tries to get the optional input component of a particular symbol.
    #[allow(private_bounds)]
    pub fn try_get_input<C, T: Input<C, Requirement = Optional> + 'static>(
        &self,
        global_id: Global<ID<T>>,
    ) -> Option<&C> {
        T::get_map(self).get(&global_id.map(Into::into))
    }
}

impl Map {
    /// Builds the [`Input`] information.
    ///
    /// # Panics
    ///
    /// If the `target_id` is already used.
    pub fn build(
        &mut self,
        target_syn: pernixc_syntax::syntax_tree::target::Target,
        target_id: usize,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // make sure the target_id is not used
        assert!(self.symbol_names.keys().all(|x| x
            .target_id
            .as_normal()
            .map_or(false, |x| *x != target_id)));

        let (syntax_tree, name) = target_syn.dissolve();

        let mut usings_by_module_id = HashMap::new();
        let mut implementations_by_module_id = HashMap::new();

        self.create_module(
            &mut (0..),
            target_id,
            name,
            syntax_tree,
            None,
            &mut usings_by_module_id,
            &mut implementations_by_module_id,
            handler,
        );

        dbg!(&usings_by_module_id);
        dbg!(&implementations_by_module_id);

        self.insert_usings(target_id, usings_by_module_id, handler);
    }

    /// Determines whether the given `referred` is accessible from the
    /// `referring_site` as if the `referred` has the given
    /// `referred_accessibility`.
    ///
    /// # Returns
    ///
    /// Returns `None` if `referred` or `referring_site` is not a valid ID.
    #[must_use]
    pub fn is_accessible_from(
        &self,
        referring_site: Global<ItemID>,
        referred_target_id: TargetID,
        referred_accessibility: Accessibility,
    ) -> bool {
        match referred_accessibility {
            Accessibility::Public => true,

            Accessibility::Scoped(module_id) => {
                if referring_site.target_id != referred_target_id {
                    return false;
                }

                let referring_site_module_id =
                    self.get_closet_module_id(referring_site);

                matches!(
                    self.symbol_hierarchy_relationship(
                        referred_target_id,
                        module_id.into(),
                        referring_site_module_id.into(),
                    ),
                    HierarchyRelationship::Parent
                        | HierarchyRelationship::Equivalent
                )
            }
        }
    }

    /// Checks if the `referred` is accessible from the `referring_site`.
    ///
    /// # Returns
    ///
    /// Returns `None` if `referred` or `referring_site` is not a valid ID.
    #[must_use]
    pub fn symbol_accessible(
        &self,
        referring_site: Global<ItemID>,
        referred: Global<ItemID>,
    ) -> bool {
        let referred_accessibility = self.get_accessibility(referred);

        self.is_accessible_from(
            referring_site,
            referred.target_id,
            referred_accessibility,
        )
    }

    /// Searches for a member with the given name in the given item ID.
    #[must_use]
    pub fn get_member_of(
        &self,
        item_id: Global<ItemID>,
        member_name: &str,
    ) -> Option<Global<ItemID>> {
        match item_id.id {
            ItemID::Module(id) => self
                .get_input::<member::Member<_>, _>(Global::new(
                    item_id.target_id,
                    id,
                ))
                .get(member_name)
                .copied()
                .map(|x| Global::new(item_id.target_id, x.into())),
            ItemID::Struct(id) => self
                .get_input::<implemented::Implemented<_>, _>(Global::new(
                    item_id.target_id,
                    id,
                ))
                .iter()
                .copied()
                .find_map(|x| {
                    self.get_input::<member::Member<_>, _>(x)
                        .get(member_name)
                        .copied()
                        .map(|y| Global::new(x.target_id, y.into()))
                }),
            ItemID::Trait(id) => self
                .get_input::<member::Member<_>, _>(Global::new(
                    item_id.target_id,
                    id,
                ))
                .get(member_name)
                .copied()
                .map(|x| Global::new(item_id.target_id, x.into())),
            ItemID::PositiveTraitImplementation(id) => self
                .get_input::<member::Member<_>, _>(Global::new(
                    item_id.target_id,
                    id,
                ))
                .get(member_name)
                .copied()
                .map(|x| Global::new(item_id.target_id, x.into())),
            ItemID::AdtImplementation(id) => self
                .get_input::<member::Member<_>, _>(Global::new(
                    item_id.target_id,
                    id,
                ))
                .get(member_name)
                .copied()
                .map(|x| Global::new(item_id.target_id, x.into())),
            ItemID::Enum(id) => {
                // search variant first then implementation
                self.get_input::<member::Member<_>, _>(Global::new(
                    item_id.target_id,
                    id,
                ))
                .get(member_name)
                .copied()
                .map(|x| Global::new(item_id.target_id, x.into()))
                .or_else(|| {
                    self.get_input::<implemented::Implemented<_>, _>(
                        Global::new(item_id.target_id, id),
                    )
                    .iter()
                    .copied()
                    .find_map(|x| {
                        self.get_input::<member::Member<_>, _>(x)
                            .get(member_name)
                            .copied()
                            .map(|y| Global::new(x.target_id, y.into()))
                    })
                })
            }

            ItemID::Type(_)
            | ItemID::Constant(_)
            | ItemID::Function(_)
            | ItemID::Variant(_)
            | ItemID::TraitType(_)
            | ItemID::TraitFunction(_)
            | ItemID::TraitConstant(_)
            | ItemID::NegativeTraitImplementation(_)
            | ItemID::TraitImplementationFunction(_)
            | ItemID::TraitImplementationType(_)
            | ItemID::TraitImplementationConstant(_)
            | ItemID::AdtImplementationFunction(_)
            | ItemID::Marker(_)
            | ItemID::PositiveMarkerImplementation(_)
            | ItemID::NegativeMarkerImplementation(_) => None,
        }
    }

    /// Returns the [`Module`] ID that is closest to the given [`ItemID`]
    /// (including itself).
    ///
    /// # Panics
    ///
    /// If the given [`ItemID`] is not a module.
    #[must_use]
    pub fn get_closet_module_id(
        &self,
        mut item_id: Global<ItemID>,
    ) -> ID<Module> {
        // including the item_id itself
        loop {
            if let ItemID::Module(module_id) = item_id.id {
                return module_id;
            }

            item_id = Global::new(
                item_id.target_id,
                self.get_parent_item_id(item_id)
                    .expect("should've found at least one module"),
            );
        }
    }

    /// Gets the parent [`ItemID`] of the given item symbol.
    ///
    /// If [`None`] is returned, it means the item symbol is a root module
    /// symbol.
    ///
    /// # Panics
    ///
    /// If the `item_id` is not a valid item symbol ID.
    #[must_use]
    pub fn get_parent_item_id(
        &self,
        item_id: Global<ItemID>,
    ) -> Option<ItemID> {
        macro_rules! impl_get_parent_item_id {
            ($(($name:ident, $test:ident)),*) => {
                match item_id.id {
                    $(
                        ItemID::$name(id) => impl_match_arm!($name, id, $test),
                    )*
                }
            };
        }

        macro_rules! impl_match_arm {
            ($name:ident, $id:ident, true) => {
                self.try_get_input::<parent::Parent<_>, _>(Global::new(
                    item_id.target_id,
                    $id,
                ))
                .map(|x| x.0.into())
            };

            ($name:ident, $id:ident, false) => {
                Some(
                    self.get_input::<parent::Parent<_>, _>(Global::new(
                        item_id.target_id,
                        $id,
                    ))
                    .0
                    .into(),
                )
            };
        }

        let parent_item_id: Option<ItemID> = impl_get_parent_item_id!(
            (Module, true),
            (Struct, false),
            (Enum, false),
            (Trait, false),
            (Type, false),
            (Constant, false),
            (Function, false),
            (Variant, false),
            (TraitType, false),
            (TraitFunction, false),
            (TraitConstant, false),
            (PositiveTraitImplementation, false),
            (NegativeTraitImplementation, false),
            (TraitImplementationFunction, false),
            (TraitImplementationType, false),
            (TraitImplementationConstant, false),
            (AdtImplementation, false),
            (AdtImplementationFunction, false),
            (Marker, false),
            (PositiveMarkerImplementation, false),
            (NegativeMarkerImplementation, false)
        );

        parent_item_id
    }

    /// Creates the [`Accessibility`] based on where the symbol is defined in
    /// and the access modifier syntax tree.
    ///
    /// # Panics
    ///
    /// If the parent item ID is not found.
    #[must_use]
    pub fn create_accessibility(
        &self,
        parent_id: Global<ItemID>,
        access_modifier: &AccessModifier,
    ) -> accessibility::Accessibility {
        match access_modifier {
            AccessModifier::Public(_) => Accessibility::Public,
            AccessModifier::Private(_) => {
                let parent_module_id = self.get_closet_module_id(parent_id);

                Accessibility::Scoped(parent_module_id)
            }
            AccessModifier::Internal(_) => Accessibility::Scoped(
                self.root_module_ids_by_target_id[&parent_id.target_id],
            ),
        }
    }

    /// Computes the [`HierarchyRelationship`] between the two given item IDs.
    ///
    /// The returned [`HierarchyRelationship`] is based on the `first` symbol.
    ///
    /// # Returns
    ///
    /// Returns `None` if either `first` or `second` is not a valid ID.
    #[must_use]
    pub fn symbol_hierarchy_relationship(
        &self,
        target_id: TargetID,
        first: ItemID,
        second: ItemID,
    ) -> HierarchyRelationship {
        // the two symbols are the same.
        if first == second {
            return HierarchyRelationship::Equivalent;
        }

        for first_parent in self.scope_walker(Global::new(target_id, first)) {
            if first_parent == second {
                return HierarchyRelationship::Child;
            }
        }

        for second_parent in self.scope_walker(Global::new(target_id, second)) {
            if second_parent == first {
                return HierarchyRelationship::Parent;
            }
        }

        HierarchyRelationship::Unrelated
    }

    /// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the
    /// given [`ItemID`].
    ///
    /// See [`ScopeWalker`] for more information.
    #[must_use]
    pub fn scope_walker(&self, item_id: Global<ItemID>) -> ScopeWalker {
        ScopeWalker {
            map: self,
            current_id: Some(item_id.id),
            target_id: item_id.target_id,
        }
    }

    /// Computes the [`HierarchyRelationship`] between the two given
    /// accessibilities.
    ///
    /// The returned [`HierarchyRelationship`] is based on the `first`
    /// accessibility.
    ///
    /// # Returns
    ///
    /// Returns `None` if either `first` or `second` contains an invalid
    /// module ID.
    #[must_use]
    pub fn accessibility_hierarchy_relationship(
        &self,
        target_id: TargetID,
        first: Accessibility,
        second: Accessibility,
    ) -> HierarchyRelationship {
        match (first, second) {
            (Accessibility::Public, Accessibility::Public) => {
                HierarchyRelationship::Equivalent
            }
            (Accessibility::Public, Accessibility::Scoped(_)) => {
                HierarchyRelationship::Parent
            }
            (Accessibility::Scoped(_), Accessibility::Public) => {
                HierarchyRelationship::Child
            }
            (Accessibility::Scoped(first), Accessibility::Scoped(second)) => {
                self.symbol_hierarchy_relationship(
                    target_id,
                    first.into(),
                    second.into(),
                )
            }
        }
    }

    /// Gets the accessibility of the given [`ItemID`].
    ///
    /// # Errors
    ///
    /// Returns `None` if the given [`ItemID`] is not a valid ID.
    ///
    /// # Panics
    ///
    /// If the given [`ItemID`] is not a valid ID.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn get_accessibility(&self, item_id: Global<ItemID>) -> Accessibility {
        macro_rules! impl_match_arm {
            ($id:ident, $exp:expr) => {
                $exp
            };

            ($id:ident) => {
                *self.get_input::<accessibility::Accessibility, _>(Global::new(
                    item_id.target_id,
                    $id,
                ))
            };
        }

        macro_rules! impl_get_accessibility {
            ($(($name:ident, $id:ident $(, $exp:expr)?)),*) => {
                match item_id.id {
                    $(
                        ItemID::$name($id)
                            => impl_match_arm!($id $(, $exp)?),
                    )*
                }
            };
        }

        impl_get_accessibility!(
            (Module, _a),
            (Struct, _a),
            (Enum, _a),
            (Trait, _a),
            (Type, _a),
            (Constant, _a),
            (Function, _a),
            (
                Variant,
                _a,
                self.get_accessibility(Global::new(
                    item_id.target_id,
                    self.get_parent_item_id(item_id)
                        .expect("should've parent enum")
                ))
            ),
            (TraitType, _a),
            (TraitFunction, _a),
            (TraitConstant, _a),
            (PositiveTraitImplementation, id, {
                let trait_id =
                    **self.get_input::<implements::Implements<_>, _>(
                        Global::new(item_id.target_id, id),
                    );

                self.get_accessibility(Global::new(
                    item_id.target_id,
                    trait_id.into(),
                ))
            }),
            (NegativeTraitImplementation, id, {
                let trait_id =
                    **self.get_input::<implements::Implements<_>, _>(
                        Global::new(item_id.target_id, id),
                    );

                self.get_accessibility(Global::new(
                    item_id.target_id,
                    trait_id.into(),
                ))
            }),
            (TraitImplementationFunction, _a, {
                self.get_accessibility(Global::new(
                    item_id.target_id,
                    self.get_parent_item_id(item_id)
                        .expect("should've trait implementation parent"),
                ))
            }),
            (TraitImplementationType, _a, {
                self.get_accessibility(Global::new(
                    item_id.target_id,
                    self.get_parent_item_id(item_id)
                        .expect("should've trait implementation parent"),
                ))
            }),
            (TraitImplementationConstant, _a, {
                self.get_accessibility(Global::new(
                    item_id.target_id,
                    self.get_parent_item_id(item_id)
                        .expect("should've trait implementation parent"),
                ))
            }),
            (AdtImplementation, id, {
                let adt_id = self
                    .implements
                    .adt_implementations
                    .get(&Global::new(item_id.target_id, id))
                    .unwrap()
                    .0;
                self.get_accessibility(Global::new(
                    item_id.target_id,
                    adt_id.into(),
                ))
            }),
            (AdtImplementationFunction, _a),
            (Marker, _a),
            (PositiveMarkerImplementation, id, {
                let marker_id =
                    **self.get_input::<implements::Implements<_>, _>(
                        Global::new(item_id.target_id, id),
                    );

                self.get_accessibility(Global::new(
                    item_id.target_id,
                    marker_id.into(),
                ))
            }),
            (NegativeMarkerImplementation, id, {
                let marker_id =
                    **self.get_input::<implements::Implements<_>, _>(
                        Global::new(item_id.target_id, id),
                    );

                self.get_accessibility(Global::new(
                    item_id.target_id,
                    marker_id.into(),
                ))
            })
        )
    }

    /// Resolves a [`SimplePath`] as a [`ItemID`].
    pub fn resolve_simple_path(
        &self,
        simple_path: &SimplePath,
        referring_site: Global<ItemID>,
        start_from_root: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Option<Global<ItemID>> {
        let root: Global<ItemID> = match simple_path.root() {
            SimplePathRoot::Target(_) => Global::new(
                referring_site.target_id,
                self.root_module_ids_by_target_id[&referring_site.target_id]
                    .into(),
            ),
            SimplePathRoot::Identifier(ident) => {
                if start_from_root {
                    let Some(id) =
                        self.target_ids_by_name.get(ident.span.str()).copied()
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: None,
                            resolution_span: ident.span.clone(),
                        }));

                        return None;
                    };

                    Global::new(
                        referring_site.target_id,
                        self.root_module_ids_by_target_id[&id].into(),
                    )
                } else {
                    let closet_module_id =
                        self.get_closet_module_id(referring_site);
                    let global_closest_module_id =
                        Global::new(referring_site.target_id, closet_module_id);

                    let Some(id) = self
                        .get_input::<member::Member<_>, _>(
                            global_closest_module_id,
                        )
                        .get(ident.span.str())
                        .map(|x| {
                            Global::new(referring_site.target_id, (*x).into())
                        })
                        .or_else(|| {
                            self.get_input::<import::Import, _>(
                                global_closest_module_id,
                            )
                            .get(ident.span.str())
                            .map(|x| x.module_member_id.map(Into::into))
                        })
                    else {
                        handler.receive(Box::new(SymbolNotFound {
                            searched_item_id: Some(closet_module_id.into()),
                            resolution_span: ident.span.clone(),
                        }));

                        return None;
                    };

                    id
                }
            }
        };

        self.resolve_sequence(
            simple_path.rest().iter().map(|x| &x.1),
            referring_site,
            root,
            handler,
        )
    }

    /// Resolves a sequence of identifier starting of from the given `root`.
    pub fn resolve_sequence<'a>(
        &self,
        simple_path: impl Iterator<Item = &'a Identifier>,
        referring_site: Global<ItemID>,
        root: Global<ItemID>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Option<Global<ItemID>> {
        let mut lastest_resolution = root;
        for identifier in simple_path {
            let Some(new_id) =
                self.get_member_of(lastest_resolution, identifier.span.str())
            else {
                handler.receive(Box::new(SymbolNotFound {
                    searched_item_id: Some(lastest_resolution.id),
                    resolution_span: identifier.span.clone(),
                }));

                return None;
            };

            // non-fatal error, no need to return early
            if !self.symbol_accessible(referring_site, new_id) {
                handler.receive(Box::new(error::SymbolIsNotAccessible {
                    referring_site: referring_site.id,
                    referred: new_id.id,
                    referred_span: identifier.span.clone(),
                }));
            }

            lastest_resolution = new_id;
        }

        Some(lastest_resolution)
    }

    /// Gets the span pointing to the location of the given item ID.
    #[allow(unused)]
    pub fn get_span(&self, item_id: Global<ItemID>) -> Option<Span> {
        macro_rules! impl_get_span_arm {
            ($id:ident, true) => {
                None
            };

            ($id:ident, false) => {
                self.try_get_input::<syntax_tree::SyntaxTree<_>, _>(
                    Global::new(item_id.target_id, $id),
                )
                .map(|x| x.0.span())
            };
        }
        macro_rules! impl_get_span {
            ($(($name:ident, $test:ident)),*) => {
                match item_id.id {
                    $(
                        ItemID::$name(id) => impl_get_span_arm!(id, $test),
                    )*
                }
            };
        }

        impl_get_span!(
            (Module, true),
            (Struct, false),
            (Enum, false),
            (Trait, false),
            (Type, false),
            (Constant, false),
            (Function, false),
            (Variant, false),
            (TraitType, false),
            (TraitFunction, false),
            (TraitConstant, false),
            (PositiveTraitImplementation, false),
            (NegativeTraitImplementation, false),
            (TraitImplementationFunction, false),
            (TraitImplementationType, false),
            (TraitImplementationConstant, false),
            (AdtImplementation, false),
            (AdtImplementationFunction, false),
            (Marker, false),
            (PositiveMarkerImplementation, false),
            (NegativeMarkerImplementation, false)
        )
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It
/// goes through all the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order including the
/// initial symbol.
#[derive(Debug, Clone, Copy)]
pub struct ScopeWalker<'a> {
    map: &'a Map,
    current_id: Option<ItemID>,
    target_id: TargetID,
}

impl<'a> Iterator for ScopeWalker<'a> {
    type Item = ItemID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next_id = self.map.get_parent_item_id(Global::new(
                    self.target_id,
                    current_id,
                ));
                self.current_id = next_id;
                Some(current_id)
            }
            None => None,
        }
    }
}

impl Map {
    fn insert_usings(
        &mut self,
        target_id: usize,
        usings_by_module_id: HashMap<
            ID<Module>,
            Vec<pernixc_syntax::syntax_tree::item::Using>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for (current_module_id, using) in
            usings_by_module_id.into_iter().flat_map(|(module_id, usings)| {
                usings.into_iter().map(move |x| (module_id, x))
            })
        {
            match using.kind() {
                UsingKind::From(from) => {
                    let Some(id) = self.resolve_simple_path(
                        from.from().simple_path(),
                        Global::new(
                            TargetID::Normal(target_id),
                            current_module_id.into(),
                        ),
                        true,
                        handler,
                    ) else {
                        return;
                    };

                    let ItemID::Module(module_id) = id.id else {
                        handler.receive(Box::new(ExpectModule {
                            module_path: from.from().simple_path().span(),
                            found_id: id.id,
                        }));

                        return;
                    };
                    let from_module_id = Global::new(id.target_id, module_id);

                    for import in from
                        .imports()
                        .connected_list()
                        .as_ref()
                        .into_iter()
                        .flat_map(ConnectedList::elements)
                    {
                        let Some(imported_id) = self
                            .get_input::<member::Member<_>, _>(from_module_id)
                            .get(import.identifier().span.str())
                            .copied()
                        else {
                            handler.receive(Box::new(SymbolNotFound {
                                searched_item_id: Some(
                                    from_module_id.id.into(),
                                ),
                                resolution_span: import
                                    .identifier()
                                    .span
                                    .clone(),
                            }));
                            continue;
                        };

                        let name = import.alias().as_ref().map_or_else(
                            || {
                                self.symbol_names
                                    .get(&Global::new(
                                        from_module_id.target_id,
                                        imported_id.into(),
                                    ))
                                    .unwrap()
                                    .clone()
                            },
                            |x| x.identifier().span.str().to_owned(),
                        );

                        if let Some(existing) = self
                            .get_input::<member::Member<_>, _>(Global::new(
                                TargetID::Normal(target_id),
                                current_module_id,
                            ))
                            .get(&name)
                            .map(|x| {
                                self.get_span(Global::new(
                                    TargetID::Normal(target_id),
                                    (*x).into(),
                                ))
                            })
                            .or_else(|| {
                                self.get_input::<import::Import, _>(
                                    Global::new(
                                        TargetID::Normal(target_id),
                                        current_module_id,
                                    ),
                                )
                                .get(&name)
                                .map(|x| x.span.clone())
                            })
                        {
                            handler.receive(Box::new(ConflictingUsing {
                                using_span: import
                                    .alias()
                                    .as_ref()
                                    .map_or_else(
                                        || import.identifier().span(),
                                        SourceElement::span,
                                    ),
                                name: name.clone(),
                                module_id,
                                conflicting_span: existing,
                            }));
                        } else {
                            self.get_input_mut::<import::Import, _>(
                                Global::new(
                                    TargetID::Normal(target_id),
                                    current_module_id,
                                ),
                            )
                            .insert(
                                name,
                                import::Using {
                                    module_member_id: Global::new(
                                        from_module_id.target_id,
                                        imported_id.into(),
                                    ),
                                    span: Some(
                                        import.alias().as_ref().map_or_else(
                                            || import.identifier().span(),
                                            SourceElement::span,
                                        ),
                                    ),
                                },
                            );
                        }
                    }
                }

                UsingKind::One(one) => {
                    let Some(id) = self.resolve_simple_path(
                        one.simple_path(),
                        Global::new(
                            TargetID::Normal(target_id),
                            current_module_id.into(),
                        ),
                        true,
                        handler,
                    ) else {
                        return;
                    };

                    let ItemID::Module(module_id) = id.id else {
                        handler.receive(Box::new(ExpectModule {
                            module_path: one.simple_path().span(),
                            found_id: id.id,
                        }));

                        return;
                    };
                    let global_module_id = Global::new(id.target_id, module_id);

                    let name = one.alias().as_ref().map_or_else(
                        || {
                            self.symbol_names
                                .get(&global_module_id.map(Into::into))
                                .unwrap()
                                .clone()
                        },
                        |x| x.identifier().span.str().to_owned(),
                    );

                    if let Some(existing) = self
                        .get_input::<member::Member<_>, _>(Global::new(
                            TargetID::Normal(target_id),
                            current_module_id,
                        ))
                        .get(&name)
                        .map(|x| {
                            self.get_span(Global::new(
                                TargetID::Normal(target_id),
                                (*x).into(),
                            ))
                        })
                        .or_else(|| {
                            self.get_input::<import::Import, _>(Global::new(
                                TargetID::Normal(target_id),
                                current_module_id,
                            ))
                            .get(&name)
                            .map(|x| x.span.clone())
                        })
                    {
                        handler.receive(Box::new(ConflictingUsing {
                            using_span: one.alias().as_ref().map_or_else(
                                || one.simple_path().span(),
                                SourceElement::span,
                            ),
                            name: name.clone(),
                            module_id,
                            conflicting_span: existing,
                        }));
                    } else {
                        self.get_input_mut::<import::Import, _>(Global::new(
                            TargetID::Normal(target_id),
                            current_module_id,
                        ))
                        .insert(name, import::Using {
                            module_member_id: global_module_id.map(Into::into),
                            span: Some(one.alias().as_ref().map_or_else(
                                || one.simple_path().span(),
                                SourceElement::span,
                            )),
                        });
                    }
                }
            }
        }
    }

    fn create_member_no_accessibility<T, S, P, Tp, Pt>(
        &mut self,
        target_id: usize,
        id_generator: &mut impl Iterator<Item = usize>,
        syntax_tree: S,
        parent_id: ID<P>,
        identifier_fn: impl FnOnce(&S) -> &Identifier,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<T>
    where
        // able to add parent id information
        T: Input<parent::Parent<Tp>> + Input<syntax_tree::SyntaxTree<S>>,
        Tp: From<ID<P>>,

        // able to add member id information,
        P: Input<member::Member<Pt>, Requirement = Required>,
        Pt: From<ID<T>> + Into<ItemID> + Clone,

        ID<T>: Into<ItemID>,
        ID<P>: Into<ItemID>,
    {
        let identifier = identifier_fn(&syntax_tree);

        // add the symbol name
        let new_id = Global::new(
            TargetID::Normal(target_id),
            ID::<T>::new(id_generator.next().unwrap()),
        );

        assert!(self
            .symbol_names
            .insert(new_id.map(Into::into), identifier.span.str().to_owned())
            .is_none());

        // add parent information
        self.insert_input(new_id, parent::Parent(Tp::from(parent_id)));

        // add the member information
        match self
            .get_input_mut::<member::Member<_>, _>(Global::new(
                TargetID::Normal(target_id),
                parent_id,
            ))
            .entry(identifier.span.str().to_owned())
        {
            Entry::Occupied(occupied_entry) => {
                handler.receive(Box::new(ItemRedifinition {
                    existing_item_id: occupied_entry.get().clone().into(),
                    new_item_id: new_id.id.clone().into(),
                    in_item_id: parent_id.into(),
                }));
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(Pt::from(new_id.id));
            }
        }

        // add the syntax tree information
        self.insert_input(new_id, syntax_tree::SyntaxTree(syntax_tree));

        new_id.id
    }

    fn create_member<T, S, P, Tp, Pt>(
        &mut self,
        target_id: usize,
        id_generator: &mut impl Iterator<Item = usize>,
        syntax_tree: S,
        parent_id: ID<P>,
        identifier_fn: impl FnOnce(&S) -> &Identifier,
        accessibility: Accessibility,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<T>
    where
        // able to add parent id information
        T: Input<parent::Parent<Tp>>
            + Input<accessibility::Accessibility>
            + Input<syntax_tree::SyntaxTree<S>>,
        Tp: From<ID<P>>,

        // able to add member id information,
        P: Input<member::Member<Pt>, Requirement = Required>,
        Pt: From<ID<T>> + Into<ItemID> + Clone,

        ID<T>: Into<ItemID>,
        ID<P>: Into<ItemID>,
    {
        let new_id = self.create_member_no_accessibility(
            target_id,
            id_generator,
            syntax_tree,
            parent_id,
            identifier_fn,
            handler,
        );

        // add the accessibility information
        self.insert_input(
            Global::new(TargetID::Normal(target_id), new_id),
            accessibility,
        );

        new_id
    }

    fn create_enum(
        &mut self,
        id_generator: &mut impl Iterator<Item = usize>,
        target_id: usize,
        syntax_tree: pernixc_syntax::syntax_tree::item::Enum,
        parent_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, variant_list, _) = body.dissolve();
        let enum_accessibility = self.create_accessibility(
            Global::new(TargetID::Normal(target_id), parent_module_id.into()),
            &access_modifier,
        );

        let enum_id: ID<Enum> = self.create_member(
            target_id,
            id_generator,
            signature,
            parent_module_id,
            |x| x.identifier(),
            enum_accessibility,
            handler,
        );
        let global_enum_id = Global::new(TargetID::Normal(target_id), enum_id);

        // add the member information for the trait
        assert!(self
            .member
            .enums
            .insert(global_enum_id, member::Member::default())
            .is_none());

        for variant in
            variant_list.into_iter().flat_map(ConnectedList::into_elements)
        {
            let _: ID<crate::symbol::Variant> = self
                .create_member_no_accessibility(
                    target_id,
                    id_generator,
                    variant,
                    enum_id,
                    |x| x.identifier(),
                    handler,
                );
        }
    }

    fn create_trait(
        &mut self,
        id_generator: &mut impl Iterator<Item = usize>,
        target_id: usize,
        syntax_tree: pernixc_syntax::syntax_tree::item::Trait,
        parent_module_id: ID<Module>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let (access_modifier, signature, body) = syntax_tree.dissolve();
        let (_, member_list, _) = body.dissolve();
        let trait_accessibility = self.create_accessibility(
            Global::new(TargetID::Normal(target_id), parent_module_id.into()),
            &access_modifier,
        );

        let trait_id: ID<Trait> = self.create_member(
            target_id,
            id_generator,
            signature,
            parent_module_id,
            |x| x.identifier(),
            trait_accessibility,
            handler,
        );
        let global_trait_id =
            Global::new(TargetID::Normal(target_id), trait_id);

        // add the member information for the trait
        assert!(self
            .member
            .traits
            .insert(global_trait_id, member::Member::default())
            .is_none());

        for trait_member in member_list {
            let trait_member_id = match trait_member {
                pernixc_syntax::syntax_tree::item::TraitMember::Function(
                    syn,
                ) => {
                    let accessibility = self.create_accessibility(
                        global_trait_id.map(Into::into),
                        syn.access_modifier(),
                    );

                    TraitMemberID::Function(self.create_member(
                        target_id,
                        id_generator,
                        syn,
                        trait_id,
                        |x| x.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
                pernixc_syntax::syntax_tree::item::TraitMember::Type(syn) => {
                    let accessibility = self.create_accessibility(
                        global_trait_id.map(Into::into),
                        syn.access_modifier(),
                    );

                    TraitMemberID::Type(self.create_member(
                        target_id,
                        id_generator,
                        syn,
                        trait_id,
                        |x| x.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
                pernixc_syntax::syntax_tree::item::TraitMember::Constant(
                    syn,
                ) => {
                    let accessibility = self.create_accessibility(
                        global_trait_id.map(Into::into),
                        syn.access_modifier(),
                    );

                    TraitMemberID::Constant(self.create_member(
                        target_id,
                        id_generator,
                        syn,
                        trait_id,
                        |x| x.signature().identifier(),
                        accessibility,
                        handler,
                    ))
                }
            };

            // check if the member is more accessible than the trait
            let member_accessibility = self.get_accessibility(Global::new(
                TargetID::Normal(target_id),
                trait_member_id.into(),
            ));

            if self.accessibility_hierarchy_relationship(
                TargetID::Normal(target_id),
                member_accessibility,
                trait_accessibility,
            ) == HierarchyRelationship::Parent
            {
                handler.receive(Box::new(SymbolIsMoreAccessibleThanParent {
                    symbol_id: trait_member_id.into(),
                    parent_id: trait_id.into(),
                }));
            }
        }
    }

    fn create_module(
        &mut self,
        id_generator: &mut impl Iterator<Item = usize>,
        target_id: usize,
        name: String,
        syntax_tree: ModuleTree,
        parent_module_id: Option<ID<Module>>,
        usings_by_module_id: &mut HashMap<
            ID<Module>,
            Vec<pernixc_syntax::syntax_tree::item::Using>,
        >,
        implementations_by_module_id: &mut HashMap<
            ID<Module>,
            Vec<pernixc_syntax::syntax_tree::item::Implementation>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> ID<Module> {
        let new_module_id = Global::new(
            TargetID::Normal(target_id),
            ID::new(id_generator.next().unwrap()),
        );

        assert!(self
            .symbol_names
            .insert(new_module_id.map(Into::into), name.clone())
            .is_none());

        // insert import component
        self.insert_input(new_module_id, import::Import::default());

        if let Some(parent_module_id) = parent_module_id {
            // insert the parent component
            assert!(self
                .parent
                .module_members
                .insert(
                    new_module_id.map(Into::into),
                    parent::Parent(parent_module_id)
                )
                .is_none());
            // insert the accessibility component
            let accessibility = syntax_tree.signature().as_ref().map_or(
                Accessibility::Public,
                |x| {
                    self.create_accessibility(
                        Global::new(
                            TargetID::Normal(target_id),
                            parent_module_id.into(),
                        ),
                        &x.access_modifier,
                    )
                },
            );

            assert!(self
                .accessibility
                .insert(new_module_id.map(Into::into), accessibility)
                .is_none());
        } else {
            // insert the root module ID
            assert!(self
                .root_module_ids_by_target_id
                .insert(TargetID::Normal(target_id), new_module_id.id)
                .is_none());

            assert!(self
                .target_ids_by_name
                .insert(name, TargetID::Normal(target_id))
                .is_none());

            // insert the accessibility
            assert!(self
                .accessibility
                .insert(new_module_id.map(Into::into), Accessibility::Public)
                .is_none());
        }

        // add member componenet
        self.insert_input(new_module_id, member::Member::default());

        let (_, content, submodule_by_name) = syntax_tree.dissolve();
        let (usings, items) = content.dissolve();

        usings_by_module_id.entry(new_module_id.id).or_default().extend(usings);

        // recusively create the submodules
        for (name, submodule) in submodule_by_name {
            let submodule_id = self.create_module(
                id_generator,
                target_id,
                name.clone(),
                submodule,
                Some(new_module_id.id),
                usings_by_module_id,
                implementations_by_module_id,
                handler,
            );

            assert!(self
                .member
                .modules
                .get_mut(&new_module_id)
                .unwrap()
                .insert(name, submodule_id.into())
                .is_none());
        }

        for item in items {
            match item {
                Item::Trait(syn) => {
                    self.create_trait(
                        id_generator,
                        target_id,
                        syn,
                        new_module_id.id,
                        handler,
                    );
                }
                Item::Function(function) => {
                    let accessibility = self.create_accessibility(
                        new_module_id.map(Into::into),
                        function.access_modifier(),
                    );

                    let _: ID<Function> = self.create_member(
                        target_id,
                        id_generator,
                        syntax_tree::FunctionKind::Normal(function),
                        new_module_id.id,
                        |x| match x {
                            syntax_tree::FunctionKind::Normal(function) => {
                                function.signature().identifier()
                            }
                            syntax_tree::FunctionKind::Extern(
                                extern_function,
                            ) => extern_function
                                .extern_function
                                .signature()
                                .identifier(),
                        },
                        accessibility,
                        handler,
                    );
                }
                Item::Type(syn) => {
                    let accessibility = self.create_accessibility(
                        new_module_id.map(Into::into),
                        syn.access_modifier(),
                    );

                    let _: ID<Type> = self.create_member(
                        target_id,
                        id_generator,
                        syn,
                        new_module_id.id,
                        |x| x.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                Item::Struct(syn) => {
                    let accessibility = self.create_accessibility(
                        new_module_id.map(Into::into),
                        syn.access_modifier(),
                    );

                    let _: ID<Struct> = self.create_member(
                        target_id,
                        id_generator,
                        syn,
                        new_module_id.id,
                        |x| x.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                Item::Implementation(implementation) => {
                    implementations_by_module_id
                        .entry(new_module_id.id)
                        .or_default()
                        .push(implementation);
                }
                Item::Enum(syn) => {
                    self.create_enum(
                        id_generator,
                        target_id,
                        syn,
                        new_module_id.id,
                        handler,
                    );
                }
                Item::Module(_) => {
                    unreachable!(
                        "should've been handled by the submodule creation"
                    )
                }
                Item::Constant(constant) => {
                    let accessibility = self.create_accessibility(
                        new_module_id.map(Into::into),
                        constant.access_modifier(),
                    );

                    let _: ID<Constant> = self.create_member(
                        target_id,
                        id_generator,
                        constant,
                        new_module_id.id,
                        |x| x.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                Item::Marker(marker) => {
                    let accessibility = self.create_accessibility(
                        new_module_id.map(Into::into),
                        marker.access_modifier(),
                    );

                    let _: ID<Marker> = self.create_member(
                        target_id,
                        id_generator,
                        marker,
                        new_module_id.id,
                        |x| x.signature().identifier(),
                        accessibility,
                        handler,
                    );
                }
                Item::Extern(syn) => {
                    let (_, calling_convention, functions) = syn.dissolve();

                    for function in functions.dissolve().1 {
                        let accessibility = self.create_accessibility(
                            new_module_id.map(Into::into),
                            function.access_modifier(),
                        );

                        let _: ID<Function> = self.create_member(
                            target_id,
                            id_generator,
                            syntax_tree::FunctionKind::Extern(
                                syntax_tree::ExternFunction {
                                    calling_convention: calling_convention
                                        .clone(),
                                    extern_function: function,
                                },
                            ),
                            new_module_id.id,
                            |x| match x {
                                FunctionKind::Normal(function) => {
                                    function.signature().identifier()
                                }
                                FunctionKind::Extern(extern_function) => {
                                    extern_function
                                        .extern_function
                                        .signature()
                                        .identifier()
                                }
                            },
                            accessibility,
                            handler,
                        );
                    }
                }
            }
        }

        new_module_id.id
    }
}
