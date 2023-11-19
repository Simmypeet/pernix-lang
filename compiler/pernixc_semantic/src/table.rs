//! Contains the definition of [`Table`]

use std::collections::{hash_map::Entry, HashMap};

use getset::Getters;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use paste::paste;
use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree::target::Target;
use rayon::iter::ParallelIterator;
use thiserror::Error;

use crate::{
    arena::{Arena, ID},
    entity::{constant, r#type, region::Region, GenericArguments, Model, Never},
    error,
    symbol::{
        Accessibility, Constant, Enum, Function, Generic, GenericID, Global, GlobalID,
        Implementation, ImplementationConstant, ImplementationFunction, ImplementationType, Module,
        NegativeImplementation, Struct, Trait, TraitConstant, TraitFunction, TraitType, Type,
        Variant,
    },
};

mod drafting;
pub mod evaluate;
mod finalizing;
pub mod resolution;
mod state;

/// A trait used to access the symbols defined in the table.
pub trait Index<Idx: ?Sized> {
    /// The output type of the indexing operation.
    type Output: ?Sized;

    /// Returns the output of the indexing operation if the index is valid.
    fn get(&self, index: Idx) -> Option<RwLockReadGuard<Self::Output>>;
}

trait IndexRaw<Idx: ?Sized>: Index<Idx> {
    /// Returns the output of the indexing operation if the index is valid.
    fn get_raw(&self, index: Idx) -> Option<&RwLock<Self::Output>>;
}

trait IndexMut<Idx: ?Sized>: Index<Idx> {
    /// Returns the output of the indexing operation if the index is valid.
    fn get_mut(&self, index: Idx) -> Option<RwLockWriteGuard<Self::Output>>;
}

/// Contains all the symbols and information defined in the target.
#[derive(Debug, Getters)]
pub struct Table {
    modules: Arena<RwLock<Module>, Module>,
    structs: Arena<RwLock<Struct>, Struct>,
    enums: Arena<RwLock<Enum>, Enum>,
    variants: Arena<RwLock<Variant>, Variant>,
    types: Arena<RwLock<Type>, Type>,
    functions: Arena<RwLock<Function>, Function>,
    constants: Arena<RwLock<Constant>, Constant>,

    traits: Arena<RwLock<Trait>, Trait>,
    trait_types: Arena<RwLock<TraitType>, TraitType>,
    trait_functions: Arena<RwLock<TraitFunction>, TraitFunction>,
    trait_constants: Arena<RwLock<TraitConstant>, TraitConstant>,

    implementations: Arena<RwLock<Implementation>, Implementation>,
    negative_implementations: Arena<RwLock<NegativeImplementation>, NegativeImplementation>,

    implementation_types: Arena<RwLock<ImplementationType>, ImplementationType>,
    implementation_functions: Arena<RwLock<ImplementationFunction>, ImplementationFunction>,
    implementation_constants: Arena<RwLock<ImplementationConstant>, ImplementationConstant>,

    root_module_ids_by_name: HashMap<String, ID<Module>>,

    state_manager: RwLock<state::Manager>,
}

impl Table {
    pub(crate) fn default() -> Self {
        Self {
            modules: Arena::default(),
            structs: Arena::default(),
            enums: Arena::default(),
            types: Arena::default(),
            functions: Arena::default(),
            constants: Arena::default(),
            traits: Arena::default(),
            variants: Arena::default(),
            implementations: Arena::default(),
            implementation_constants: Arena::default(),
            implementation_functions: Arena::default(),
            implementation_types: Arena::default(),
            trait_constants: Arena::default(),
            trait_functions: Arena::default(),
            trait_types: Arena::default(),
            negative_implementations: Arena::default(),
            root_module_ids_by_name: HashMap::new(),
            state_manager: RwLock::new(state::Manager::default()),
        }
    }
}

macro_rules! index {
    ($field:ident, $struct_name:ident) => {
        impl Index<crate::arena::ID<$struct_name>> for Table {
            type Output = $struct_name;

            fn get(
                &self,
                id: crate::arena::ID<$struct_name>,
            ) -> Option<RwLockReadGuard<Self::Output>> {
                self.$field.get(id).map(|x| x.read())
            }
        }

        impl IndexRaw<crate::arena::ID<$struct_name>> for Table {
            fn get_raw(&self, id: crate::arena::ID<$struct_name>) -> Option<&RwLock<Self::Output>> {
                self.$field.get(id)
            }
        }

        impl IndexMut<crate::arena::ID<$struct_name>> for Table {
            fn get_mut(
                &self,
                id: crate::arena::ID<$struct_name>,
            ) -> Option<RwLockWriteGuard<Self::Output>> {
                self.$field.get(id).map(|x| x.write())
            }
        }
    };
}

index!(modules, Module);
index!(structs, Struct);
index!(enums, Enum);
index!(types, Type);
index!(functions, Function);
index!(constants, Constant);
index!(traits, Trait);
index!(trait_types, TraitType);
index!(trait_functions, TraitFunction);
index!(trait_constants, TraitConstant);
index!(variants, Variant);
index!(implementations, Implementation);
index!(negative_implementations, NegativeImplementation);
index!(implementation_types, ImplementationType);
index!(implementation_constants, ImplementationConstant);
index!(implementation_functions, ImplementationFunction);

/// The error type returned by [`Table::build()`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Error)]
#[allow(missing_docs)]
pub enum BuildError {
    #[error("the target `{0}` was already defined")]
    DuplicateTargetName(String),
}

/// The error type returned by most operations on [`Table`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("The given ID does not exist in the table")]
    InvalidID,

    #[error("Encountered a fatal semantic error that aborts the process")]
    SemanticError,
}

/// The error type returned by [`Table::get_by_qualified_name()`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum GetByQualifiedNameError<'a> {
    #[error(
        "The given name `{name}` does not exist in the table (searched in \
         `{searched_in_global_id:?}`)"
    )]
    SymbolNotFound {
        /// The global ID that was searched in. If `None`, the search was started from the root.
        searched_in_global_id: Option<GlobalID>,

        /// The name that was searched.
        name: &'a str,
    },
    #[error("the given iterator is empty")]
    EmptyIterator,
}

macro_rules! get {
    ($self:ident, $id:ident, $kind:ident, $($field:ident),*) => {
        match $id {
            $(
                $kind::$field($id) => $self.get($id).map(|x| RwLockReadGuard::map(x, |x| x as _)),
            )*
        }
    };
}

impl Table {
    /// Builds a symbol table from the given targets.
    ///
    /// # Errors
    ///
    /// - [`BuildError::DuplicateTargetName`]: if there are two targets with the same name.
    pub fn build(
        targets: impl ParallelIterator<Item = Target>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, BuildError> {
        let table = RwLock::new(Self::default());

        let drafting_context = drafting::Context {
            table: &table,
            handler,
            usings_by_module_id: RwLock::new(HashMap::new()),
            implementations_by_module_id: RwLock::new(HashMap::new()),
        };

        // Collect all the targets.
        targets.try_for_each(|target| {
            let (syntax_tree, name) = target.dissolve();

            let module_id = drafting_context.draft_module(syntax_tree, name.clone(), None);

            #[allow(clippy::significant_drop_in_scrutinee)]
            match table.write().root_module_ids_by_name.entry(name) {
                Entry::Occupied(error) => Err(BuildError::DuplicateTargetName(error.key().clone())),
                Entry::Vacant(entry) => {
                    entry.insert(module_id);
                    Ok(())
                }
            }
        })?;

        let drafting::Context {
            usings_by_module_id,
            implementations_by_module_id,
            ..
        } = drafting_context;

        let mut table = table.into_inner();

        // populate the usings
        for (module_id, usings) in usings_by_module_id.into_inner() {
            for using in usings {
                let Ok(using_module_id) =
                    table.resolve_module_path(using.module_path(), module_id.into(), handler)
                else {
                    continue;
                };

                table
                    .get_mut(module_id)
                    .unwrap()
                    .usings
                    .insert(using_module_id);
            }
        }

        // attach the implementations to the trait
        for (module_id, implementations) in implementations_by_module_id.into_inner() {
            for implementation in implementations {
                let Ok(trait_id) = table.resolve_trait_path(
                    implementation.signature().qualified_identifier(),
                    module_id.into(),
                    handler,
                ) else {
                    continue;
                };

                table.draft_implementation(implementation, module_id, trait_id, handler);
            }
        }

        // finalize all symbols
        let drafted_symbol_ids = table
            .state_manager
            .read()
            .all_drafted_symbols()
            .collect::<Vec<_>>();

        for symbol_id in drafted_symbol_ids {
            let _ = table.finalize(symbol_id, None, handler);
        }

        Ok(table)
    }

    /// Checks if the `referred` is accessible from the `referring_site`.
    ///
    /// # Returns
    ///
    /// Returns `None` if `referred` or `referring_site` is not a valid ID.
    pub fn symbol_accessible(&self, referring_site: GlobalID, referred: GlobalID) -> Option<bool> {
        match self.get_accessibility(referred)? {
            Accessibility::Public => {
                // PEDANTIC: check if the referring site is a valid ID.
                drop(self.get_global(referring_site)?);

                Some(true)
            }
            Accessibility::Private => {
                let mut referring_module_id = self.get_closet_module_id(referring_site)?;
                let referred_module_id = self.get_closet_module_id(referred)?;

                loop {
                    if referring_module_id == referred_module_id {
                        return Some(true);
                    }

                    let Some(next) = self.get(referring_module_id)?.parent_module_id else {
                        return Some(false);
                    };

                    referring_module_id = next;
                }
            }
            Accessibility::Internal => Some(
                self.get_root_module_id(referred)? == self.get_root_module_id(referring_site)?,
            ),
        }
    }

    /// Returns the root [`Module`] ID that contains the given [`GlobalID`] (including itself).
    pub fn get_root_module_id(&self, mut global_id: GlobalID) -> Option<ID<Module>> {
        while let Some(parent_id) = self.get_global(global_id)?.parent_global_id() {
            global_id = parent_id;
        }

        Some(
            global_id
                .into_module()
                .expect("It should be a module at the root."),
        )
    }

    /// Returns the [`Module`] ID that is the closest to the given [`GlobalID`] (including itself).
    pub fn get_closet_module_id(&self, mut global_id: GlobalID) -> Option<ID<Module>> {
        // including itself
        loop {
            if let GlobalID::Module(module_id) = global_id {
                drop(self.get(module_id)?);
                return Some(module_id);
            }

            global_id = self
                .get_global(global_id)?
                .parent_global_id()
                .expect("should've found at least one module");
        }
    }

    /// Gets a [`GlobalID`] from the given qualified identifier.
    ///
    /// # Errors
    ///
    /// - [`GetByQualifiedNameError::SymbolNotFound`]: if the symbol is not found.
    /// - [`GetByQualifiedNameError::EmptyIterator`]: if the given iterator is empty.
    pub fn get_by_qualified_name<'a>(
        &self,
        qualified_names: impl Iterator<Item = &'a str>,
    ) -> Result<GlobalID, GetByQualifiedNameError<'a>> {
        let mut current_id: Option<GlobalID> = None;

        for name in qualified_names {
            match current_id {
                Some(searched_in_global_id) => {
                    current_id = Some(
                        self.get_global(searched_in_global_id)
                            .unwrap()
                            .get_member(name)
                            .ok_or(GetByQualifiedNameError::SymbolNotFound {
                                searched_in_global_id: Some(searched_in_global_id),
                                name,
                            })?,
                    );
                }
                None => {
                    current_id = Some(self.root_module_ids_by_name.get(name).map_or(
                        Err(GetByQualifiedNameError::SymbolNotFound {
                            searched_in_global_id: None,
                            name,
                        }),
                        |some| Ok(GlobalID::Module(*some)),
                    )?);
                }
            }
        }

        current_id.ok_or(GetByQualifiedNameError::EmptyIterator)
    }

    /// Gets the fully qualified name of the given [`GlobalID`].
    ///
    /// # Returns
    ///
    /// Returns `None` if the given [`GlobalID`] is not valid.
    #[must_use]
    pub fn get_qualified_name(&self, global_id: GlobalID) -> Option<String> {
        match global_id {
            GlobalID::Implementation(id) => {
                self.get_qualified_name(self.get(id)?.signature.trait_id.into())
            }
            GlobalID::NegativeImplementation(id) => {
                self.get_qualified_name(self.get(id)?.signature.trait_id.into())
            }

            GlobalID::ImplementationType(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_implementation_id)
                        .unwrap()
                        .signature
                        .trait_id
                        .into(),
                )?;
                qualified_name.push_str("::");
                qualified_name.push_str(self.get(id)?.name.as_str());
                Some(qualified_name)
            }
            GlobalID::ImplementationFunction(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_implementation_id)
                        .unwrap()
                        .signature
                        .trait_id
                        .into(),
                )?;
                qualified_name.push_str("::");
                qualified_name.push_str(self.get(id)?.name.as_str());
                Some(qualified_name)
            }
            GlobalID::ImplementationConstant(id) => {
                let mut qualified_name = self.get_qualified_name(
                    self.get(self.get(id)?.parent_implementation_id)
                        .unwrap()
                        .signature
                        .trait_id
                        .into(),
                )?;
                qualified_name.push_str("::");
                qualified_name.push_str(self.get(id)?.name.as_str());
                Some(qualified_name)
            }

            mut normal => {
                let mut qualified_name = String::new();

                loop {
                    let global = self.get_global(normal)?;

                    if qualified_name.is_empty() {
                        qualified_name.push_str(global.name());
                    } else {
                        qualified_name.insert_str(0, "::");
                        qualified_name.insert_str(0, global.name());
                    }

                    if let Some(parent_id) = global.parent_global_id() {
                        normal = parent_id;
                    } else {
                        break;
                    }
                }

                Some(qualified_name)
            }
        }
    }

    /// Gets overall accessibility of the given [`r#type::Type`].
    ///
    /// **Overall Accessibility** describes the **least** accessibility of the least accessible
    /// type component of the given [`r#type::Type`].
    ///
    /// Example:
    ///
    /// ``` txt
    /// private struct Foo{}
    /// inernal struct Bar{}
    ///
    /// // The overall accessibility of `Spam<Foo, Bar>` is `private`.
    /// ```
    ///
    /// # Returns
    ///
    /// `None` if the type contains an invalid id as its component.
    #[must_use]
    pub fn get_type_overall_accessibility<S>(&self, ty: &r#type::Type<S>) -> Option<Accessibility>
    where
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LocalRegion = Never,
            ForallRegion = Never,
        >,
    {
        match ty {
            r#type::Type::Primitive(_) => Some(Accessibility::Public),
            r#type::Type::Inference(never) => match *never {},
            r#type::Type::Algebraic(adt) => Some(
                self.get_accessibility(adt.kind.into())?
                    .min(self.get_generic_arguments_overall_accessibility(&adt.generic_arguments)?),
            ),
            r#type::Type::Pointer(pointer) => {
                self.get_type_overall_accessibility(&*pointer.pointee)
            }
            r#type::Type::Reference(reference) => Some(
                self.get_region_overall_accessibility(&reference.region)?
                    .min(self.get_type_overall_accessibility(&*reference.pointee)?),
            ),
            r#type::Type::Array(array) => Some(
                self.get_constant_overall_accessibility(&array.length)?
                    .min(self.get_type_overall_accessibility(&*array.element)?),
            ),
            r#type::Type::TraitMember(trait_member) => Some(
                self.get_accessibility(trait_member.trait_type_id.into())?
                    .min(self.get_generic_arguments_overall_accessibility(
                        &trait_member.trait_generic_arguments,
                    )?)
                    .min(self.get_generic_arguments_overall_accessibility(
                        &trait_member.member_generic_arguments,
                    )?),
            ),
            r#type::Type::Parameter(parameter) => self.get_accessibility(parameter.parent.into()),
            r#type::Type::Tuple(tuple) => {
                let mut current_min = Accessibility::Public;

                for element in &tuple.elements {
                    let candidate = match element {
                        r#type::TupleElement::Regular(reg) => {
                            self.get_type_overall_accessibility(reg)?
                        }
                        r#type::TupleElement::Unpacked(r#type::Unpacked::Parameter(param)) => {
                            self.get_accessibility(param.parent.into())?
                        }
                        r#type::TupleElement::Unpacked(r#type::Unpacked::TraitMember(
                            trait_member,
                        )) => self
                            .get_accessibility(trait_member.trait_type_id.into())?
                            .min(self.get_generic_arguments_overall_accessibility(
                                &trait_member.trait_generic_arguments,
                            )?)
                            .min(self.get_generic_arguments_overall_accessibility(
                                &trait_member.member_generic_arguments,
                            )?),
                    };

                    current_min = current_min.min(candidate);
                }

                Some(current_min)
            }
        }
    }

    /// Gets overall accessibility of the given [`constant::Constant`].
    ///
    /// **Overall Accessibility** describes the **least** accessibility of the least accessible
    /// component of the given [`constant::Constant`].
    ///
    /// # Returns
    ///
    /// `None` if the constant contains an invalid id as its component.
    #[must_use]
    pub fn get_constant_overall_accessibility<S>(
        &self,
        constant: &constant::Constant<S>,
    ) -> Option<Accessibility>
    where
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LocalRegion = Never,
            ForallRegion = Never,
        >,
    {
        match constant {
            constant::Constant::Primitive(_) => Some(Accessibility::Public),
            constant::Constant::Inference(never) => match *never {},
            constant::Constant::Struct(constant) => {
                let mut current_min = self.get_accessibility(constant.struct_id.into())?.min(
                    self.get_generic_arguments_overall_accessibility(&constant.generic_arguments)?,
                );

                for field in &constant.fields {
                    current_min = current_min.min(self.get_constant_overall_accessibility(field)?);
                }

                Some(current_min)
            }
            constant::Constant::Enum(_) => todo!(),
            constant::Constant::Array(_) => todo!(),
            constant::Constant::Parameter(_) => todo!(),
            constant::Constant::TraitMember(_) => todo!(),
            constant::Constant::Tuple(_) => todo!(),
        }
    }

    /// Gets overall accessibility of the given [`Region`].
    ///
    /// # Returns
    ///
    /// `None` if the region contains an invalid id as its component.
    #[must_use]
    pub fn get_region_overall_accessibility<S>(&self, region: &Region<S>) -> Option<Accessibility>
    where
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LocalRegion = Never,
            ForallRegion = Never,
        >,
    {
        match region {
            Region::Static => Some(Accessibility::Public),
            Region::Named(lifetime_parameter_id) => {
                self.get_accessibility(lifetime_parameter_id.parent.into())
            }
            Region::Local(never) | Region::Forall(never) => match *never {},
        }
    }

    /// Gets overall accessibility of the given [`GenericArguments`].
    ///
    /// **Overall Accessibility** describes the **least** accessibility of the least accessible
    /// component of the given [`GenericArguments`].
    ///
    /// # Returns
    ///
    /// `None` if the generic arguments contains an invalid id as its component.
    /// If the generic arguments is empty, returns `Some(Accessibility::Public)`.
    #[must_use]
    pub fn get_generic_arguments_overall_accessibility<S>(
        &self,
        generic_arguments: &GenericArguments<S>,
    ) -> Option<Accessibility>
    where
        S: Model<
            TypeInference = Never,
            ConstantInference = Never,
            LocalRegion = Never,
            ForallRegion = Never,
        >,
    {
        let mut current_min = Accessibility::Public;

        for region in &generic_arguments.regions {
            current_min = current_min.min(self.get_region_overall_accessibility(region)?);
        }

        for ty in &generic_arguments.types {
            current_min = current_min.min(self.get_type_overall_accessibility(ty)?);
        }

        for constant in &generic_arguments.constants {
            current_min = current_min.min(self.get_constant_overall_accessibility(constant)?);
        }

        Some(current_min)
    }

    /// Gets the [`ScopeWalker`] that walks through the scope hierarchy of the given [`GlobalID`].
    ///
    /// See [`ScopeWalker`] for more information.
    #[must_use]
    pub fn scope_walker(&self, global_id: GlobalID) -> Option<ScopeWalker> {
        drop(self.get_global(global_id)?);

        Some(ScopeWalker {
            table: self,
            current_id: Some(global_id),
        })
    }

    /// Gets the accessibility of the given [`GlobalID`].
    ///
    /// # Returns
    ///
    /// Returns `None` if the given [`GlobalID`] is not a valid ID.
    pub fn get_accessibility(&self, global_id: GlobalID) -> Option<Accessibility> {
        macro_rules! arm_expression {
            ($table:ident, $id:ident, $kind:ident) => {
                paste! {
                    $table.[<$kind:lower s>].get($id).map(|$id| $id.read().accessibility)
                }
            };

            ($table:ident, $id:ident, $kind:ident, $expr:expr) => {
                paste! {
                    $table.[<$kind:snake s>].get($id).and_then(|$id| $expr)
                }
            };
        }
        macro_rules! get_accessibility {
            ($self:ident, $table:ident, $(($kind:ident $(, $expr:expr)?)),*) => {

                match $self {
                    $(
                        GlobalID::$kind($self) => arm_expression!($table, $self, $kind $(, $expr)?),
                    )*
                }
            };
        }

        get_accessibility!(
            global_id,
            self,
            (Module),
            (Struct),
            (Enum),
            (Trait),
            (Type),
            (Constant),
            (Function),
            (
                Variant,
                self.get_accessibility(global_id.read().parent_enum_id.into())
            ),
            (
                TraitType,
                self.get_accessibility(global_id.read().parent_trait_id.into())
            ),
            (
                TraitConstant,
                self.get_accessibility(global_id.read().parent_trait_id.into())
            ),
            (
                TraitFunction,
                self.get_accessibility(global_id.read().parent_trait_id.into())
            ),
            (
                Implementation,
                self.get_accessibility(global_id.read().signature.trait_id.into())
            ),
            (
                ImplementationType,
                self.get_accessibility(global_id.read().parent_implementation_id.into())
            ),
            (
                ImplementationFunction,
                self.get_accessibility(global_id.read().parent_implementation_id.into())
            ),
            (
                ImplementationConstant,
                self.get_accessibility(global_id.read().parent_implementation_id.into())
            ),
            (
                NegativeImplementation,
                self.get_accessibility(global_id.read().signature.trait_id.into())
            )
        )
    }

    /// Returns the [`Generic`] symbol from the given [`GenericID`]
    pub fn get_generic(&self, generic_id: GenericID) -> Option<MappedRwLockReadGuard<dyn Generic>> {
        get!(
            self,
            generic_id,
            GenericID,
            Struct,
            Trait,
            Enum,
            Function,
            Type,
            TraitFunction,
            TraitType,
            Implementation,
            ImplementationType,
            ImplementationFunction,
            NegativeImplementation
        )
    }

    /// Returns the [`Global`] symbol from the given [`GlobalID`].
    #[must_use]
    pub fn get_global(&self, global_id: GlobalID) -> Option<MappedRwLockReadGuard<dyn Global>> {
        get!(
            self,
            global_id,
            GlobalID,
            Module,
            Struct,
            TraitType,
            TraitConstant,
            TraitFunction,
            Trait,
            Enum,
            Type,
            Constant,
            Function,
            Variant,
            ImplementationFunction,
            Implementation,
            ImplementationType,
            ImplementationConstant,
            NegativeImplementation
        )
    }
}

/// Represents an iterator that walks through the scope of the given symbol. It goes through all
/// the parent symbols until it reaches the root.
///
/// The iterator iterates through the scope in id-to-parent order.
#[derive(Debug, Clone)]
pub struct ScopeWalker<'a> {
    table: &'a Table,
    current_id: Option<GlobalID>,
}

impl<'a> ScopeWalker<'a> {
    /// Creates a new scope walker.
    #[must_use]
    pub fn table(&self) -> &'a Table { self.table }
}

impl<'a> Iterator for ScopeWalker<'a> {
    type Item = GlobalID;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(current_id) => {
                let next_id = self
                    .table
                    .get_global(current_id)
                    .unwrap()
                    .parent_global_id();
                self.current_id = next_id;
                Some(current_id)
            }
            None => None,
        }
    }
}
