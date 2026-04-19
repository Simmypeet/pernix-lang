//! Contains code for applying generic instantiations to interned terms.

use std::collections::BTreeMap;

use derive_new::new;
use linkme::distributed_slice;
use pernixc_arena::ID;
use pernixc_extend::extend;
use pernixc_qbice::{Config, Interner, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{MemberID, SymbolID, parent::get_parent_global};
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
};

use crate::{
    constant::Constant,
    folding::{Abort, Foldable, Folder},
    generic_arguments::{AssociatedSymbol, GenericArguments, Symbol},
    generic_parameters::{
        ConstantParameterID, GenericParameters, InstanceParameterID,
        LifetimeParameterID, TypeParameterID, get_generic_parameters,
    },
    instance::{Instance, TraitRef},
    lifetime::Lifetime,
    r#type::Type,
};

/// Represents an instantiation of generic parameters.
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
    new,
)]
#[allow(missing_docs)]
pub struct Instantiation {
    lifetimes: BTreeMap<Interned<Lifetime>, Interned<Lifetime>>,
    types: BTreeMap<Interned<Type>, Interned<Type>>,
    constants: BTreeMap<Interned<Constant>, Interned<Constant>>,
    instances: BTreeMap<Interned<Instance>, Interned<Instance>>,
}

impl Instantiation {
    /// Retrieves the mapping for the given lifetime if it exists.
    #[must_use]
    pub fn get_lifetime_mapping(
        &self,
        lifetime: &Interned<Lifetime>,
    ) -> Option<&Interned<Lifetime>> {
        self.lifetimes.get(lifetime)
    }

    /// Retrieves the mapping for the given type if it exists.
    #[must_use]
    pub fn get_type_mapping(
        &self,
        r#type: &Interned<Type>,
    ) -> Option<&Interned<Type>> {
        self.types.get(r#type)
    }

    /// Retrieves the mapping for the given constant if it exists.
    #[must_use]
    pub fn get_constant_mapping(
        &self,
        constant: &Interned<Constant>,
    ) -> Option<&Interned<Constant>> {
        self.constants.get(constant)
    }

    /// Retrieves the mapping for the given instance if it exists.
    #[must_use]
    pub fn get_instance_mapping(
        &self,
        instance: &Interned<Instance>,
    ) -> Option<&Interned<Instance>> {
        self.instances.get(instance)
    }

    /// Returns an iterator over the mutable lifetime mappings.
    pub fn lifetime_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Lifetime>, &mut Interned<Lifetime>)>
    {
        self.lifetimes.iter_mut()
    }

    /// Returns an iterator over the mutable type mappings.
    pub fn type_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Type>, &mut Interned<Type>)>
    {
        self.types.iter_mut()
    }

    /// Returns an iterator over the mutable constant mappings.
    pub fn constant_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Constant>, &mut Interned<Constant>)>
    {
        self.constants.iter_mut()
    }

    /// Returns an iterator over the mutable instance mappings.
    pub fn instance_mappings_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Instance>, &mut Interned<Instance>)>
    {
        self.instances.iter_mut()
    }

    /// Returns an iterator over the mutable lifetime values.
    #[must_use]
    pub fn lifetime_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Interned<Lifetime>> {
        self.lifetimes.values_mut()
    }

    /// Returns an iterator over the mutable type values.
    #[must_use]
    pub fn type_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Interned<Type>> {
        self.types.values_mut()
    }

    /// Returns an iterator over the mutable constant values.
    #[must_use]
    pub fn constant_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Interned<Constant>> {
        self.constants.values_mut()
    }

    /// Returns an iterator over the mutable instance values.
    #[must_use]
    pub fn instance_values_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut Interned<Instance>> {
        self.instances.values_mut()
    }

    /// Returns an iterator over the lifetime mappings.
    #[must_use]
    pub fn lifetime_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Lifetime>, &Interned<Lifetime>)>
    {
        self.lifetimes.iter()
    }

    /// Returns an iterator over the type mappings.
    #[must_use]
    pub fn type_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Type>, &Interned<Type>)> {
        self.types.iter()
    }

    /// Returns an iterator over the constant mappings.
    #[must_use]
    pub fn constant_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Constant>, &Interned<Constant>)>
    {
        self.constants.iter()
    }

    /// Returns an iterator over the instance mappings.
    #[must_use]
    pub fn instance_mappings(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Interned<Instance>, &Interned<Instance>)>
    {
        self.instances.iter()
    }

    /// Extends the lifetime mappings with the given mappings.
    pub fn extend_lifetimes_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Interned<Lifetime>, Interned<Lifetime>)>,
    ) {
        self.lifetimes.extend(mappings);
    }

    /// Extends the type mappings with the given mappings.
    pub fn extend_types_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Interned<Type>, Interned<Type>)>,
    ) {
        self.types.extend(mappings);
    }

    /// Extends the constant mappings with the given mappings.
    pub fn extend_constants_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Interned<Constant>, Interned<Constant>)>,
    ) {
        self.constants.extend(mappings);
    }

    /// Extends the instance mappings with the given mappings.
    pub fn extend_instances_mappings(
        &mut self,
        mappings: impl IntoIterator<Item = (Interned<Instance>, Interned<Instance>)>,
    ) {
        self.instances.extend(mappings);
    }

    /// Instantiates all mapped values in this instantiation with another
    /// instantiation.
    pub fn instantiate_values(
        &mut self,
        instantiation: &Self,
        engine: &TrackedEngine,
    ) {
        for term in self.lifetime_values_mut() {
            instantiation.instantiate(term, engine);
        }

        for term in self.type_values_mut() {
            instantiation.instantiate(term, engine);
        }

        for term in self.constant_values_mut() {
            instantiation.instantiate(term, engine);
        }

        for term in self.instance_values_mut() {
            instantiation.instantiate(term, engine);
        }
    }

    /// Inserts a mapping from one lifetime to another.
    pub fn insert_lifetime_mapping(
        &mut self,
        from: Interned<Lifetime>,
        to: Interned<Lifetime>,
    ) {
        self.lifetimes.insert(from, to);
    }

    /// Inserts a mapping from one type to another.
    pub fn insert_type_mapping(
        &mut self,
        from: Interned<Type>,
        to: Interned<Type>,
    ) {
        self.types.insert(from, to);
    }

    /// Inserts a mapping from one constant to another.
    pub fn insert_constant_mapping(
        &mut self,
        from: Interned<Constant>,
        to: Interned<Constant>,
    ) {
        self.constants.insert(from, to);
    }

    /// Inserts a mapping from one instance to another.
    pub fn insert_instance_mapping(
        &mut self,
        from: Interned<Instance>,
        to: Interned<Instance>,
    ) {
        self.instances.insert(from, to);
    }

    /// Removes the mapping for the given lifetime if it exists.
    pub fn remove_lifetime_mapping(
        &mut self,
        from: &Interned<Lifetime>,
    ) -> Option<Interned<Lifetime>> {
        self.lifetimes.remove(from)
    }

    /// Removes the mapping for the given type if it exists.
    pub fn remove_type_mapping(
        &mut self,
        from: &Interned<Type>,
    ) -> Option<Interned<Type>> {
        self.types.remove(from)
    }

    /// Removes the mapping for the given constant if it exists.
    pub fn remove_constant_mapping(
        &mut self,
        from: &Interned<Constant>,
    ) -> Option<Interned<Constant>> {
        self.constants.remove(from)
    }

    /// Removes the mapping for the given instance if it exists.
    pub fn remove_instance_mapping(
        &mut self,
        from: &Interned<Instance>,
    ) -> Option<Interned<Instance>> {
        self.instances.remove(from)
    }

    fn append_from_arguments<T: Element + Ord, U>(
        &mut self,
        terms: impl Iterator<Item = Interned<T>>,
        term_parameter_order: impl Iterator<Item = ID<U>>,
        generic_id: Global<SymbolID>,
        interner: &impl Interner,
    ) where
        MemberID<ID<U>>: Into<T>,
    {
        for (term, term_id) in terms.zip(term_parameter_order) {
            let parameter_id = MemberID::new(generic_id, term_id);

            assert!(
                T::get_mut(self)
                    .insert(interner.intern(parameter_id.into()), term)
                    .is_none()
            );
        }
    }

    /// Appends the given generic arguments as a substitution.
    pub fn append_from_generic_arguments(
        &mut self,
        generic_arguments: &GenericArguments,
        generic_id: Global<SymbolID>,
        generic_parameters: &GenericParameters,
        interner: &impl Interner,
    ) {
        assert_eq!(
            generic_arguments.types().len(),
            generic_parameters.type_parameter_order().len(),
            "Mismatched number of type generic arguments",
        );

        assert_eq!(
            generic_arguments.lifetimes().len(),
            generic_parameters.lifetime_parameter_order().len(),
            "Mismatched number of lifetime generic arguments",
        );

        assert_eq!(
            generic_arguments.constants().len(),
            generic_parameters.constant_parameter_order().len(),
            "Mismatched number of constant generic arguments",
        );

        assert_eq!(
            generic_arguments.instances().len(),
            generic_parameters.instance_parameter_order().len(),
            "Mismatched number of instance generic arguments",
        );

        self.append_from_arguments(
            generic_arguments.lifetimes().iter().cloned(),
            generic_parameters.lifetime_parameter_order(),
            generic_id,
            interner,
        );

        self.append_from_arguments(
            generic_arguments.types().iter().cloned(),
            generic_parameters.type_parameter_order(),
            generic_id,
            interner,
        );

        self.append_from_arguments(
            generic_arguments.constants().iter().cloned(),
            generic_parameters.constant_parameter_order(),
            generic_id,
            interner,
        );

        self.append_from_arguments(
            generic_arguments.instances().iter().cloned(),
            generic_parameters.instance_parameter_order(),
            generic_id,
            interner,
        );
    }

    /// Creates a mapping from one generic parameter set to another.
    ///
    /// Suppose A[B, C, D] and T[U, V, W]. To map generic parameters of A to T,
    /// this function creates {B -> U, C -> V, D -> W}.
    pub fn append_from_generic_parameters_mapping(
        &mut self,
        from_id: Global<SymbolID>,
        from_parameters: &GenericParameters,
        to_id: Global<SymbolID>,
        to_parameters: &GenericParameters,
        interner: &impl Interner,
    ) {
        self.append_from_arguments(
            from_parameters.lifetime_parameter_order().map(|id| {
                interner.intern(Lifetime::Parameter(LifetimeParameterID::new(
                    from_id, id,
                )))
            }),
            to_parameters.lifetime_parameter_order(),
            to_id,
            interner,
        );

        self.append_from_arguments(
            from_parameters.type_parameter_order().map(|id| {
                interner
                    .intern(Type::Parameter(TypeParameterID::new(from_id, id)))
            }),
            to_parameters.type_parameter_order(),
            to_id,
            interner,
        );

        self.append_from_arguments(
            from_parameters.constant_parameter_order().map(|id| {
                interner.intern(Constant::Parameter(ConstantParameterID::new(
                    from_id, id,
                )))
            }),
            to_parameters.constant_parameter_order(),
            to_id,
            interner,
        );

        self.append_from_arguments(
            from_parameters.instance_parameter_order().map(|id| {
                interner.intern(Instance::Parameter(InstanceParameterID::new(
                    from_id, id,
                )))
            }),
            to_parameters.instance_parameter_order(),
            to_id,
            interner,
        );
    }

    /// Converts this instantiation into generic arguments by substituting each
    /// generic parameter key.
    #[must_use]
    pub fn create_generic_arguments(
        &self,
        global_id: Global<SymbolID>,
        parameters: &GenericParameters,
        interner: &impl Interner,
    ) -> Interned<GenericArguments> {
        interner.intern(GenericArguments::new(
            parameters
                .lifetime_parameter_order()
                .map(|id| {
                    let parameter = interner.intern(Lifetime::Parameter(
                        LifetimeParameterID::new(global_id, id),
                    ));

                    self.lifetimes.get(&parameter).cloned().unwrap()
                })
                .collect(),
            parameters
                .type_parameter_order()
                .map(|id| {
                    let parameter = interner.intern(Type::Parameter(
                        TypeParameterID::new(global_id, id),
                    ));

                    self.types.get(&parameter).cloned().unwrap()
                })
                .collect(),
            parameters
                .constant_parameter_order()
                .map(|id| {
                    let parameter = interner.intern(Constant::Parameter(
                        ConstantParameterID::new(global_id, id),
                    ));

                    self.constants.get(&parameter).cloned().unwrap()
                })
                .collect(),
            parameters
                .instance_parameter_order()
                .map(|id| {
                    let parameter = interner.intern(Instance::Parameter(
                        InstanceParameterID::new(global_id, id),
                    ));

                    self.instances.get(&parameter).cloned().unwrap()
                })
                .collect(),
        ))
    }

    /// Converts the given generic arguments into an instantiation.
    #[must_use]
    pub fn from_generic_arguments(
        generic_arguments: &GenericArguments,
        global_id: Global<SymbolID>,
        generic_parameters: &GenericParameters,
        interner: &impl Interner,
    ) -> Self {
        let mut instantiation = Self::default();

        instantiation.append_from_generic_arguments(
            generic_arguments,
            global_id,
            generic_parameters,
            interner,
        );

        instantiation
    }

    /// Applies this instantiation recursively to an interned term.
    pub fn instantiate<T>(&self, term: &mut Interned<T>, engine: &TrackedEngine)
    where
        Interned<T>: Foldable,
    {
        term.fold_with(&mut Instantiater { substitution: self }, engine)
            .expect("instantiation folder is infallible");
    }

    /// Clones an interned term, applies this instantiation, and returns the
    /// instantiated clone.
    pub fn clone_and_instantiate<T>(
        &self,
        term: &Interned<T>,
        engine: &TrackedEngine,
    ) -> Interned<T>
    where
        Interned<T>: Foldable,
    {
        let mut cloned = term.clone();
        self.instantiate(&mut cloned, engine);
        cloned
    }
}

struct Instantiater<'a> {
    substitution: &'a Instantiation,
}

impl Instantiater<'_> {
    fn substitute<T: Element>(&self, term: &mut Interned<T>) {
        if let Some(substitution) = T::get(self.substitution).get(term) {
            *term = substitution.clone();
        }
    }
}

impl Folder for Instantiater<'_> {
    fn fold_lifetime(
        &mut self,
        lifetime: &mut Interned<Lifetime>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        self.substitute(lifetime);
        Ok(())
    }

    fn fold_type(
        &mut self,
        r#type: &mut Interned<Type>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        self.substitute(r#type);
        Ok(())
    }

    fn fold_constant(
        &mut self,
        constant: &mut Interned<Constant>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        self.substitute(constant);
        Ok(())
    }

    fn fold_instance(
        &mut self,
        instance: &mut Interned<Instance>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        self.substitute(instance);
        Ok(())
    }
}

/// Retrieves generic arguments for the given symbol id using an instantiation.
#[extend]
pub async fn create_generic_arguments_from_instantiation(
    self: &TrackedEngine,
    id: Global<SymbolID>,
    instantiation: Instantiation,
) -> Interned<GenericArguments> {
    let generic_parameters = self.get_generic_parameters(id).await;

    instantiation.create_generic_arguments(
        id,
        generic_parameters.as_ref(),
        self,
    )
}

/// Query key for retrieving the instantiation corresponding to a symbol
/// application.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<Instantiation>)]
#[extend(name = get_symbol_instantiation, by_val)]
pub struct SymbolKey {
    /// The symbol application to instantiate.
    pub symbol: Interned<Symbol>,
}

/// Query key for retrieving the instantiation corresponding to the parent of a
/// symbol application.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<Instantiation>)]
#[extend(name = get_symbol_parent_instantiation, by_val)]
pub struct SymbolParentKey {
    /// The symbol application whose parent instantiation is requested.
    pub symbol: Interned<Symbol>,
}

/// Query key for retrieving the instantiation corresponding to an associated
/// symbol application.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<Instantiation>)]
#[extend(name = get_associated_symbol_instantiation, by_val)]
pub struct AssociatedSymbolKey {
    /// The associated symbol application to instantiate.
    pub associated_symbol: Interned<AssociatedSymbol>,
}

/// Query key for retrieving the instantiation corresponding to a trait
/// reference.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Interned<Instantiation>)]
#[extend(name = get_trait_ref_instantiation, by_val)]
pub struct TraitRefKey {
    /// The trait reference to instantiate.
    pub trait_ref: Interned<TraitRef>,
}

async fn create_symbol_instantiation(
    symbol: &Symbol,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    let generic_parameters = engine.get_generic_parameters(symbol.id()).await;

    engine.intern(Instantiation::from_generic_arguments(
        symbol.generic_arguments().as_ref(),
        symbol.id(),
        generic_parameters.as_ref(),
        engine,
    ))
}

async fn create_symbol_parent_instantiation(
    symbol: &Symbol,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    let parent_id = engine.get_parent_global(symbol.id()).await.unwrap();
    let generic_parameters = engine.get_generic_parameters(parent_id).await;

    engine.intern(Instantiation::from_generic_arguments(
        symbol.generic_arguments().as_ref(),
        parent_id,
        generic_parameters.as_ref(),
        engine,
    ))
}

async fn create_associated_symbol_instantiation(
    associated_symbol: &AssociatedSymbol,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    let member_generic_parameters =
        engine.get_generic_parameters(associated_symbol.id()).await;

    let mut instantiation = Instantiation::from_generic_arguments(
        associated_symbol.member_generic_arguments().as_ref(),
        associated_symbol.id(),
        member_generic_parameters.as_ref(),
        engine,
    );

    let parent_id =
        engine.get_parent_global(associated_symbol.id()).await.unwrap();
    let parent_generic_parameters =
        engine.get_generic_parameters(parent_id).await;

    instantiation.append_from_generic_arguments(
        associated_symbol.parent_generic_arguments().as_ref(),
        parent_id,
        parent_generic_parameters.as_ref(),
        engine,
    );

    engine.intern(instantiation)
}

async fn create_trait_ref_instantiation(
    trait_ref: &TraitRef,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    let generic_parameters =
        engine.get_generic_parameters(trait_ref.trait_id()).await;

    engine.intern(Instantiation::from_generic_arguments(
        trait_ref.generic_arguments().as_ref(),
        trait_ref.trait_id(),
        generic_parameters.as_ref(),
        engine,
    ))
}

#[executor(config = Config)]
async fn symbol_instantiation_executor(
    key: &SymbolKey,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    create_symbol_instantiation(key.symbol.as_ref(), engine).await
}

#[distributed_slice(PERNIX_PROGRAM)]
static SYMBOL_INSTANTIATION_EXECUTOR: Registration<Config> =
    Registration::new::<SymbolKey, SymbolInstantiationExecutor>();

#[executor(config = Config)]
async fn symbol_parent_instantiation_executor(
    key: &SymbolParentKey,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    create_symbol_parent_instantiation(key.symbol.as_ref(), engine).await
}

#[distributed_slice(PERNIX_PROGRAM)]
static SYMBOL_PARENT_INSTANTIATION_EXECUTOR: Registration<Config> =
    Registration::new::<SymbolParentKey, SymbolParentInstantiationExecutor>();

#[executor(config = Config)]
async fn associated_symbol_instantiation_executor(
    key: &AssociatedSymbolKey,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    create_associated_symbol_instantiation(
        key.associated_symbol.as_ref(),
        engine,
    )
    .await
}

#[distributed_slice(PERNIX_PROGRAM)]
static ASSOCIATED_SYMBOL_INSTANTIATION_EXECUTOR: Registration<Config> =
    Registration::new::<
        AssociatedSymbolKey,
        AssociatedSymbolInstantiationExecutor,
    >();

#[executor(config = Config)]
async fn trait_ref_instantiation_executor(
    key: &TraitRefKey,
    engine: &TrackedEngine,
) -> Interned<Instantiation> {
    create_trait_ref_instantiation(key.trait_ref.as_ref(), engine).await
}

#[distributed_slice(PERNIX_PROGRAM)]
static TRAIT_REF_INSTANTIATION_EXECUTOR: Registration<Config> =
    Registration::new::<TraitRefKey, TraitRefInstantiationExecutor>();

impl Symbol {
    /// Creates an instantiation for this symbol by using the generic
    /// arguments supplied to this symbol and the symbol generic parameters.
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Interned<Instantiation> {
        engine.get_symbol_instantiation(engine.intern(self.clone())).await
    }

    /// Creates an instantiation for the parent of this symbol by using the
    /// generic arguments supplied to this symbol and the parent generic
    /// parameters.
    pub async fn create_instantiation_parent(
        &self,
        engine: &TrackedEngine,
    ) -> Interned<Instantiation> {
        engine
            .get_symbol_parent_instantiation(engine.intern(self.clone()))
            .await
    }
}

impl AssociatedSymbol {
    /// Creates an instantiation for this associated symbol by using both
    /// member and parent generic arguments.
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Interned<Instantiation> {
        engine
            .get_associated_symbol_instantiation(engine.intern(self.clone()))
            .await
    }
}

impl TraitRef {
    /// Creates an instantiation for this trait reference.
    pub async fn create_instantiation(
        &self,
        engine: &TrackedEngine,
    ) -> Interned<Instantiation> {
        engine.get_trait_ref_instantiation(engine.intern(self.clone())).await
    }
}

/// A term kind that can provide access to its instantiation mapping table.
#[allow(missing_docs)]
pub trait Element:
    Clone + Ord + StableHash + Identifiable + Send + Sync + 'static
{
    fn get(
        instantiation: &Instantiation,
    ) -> &BTreeMap<Interned<Self>, Interned<Self>>
    where
        Self: Sized;

    fn get_mut(
        instantiation: &mut Instantiation,
    ) -> &mut BTreeMap<Interned<Self>, Interned<Self>>
    where
        Self: Sized;
}

impl Element for Lifetime {
    fn get(
        instantiation: &Instantiation,
    ) -> &BTreeMap<Interned<Self>, Interned<Self>> {
        &instantiation.lifetimes
    }

    fn get_mut(
        instantiation: &mut Instantiation,
    ) -> &mut BTreeMap<Interned<Self>, Interned<Self>> {
        &mut instantiation.lifetimes
    }
}

impl Element for Type {
    fn get(
        instantiation: &Instantiation,
    ) -> &BTreeMap<Interned<Self>, Interned<Self>> {
        &instantiation.types
    }

    fn get_mut(
        instantiation: &mut Instantiation,
    ) -> &mut BTreeMap<Interned<Self>, Interned<Self>> {
        &mut instantiation.types
    }
}

impl Element for Constant {
    fn get(
        instantiation: &Instantiation,
    ) -> &BTreeMap<Interned<Self>, Interned<Self>> {
        &instantiation.constants
    }

    fn get_mut(
        instantiation: &mut Instantiation,
    ) -> &mut BTreeMap<Interned<Self>, Interned<Self>> {
        &mut instantiation.constants
    }
}

impl Element for Instance {
    fn get(
        instantiation: &Instantiation,
    ) -> &BTreeMap<Interned<Self>, Interned<Self>> {
        &instantiation.instances
    }

    fn get_mut(
        instantiation: &mut Instantiation,
    ) -> &mut BTreeMap<Interned<Self>, Interned<Self>> {
        &mut instantiation.instances
    }
}

#[cfg(test)]
mod test;
