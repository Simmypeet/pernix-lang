use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::GlobalSymbolID;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    generic_parameters::{GenericParameterID, get_generic_parameters2},
    r#type::{
        bound::{Binder, BoundVariable},
        constructor::{
            AnonymousTraitInstance, Application, Constructor, EffectRowExtend,
            FunctionPointer, InstanceAssociated, Lifetime, Mutability,
            Primitive, Reference, Symbolic, Tuple,
        },
        inference::InferenceVariable,
        skolem::SkolemizedVariable,
        universe::UniverseIndex,
    },
};

pub mod bound;
pub mod constructor;
pub mod inference;
pub mod kind;
pub mod skolem;
pub mod universe;

pub use constructor::rewrite;

/// The main representation of types in the compiler.
///
/// The representation is highly homogeneous, making easy to manipulate and
/// reason about them.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
    EnumAsInner,
)]
pub enum Type2 {
    GenericParameter(GenericParameterID),
    InferenceVariable(InferenceVariable),
    BoundVariable(BoundVariable),
    SkolemizedVariable(SkolemizedVariable),
    Application(Application),
}

impl Type2 {
    /// Interns a type constructor application with the given arguments.
    #[must_use]
    pub fn new_application(
        constructor: Constructor,
        arguments: impl IntoIterator<Item = Interned<Self>>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        engine.intern(Self::Application(Application::new(
            constructor,
            engine.intern_unsized(arguments.into_iter().collect::<Vec<_>>()),
        )))
    }

    /// Interns a primitive type.
    #[must_use]
    pub fn new_primitive(
        primitive: Primitive,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(Constructor::Primitive(primitive), [], engine)
    }

    /// Interns a simple lifetime type.
    #[must_use]
    pub fn new_lifetime(
        lifetime: Lifetime,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(Constructor::Lifetime(lifetime), [], engine)
    }

    /// Interns a reference type.
    #[must_use]
    pub fn new_reference(
        lifetime: Interned<Self>,
        pointee: Interned<Self>,
        mutability: Mutability,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(
            Constructor::Reference(Reference::new(mutability)),
            [lifetime, pointee],
            engine,
        )
    }

    /// Interns an immutable reference type.
    #[must_use]
    pub fn new_immutable_reference(
        lifetime: Interned<Self>,
        pointee: Interned<Self>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_reference(lifetime, pointee, Mutability::Immutable, engine)
    }

    /// Interns a symbolic type with its generic arguments.
    #[must_use]
    pub fn new_symbolic(
        symbol_id: GlobalSymbolID,
        arguments: impl IntoIterator<Item = Interned<Self>>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_symbolic_with_binder(
            symbol_id,
            Binder::new(engine.intern_unsized(Vec::new())),
            arguments,
            engine,
        )
    }

    /// Interns a symbolic type whose arguments are under the given binder.
    #[must_use]
    pub fn new_symbolic_with_binder(
        symbol_id: GlobalSymbolID,
        binder: Binder,
        arguments: impl IntoIterator<Item = Interned<Self>>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(
            Constructor::Symbolic(Symbolic::new(symbol_id, binder)),
            arguments,
            engine,
        )
    }

    /// Interns a tuple type without unpacked elements.
    #[must_use]
    pub fn new_tuple(
        arguments: impl IntoIterator<Item = Interned<Self>>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_tuple_with_unpack(arguments, [], engine)
    }

    /// Interns a tuple type, including the positions of unpacked elements.
    #[must_use]
    pub fn new_tuple_with_unpack(
        arguments: impl IntoIterator<Item = Interned<Self>>,
        unpacked_positions: impl IntoIterator<Item = usize>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(
            Constructor::Tuple(Tuple::new(engine.intern_unsized(
                unpacked_positions.into_iter().collect::<Vec<_>>(),
            ))),
            arguments,
            engine,
        )
    }

    /// Interns a function pointer type without bound variables.
    #[must_use]
    pub fn new_function_pointer(
        argument_types: impl IntoIterator<Item = Interned<Self>>,
        return_type: Interned<Self>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_function_pointer_with_binder(
            Binder::new(engine.intern_unsized(Vec::new())),
            argument_types,
            return_type,
            engine,
        )
    }

    /// Interns a function pointer type with the given binder.
    #[must_use]
    pub fn new_function_pointer_with_binder(
        binder: Binder,
        argument_types: impl IntoIterator<Item = Interned<Self>>,
        return_type: Interned<Self>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(
            Constructor::FunctionPointer(FunctionPointer::new(binder)),
            argument_types.into_iter().chain(std::iter::once(return_type)),
            engine,
        )
    }

    /// Interns a function pointer type binding the given number of
    /// higher-ranked lifetimes.
    #[must_use]
    pub fn new_function_pointer_with_higher_ranked_lifetimes(
        higher_ranked_lifetime_count: usize,
        argument_types: impl IntoIterator<Item = Interned<Self>>,
        return_type: Interned<Self>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_function_pointer_with_binder(
            Binder::new(engine.intern_unsized(vec![
                kind::TyKind::Lifetime;
                higher_ranked_lifetime_count
            ])),
            argument_types,
            return_type,
            engine,
        )
    }

    /// Interns an effect-row extension. The effect signature is the first
    /// argument and the row tail is the second argument.
    #[must_use]
    pub fn new_effect_row_extend(
        label: Interned<str>,
        effect_signature: Interned<Self>,
        row_tail: Interned<Self>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(
            Constructor::EffectRowExtend(EffectRowExtend::new(label)),
            [effect_signature, row_tail],
            engine,
        )
    }

    /// Interns the closed empty effect row.
    #[must_use]
    pub fn new_effect_row_empty(engine: &TrackedEngine) -> Interned<Self> {
        Self::new_application(Constructor::EffectRowEmpty, [], engine)
    }

    /// Interns the anonymous instance of a trait.
    #[must_use]
    pub fn new_anonymous_trait_instance(
        trait_id: GlobalSymbolID,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(
            Constructor::AnonymousTraitInstance(AnonymousTraitInstance::new(
                trait_id,
            )),
            [],
            engine,
        )
    }

    /// Interns an instance-associated type or instance.
    #[must_use]
    pub fn new_instance_associated(
        associated_id: GlobalSymbolID,
        instance: Interned<Self>,
        arguments: impl IntoIterator<Item = Interned<Self>>,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        Self::new_application(
            Constructor::InstanceAssociated(InstanceAssociated::new(
                associated_id,
            )),
            std::iter::once(instance).chain(arguments),
            engine,
        )
    }

    /// Interns a generic parameter type.
    #[must_use]
    pub fn new_generic_parameter(
        parameter_id: GenericParameterID,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        engine.intern(Self::GenericParameter(parameter_id))
    }

    /// Interns an inference variable type.
    #[must_use]
    pub fn new_inference_variable(
        variable: InferenceVariable,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        engine.intern(Self::InferenceVariable(variable))
    }

    /// Interns a bound variable type.
    #[must_use]
    pub fn new_bound_variable(
        variable: BoundVariable,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        engine.intern(Self::BoundVariable(variable))
    }

    /// Interns a skolemized variable type.
    #[must_use]
    pub fn new_skolemized_variable(
        variable: SkolemizedVariable,
        engine: &TrackedEngine,
    ) -> Interned<Self> {
        engine.intern(Self::SkolemizedVariable(variable))
    }

    /// Returns whether this type recursively contains any inference variable.
    #[must_use]
    pub fn contains_inference_variable(&self) -> bool {
        self.contains_inference_variable_matching(|_| true)
    }

    /// Returns whether this type recursively contains any inference variable
    /// that satisfies the given predicate.
    pub fn contains_inference_variable_matching(
        &self,
        mut predicate: impl FnMut(InferenceVariable) -> bool,
    ) -> bool {
        self.contains_inference_variable_matching_impl(&mut predicate)
    }

    fn contains_inference_variable_matching_impl(
        &self,
        predicate: &mut impl FnMut(InferenceVariable) -> bool,
    ) -> bool {
        match self {
            Self::InferenceVariable(variable) => predicate(*variable),

            Self::BoundVariable(_)
            | Self::GenericParameter(_)
            | Self::SkolemizedVariable(_) => false,

            Self::Application(application) => {
                application.arguments().iter().any(|argument| {
                    argument
                        .as_ref()
                        .contains_inference_variable_matching_impl(predicate)
                })
            }
        }
    }

    pub async fn kind(&self, engine: &TrackedEngine) -> kind::TyKind {
        match self {
            Self::GenericParameter(member_id) => engine
                .get_generic_parameters2(member_id.parent_id())
                .await[member_id.id()]
            .kind(),

            Self::InferenceVariable(inference_variable) => {
                inference_variable.kind()
            }

            Self::Application(application) => application.kind(engine).await,

            Self::BoundVariable(_) => todo!(),

            Self::SkolemizedVariable(skolemized_var) => skolemized_var.kind(),
        }
    }

    #[must_use]
    pub fn max_universe(&self) -> UniverseIndex {
        match self {
            Self::GenericParameter(_) | Self::BoundVariable(_) => {
                UniverseIndex::root()
            }

            Self::InferenceVariable(inference_variable) => {
                inference_variable.universe_index()
            }

            Self::SkolemizedVariable(skolemized_variable) => {
                skolemized_variable.universe_index()
            }

            Self::Application(application) => application.max_universe(),
        }
    }
}

#[cfg(test)]
mod test {
    use pernixc_qbice::create_minimal_engine as create_engine;

    use super::*;
    use crate::r#type::{
        inference::InferenceVariable, skolem::SkolemizedVariable,
    };

    // input: effect-signature inference/skolem and effect-row binder entries
    // premise: {}
    // output: each representation retains its effect kind
    #[tokio::test]
    async fn variable_representations_accept_effect_kinds() {
        let engine = create_engine().await;
        let inference = InferenceVariable::new(
            0,
            kind::TyKind::EffectSignature,
            UniverseIndex::root(),
        );
        let skolem = SkolemizedVariable::new(
            1,
            kind::TyKind::EffectRow,
            UniverseIndex::root(),
        );
        let binder = Binder::new(engine.intern_unsized(vec![
            kind::TyKind::EffectSignature,
            kind::TyKind::EffectRow,
        ]));
        let bound_signature =
            Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
        let bound_row =
            Type2::new_bound_variable(BoundVariable::new(0, 1), &engine);
        let replacements = [
            Type2::new_inference_variable(inference, &engine),
            Type2::new_skolemized_variable(skolem, &engine),
        ];

        assert_eq!(inference.kind(), kind::TyKind::EffectSignature);
        assert_eq!(skolem.kind(), kind::TyKind::EffectRow);
        assert_eq!(binder.kinds().collect::<Vec<_>>(), vec![
            kind::TyKind::EffectSignature,
            kind::TyKind::EffectRow
        ]);
        assert_eq!(
            binder.instantiate(&bound_signature, &replacements, &engine),
            replacements[0]
        );
        assert_eq!(
            binder.instantiate(&bound_row, &replacements, &engine),
            replacements[1]
        );
    }

    // input: {X: ?signature | ?tail}
    // premise: both arguments are inference variables
    // output: occurs traversal finds either variable by predicate
    #[tokio::test]
    async fn effect_row_occurs_traversal_visits_both_arguments() {
        let engine = create_engine().await;
        let signature = InferenceVariable::new(
            0,
            kind::TyKind::EffectSignature,
            UniverseIndex::root(),
        );
        let tail = InferenceVariable::new(
            1,
            kind::TyKind::EffectRow,
            UniverseIndex::root(),
        );
        let row = Type2::new_effect_row_extend(
            engine.intern_unsized("X".to_owned()),
            Type2::new_inference_variable(signature, &engine),
            Type2::new_inference_variable(tail, &engine),
            &engine,
        );

        assert!(
            row.contains_inference_variable_matching(|var| var == signature)
        );
        assert!(row.contains_inference_variable_matching(|var| var == tail));
    }

    // input: {X: ?signature@U1 | ?tail@U2}
    // premise: U2 is deeper than U1
    // output: U2
    #[tokio::test]
    async fn effect_row_max_universe_visits_both_arguments() {
        let engine = create_engine().await;
        let lower_universe = UniverseIndex::root().next();
        let higher_universe = lower_universe.next();
        let row = Type2::new_effect_row_extend(
            engine.intern_unsized("X".to_owned()),
            Type2::new_inference_variable(
                InferenceVariable::new(
                    0,
                    kind::TyKind::EffectSignature,
                    lower_universe,
                ),
                &engine,
            ),
            Type2::new_inference_variable(
                InferenceVariable::new(
                    1,
                    kind::TyKind::EffectRow,
                    higher_universe,
                ),
                &engine,
            ),
            &engine,
        );

        assert_eq!(row.max_universe(), higher_universe);
    }
}
