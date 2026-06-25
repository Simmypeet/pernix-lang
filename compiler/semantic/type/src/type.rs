use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::GlobalSymbolID;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    generic_parameters::{GenericParameterID, get_generic_parameters},
    r#type::{
        bound::{Binder, BoundVariable},
        constructor::{
            AnonymousTraitInstance, Application, Constructor, FunctionPointer,
            InstanceAssociated, Lifetime, Mutability, Primitive, Reference,
            Symbolic, Tuple,
        },
        context::TyContext,
        inference::InferenceVariable,
        skolem::SkolemizedVariable,
    },
};

pub mod bound;
pub mod constructor;
pub mod context;
pub mod inference;
pub mod kind;
pub mod skolem;

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
pub enum Type {
    GenericParameter(GenericParameterID),
    InferenceVariable(InferenceVariable),
    BoundVariable(BoundVariable),
    SkolemizedVariable(SkolemizedVariable),
    Application(Application),
}

impl Type {
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
        Self::new_application(
            Constructor::Symbolic(Symbolic::new(symbol_id)),
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

    pub async fn kind(
        &self,
        engine: &TrackedEngine,
        ctx: &impl TyContext,
    ) -> kind::TyKind {
        match self {
            Self::GenericParameter(member_id) => engine
                .get_generic_parameters(member_id.parent_id())
                .await[member_id.id()]
            .kind(),

            Self::InferenceVariable(inference_variable) => {
                ctx.get_inference_variable_kind(inference_variable)
            }

            Self::Application(application) => application.kind(engine).await,

            Self::BoundVariable(bound_var) => {
                ctx.get_bound_variable_kind(bound_var)
            }

            Self::SkolemizedVariable(skolemized_var) => {
                ctx.get_skolemized_variable_kind(skolemized_var)
            }
        }
    }
}
