use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
};
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::r#type::{
    Type2, bound::Binder, kind::TyKind, universe::UniverseIndex,
};

mod destructure;
mod reduction;
pub mod rewrite;

/// Simple primitive types
///
/// Kind: Type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display("int8")]
    Int8,
    #[display("int16")]
    Int16,
    #[display("int32")]
    Int32,
    #[display("int64")]
    Int64,
    #[display("uint8")]
    Uint8,
    #[display("uint16")]
    Uint16,
    #[display("uint32")]
    Uint32,
    #[display("uint64")]
    Uint64,
    #[display("float32")]
    Float32,
    #[display("float64")]
    Float64,
    #[display("bool")]
    Bool,
    #[display("usize")]
    Usize,
    #[display("isize")]
    Isize,
}

/// Represents a simple lifetime.
///
/// Kind: Lifetime
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub enum Lifetime {
    Static,
    Erased,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub enum Mutability {
    Mutable,
    Immutable,
}

/// Represents a reference type constructor, such as `&T` or `&mut T`.
///
/// Kind: (Lifetime, Type) -> Type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Reference {
    mutability: Mutability,
}

impl Reference {
    #[must_use]
    pub const fn new(mutability: Mutability) -> Self { Self { mutability } }

    #[must_use]
    pub const fn mutability(&self) -> Mutability { self.mutability }
}

/// Represents a symbolic type constructor, supplying generic arguments to a
/// symbol, such as `Option<T>` or `SomeInstance<X, Y, Z>`.
///
/// Kind: ( <Symbol's Generic Parameter Kinds> ) -> (Type | Instance)
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
)]
pub struct Symbolic {
    symbolic_id: GlobalSymbolID,
    binder: Binder,
}

impl Symbolic {
    #[must_use]
    pub const fn new(symbolic_id: GlobalSymbolID, binder: Binder) -> Self {
        Self { symbolic_id, binder }
    }

    #[must_use]
    pub const fn symbol_id(&self) -> GlobalSymbolID { self.symbolic_id }

    /// Returns the variables bound over the symbolic arguments.
    #[must_use]
    pub const fn binder(&self) -> &Binder { &self.binder }
}

/// Represents a tuple type constructor, such as `(T1, T2, T3)`. Which can
/// include `Unpacked` elements.
///
/// Kinds: ( Type* ) -> Type
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
)]
pub struct Tuple {
    unpacked_positions: Interned<[usize]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TupleShape {
    Regular,
    Unpacked(usize),
}

impl Tuple {
    #[must_use]
    pub const fn new(unpacked_positions: Interned<[usize]>) -> Self {
        Self { unpacked_positions }
    }

    fn shape(&self) -> Option<TupleShape> {
        match self.unpacked_positions.len() {
            0 => Some(TupleShape::Regular),
            1 => Some(TupleShape::Unpacked(self.unpacked_positions[0])),
            _ => None,
        }
    }
}

/// Represents an associated member of an instance, such as an associated type
/// or an associated instance.
///
/// The first argument is always the instance, and the remaining arguments are
/// the generic arguments supplied to the associated member.
///
/// Kind: ( Instance, <Associated's Generic Parameter Kinds> ) -> (Type |
/// Instance)
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct InstanceAssociated {
    trait_associated_id: GlobalSymbolID,
}

impl InstanceAssociated {
    #[must_use]
    pub const fn new(trait_associated_id: GlobalSymbolID) -> Self {
        Self { trait_associated_id }
    }

    #[must_use]
    pub const fn trait_associated_id(&self) -> GlobalSymbolID {
        self.trait_associated_id
    }
}

/// Refers to an instance that is coupled with a trait when user writes
/// `this.Associated` syntax.
///
/// For example, consider the following program:
///
/// ```pnx
/// public trait MyTrait:
///   public type Assoc
///
///   public function a(a: this.Assoc)
/// ```
///
/// The type `this.Assoc` declared under `MyTrait` would be represented as an
/// instance associated type where the instance is `AnonymousTraitInstance
/// {MyTrait}`.
///
/// The `AnonymousTraitInstance` can only appear under the trait declaration.
///
/// Kind: Instance
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct AnonymousTraitInstance {
    trait_id: GlobalSymbolID,
}

impl AnonymousTraitInstance {
    #[must_use]
    pub const fn new(trait_id: GlobalSymbolID) -> Self { Self { trait_id } }

    #[must_use]
    pub const fn trait_id(&self) -> GlobalSymbolID { self.trait_id }
}

/// Represents a function pointer, such as `fn(T1, T2) -> T3`.
///
/// The last type argument is assumed to be return type, and the preceding type
/// arguments are assumed to be parameter types. The binder contains the late
/// bound lifetimes of the function pointer, if any.
///
/// Kind: (Type*) -> Type
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
)]
pub struct FunctionPointer {
    binder: Binder,
}

impl FunctionPointer {
    #[must_use]
    pub const fn new(binder: Binder) -> Self { Self { binder } }
}

/// Extends an effect row with a labeled effect signature.
///
/// Kind: `(EffectSignature, EffectRow) -> EffectRow`
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
)]
pub struct EffectRowExtend {
    label: Interned<str>,
}

impl EffectRowExtend {
    #[must_use]
    pub const fn new(label: Interned<str>) -> Self { Self { label } }

    #[must_use]
    pub const fn label(&self) -> &Interned<str> { &self.label }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    StableHash,
    Encode,
    Decode,
)]
pub enum Constructor {
    Primitive(Primitive),
    Lifetime(Lifetime),
    Reference(Reference),
    Symbolic(Symbolic),
    Tuple(Tuple),
    FunctionPointer(FunctionPointer),
    AnonymousTraitInstance(AnonymousTraitInstance),
    InstanceAssociated(InstanceAssociated),
    EffectRowExtend(EffectRowExtend),
    EffectRowEmpty,
}

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
)]
pub struct Application {
    constructor: Constructor,
    arguments: Interned<[Interned<Type2>]>,
}

impl Application {
    #[must_use]
    pub const fn new(
        constructor: Constructor,
        arguments: Interned<[Interned<Type2>]>,
    ) -> Self {
        Self { constructor, arguments }
    }

    #[must_use]
    pub const fn arguments(&self) -> &Interned<[Interned<Type2>]> {
        &self.arguments
    }

    #[must_use]
    pub const fn binder(&self) -> Option<&Binder> {
        match &self.constructor {
            Constructor::Symbolic(symbolic) => Some(&symbolic.binder),
            Constructor::FunctionPointer(fp) => Some(&fp.binder),
            Constructor::Primitive(_)
            | Constructor::Lifetime(_)
            | Constructor::Reference(_)
            | Constructor::Tuple(_)
            | Constructor::AnonymousTraitInstance(_)
            | Constructor::InstanceAssociated(_)
            | Constructor::EffectRowExtend(_)
            | Constructor::EffectRowEmpty => None,
        }
    }

    #[must_use]
    pub const fn constructor(&self) -> &Constructor { &self.constructor }

    #[must_use]
    pub fn max_universe(&self) -> UniverseIndex {
        self.arguments
            .iter()
            .map(|argument| argument.max_universe())
            .max()
            .unwrap_or(UniverseIndex::root())
    }

    pub async fn kind(&self, engine: &TrackedEngine) -> TyKind {
        match &self.constructor {
            Constructor::Tuple(_)
            | Constructor::Primitive(_)
            | Constructor::FunctionPointer(_)
            | Constructor::Reference(_) => TyKind::Type,

            Constructor::Symbolic(symbol) => {
                let kind = engine.get_kind(symbol.symbolic_id).await;

                match kind {
                    Kind::Struct | Kind::Enum => TyKind::Type,
                    Kind::Instance => TyKind::Instance,
                    Kind::Effect => TyKind::EffectSignature,

                    _ => panic!(
                        "Expected an ADT, instance, or effect, but got a \
                         different kind"
                    ),
                }
            }

            Constructor::Lifetime(_) => TyKind::Lifetime,

            Constructor::AnonymousTraitInstance(_) => TyKind::Instance,

            Constructor::InstanceAssociated(inst) => {
                let kind = engine.get_kind(inst.trait_associated_id).await;

                match kind {
                    Kind::InstanceAssociatedType => TyKind::Type,
                    Kind::InstanceAssociatedInstance => TyKind::Instance,

                    _ => panic!(
                        "Expected an instance associated type or instance, \
                         but got a different kind"
                    ),
                }
            }

            Constructor::EffectRowExtend(_) | Constructor::EffectRowEmpty => {
                TyKind::EffectRow
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use pernixc_qbice::{
        Config, Engine, InMemoryFactory, TrackedEngine,
        create_minimal_engine as create_engine,
    };
    use pernixc_symbol::SymbolID;
    use pernixc_target::TargetID;
    use qbice::{
        executor, serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    };

    use super::*;
    use crate::r#type::inference::InferenceVariable;

    const EFFECT_ID: GlobalSymbolID =
        TargetID::TEST.make_global(SymbolID::from_u128(1));

    struct EffectKindExecutor;

    impl executor::Executor<pernixc_symbol::kind::Key, Config>
        for EffectKindExecutor
    {
        async fn execute(
            &self,
            _: &pernixc_symbol::kind::Key,
            _: &TrackedEngine,
        ) -> Kind {
            Kind::Effect
        }
    }

    async fn create_effect_engine() -> TrackedEngine {
        let mut engine = Engine::new_with(
            Plugin::default(),
            InMemoryFactory,
            SeededStableHasherBuilder::new(0),
        )
        .await
        .unwrap();

        engine.register_executor(Arc::new(EffectKindExecutor));

        Arc::new(engine).tracked().await
    }

    // input: Effect
    // premise: Effect is a symbol with Kind::Effect
    // output: EffectSignature
    #[tokio::test]
    async fn effect_symbolic_application_has_effect_signature_kind() {
        let engine = create_effect_engine().await;
        let effect = Type2::new_symbolic(EFFECT_ID, [], &engine);

        assert_eq!(effect.kind(&engine).await, TyKind::EffectSignature);
    }

    // input: {} and {Console: Effect | {}}
    // premise: Effect: EffectSignature
    // output: EffectRow and EffectRow
    #[tokio::test]
    async fn effect_row_applications_have_effect_row_kind() {
        let engine = create_effect_engine().await;
        let empty = Type2::new_effect_row_empty(&engine);
        let extended = Type2::new_effect_row_extend(
            engine.intern_unsized("Console".to_owned()),
            Type2::new_symbolic(EFFECT_ID, [], &engine),
            empty.clone(),
            &engine,
        );

        assert_eq!(empty.kind(&engine).await, TyKind::EffectRow);
        assert_eq!(extended.kind(&engine).await, TyKind::EffectRow);
    }

    // input: {Console: Effect | tail}
    // premise: the extension constructor takes (signature, tail)
    // output: label Console and arguments [Effect, tail]
    #[tokio::test]
    async fn effect_row_extension_retains_label_and_argument_order() {
        let engine = create_engine().await;
        let label: Interned<str> = engine.intern_unsized("Console".to_owned());
        let signature = Type2::new_inference_variable(
            InferenceVariable::new(
                0,
                TyKind::EffectSignature,
                UniverseIndex::root(),
            ),
            &engine,
        );
        let tail = Type2::new_inference_variable(
            InferenceVariable::new(1, TyKind::EffectRow, UniverseIndex::root()),
            &engine,
        );
        let row = Type2::new_effect_row_extend(
            label.clone(),
            signature.clone(),
            tail.clone(),
            &engine,
        );

        let Type2::Application(application) = row.as_ref() else {
            panic!("expected effect-row application");
        };
        let Constructor::EffectRowExtend(extension) = application.constructor()
        else {
            panic!("expected effect-row extension");
        };

        assert_eq!(extension.label(), &label);
        assert_eq!(&**application.arguments(), &[signature, tail]);
    }

    // input: fn(?T@U1, ?U@U2) -> bool
    // premise: the application arguments contain inference variables in U1/U2
    // output: U2
    #[tokio::test]
    async fn application_max_universe_returns_deepest_argument_universe() {
        let engine = create_engine().await;
        let lower_variable = InferenceVariable::new(
            0,
            TyKind::Type,
            UniverseIndex::root().next(),
        );
        let higher_variable = InferenceVariable::new(
            1,
            TyKind::Type,
            UniverseIndex::root().next().next(),
        );
        let bool_type = Type2::new_primitive(Primitive::Bool, &engine);
        let lower_argument =
            engine.intern(Type2::InferenceVariable(lower_variable));
        let higher_argument =
            engine.intern(Type2::InferenceVariable(higher_variable));
        let empty_binder = || Binder::new(engine.intern_unsized(Vec::new()));

        let application = Application::new(
            Constructor::FunctionPointer(FunctionPointer::new(empty_binder())),
            engine.intern_unsized(vec![
                lower_argument,
                higher_argument,
                bool_type,
            ]),
        );

        assert_eq!(
            application.max_universe(),
            UniverseIndex::root().next().next()
        );
    }
}
