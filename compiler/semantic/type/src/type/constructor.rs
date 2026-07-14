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

/// A borrowed, constructor-specific view of a type application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ApplicationView<'a> {
    Primitive(Primitive),
    Lifetime(Lifetime),
    Reference(ReferenceView<'a>),
    Symbolic(SymbolicView<'a>),
    Tuple(TupleView<'a>),
    FunctionPointer(FunctionPointerView<'a>),
    AnonymousTraitInstance(AnonymousTraitInstanceView<'a>),
    InstanceAssociated(InstanceAssociatedView<'a>),
    EffectRowExtend(EffectRowExtendView<'a>),
    EffectRowEmpty,
}

/// A borrowed view of a reference application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ReferenceView<'a> {
    reference: &'a Reference,
    lifetime: &'a Interned<Type2>,
    referent: &'a Interned<Type2>,
}

impl<'a> ReferenceView<'a> {
    #[must_use]
    pub const fn mutability(&self) -> Mutability { self.reference.mutability() }

    #[must_use]
    pub const fn lifetime(&self) -> &'a Interned<Type2> { self.lifetime }

    #[must_use]
    pub const fn referent(&self) -> &'a Interned<Type2> { self.referent }
}

/// A borrowed view of a symbolic application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymbolicView<'a> {
    symbolic: &'a Symbolic,
    generic_arguments: &'a [Interned<Type2>],
}

impl<'a> SymbolicView<'a> {
    #[must_use]
    pub const fn symbol_id(&self) -> GlobalSymbolID {
        self.symbolic.symbol_id()
    }

    #[must_use]
    pub const fn binder(&self) -> &'a Binder { self.symbolic.binder() }

    #[must_use]
    pub const fn generic_arguments(&self) -> &'a [Interned<Type2>] {
        self.generic_arguments
    }
}

/// A borrowed view of a tuple application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TupleView<'a> {
    tuple: &'a Tuple,
    elements: &'a [Interned<Type2>],
}

impl<'a> TupleView<'a> {
    #[must_use]
    pub const fn elements(&self) -> &'a [Interned<Type2>] { self.elements }

    #[must_use]
    pub fn unpacked_positions(&self) -> &'a [usize] {
        &self.tuple.unpacked_positions
    }
}

/// A borrowed view of a function-pointer application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionPointerView<'a> {
    function_pointer: &'a FunctionPointer,
    parameter_types: &'a [Interned<Type2>],
    return_type: &'a Interned<Type2>,
}

impl<'a> FunctionPointerView<'a> {
    #[must_use]
    pub const fn binder(&self) -> &'a Binder { &self.function_pointer.binder }

    #[must_use]
    pub const fn parameter_types(&self) -> &'a [Interned<Type2>] {
        self.parameter_types
    }

    #[must_use]
    pub const fn return_type(&self) -> &'a Interned<Type2> { self.return_type }
}

/// A borrowed view of an anonymous trait-instance application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AnonymousTraitInstanceView<'a> {
    anonymous_trait_instance: &'a AnonymousTraitInstance,
}

impl AnonymousTraitInstanceView<'_> {
    #[must_use]
    pub const fn trait_id(&self) -> GlobalSymbolID {
        self.anonymous_trait_instance.trait_id()
    }
}

/// A borrowed view of an instance-associated application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InstanceAssociatedView<'a> {
    instance_associated: &'a InstanceAssociated,
    instance: &'a Interned<Type2>,
    generic_arguments: &'a [Interned<Type2>],
}

impl<'a> InstanceAssociatedView<'a> {
    #[must_use]
    pub const fn trait_associated_id(&self) -> GlobalSymbolID {
        self.instance_associated.trait_associated_id()
    }

    #[must_use]
    pub const fn instance(&self) -> &'a Interned<Type2> { self.instance }

    #[must_use]
    pub const fn generic_arguments(&self) -> &'a [Interned<Type2>] {
        self.generic_arguments
    }
}

/// A borrowed view of an effect-row extension application.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EffectRowExtendView<'a> {
    effect_row_extend: &'a EffectRowExtend,
    effect_signature: &'a Interned<Type2>,
    row_tail: &'a Interned<Type2>,
}

impl<'a> EffectRowExtendView<'a> {
    #[must_use]
    pub const fn label(&self) -> &'a Interned<str> {
        self.effect_row_extend.label()
    }

    #[must_use]
    pub const fn effect_signature(&self) -> &'a Interned<Type2> {
        self.effect_signature
    }

    #[must_use]
    pub const fn row_tail(&self) -> &'a Interned<Type2> { self.row_tail }
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
    pub(super) const fn new(
        constructor: Constructor,
        arguments: Interned<[Interned<Type2>]>,
    ) -> Self {
        Self { constructor, arguments }
    }

    #[must_use]
    pub const fn arguments(&self) -> &Interned<[Interned<Type2>]> {
        &self.arguments
    }

    /// Returns a constructor-specific borrowed view of this application.
    ///
    /// # Panics
    ///
    /// Panics when the application's arguments do not satisfy the layout
    /// required by its constructor.
    #[must_use]
    pub fn view(&self) -> ApplicationView<'_> {
        match &self.constructor {
            Constructor::Primitive(primitive) => {
                assert!(
                    self.arguments.is_empty(),
                    "primitive applications require no arguments"
                );
                ApplicationView::Primitive(*primitive)
            }
            Constructor::Lifetime(lifetime) => {
                assert!(
                    self.arguments.is_empty(),
                    "lifetime applications require no arguments"
                );
                ApplicationView::Lifetime(*lifetime)
            }
            Constructor::Reference(reference) => {
                let [lifetime, referent] = &self.arguments[..] else {
                    panic!(
                        "reference applications require exactly two arguments"
                    )
                };

                ApplicationView::Reference(ReferenceView {
                    reference,
                    lifetime,
                    referent,
                })
            }
            Constructor::Symbolic(symbolic) => {
                ApplicationView::Symbolic(SymbolicView {
                    symbolic,
                    generic_arguments: &self.arguments,
                })
            }
            Constructor::Tuple(tuple) => ApplicationView::Tuple(TupleView {
                tuple,
                elements: &self.arguments,
            }),
            Constructor::FunctionPointer(function_pointer) => {
                let Some((return_type, parameter_types)) =
                    self.arguments.split_last()
                else {
                    panic!(
                        "function-pointer applications require a return type"
                    )
                };

                ApplicationView::FunctionPointer(FunctionPointerView {
                    function_pointer,
                    parameter_types,
                    return_type,
                })
            }
            Constructor::AnonymousTraitInstance(anonymous_trait_instance) => {
                assert!(
                    self.arguments.is_empty(),
                    "anonymous trait-instance applications require no \
                     arguments"
                );
                ApplicationView::AnonymousTraitInstance(
                    AnonymousTraitInstanceView { anonymous_trait_instance },
                )
            }
            Constructor::InstanceAssociated(instance_associated) => {
                let Some((instance, generic_arguments)) =
                    self.arguments.split_first()
                else {
                    panic!(
                        "instance-associated applications require an instance"
                    )
                };

                ApplicationView::InstanceAssociated(InstanceAssociatedView {
                    instance_associated,
                    instance,
                    generic_arguments,
                })
            }
            Constructor::EffectRowExtend(effect_row_extend) => {
                let [effect_signature, row_tail] = &self.arguments[..] else {
                    panic!(
                        "effect-row extension applications require exactly \
                         two arguments"
                    )
                };

                ApplicationView::EffectRowExtend(EffectRowExtendView {
                    effect_row_extend,
                    effect_signature,
                    row_tail,
                })
            }
            Constructor::EffectRowEmpty => {
                assert!(
                    self.arguments.is_empty(),
                    "empty effect-row applications require no arguments"
                );
                ApplicationView::EffectRowEmpty
            }
        }
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
    const TRAIT_ID: GlobalSymbolID =
        TargetID::TEST.make_global(SymbolID::from_u128(2));
    const ASSOCIATED_ID: GlobalSymbolID =
        TargetID::TEST.make_global(SymbolID::from_u128(3));

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

    // input: int32
    // premise: {}
    // output: Primitive(Int32)
    #[tokio::test]
    async fn primitive_application_view_exposes_primitive() {
        let engine = create_engine().await;
        let primitive = Type2::new_primitive(Primitive::Int32, &engine);
        let Type2::Application(application) = primitive.as_ref() else {
            panic!("expected primitive application");
        };

        assert_eq!(
            application.view(),
            ApplicationView::Primitive(Primitive::Int32)
        );
    }

    // input: 'static
    // premise: {}
    // output: Lifetime(Static)
    #[tokio::test]
    async fn lifetime_application_view_exposes_lifetime() {
        let engine = create_engine().await;
        let lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
        let Type2::Application(application) = lifetime.as_ref() else {
            panic!("expected lifetime application");
        };

        assert_eq!(
            application.view(),
            ApplicationView::Lifetime(Lifetime::Static)
        );
    }

    // input: &mut 'static bool
    // premise: {}
    // output: Mutable, 'static, bool
    #[tokio::test]
    async fn reference_application_view_exposes_semantic_arguments() {
        let engine = create_engine().await;
        let lifetime = Type2::new_lifetime(Lifetime::Static, &engine);
        let referent = Type2::new_primitive(Primitive::Bool, &engine);
        let reference = Type2::new_reference(
            lifetime.clone(),
            referent.clone(),
            Mutability::Mutable,
            &engine,
        );
        let Type2::Application(application) = reference.as_ref() else {
            panic!("expected reference application");
        };
        let ApplicationView::Reference(reference) = application.view() else {
            panic!("expected reference view");
        };

        assert_eq!(reference.mutability(), Mutability::Mutable);
        assert_eq!(reference.lifetime(), &lifetime);
        assert_eq!(reference.referent(), &referent);
    }

    // input: for<'a> Effect[bool, int32]
    // premise: Effect binds one lifetime
    // output: Effect, binder ['a], [bool, int32]
    #[tokio::test]
    async fn symbolic_application_view_exposes_metadata_and_arguments() {
        let engine = create_engine().await;
        let binder = Binder::new(engine.intern_unsized(vec![TyKind::Lifetime]));
        let arguments = [
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Int32, &engine),
        ];
        let symbolic = Type2::new_symbolic_with_binder(
            EFFECT_ID,
            binder.clone(),
            arguments.clone(),
            &engine,
        );
        let Type2::Application(application) = symbolic.as_ref() else {
            panic!("expected symbolic application");
        };
        let ApplicationView::Symbolic(symbolic) = application.view() else {
            panic!("expected symbolic view");
        };

        assert_eq!(symbolic.symbol_id(), EFFECT_ID);
        assert_eq!(symbolic.binder(), &binder);
        assert_eq!(symbolic.generic_arguments(), &arguments);
    }

    // input: (bool, ...int32, uint64)
    // premise: element 1 is unpacked
    // output: [bool, int32, uint64], [1]
    #[tokio::test]
    async fn tuple_application_view_exposes_elements_and_unpacking() {
        let engine = create_engine().await;
        let elements = [
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Int32, &engine),
            Type2::new_primitive(Primitive::Uint64, &engine),
        ];
        let tuple =
            Type2::new_tuple_with_unpack(elements.clone(), [1], &engine);
        let Type2::Application(application) = tuple.as_ref() else {
            panic!("expected tuple application");
        };
        let ApplicationView::Tuple(tuple) = application.view() else {
            panic!("expected tuple view");
        };

        assert_eq!(tuple.elements(), &elements);
        assert_eq!(tuple.unpacked_positions(), &[1]);
    }

    // input: for<'a> fn(bool, int32) -> uint64
    // premise: the function pointer binds one lifetime
    // output: binder ['a], parameters [bool, int32], return uint64
    #[tokio::test]
    async fn function_pointer_application_view_splits_parameters_and_return() {
        let engine = create_engine().await;
        let binder = Binder::new(engine.intern_unsized(vec![TyKind::Lifetime]));
        let parameters = [
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Int32, &engine),
        ];
        let return_type = Type2::new_primitive(Primitive::Uint64, &engine);
        let function_pointer = Type2::new_function_pointer_with_binder(
            binder.clone(),
            parameters.clone(),
            return_type.clone(),
            &engine,
        );
        let Type2::Application(application) = function_pointer.as_ref() else {
            panic!("expected function-pointer application");
        };
        let ApplicationView::FunctionPointer(function_pointer) =
            application.view()
        else {
            panic!("expected function-pointer view");
        };

        assert_eq!(function_pointer.binder(), &binder);
        assert_eq!(function_pointer.parameter_types(), &parameters);
        assert_eq!(function_pointer.return_type(), &return_type);
    }

    // input: anonymous instance of Trait
    // premise: {}
    // output: Trait
    #[tokio::test]
    async fn anonymous_trait_instance_view_exposes_trait_id() {
        let engine = create_engine().await;
        let instance = Type2::new_anonymous_trait_instance(TRAIT_ID, &engine);
        let Type2::Application(application) = instance.as_ref() else {
            panic!("expected anonymous trait-instance application");
        };
        let ApplicationView::AnonymousTraitInstance(instance) =
            application.view()
        else {
            panic!("expected anonymous trait-instance view");
        };

        assert_eq!(instance.trait_id(), TRAIT_ID);
    }

    // input: Associated[Instance, bool, int32]
    // premise: Instance is the receiver and the remaining types are generics
    // output: Associated, Instance, [bool, int32]
    #[tokio::test]
    async fn instance_associated_view_separates_instance_and_generics() {
        let engine = create_engine().await;
        let instance = Type2::new_anonymous_trait_instance(TRAIT_ID, &engine);
        let generic_arguments = [
            Type2::new_primitive(Primitive::Bool, &engine),
            Type2::new_primitive(Primitive::Int32, &engine),
        ];
        let associated = Type2::new_instance_associated(
            ASSOCIATED_ID,
            instance.clone(),
            generic_arguments.clone(),
            &engine,
        );
        let Type2::Application(application) = associated.as_ref() else {
            panic!("expected instance-associated application");
        };
        let ApplicationView::InstanceAssociated(associated) =
            application.view()
        else {
            panic!("expected instance-associated view");
        };

        assert_eq!(associated.trait_associated_id(), ASSOCIATED_ID);
        assert_eq!(associated.instance(), &instance);
        assert_eq!(associated.generic_arguments(), &generic_arguments);
    }

    // input: {Console: Effect | tail}
    // premise: the extension constructor takes (signature, tail)
    // output: label Console, signature Effect, and row tail
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
        let ApplicationView::EffectRowExtend(extension) = application.view()
        else {
            panic!("expected effect-row extension");
        };

        assert_eq!(extension.label(), &label);
        assert_eq!(extension.effect_signature(), &signature);
        assert_eq!(extension.row_tail(), &tail);
    }

    // input: {}
    // premise: {}
    // output: EffectRowEmpty
    #[tokio::test]
    async fn empty_effect_row_application_view_has_no_payload() {
        let engine = create_engine().await;
        let empty = Type2::new_effect_row_empty(&engine);
        let Type2::Application(application) = empty.as_ref() else {
            panic!("expected empty effect-row application");
        };

        assert_eq!(application.view(), ApplicationView::EffectRowEmpty);
    }

    // input: malformed reference with one argument
    // premise: references require a lifetime and referent
    // output: panic
    #[tokio::test]
    #[should_panic(
        expected = "reference applications require exactly two arguments"
    )]
    async fn reference_application_view_rejects_malformed_layout() {
        let engine = create_engine().await;
        let application = Application::new(
            Constructor::Reference(Reference::new(Mutability::Immutable)),
            engine.intern_unsized(vec![Type2::new_lifetime(
                Lifetime::Static,
                &engine,
            )]),
        );

        let _ = application.view();
    }

    // input: malformed effect-row extension with one argument
    // premise: extensions require a signature and row tail
    // output: panic
    #[tokio::test]
    #[should_panic(expected = "effect-row extension applications require \
                               exactly two arguments")]
    async fn effect_row_extension_view_rejects_malformed_layout() {
        let engine = create_engine().await;
        let application = Application::new(
            Constructor::EffectRowExtend(EffectRowExtend::new(
                engine.intern_unsized("Console".to_owned()),
            )),
            engine.intern_unsized(vec![Type2::new_primitive(
                Primitive::Bool,
                &engine,
            )]),
        );

        let _ = application.view();
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
