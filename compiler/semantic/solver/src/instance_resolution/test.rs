use std::{collections::HashMap, sync::Arc};

use pernixc_arena::ID;
use pernixc_qbice::{
    Engine, InMemoryFactory, PrecomputedExecutor, TrackedEngine,
};
use pernixc_symbol::{GlobalSymbolID, SymbolID, kind::Kind};
use pernixc_target::TargetID;
use pernixc_type::{
    generic_parameters::{
        self, GenericParameter, GenericParameterID, GenericParameterKind,
        GenericParameters2, InstanceParameterKind,
    },
    instance_associated,
    predicate::{Equality, Predicate2},
    symbol::{Symbol2, TraitRef2, TraitRefKey},
    r#type::{
        Type2,
        bound::{Binder, BoundVariable},
        constructor::{
            AnonymousTraitInstance, Constructor, InstanceAssociated, Primitive,
        },
        kind::TyKind,
        skolem::SkolemizedVariable,
    },
};
use qbice::{
    serialize::Plugin, stable_hash::SeededStableHasherBuilder,
    storage::intern::Interned,
};

use super::{
    GlobalInstanceCandidatesKey, InstanceSource, LexicalInstanceCandidate,
    LexicalInstanceCandidates, LexicalInstanceCandidatesKey, NormalFormFailure,
    ResolveError,
};
use crate::{
    constraints::Constraints,
    order::{InstanceOrderKey, Order},
    premise::Premise,
    solver::Solver,
};

const SITE: GlobalSymbolID = TargetID::TEST.make_global(SymbolID::from_u128(1));
const TRAIT: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(2));
const REQUIRED_TRAIT: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(3));
const GENERAL: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(10));
const SPECIFIC: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(11));
const REQUIRED: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(12));
const ASSOCIATED: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(13));

struct Inputs {
    lexical: Vec<LexicalInstanceCandidate>,
    globals: HashMap<GlobalSymbolID, Vec<GlobalSymbolID>>,
    trait_refs: HashMap<GlobalSymbolID, TraitRef2>,
    parameters: HashMap<GlobalSymbolID, GenericParameters2>,
    orders: HashMap<(GlobalSymbolID, GlobalSymbolID), Order>,
    kinds: HashMap<GlobalSymbolID, Kind>,
    parents: HashMap<GlobalSymbolID, Option<SymbolID>>,
    associated_values: HashMap<GlobalSymbolID, Interned<Type2>>,
}

impl Inputs {
    fn new() -> Self {
        Self {
            lexical: Vec::new(),
            globals: HashMap::new(),
            trait_refs: HashMap::new(),
            parameters: HashMap::new(),
            orders: HashMap::new(),
            kinds: HashMap::new(),
            parents: HashMap::new(),
            associated_values: HashMap::new(),
        }
    }
}

async fn create_engine(inputs: Inputs) -> TrackedEngine {
    let mut engine = Engine::new_with(
        Plugin::default(),
        InMemoryFactory,
        SeededStableHasherBuilder::new(0),
    )
    .await
    .unwrap();
    let lexical = Interned::new_duplicating(
        LexicalInstanceCandidates::new_duplicating(inputs.lexical),
    );
    let globals = inputs
        .globals
        .into_iter()
        .map(|(trait_id, candidates)| {
            (
                GlobalInstanceCandidatesKey { current_site: SITE, trait_id },
                Interned::new_duplicating_unsized(candidates),
            )
        })
        .collect();
    let trait_refs = inputs
        .trait_refs
        .into_iter()
        .map(|(symbol_id, trait_ref)| {
            (
                TraitRefKey { symbol_id },
                Some(Interned::new_duplicating(trait_ref)),
            )
        })
        .collect();
    let parameters = inputs
        .parameters
        .into_iter()
        .map(|(symbol_id, parameters)| {
            (
                generic_parameters::Key { symbol_id },
                Interned::new_duplicating(parameters),
            )
        })
        .collect();
    let orders = inputs
        .orders
        .into_iter()
        .map(|((this, other), order)| {
            (InstanceOrderKey { this, other }, Ok(Some(order)))
        })
        .collect();
    let kinds = inputs
        .kinds
        .into_iter()
        .map(|(symbol_id, kind)| {
            (pernixc_symbol::kind::Key { symbol_id }, kind)
        })
        .collect();
    let parents = inputs
        .parents
        .into_iter()
        .map(|(symbol_id, parent)| {
            (pernixc_symbol::parent::Key { symbol_id }, parent)
        })
        .collect();
    let associated_values = inputs
        .associated_values
        .into_iter()
        .map(|(symbol_id, value)| {
            (instance_associated::Key { symbol_id }, value)
        })
        .collect();

    engine.register_executor(Arc::new(PrecomputedExecutor::new(
        HashMap::from([(
            LexicalInstanceCandidatesKey { current_site: SITE },
            lexical,
        )]),
    )));
    engine.register_executor(Arc::new(PrecomputedExecutor::new(globals)));
    engine.register_executor(Arc::new(PrecomputedExecutor::new(trait_refs)));
    engine.register_executor(Arc::new(PrecomputedExecutor::new(parameters)));
    engine.register_executor(Arc::new(PrecomputedExecutor::new(orders)));
    engine.register_executor(Arc::new(PrecomputedExecutor::new(kinds)));
    engine.register_executor(Arc::new(PrecomputedExecutor::new(parents)));
    engine.register_executor(Arc::new(PrecomputedExecutor::new(
        associated_values,
    )));
    Arc::new(engine).tracked().await
}

fn empty_parameters() -> GenericParameters2 { GenericParameters2::new([]) }

fn parameter(name: &str, kind: GenericParameterKind) -> GenericParameter {
    GenericParameter::new(
        Interned::new_duplicating_unsized(name.to_owned()),
        None,
        kind,
    )
}

fn generic(
    owner: GlobalSymbolID,
    index: u64,
    engine: &TrackedEngine,
) -> Interned<Type2> {
    Type2::new_generic_parameter(
        GenericParameterID::new(owner, ID::new(index)),
        engine,
    )
}

fn trait_ref(
    trait_id: GlobalSymbolID,
    arguments: impl IntoIterator<Item = Interned<Type2>>,
    binder: Binder,
    engine: &TrackedEngine,
) -> TraitRef2 {
    TraitRef2::new(
        trait_id,
        engine.intern_unsized(arguments.into_iter().collect::<Vec<_>>()),
        binder,
    )
}

fn empty_binder(engine: &TrackedEngine) -> Binder {
    Binder::new(engine.intern_unsized(Vec::new()))
}

async fn resolve(
    engine: &TrackedEngine,
    request: &TraitRef2,
) -> Result<super::ResolvedInstance, ResolveError> {
    Solver::new(&Premise::new(SITE), engine)
        .resolve_instance(request)
        .await
        .unwrap()
        .map(|(resolved, _)| resolved)
}

// input: Trait[infer_type]
// premise: {}
// output: NormalForm(NotClosed)
#[tokio::test]
async fn rejects_request_without_closed_normal_form() {
    let engine = create_engine(Inputs::new()).await;
    let premise = Premise::new(SITE);
    let mut solver = Solver::new(&premise, &engine);
    let inference = Type2::new_inference_variable(
        solver.fresh_inference_variable(TyKind::Type),
        &engine,
    );
    let request = trait_ref(TRAIT, [inference], empty_binder(&engine), &engine);

    assert_eq!(
        solver.resolve_instance(&request).await.unwrap(),
        Err(ResolveError::NormalForm(NormalFormFailure::NotClosed))
    );
}

// input: Trait[&skolem(1) bool]
// premise: &skolem(0) bool = bool; GENERAL implements Trait[bool]
// output: GENERAL, {skolem(0) = skolem(1)}
#[tokio::test]
async fn propagates_normal_form_constraints() {
    let bootstrap = create_engine(Inputs::new()).await;
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL]);
    inputs.trait_refs.insert(
        GENERAL,
        trait_ref(
            TRAIT,
            [Type2::new_primitive(Primitive::Bool, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.parameters.insert(GENERAL, empty_parameters());
    let engine = create_engine(inputs).await;
    let skolem_0 =
        Type2::new_skolemized_variable(SkolemizedVariable::new(0), &engine);
    let skolem_1 =
        Type2::new_skolemized_variable(SkolemizedVariable::new(1), &engine);
    let bool_ty = Type2::new_primitive(Primitive::Bool, &engine);
    let mut premise = Premise::new(SITE);
    premise.insert(Predicate2::Equality(Equality::new(
        empty_binder(&engine),
        Type2::new_immutable_reference(
            skolem_0.clone(),
            bool_ty.clone(),
            &engine,
        ),
        bool_ty.clone(),
    )));
    let mut solver = Solver::new(&premise, &engine);
    assert_eq!(
        solver.fresh_skolem_variable(TyKind::Lifetime),
        SkolemizedVariable::new(0)
    );
    assert_eq!(
        solver.fresh_skolem_variable(TyKind::Lifetime),
        SkolemizedVariable::new(1)
    );
    let request = trait_ref(
        TRAIT,
        [Type2::new_immutable_reference(skolem_1.clone(), bool_ty, &engine)],
        empty_binder(&engine),
        &engine,
    );

    let (resolved, constraints) =
        solver.resolve_instance(&request).await.unwrap().unwrap();
    assert_eq!(resolved.source(), InstanceSource::GlobalInstance(GENERAL));
    assert_eq!(constraints, Constraints::lifetimes_eq(skolem_0, skolem_1));
}

// input: Trait[bool]
// premise: lexical SPECIFIC and global GENERAL both match
// output: SPECIFIC from InstanceScope
#[tokio::test]
async fn lexical_candidate_precedes_global_candidates() {
    let bootstrap = create_engine(Inputs::new()).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &bootstrap);
    let candidate_ref = trait_ref(
        TRAIT,
        [bool_ty.clone()],
        empty_binder(&bootstrap),
        &bootstrap,
    );
    let mut inputs = Inputs::new();
    inputs.lexical.push(LexicalInstanceCandidate::InstanceScope(SPECIFIC));
    inputs.globals.insert(TRAIT, vec![GENERAL]);
    inputs.trait_refs.insert(SPECIFIC, candidate_ref.clone());
    inputs.trait_refs.insert(GENERAL, candidate_ref);
    inputs.parameters.insert(SPECIFIC, empty_parameters());
    inputs.parameters.insert(GENERAL, empty_parameters());
    let engine = create_engine(inputs).await;
    let request = trait_ref(
        TRAIT,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        empty_binder(&engine),
        &engine,
    );

    let resolved = resolve(&engine, &request).await.unwrap();
    assert_eq!(resolved.source(), InstanceSource::InstanceScope(SPECIFIC));
    assert_eq!(
        resolved.instance(),
        &Type2::new_symbolic(SPECIFIC, [], &engine)
    );
}

// input: Trait[bool]
// premise: SITE has visible instance parameter I: Trait[bool]
// output: I from InstanceParameter
#[tokio::test]
async fn resolves_visible_instance_parameter() {
    let bootstrap = create_engine(Inputs::new()).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &bootstrap);
    let parameter_id = GenericParameterID::new(SITE, ID::new(0));
    let mut inputs = Inputs::new();
    inputs
        .lexical
        .push(LexicalInstanceCandidate::InstanceParameter(parameter_id));
    inputs.parameters.insert(
        SITE,
        GenericParameters2::new([parameter(
            "I",
            GenericParameterKind::Instance(InstanceParameterKind::new(Some(
                Symbol2::new(TRAIT, bootstrap.intern_unsized(vec![bool_ty])),
            ))),
        )]),
    );
    let engine = create_engine(inputs).await;
    let request = trait_ref(
        TRAIT,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        empty_binder(&engine),
        &engine,
    );

    let resolved = resolve(&engine, &request).await.unwrap();
    assert_eq!(
        resolved.source(),
        InstanceSource::InstanceParameter(parameter_id)
    );
    assert_eq!(
        resolved.instance(),
        &Type2::new_generic_parameter(parameter_id, &engine)
    );
}

// input: Trait[bool]
// premise: ASSOCIATED is a trait-associated instance of TRAIT
// output: this.ASSOCIATED from AssociatedInstance
#[tokio::test]
async fn constructs_trait_associated_instance() {
    let bootstrap = create_engine(Inputs::new()).await;
    let mut inputs = Inputs::new();
    inputs.lexical.push(LexicalInstanceCandidate::Associated(ASSOCIATED));
    inputs.trait_refs.insert(
        ASSOCIATED,
        trait_ref(
            TRAIT,
            [Type2::new_primitive(Primitive::Bool, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.parameters.insert(ASSOCIATED, empty_parameters());
    inputs.kinds.insert(ASSOCIATED, Kind::TraitAssociatedInstance);
    inputs.parents.insert(ASSOCIATED, Some(TRAIT.id));
    let engine = create_engine(inputs).await;
    let request = trait_ref(
        TRAIT,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        empty_binder(&engine),
        &engine,
    );
    let anonymous = Type2::new_application(
        Constructor::AnonymousTraitInstance(AnonymousTraitInstance::new(TRAIT)),
        [],
        &engine,
    );
    let expected = Type2::new_application(
        Constructor::InstanceAssociated(InstanceAssociated::new(ASSOCIATED)),
        [anonymous],
        &engine,
    );

    let resolved = resolve(&engine, &request).await.unwrap();
    assert_eq!(
        resolved.source(),
        InstanceSource::AssociatedInstance(ASSOCIATED)
    );
    assert_eq!(resolved.instance(), &expected);
}

// input: Trait[bool]
// premise: ASSOCIATED has instance-associated value REQUIRED
// output: REQUIRED from AssociatedInstance
#[tokio::test]
async fn substitutes_instance_associated_value() {
    let bootstrap = create_engine(Inputs::new()).await;
    let mut inputs = Inputs::new();
    inputs.lexical.push(LexicalInstanceCandidate::Associated(ASSOCIATED));
    inputs.trait_refs.insert(
        ASSOCIATED,
        trait_ref(
            TRAIT,
            [Type2::new_primitive(Primitive::Bool, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.parameters.insert(ASSOCIATED, empty_parameters());
    inputs.kinds.insert(ASSOCIATED, Kind::InstanceAssociatedInstance);
    inputs
        .associated_values
        .insert(ASSOCIATED, Type2::new_symbolic(REQUIRED, [], &bootstrap));
    let engine = create_engine(inputs).await;
    let request = trait_ref(
        TRAIT,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        empty_binder(&engine),
        &engine,
    );

    let resolved = resolve(&engine, &request).await.unwrap();
    assert_eq!(
        resolved.source(),
        InstanceSource::AssociatedInstance(ASSOCIATED)
    );
    assert_eq!(
        resolved.instance(),
        &Type2::new_symbolic(REQUIRED, [], &engine)
    );
}

// input: Trait[bool]
// premise: GENERAL implements Trait[T]
// output: GENERAL[bool]
#[tokio::test]
async fn deduces_generic_arguments_for_global_candidate() {
    let bootstrap = create_engine(Inputs::new()).await;
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL]);
    inputs.trait_refs.insert(
        GENERAL,
        trait_ref(
            TRAIT,
            [generic(GENERAL, 0, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.parameters.insert(
        GENERAL,
        GenericParameters2::new([parameter("T", GenericParameterKind::Type)]),
    );
    let engine = create_engine(inputs).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &engine);
    let request =
        trait_ref(TRAIT, [bool_ty.clone()], empty_binder(&engine), &engine);

    let resolved = resolve(&engine, &request).await.unwrap();
    assert_eq!(
        resolved.instance(),
        &Type2::new_symbolic(GENERAL, [bool_ty], &engine)
    );
}

// input: Trait[bool]
// premise: GENERAL requires I: Required[bool], provided by REQUIRED
// output: GENERAL[bool, REQUIRED]
#[tokio::test]
async fn resolves_dependent_instance_parameter_in_declaration_order() {
    let bootstrap = create_engine(Inputs::new()).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &bootstrap);
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL]);
    inputs.globals.insert(REQUIRED_TRAIT, vec![REQUIRED]);
    inputs.trait_refs.insert(
        GENERAL,
        trait_ref(
            TRAIT,
            [generic(GENERAL, 0, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.trait_refs.insert(
        REQUIRED,
        trait_ref(
            REQUIRED_TRAIT,
            [bool_ty],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.parameters.insert(
        GENERAL,
        GenericParameters2::new([
            parameter("T", GenericParameterKind::Type),
            parameter(
                "I",
                GenericParameterKind::Instance(InstanceParameterKind::new(
                    Some(Symbol2::new(
                        REQUIRED_TRAIT,
                        bootstrap.intern_unsized(vec![generic(
                            GENERAL, 0, &bootstrap,
                        )]),
                    )),
                )),
            ),
        ]),
    );
    inputs.parameters.insert(REQUIRED, empty_parameters());
    let engine = create_engine(inputs).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &engine);
    let request =
        trait_ref(TRAIT, [bool_ty.clone()], empty_binder(&engine), &engine);

    assert_eq!(
        resolve(&engine, &request).await.unwrap().instance(),
        &Type2::new_symbolic(
            GENERAL,
            [bool_ty, Type2::new_symbolic(REQUIRED, [], &engine)],
            &engine,
        )
    );
}

// input: Trait[bool]
// premise: GENERAL requires I: Trait[bool]
// output: Recursive(I, Cyclic)
#[tokio::test]
async fn reports_nested_cycle_for_recursive_requirement() {
    let bootstrap = create_engine(Inputs::new()).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &bootstrap);
    let candidate_ref = trait_ref(
        TRAIT,
        [bool_ty.clone()],
        empty_binder(&bootstrap),
        &bootstrap,
    );
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL]);
    inputs.trait_refs.insert(GENERAL, candidate_ref);
    inputs.parameters.insert(
        GENERAL,
        GenericParameters2::new([parameter(
            "I",
            GenericParameterKind::Instance(InstanceParameterKind::new(Some(
                Symbol2::new(TRAIT, bootstrap.intern_unsized(vec![bool_ty])),
            ))),
        )]),
    );
    let engine = create_engine(inputs).await;
    let request = trait_ref(
        TRAIT,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        empty_binder(&engine),
        &engine,
    );

    let ResolveError::Recursive(error) =
        resolve(&engine, &request).await.unwrap_err()
    else {
        panic!("expected recursive error")
    };
    assert_eq!(error.resolving_symbol(), GENERAL);
    assert!(matches!(error.errors()[0].1, ResolveError::Cyclic));
}

// input: Trait[bool]
// premise: GENERAL and SPECIFIC match; SPECIFIC is more specific
// output: SPECIFIC
#[tokio::test]
async fn selects_most_specific_global_candidate() {
    let bootstrap = create_engine(Inputs::new()).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &bootstrap);
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL, SPECIFIC]);
    inputs.trait_refs.insert(
        GENERAL,
        trait_ref(
            TRAIT,
            [generic(GENERAL, 0, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.trait_refs.insert(
        SPECIFIC,
        trait_ref(TRAIT, [bool_ty], empty_binder(&bootstrap), &bootstrap),
    );
    inputs.parameters.insert(
        GENERAL,
        GenericParameters2::new([parameter("T", GenericParameterKind::Type)]),
    );
    inputs.parameters.insert(SPECIFIC, empty_parameters());
    inputs.orders.insert((GENERAL, SPECIFIC), Order::MoreGeneral);
    let engine = create_engine(inputs).await;
    let request = trait_ref(
        TRAIT,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        empty_binder(&engine),
        &engine,
    );

    assert_eq!(
        resolve(&engine, &request).await.unwrap().source(),
        InstanceSource::GlobalInstance(SPECIFIC)
    );
}

// input: Trait[bool]
// premise: GENERAL and SPECIFIC overlap ambiguously
// output: Ambiguous(GENERAL, SPECIFIC)
#[tokio::test]
async fn reports_overlapping_global_candidates_as_ambiguous() {
    let bootstrap = create_engine(Inputs::new()).await;
    let bool_ty = Type2::new_primitive(Primitive::Bool, &bootstrap);
    let candidate_ref =
        trait_ref(TRAIT, [bool_ty], empty_binder(&bootstrap), &bootstrap);
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL, SPECIFIC]);
    inputs.trait_refs.insert(GENERAL, candidate_ref.clone());
    inputs.trait_refs.insert(SPECIFIC, candidate_ref);
    inputs.parameters.insert(GENERAL, empty_parameters());
    inputs.parameters.insert(SPECIFIC, empty_parameters());
    inputs.orders.insert((GENERAL, SPECIFIC), Order::Ambiguous);
    let engine = create_engine(inputs).await;
    let request = trait_ref(
        TRAIT,
        [Type2::new_primitive(Primitive::Bool, &engine)],
        empty_binder(&engine),
        &engine,
    );

    assert_eq!(
        resolve(&engine, &request).await,
        Err(ResolveError::Ambiguous(Arc::new([
            InstanceSource::GlobalInstance(GENERAL),
            InstanceSource::GlobalInstance(SPECIFIC),
        ])))
    );
}

// input: for['x] Trait['x]
// premise: GENERAL implements Trait['static]
// output: NotFound because the request skolem would escape
#[tokio::test]
async fn rejects_higher_ranked_candidate_that_leaks_request_skolem() {
    use pernixc_type::r#type::constructor::Lifetime;

    let bootstrap = create_engine(Inputs::new()).await;
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL]);
    inputs.trait_refs.insert(
        GENERAL,
        trait_ref(
            TRAIT,
            [Type2::new_lifetime(Lifetime::Static, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.parameters.insert(GENERAL, empty_parameters());
    let engine = create_engine(inputs).await;
    let binder = Binder::new(engine.intern_unsized(vec![TyKind::Lifetime]));
    let request = trait_ref(
        TRAIT,
        [engine.intern(Type2::BoundVariable(BoundVariable::new(0, 0)))],
        binder,
        &engine,
    );

    assert_eq!(resolve(&engine, &request).await, Err(ResolveError::NotFound));
}

// input: for['x] Trait['x]
// premise: GENERAL['y, I: Required['y]] and REQUIRED['z]
// output: for['x] GENERAL['x, REQUIRED['x]]
#[tokio::test]
async fn resolves_and_rebinds_higher_ranked_recursive_instance() {
    let bootstrap = create_engine(Inputs::new()).await;
    let mut inputs = Inputs::new();
    inputs.globals.insert(TRAIT, vec![GENERAL]);
    inputs.globals.insert(REQUIRED_TRAIT, vec![REQUIRED]);
    inputs.trait_refs.insert(
        GENERAL,
        trait_ref(
            TRAIT,
            [generic(GENERAL, 0, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.trait_refs.insert(
        REQUIRED,
        trait_ref(
            REQUIRED_TRAIT,
            [generic(REQUIRED, 0, &bootstrap)],
            empty_binder(&bootstrap),
            &bootstrap,
        ),
    );
    inputs.parameters.insert(
        GENERAL,
        GenericParameters2::new([
            parameter("x", GenericParameterKind::Lifetime),
            parameter(
                "I",
                GenericParameterKind::Instance(InstanceParameterKind::new(
                    Some(Symbol2::new(
                        REQUIRED_TRAIT,
                        bootstrap.intern_unsized(vec![generic(
                            GENERAL, 0, &bootstrap,
                        )]),
                    )),
                )),
            ),
        ]),
    );
    inputs.parameters.insert(
        REQUIRED,
        GenericParameters2::new([parameter(
            "x",
            GenericParameterKind::Lifetime,
        )]),
    );
    let engine = create_engine(inputs).await;
    let binder = Binder::new(engine.intern_unsized(vec![TyKind::Lifetime]));
    let request = trait_ref(
        TRAIT,
        [engine.intern(Type2::BoundVariable(BoundVariable::new(0, 0)))],
        binder.clone(),
        &engine,
    );

    let direct_bound =
        engine.intern(Type2::BoundVariable(BoundVariable::new(0, 0)));
    let nested_bound =
        engine.intern(Type2::BoundVariable(BoundVariable::new(1, 0)));
    let expected = Type2::new_symbolic_with_binder(
        GENERAL,
        binder,
        [direct_bound, Type2::new_symbolic(REQUIRED, [nested_bound], &engine)],
        &engine,
    );
    assert_eq!(resolve(&engine, &request).await.unwrap().instance(), &expected);
}
