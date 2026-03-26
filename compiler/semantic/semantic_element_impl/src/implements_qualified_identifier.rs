use linkme::distributed_slice;
use pernixc_arena::ID;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::FxHashSet;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{PERNIX_PROGRAM, TrackedEngine};
use pernixc_resolution::{
    Resolver,
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::Resolution,
    term::{ResolutionToTermError, resolution_to_type},
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    final_implements::get_is_implements_final,
    kind::{Kind, get_kind},
    member::get_members,
    syntax::{
        get_implements_member_access_modifier,
        get_implements_qualified_identifier,
    },
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{
        ConstantParameter, GenericParameter, InstanceParameter,
        LifetimeParameter, TypeParameter, get_generic_parameters,
    },
    instance::Instance,
    lifetime::Lifetime,
    r#type::Type,
    visitor::{self, Element},
};
use qbice::{
    Decode, Encode, Executor, Query, StableHash, program::Registration,
    storage::intern::Interned,
};

use crate::{
    build::{self, Output},
    implements_qualified_identifier::diagnostic::{
        AdtImplementationCannotBeFinal, AdtImplementationCannotBeNegative,
        InvalidSymbolForImplements, MarkerImplementsNotFinal,
    },
    occurrences,
};

pub mod diagnostic;

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
    Query,
)]
#[value(Option<Interned<Resolution>>)]
pub struct Key {
    pub symbol_id: Global<pernixc_symbol::SymbolID>,
}

build::register_build!(Key);

impl crate::build::Build for Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(engine: &TrackedEngine, key: &Self) -> Output<Self> {
        let qualified_identifier =
            engine.get_implements_qualified_identifier(key.symbol_id).await;

        let mut occurrences = occurrences::Occurrences::default();
        let storage = Storage::<diagnostic::Diagnostic>::new();
        let generic_parameter_namespace =
            engine.get_generic_parameter_namespace(key.symbol_id).await;

        let resolution = match Resolver::builder()
            .tracked_engine(engine)
            .handler(&storage)
            .consider_adt_implements(false)
            .observer(&mut occurrences)
            .referring_site(key.symbol_id)
            .extra_namespace(&generic_parameter_namespace)
            .build()
            .resolve_qualified_identifier(&qualified_identifier)
            .await
        {
            Ok(resolution) => resolution,

            // couldn't resolve, but we still want to return diagnostics
            Err(pernixc_resolution::Error::Abort) => {
                return Output {
                    item: None,
                    diagnostics: engine.intern_unsized(storage.into_vec()),
                    occurrences: engine.intern(occurrences),
                };
            }
        };

        check_valid_resolution(
            engine,
            resolution.clone(),
            qualified_identifier.span(),
            &storage,
        )
        .await;

        // performs extra necessary check
        if let Resolution::GenericSymbol(symbol) = &resolution {
            let kind = engine.get_kind(symbol.id()).await;

            #[allow(clippy::match_same_arms)]
            match kind {
                Kind::Marker => {
                    check_marker(
                        engine,
                        key.symbol_id,
                        qualified_identifier.span(),
                        &storage,
                    )
                    .await;
                }

                Kind::Struct | Kind::Enum => {
                    check_adt(engine, key.symbol_id, &storage).await;
                }

                _ => {}
            }

            // Check for unused generic parameters
            check_unused_generic_parameters(
                engine,
                key.symbol_id,
                symbol.generic_arguments(),
                &storage,
            )
            .await;
        }

        Output {
            item: Some(engine.intern(resolution)),
            diagnostics: engine.intern_unsized(storage.into_vec()),
            occurrences: engine.intern(occurrences),
        }
    }
}

async fn check_valid_resolution(
    tracked_engine: &TrackedEngine,
    resolution: Resolution,
    qualified_identifier: RelativeSpan,
    storage: &Storage<diagnostic::Diagnostic>,
) {
    match resolution {
        Resolution::GenericSymbol(symbol) => {
            let kind = tracked_engine.get_kind(symbol.id()).await;

            match kind {
                Kind::Marker | Kind::Struct | Kind::Enum => {
                    // valid implements symbol
                }

                Kind::Type => {
                    // if it's a type, it must be a struct/enum
                    let ty = match tracked_engine
                        .resolution_to_type(Resolution::GenericSymbol(symbol))
                        .await
                    {
                        Ok(ty) => ty,
                        Err(ResolutionToTermError::Failed(_)) => unreachable!(),
                    };

                    if !matches!(
                        &ty,
                        Type::Symbol(sym)
                            if is_adt_type(tracked_engine, sym).await
                    ) {
                        storage.receive(
                            diagnostic::Diagnostic::InvalidTypeForImplements(
                                diagnostic::InvalidTypeForImplements {
                                    qualified_identifier_span:
                                        qualified_identifier,
                                    r#type: ty,
                                },
                            ),
                        );
                    }
                }

                _ => {
                    storage.receive(
                        diagnostic::Diagnostic::InvalidSymbolForImplements(
                            InvalidSymbolForImplements {
                                qualified_identifier_span: qualified_identifier,
                                found: Resolution::GenericSymbol(symbol),
                            },
                        ),
                    );
                }
            }
        }

        resolution => {
            storage.receive(
                diagnostic::Diagnostic::InvalidSymbolForImplements(
                    InvalidSymbolForImplements {
                        qualified_identifier_span: qualified_identifier,
                        found: resolution,
                    },
                ),
            );
        }
    }
}

async fn is_adt_type(engine: &TrackedEngine, sym_ty: &Symbol) -> bool {
    let kind = engine.get_kind(sym_ty.id()).await;
    matches!(kind, Kind::Struct | Kind::Enum)
}

async fn check_marker(
    engine: &TrackedEngine,
    implements: Global<pernixc_symbol::SymbolID>,
    qualified_identifier: RelativeSpan,
    storage: &Storage<diagnostic::Diagnostic>,
) {
    let is_final = engine.get_is_implements_final(implements).await;

    if !is_final {
        storage.receive(diagnostic::Diagnostic::MarkerImplementsNotFinal(
            MarkerImplementsNotFinal {
                qualified_identifier_span: qualified_identifier,
            },
        ));
    }

    // we'll check if he marker implementation has any members (it shouldn't).
    if engine.get_kind(implements).await != Kind::PositiveImplementation {
        return;
    }

    let members = engine.get_members(implements).await;

    for member_id in members
        .member_ids_by_name
        .values()
        .copied()
        .chain(members.unnameds.iter().copied())
    {
        storage.receive(
            diagnostic::Diagnostic::MemberInMarkerImplementationIsNotAllowed(
                diagnostic::MemberInMarkerImplementationIsNotAllowed {
                    implements_member_id: implements
                        .target_id
                        .make_global(member_id),
                },
            ),
        );
    }
}

async fn check_adt(
    engine: &TrackedEngine,
    implements: Global<pernixc_symbol::SymbolID>,
    storage: &Storage<diagnostic::Diagnostic>,
) {
    let kind = engine.get_kind(implements).await;

    // Check if it's a negative implementation
    if kind == Kind::NegativeImplementation {
        storage.receive(
            diagnostic::Diagnostic::AdtImplementationCannotBeNegative(
                AdtImplementationCannotBeNegative {
                    implementation_id: implements,
                },
            ),
        );
    }

    // Check if it's a final implementation
    let is_final = engine.get_is_implements_final(implements).await;
    if is_final {
        storage.receive(
            diagnostic::Diagnostic::AdtImplementationCannotBeFinal(
                AdtImplementationCannotBeFinal {
                    implementation_id: implements,
                },
            ),
        );
    }

    // Check access modifiers for ADT implementation members (only for positive
    // implementations)
    if kind == Kind::PositiveImplementation {
        let implements_members = engine.get_members(implements).await;

        // Check each implementation member
        for implements_member_id in implements_members
            .member_ids_by_name
            .values()
            .copied()
            .map(|x| implements.target_id.make_global(x))
        {
            // Check if ADT implementation members have access modifiers (they
            // should)
            let access_modifier = engine
                .get_implements_member_access_modifier(implements_member_id)
                .await;
            if access_modifier.is_none() {
                storage.receive(
                    diagnostic::Diagnostic::AdtMemberMissingAccessModifier(
                        diagnostic::AdtMemberMissingAccessModifier {
                            implementation_member_id: implements_member_id,
                        },
                    ),
                );
            }
        }
    }
}

/// A visitor that collects all used generic parameter IDs from generic
/// arguments. It skips parameters that appear under trait or trait associated
/// types.
#[derive(Debug, Default)]
struct UsedParameterCollector {
    used_lifetime_ids: FxHashSet<ID<LifetimeParameter>>,
    used_type_ids: FxHashSet<ID<TypeParameter>>,
    used_constant_ids: FxHashSet<ID<ConstantParameter>>,
    used_instance_ids: FxHashSet<ID<InstanceParameter>>,
    current_symbol_id: Global<pernixc_symbol::SymbolID>,
}

impl UsedParameterCollector {
    fn handle_lifetime(&mut self, lifetime: &Lifetime) {
        if let Lifetime::Parameter(param) = lifetime
            && param.parent_id() == self.current_symbol_id
        {
            self.used_lifetime_ids.insert(param.id());
        }
    }

    fn handle_type(&mut self, ty: &Type) {
        // if it's a trait associated type, we don't go deeper
        if ty.is_instance_associated() {
            return;
        }

        // found a type parameter, try to collect it
        if let Some(ty) = ty.as_parameter() {
            if ty.parent_id() == self.current_symbol_id {
                self.used_type_ids.insert(ty.id());
            }

            return;
        }

        // go deeper
        let _ = ty.accept_one_level(self);
    }

    fn handle_constant(&mut self, constant: &Constant) {
        // found a constant parameter, try to collect it
        if let Some(constant) = constant.as_parameter() {
            if constant.parent_id() == self.current_symbol_id {
                self.used_constant_ids.insert(constant.id());
            }

            return;
        }

        // go deeper
        let _ = constant.accept_one_level(self);
    }

    fn handle_instance(&mut self, instance: &Instance) {
        // if it's a trait associated type, we don't go deeper
        if instance.is_instance_associated() {
            return;
        }

        // found an instance parameter, try to collect it
        if let Some(instance) = instance.as_parameter() {
            if instance.parent_id() == self.current_symbol_id {
                self.used_instance_ids.insert(instance.id());
            }

            return;
        }

        // go deeper
        let _ = instance.accept_one_level(self);
    }
}

impl visitor::Visitor<'_, Lifetime> for UsedParameterCollector {
    fn visit(
        &mut self,
        term: &'_ Lifetime,
        _: <Lifetime as visitor::Element>::Location,
    ) -> bool {
        self.handle_lifetime(term);
        true
    }
}

impl visitor::Visitor<'_, Type> for UsedParameterCollector {
    fn visit(
        &mut self,
        term: &'_ Type,
        _: <Type as visitor::Element>::Location,
    ) -> bool {
        self.handle_type(term);
        true
    }
}

impl visitor::Visitor<'_, Constant> for UsedParameterCollector {
    fn visit(
        &mut self,
        term: &'_ Constant,
        _: <Constant as visitor::Element>::Location,
    ) -> bool {
        self.handle_constant(term);
        true
    }
}

impl visitor::Visitor<'_, Instance> for UsedParameterCollector {
    fn visit(
        &mut self,
        term: &'_ Instance,
        _: <Instance as visitor::Element>::Location,
    ) -> bool {
        self.handle_instance(term);
        true
    }
}

/// Check for unused generic parameters in the implements declaration.
async fn check_unused_generic_parameters(
    engine: &TrackedEngine,
    implements_id: Global<pernixc_symbol::SymbolID>,
    generic_arguments: &GenericArguments,
    storage: &Storage<diagnostic::Diagnostic>,
) {
    // Get the generic parameters defined on this implements
    let generic_parameters = engine.get_generic_parameters(implements_id).await;

    // Collect used parameters from generic arguments
    let mut collector = UsedParameterCollector {
        current_symbol_id: implements_id,
        ..Default::default()
    };

    // Visit all lifetimes
    for lifetime in generic_arguments.lifetimes() {
        collector.handle_lifetime(lifetime);
    }

    // Visit all types
    for ty in generic_arguments.types() {
        collector.handle_type(ty);
    }

    // Visit all constants
    for constant in generic_arguments.constants() {
        collector.handle_constant(constant);
    }

    // Visit all instances
    for instance in generic_arguments.instances() {
        collector.handle_instance(instance);
    }

    // Check for unused lifetime parameters
    for (param_id, param) in generic_parameters.lifetime_parameters_as_order() {
        if !collector.used_lifetime_ids.contains(&param_id)
            && let Some(span) = param.span().copied()
        {
            storage.receive(diagnostic::Diagnostic::UnusedGenericParameter(
                diagnostic::UnusedGenericParameter {
                    implementation_id: implements_id,
                    unused_parameter_span: span,
                    parameter_name: param.name().clone(),
                },
            ));
        }
    }

    // Check for unused type parameters
    for (param_id, param) in generic_parameters.type_parameters_as_order() {
        if !collector.used_type_ids.contains(&param_id)
            && let Some(span) = param.span().copied()
        {
            storage.receive(diagnostic::Diagnostic::UnusedGenericParameter(
                diagnostic::UnusedGenericParameter {
                    implementation_id: implements_id,
                    unused_parameter_span: span,
                    parameter_name: param.name().clone(),
                },
            ));
        }
    }

    // Check for unused constant parameters
    for (param_id, param) in generic_parameters.constant_parameters_as_order() {
        if !collector.used_constant_ids.contains(&param_id)
            && let Some(span) = param.span().copied()
        {
            storage.receive(diagnostic::Diagnostic::UnusedGenericParameter(
                diagnostic::UnusedGenericParameter {
                    implementation_id: implements_id,
                    unused_parameter_span: span,
                    parameter_name: param.name().clone(),
                },
            ));
        }
    }

    // Check for unused instance parameters
    for (param_id, param) in generic_parameters.instance_parameters_as_order() {
        if !collector.used_instance_ids.contains(&param_id)
            && let Some(span) = param.span().copied()
        {
            storage.receive(diagnostic::Diagnostic::UnusedGenericParameter(
                diagnostic::UnusedGenericParameter {
                    implementation_id: implements_id,
                    unused_parameter_span: span,
                    parameter_name: param.name().clone(),
                },
            ));
        }
    }
}

#[derive(Debug, Default)]
pub struct ExtractImplementsID;

impl Executor<pernixc_semantic_element::implements::Key, pernixc_qbice::Config>
    for ExtractImplementsID
{
    async fn execute(
        &self,
        key: &pernixc_semantic_element::implements::Key,
        engine: &TrackedEngine,
    ) -> Option<Global<pernixc_symbol::SymbolID>> {
        let resolution =
            engine.query(&Key { symbol_id: key.symbol_id }).await?;

        resolution.global_id()
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_ID_EXECUTOR: Registration<pernixc_qbice::Config> =
    Registration::new::<
        pernixc_semantic_element::implements::Key,
        ExtractImplementsID,
    >();

#[derive(Debug, Default)]
pub struct ExtractGenericArguments;

impl
    Executor<
        pernixc_semantic_element::implements_arguments::Key,
        pernixc_qbice::Config,
    > for ExtractGenericArguments
{
    async fn execute(
        &self,
        key: &pernixc_semantic_element::implements_arguments::Key,
        engine: &TrackedEngine,
    ) -> Option<Interned<GenericArguments>> {
        let resolution =
            engine.query(&Key { symbol_id: key.symbol_id }).await?;

        resolution
            .as_generic_symbol()
            .map(|x| engine.intern(x.generic_arguments().clone()))
    }
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTS_ARGUMENTS_EXECUTOR: Registration<pernixc_qbice::Config> =
    Registration::new::<
        pernixc_semantic_element::implements_arguments::Key,
        ExtractGenericArguments,
    >();
