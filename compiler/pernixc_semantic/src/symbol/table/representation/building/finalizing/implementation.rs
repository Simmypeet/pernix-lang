//! Contains code related to building the implementation symbols.

use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{
    finalizer::{self, builder},
    occurrences::Occurrences,
    Finalize, Finalizer,
};
use crate::{
    arena::ID,
    error::Error,
    symbol::{
        table::{
            self,
            representation::RwLockContainer,
            resolution::{self, Observer},
            Building, State, Table,
        },
        GenericID, GlobalID,
    },
    type_system::{
        environment::Environment,
        model::{self, Model},
        normalizer::{Normalizer, NO_OP},
        predicate, simplify,
        term::{self, Term},
        Succeeded,
    },
};

fn simplify_generic_arguments<T: Term, S: SourceElement>(
    generic_arguments: &mut [T],
    syntax_tree: &[&S],
    generic_identifier: &syntax_tree::GenericIdentifier,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    handler: &dyn Handler<Box<dyn Error>>,
) where
    predicate::Predicate<T::Model>: table::Display<table::Suboptimal>,

    <T::Model as Model>::LifetimeInference: table::Display<table::Suboptimal>,
    <T::Model as Model>::TypeInference: table::Display<table::Suboptimal>,
    <T::Model as Model>::ConstantInference: table::Display<table::Suboptimal>,
{
    for (idx, arguments) in generic_arguments.iter_mut().enumerate() {
        let Succeeded { result: simplified, constraints } =
            simplify::simplify(arguments, environment);

        environment.check_lifetime_constraints(
            constraints,
            &syntax_tree
                .get(idx)
                .map_or_else(|| generic_identifier.span(), |syn| syn.span()),
            handler,
        );

        *arguments = simplified;
    }
}

fn extract_syntax_tree(
    syntax_tree: &syntax_tree::GenericArguments,
) -> (Vec<&syntax_tree::r#type::Type>, Vec<&syntax_tree::expression::Expression>)
{
    let mut types = Vec::new();
    let mut constants = Vec::new();

    for argument in
        syntax_tree.argument_list().iter().flat_map(ConnectedList::elements)
    {
        match argument {
            syntax_tree::GenericArgument::Lifetime(_) => {}

            syntax_tree::GenericArgument::Type(ty) => {
                types.push(&**ty);
            }

            syntax_tree::GenericArgument::Constant(constant) => {
                constants.push(&**constant.expression());
            }
        }
    }

    (types, constants)
}

impl Table<Building<RwLockContainer, Finalizer>> {
    pub(super) fn create_implementation_arguments<
        T: Finalize + finalizer::Element,
    >(
        &self,
        implementation_id: GenericID,
        implemented_id: ID<T>,
        implemented_id_generic_parameter_state: usize,
        generic_identifier: &syntax_tree::GenericIdentifier,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> term::GenericArguments<model::Default>
    where
        ID<T>: Into<GenericID> + Into<GlobalID>,
    {
        // make sure the implemented symbol has generic parameters already
        let _ = self.build_to(
            implemented_id,
            Some(implementation_id.into()),
            implemented_id_generic_parameter_state,
            handler,
        );

        let active_premise = self
            .get_active_premise(implementation_id.into())
            .expect("should be a valid id");

        let environment = Environment::new(active_premise, self, &NO_OP);
        let mut builder = builder::Definition::new(&environment);

        let mut generic_arguments = self
            .resolve_generic_arguments(
                generic_identifier,
                implementation_id.into(),
                implemented_id.into(),
                resolution::Config {
                    ellided_lifetime_provider: None,
                    ellided_type_provider: None,
                    ellided_constant_provider: None,
                    observer: Some(&mut occurrences.chain(&mut builder)),
                    higher_ranked_lifetimes: None,
                },
                handler,
            )
            .expect("the referring site should be valid")
            .expect("should have generic arguments");

        let (types, constants) = generic_identifier
            .generic_arguments()
            .as_ref()
            .map_or_else(Default::default, |x| extract_syntax_tree(x));

        simplify_generic_arguments(
            &mut generic_arguments.types,
            &types,
            generic_identifier,
            &environment,
            handler,
        );

        simplify_generic_arguments(
            &mut generic_arguments.constants,
            &constants,
            generic_identifier,
            &environment,
            handler,
        );

        generic_arguments
    }
}
