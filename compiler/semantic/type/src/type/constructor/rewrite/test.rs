use pernixc_arena::ID;
use pernixc_qbice::{DuplicatingInterner, Interner};
use pernixc_symbol::{GlobalSymbolID, SymbolID};
use pernixc_target::TargetID;
use qbice::storage::intern::Interned;

use super::*;
use crate::{
    generic_parameters::{GenericParameter, GenericParameterID},
    substitution::{Substitutable, Substitution},
    r#type::{
        Type,
        bound::{Binder, BoundVariable, Instantiate},
        constructor::{FunctionPointer, Primitive},
    },
};

const SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));

fn generic_parameter_id(index: u64) -> GenericParameterID {
    GenericParameterID::new(SYMBOL_ID, ID::<GenericParameter>::new(index))
}

fn generic_parameter_type(
    id: GenericParameterID,
    interner: &impl Interner,
) -> Interned<Type> {
    interner.intern(Type::GenericParameter(id))
}

fn bound_variable_type(
    depth: usize,
    index: usize,
    interner: &impl Interner,
) -> Interned<Type> {
    interner.intern(Type::BoundVariable(BoundVariable::new(depth, index)))
}

fn primitive_type(
    primitive: Primitive,
    interner: &impl Interner,
) -> Interned<Type> {
    application_type(Constructor::Primitive(primitive), &[], interner)
}

fn function_pointer_type(
    arguments: &[Interned<Type>],
    interner: &impl Interner,
) -> Interned<Type> {
    application_type(
        Constructor::FunctionPointer(FunctionPointer {
            binder: Binder::new(interner.intern_unsized(
                vec![crate::r#type::kind::TyKind::Type; arguments.len()],
            )),
        }),
        arguments,
        interner,
    )
}

fn application_type(
    constructor: Constructor,
    arguments: &[Interned<Type>],
    interner: &impl Interner,
) -> Interned<Type> {
    interner.intern(Type::Application(Application {
        constructor,
        arguments: interner.intern_unsized(arguments.to_vec()),
    }))
}

fn as_application(ty: &Interned<Type>) -> &Application {
    let Type::Application(application) = ty.as_ref() else {
        panic!("expected application");
    };

    application
}

fn as_bound_variable(ty: &Interned<Type>) -> BoundVariable {
    let Type::BoundVariable(variable) = ty.as_ref() else {
        panic!("expected bound variable");
    };

    *variable
}

fn same_type_handle(lhs: &Interned<Type>, rhs: &Interned<Type>) -> bool {
    std::ptr::eq(lhs.as_ref(), rhs.as_ref())
}

struct NoopRewriter;

impl TypeRewriter for NoopRewriter {}

#[test]
fn noop_rewriter_returns_original_type() {
    let interner = DuplicatingInterner;
    let argument = generic_parameter_type(generic_parameter_id(0), &interner);
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[argument],
        &interner,
    );

    let rewritten = rewrite_type_or_clone(&ty, &mut NoopRewriter, &interner);

    assert!(same_type_handle(&ty, &rewritten));
}

struct GenericParameterRewriter {
    target: GenericParameterID,
    replacement: Interned<Type>,
}

impl TypeRewriter for GenericParameterRewriter {
    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _: RewriteContext,
    ) -> Option<Interned<Type>> {
        (id == self.target).then(|| self.replacement.clone())
    }
}

#[test]
fn rewrite_nested_generic_parameter_preserves_unchanged_siblings() {
    let interner = DuplicatingInterner;
    let target = generic_parameter_id(0);
    let unchanged = generic_parameter_type(generic_parameter_id(1), &interner);
    let target_type = generic_parameter_type(target, &interner);
    let replacement = primitive_type(Primitive::Bool, &interner);
    let nested = application_type(
        Constructor::Primitive(Primitive::Int16),
        &[target_type],
        &interner,
    );
    let trailing = primitive_type(Primitive::Float32, &interner);
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[unchanged.clone(), nested.clone(), trailing.clone()],
        &interner,
    );

    let rewritten = rewrite_type_or_clone(
        &ty,
        &mut GenericParameterRewriter {
            target,
            replacement: replacement.clone(),
        },
        &interner,
    );

    assert!(!same_type_handle(&ty, &rewritten));

    let rewritten_application = as_application(&rewritten);
    assert!(same_type_handle(&rewritten_application.arguments[0], &unchanged));
    assert!(!same_type_handle(&rewritten_application.arguments[1], &nested));
    assert!(same_type_handle(&rewritten_application.arguments[2], &trailing));

    let rewritten_nested = as_application(&rewritten_application.arguments[1]);
    assert!(same_type_handle(&rewritten_nested.arguments[0], &replacement));
}

struct ApplicationRewriter {
    target_constructor: Constructor,
    replacement: Interned<Type>,
    argument_replacement: Interned<Type>,
    visited_generic_parameters: usize,
    saw_rewritten_argument: bool,
}

impl TypeRewriter for ApplicationRewriter {
    fn rewrite_application(
        &mut self,
        application: &Application,
        _: RewriteContext,
    ) -> Option<Interned<Type>> {
        if application.constructor() == &self.target_constructor {
            self.saw_rewritten_argument = same_type_handle(
                &application.arguments()[0],
                &self.argument_replacement,
            );
        }

        (application.constructor() == &self.target_constructor)
            .then(|| self.replacement.clone())
    }

    fn rewrite_generic_parameter(
        &mut self,
        _: GenericParameterID,
        _: RewriteContext,
    ) -> Option<Interned<Type>> {
        self.visited_generic_parameters += 1;
        Some(self.argument_replacement.clone())
    }
}

#[test]
fn application_rewriter_runs_after_rewriting_arguments() {
    let interner = DuplicatingInterner;
    let target_constructor = Constructor::Primitive(Primitive::Int16);
    let replacement = primitive_type(Primitive::Bool, &interner);
    let argument_replacement = primitive_type(Primitive::Uint8, &interner);
    let nested = application_type(
        target_constructor.clone(),
        &[generic_parameter_type(generic_parameter_id(0), &interner)],
        &interner,
    );
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[nested],
        &interner,
    );
    let mut rewriter = ApplicationRewriter {
        target_constructor,
        replacement: replacement.clone(),
        argument_replacement,
        visited_generic_parameters: 0,
        saw_rewritten_argument: false,
    };

    let rewritten = rewrite_type_or_clone(&ty, &mut rewriter, &interner);

    let rewritten_application = as_application(&rewritten);
    assert!(same_type_handle(
        &rewritten_application.arguments[0],
        &replacement
    ));
    assert_eq!(rewriter.visited_generic_parameters, 1);
    assert!(rewriter.saw_rewritten_argument);
}

#[test]
fn instantiation_replaces_generic_parameter_and_leaves_missing_unchanged() {
    let interner = DuplicatingInterner;
    let target = generic_parameter_id(0);
    let missing = generic_parameter_id(1);
    let target_type = generic_parameter_type(target, &interner);
    let missing_type = generic_parameter_type(missing, &interner);
    let replacement = primitive_type(Primitive::Bool, &interner);
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[target_type, missing_type.clone()],
        &interner,
    );
    let mut instantiation = Substitution::default();
    instantiation.insert_generic(target, replacement.clone());

    let instantiated = ty.apply_or_clone(&instantiation, &interner);

    let instantiated_application = as_application(&instantiated);
    assert!(same_type_handle(
        &instantiated_application.arguments[0],
        &replacement
    ));
    assert!(same_type_handle(
        &instantiated_application.arguments[1],
        &missing_type
    ));
}

#[derive(Default)]
struct BinderDepthRecorder {
    records: Vec<(GenericParameterID, usize)>,
}

impl TypeRewriter for BinderDepthRecorder {
    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        ctx: RewriteContext,
    ) -> Option<Interned<Type>> {
        self.records.push((id, ctx.binder_depth()));
        None
    }
}

#[test]
fn binder_depth_tracks_function_pointer_nesting() {
    let interner = DuplicatingInterner;
    let outside = generic_parameter_id(0);
    let inside_one = generic_parameter_id(1);
    let inside_two = generic_parameter_id(2);
    let nested_function_pointer = function_pointer_type(
        &[generic_parameter_type(inside_two, &interner)],
        &interner,
    );
    let function_pointer = function_pointer_type(
        &[
            generic_parameter_type(inside_one, &interner),
            nested_function_pointer,
        ],
        &interner,
    );
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[generic_parameter_type(outside, &interner), function_pointer],
        &interner,
    );
    let mut recorder = BinderDepthRecorder::default();

    let rewritten = rewrite_type_or_clone(&ty, &mut recorder, &interner);

    assert!(same_type_handle(&ty, &rewritten));
    assert_eq!(recorder.records, vec![
        (outside, 0),
        (inside_one, 1),
        (inside_two, 2),
    ]);
}

#[tokio::test]
async fn rewrite_application_supports_async_failable_rewrite() {
    let interner = DuplicatingInterner;
    let target = generic_parameter_id(0);
    let target_type = generic_parameter_type(target, &interner);
    let unchanged = primitive_type(Primitive::Int16, &interner);
    let replacement = primitive_type(Primitive::Bool, &interner);
    let function_pointer =
        function_pointer_type(&[target_type, unchanged.clone()], &interner);
    let application = as_application(&function_pointer);
    let mut visited_count = 0;

    let rewritten = rewrite_application(application, async |argument| {
        visited_count += 1;

        Ok::<_, ()>(
            argument
                .as_generic_parameter()
                .is_some_and(|id| *id == target)
                .then(|| replacement.clone()),
        )
    })
    .await
    .unwrap();

    let rewritten_application =
        rewritten.expect("expected rewritten application");

    assert_eq!(visited_count, 2);
    assert!(same_type_handle(
        &rewritten_application.arguments[0],
        &replacement
    ));
    assert!(same_type_handle(&rewritten_application.arguments[1], &unchanged));
}

#[tokio::test]
async fn rewrite_application_returns_callback_error() {
    let interner = DuplicatingInterner;
    let argument = generic_parameter_type(generic_parameter_id(0), &interner);
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[argument],
        &interner,
    );

    let result = rewrite_application(as_application(&ty), async |_| {
        Err::<Option<Interned<Type>>, _>("stop")
    })
    .await;

    assert_eq!(result, Err("stop"));
}

#[test]
fn instantiate_respects_nested_binders() {
    let interner = DuplicatingInterner;
    let replacement_zero = primitive_type(Primitive::Bool, &interner);
    let replacement_one = primitive_type(Primitive::Int16, &interner);
    let nested_inner_bound = bound_variable_type(0, 0, &interner);
    let nested_outer_bound = bound_variable_type(1, 0, &interner);
    let nested_function_pointer = function_pointer_type(
        &[nested_inner_bound.clone(), nested_outer_bound],
        &interner,
    );
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[
            bound_variable_type(0, 0, &interner),
            nested_function_pointer,
            bound_variable_type(0, 1, &interner),
        ],
        &interner,
    );
    let binder = Binder::new(interner.intern_unsized([
        crate::r#type::kind::TyKind::Type,
        crate::r#type::kind::TyKind::Type,
    ]));

    let rewritten = binder.instantiate(
        &ty,
        &[replacement_zero.clone(), replacement_one.clone()],
        &interner,
    );

    let rewritten_application = as_application(&rewritten);
    assert!(same_type_handle(
        &rewritten_application.arguments[0],
        &replacement_zero
    ));
    assert!(same_type_handle(
        &rewritten_application.arguments[2],
        &replacement_one
    ));

    let rewritten_nested = as_application(&rewritten_application.arguments[1]);
    assert!(same_type_handle(
        &rewritten_nested.arguments[0],
        &nested_inner_bound
    ));
    assert!(same_type_handle(
        &rewritten_nested.arguments[1],
        &replacement_zero
    ));
}

#[test]
fn instantiate_leaves_missing_replacement_unchanged() {
    let interner = DuplicatingInterner;
    let missing = bound_variable_type(0, 1, &interner);
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        std::slice::from_ref(&missing),
        &interner,
    );

    let rewritten = ty.instantiate(&[], &interner);

    let rewritten_application = as_application(&rewritten);
    assert!(same_type_handle(&rewritten_application.arguments[0], &missing));
}

#[test]
fn instantiate_interned_slice_of_types() {
    let interner = DuplicatingInterner;
    let replacement = primitive_type(Primitive::Bool, &interner);
    let unchanged = primitive_type(Primitive::Int16, &interner);
    let arguments: Interned<[Interned<Type>]> = interner.intern_unsized(vec![
        bound_variable_type(0, 0, &interner),
        unchanged.clone(),
    ]);

    let instantiated =
        arguments.instantiate(std::slice::from_ref(&replacement), &interner);

    assert!(same_type_handle(&instantiated[0], &replacement));
    assert!(same_type_handle(&instantiated[1], &unchanged));
}

#[test]
fn instantiate_shifts_free_bound_variables_in_replacement() {
    let interner = DuplicatingInterner;
    let replacement = bound_variable_type(0, 7, &interner);
    let nested_function_pointer = function_pointer_type(
        &[bound_variable_type(1, 0, &interner)],
        &interner,
    );
    let ty = application_type(
        Constructor::Primitive(Primitive::Int32),
        &[nested_function_pointer],
        &interner,
    );
    let binder = Binder::new(
        interner.intern_unsized(vec![crate::r#type::kind::TyKind::Type]),
    );

    let rewritten =
        binder.instantiate(&ty, std::slice::from_ref(&replacement), &interner);

    let rewritten_application = as_application(&rewritten);
    let rewritten_nested = as_application(&rewritten_application.arguments[0]);
    let rewritten_variable = as_bound_variable(&rewritten_nested.arguments[0]);

    assert_eq!(rewritten_variable.depth(), 1);
    assert_eq!(rewritten_variable.index(), 7);
}
