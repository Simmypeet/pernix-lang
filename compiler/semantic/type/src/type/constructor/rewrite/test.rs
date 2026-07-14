use pernixc_arena::ID;
use pernixc_qbice::create_minimal_engine as create_test_engine;
use pernixc_symbol::{GlobalSymbolID, SymbolID};
use pernixc_target::TargetID;
use qbice::storage::intern::Interned;

use super::*;
use crate::{
    generic_parameters::{GenericParameter, GenericParameterID},
    substitution::{Substitutable, Substitution},
    r#type::{
        Type2,
        bound::{Binder, BoundVariable, Instantiate},
        constructor::Primitive,
    },
};

const SYMBOL_ID: GlobalSymbolID =
    TargetID::TEST.make_global(SymbolID::from_u128(1));

fn generic_parameter_id(index: u64) -> GenericParameterID {
    GenericParameterID::new(SYMBOL_ID, ID::<GenericParameter>::new(index))
}

fn as_application(ty: &Interned<Type2>) -> &Application {
    let Type2::Application(application) = ty.as_ref() else {
        panic!("expected application");
    };

    application
}

fn as_bound_variable(ty: &Interned<Type2>) -> BoundVariable {
    let Type2::BoundVariable(variable) = ty.as_ref() else {
        panic!("expected bound variable");
    };

    *variable
}

fn same_type_handle(lhs: &Interned<Type2>, rhs: &Interned<Type2>) -> bool {
    std::ptr::eq(lhs.as_ref(), rhs.as_ref())
}

struct NoopRewriter;

impl TypeRewriter for NoopRewriter {}

struct EffectRowArgumentRewriter {
    signature_id: GenericParameterID,
    tail_id: GenericParameterID,
    signature: Interned<Type2>,
    tail: Interned<Type2>,
}

impl TypeRewriter for EffectRowArgumentRewriter {
    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _: RewriteContext,
    ) -> Option<Interned<Type2>> {
        match id {
            id if id == self.signature_id => Some(self.signature.clone()),
            id if id == self.tail_id => Some(self.tail.clone()),
            _ => None,
        }
    }
}

// input: {X: T0 | T1}
// premise: rewrite T0 -> bool and T1 -> {}
// output: {X: bool | {}}
#[tokio::test]
async fn rewriting_recurses_through_both_effect_row_arguments() {
    let engine = create_test_engine().await;
    let signature_id = generic_parameter_id(0);
    let tail_id = generic_parameter_id(1);
    let label: Interned<str> = engine.intern_unsized("X".to_owned());
    let row = Type2::new_effect_row_extend(
        label.clone(),
        Type2::new_generic_parameter(signature_id, &engine),
        Type2::new_generic_parameter(tail_id, &engine),
        &engine,
    );
    let signature = Type2::new_primitive(Primitive::Bool, &engine);
    let tail = Type2::new_effect_row_empty(&engine);

    let rewritten = rewrite_type_or_clone(
        &row,
        &mut EffectRowArgumentRewriter {
            signature_id,
            tail_id,
            signature: signature.clone(),
            tail: tail.clone(),
        },
        &engine,
    );

    assert_eq!(
        rewritten,
        Type2::new_effect_row_extend(label, signature, tail, &engine)
    );
}

// input: {X: ^0.0 | ^0.1}
// premise: instantiate (EffectSignature, EffectRow) with (bool, {})
// output: {X: bool | {}}
#[tokio::test]
async fn instantiation_recurses_through_both_effect_row_arguments() {
    let engine = create_test_engine().await;
    let label: Interned<str> = engine.intern_unsized("X".to_owned());
    let row = Type2::new_effect_row_extend(
        label.clone(),
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine),
        Type2::new_bound_variable(BoundVariable::new(0, 1), &engine),
        &engine,
    );
    let binder = Binder::new(engine.intern_unsized(vec![
        crate::r#type::kind::TyKind::EffectSignature,
        crate::r#type::kind::TyKind::EffectRow,
    ]));
    let signature = Type2::new_primitive(Primitive::Bool, &engine);
    let tail = Type2::new_effect_row_empty(&engine);

    let instantiated =
        binder.instantiate(&row, &[signature.clone(), tail.clone()], &engine);

    assert_eq!(
        instantiated,
        Type2::new_effect_row_extend(label, signature, tail, &engine)
    );
}

// input: int32[T0] rewritten by a no-op rewriter
// premise: {}
// output: the original interned type
#[tokio::test]
async fn noop_rewriter_returns_original_type() {
    let engine = create_test_engine().await;
    let argument =
        Type2::new_generic_parameter(generic_parameter_id(0), &engine);
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [argument],
        &engine,
    );

    let rewritten = rewrite_type_or_clone(&ty, &mut NoopRewriter, &engine);

    assert!(same_type_handle(&ty, &rewritten));
}

struct GenericParameterRewriter {
    target: GenericParameterID,
    replacement: Interned<Type2>,
}

impl TypeRewriter for GenericParameterRewriter {
    fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _: RewriteContext,
    ) -> Option<Interned<Type2>> {
        (id == self.target).then(|| self.replacement.clone())
    }
}

// input: int32[T1, int16[T0], float32], rewriting T0 to bool
// premise: {}
// output: int32[T1, int16[bool], float32] with unchanged nodes preserved
#[tokio::test]
async fn rewrite_nested_generic_parameter_preserves_unchanged_siblings() {
    let engine = create_test_engine().await;
    let target = generic_parameter_id(0);
    let unchanged =
        Type2::new_generic_parameter(generic_parameter_id(1), &engine);
    let target_type = Type2::new_generic_parameter(target, &engine);
    let replacement = Type2::new_primitive(Primitive::Bool, &engine);
    let nested = Type2::new_application(
        Constructor::Primitive(Primitive::Int16),
        [target_type],
        &engine,
    );
    let trailing = Type2::new_primitive(Primitive::Float32, &engine);
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [unchanged.clone(), nested.clone(), trailing.clone()],
        &engine,
    );

    let rewritten = rewrite_type_or_clone(
        &ty,
        &mut GenericParameterRewriter {
            target,
            replacement: replacement.clone(),
        },
        &engine,
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
    replacement: Interned<Type2>,
    argument_replacement: Interned<Type2>,
    visited_generic_parameters: usize,
    saw_rewritten_argument: bool,
}

struct AsyncApplicationRewriter {
    target: GenericParameterID,
    target_constructor: Constructor,
    replacement: Interned<Type2>,
    argument_replacement: Interned<Type2>,
    saw_argument_binder_depth: Option<usize>,
    saw_rewritten_argument: bool,
}

impl AsyncTypeRewriter for AsyncApplicationRewriter {
    type Error = ();

    async fn rewrite_application(
        &mut self,
        application: &Application,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type2>>, Self::Error> {
        if application.constructor() == &self.target_constructor {
            self.saw_rewritten_argument = same_type_handle(
                &application.arguments()[0],
                &self.argument_replacement,
            );
        }

        Ok((application.constructor() == &self.target_constructor)
            .then(|| self.replacement.clone()))
    }

    async fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        ctx: RewriteContext,
    ) -> Result<Option<Interned<Type2>>, Self::Error> {
        if id == self.target {
            self.saw_argument_binder_depth = Some(ctx.binder_depth());
            Ok(Some(self.argument_replacement.clone()))
        } else {
            Ok(None)
        }
    }
}

struct AsyncNoopRewriter;

impl AsyncTypeRewriter for AsyncNoopRewriter {
    type Error = ();
}

impl TypeRewriter for ApplicationRewriter {
    fn rewrite_application(
        &mut self,
        application: &Application,
        _: RewriteContext,
    ) -> Option<Interned<Type2>> {
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
    ) -> Option<Interned<Type2>> {
        self.visited_generic_parameters += 1;
        Some(self.argument_replacement.clone())
    }
}

// input: int32[T0] rewritten asynchronously by a no-op rewriter
// premise: {}
// output: no replacement
#[tokio::test]
async fn async_type_rewriter_uses_async_traversal() {
    let engine = create_test_engine().await;
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [Type2::new_generic_parameter(generic_parameter_id(0), &engine)],
        &engine,
    );

    let rewritten =
        rewrite_type_async(&ty, &mut AsyncNoopRewriter, &engine).await.unwrap();

    assert!(rewritten.is_none());
}

// input: for<T>. fn(int16[T0]), rewriting T0 to uint8 and int16[_] to bool
// premise: {}
// output: for<T>. fn(bool), with T0 visited at binder depth 1
#[tokio::test]
async fn async_application_rewriter_runs_after_rewriting_arguments() {
    let engine = create_test_engine().await;
    let target = generic_parameter_id(0);
    let target_constructor = Constructor::Primitive(Primitive::Int16);
    let replacement = Type2::new_primitive(Primitive::Bool, &engine);
    let argument_replacement = Type2::new_primitive(Primitive::Uint8, &engine);
    let nested = Type2::new_application(
        target_constructor.clone(),
        [Type2::new_generic_parameter(target, &engine)],
        &engine,
    );
    let ty = Type2::new_function_pointer_with_binder(
        Binder::new(
            engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type]),
        ),
        [],
        nested,
        &engine,
    );
    let mut rewriter = AsyncApplicationRewriter {
        target,
        target_constructor,
        replacement: replacement.clone(),
        argument_replacement,
        saw_argument_binder_depth: None,
        saw_rewritten_argument: false,
    };

    let rewritten =
        rewrite_type_or_clone_async(&ty, &mut rewriter, &engine).await.unwrap();

    let rewritten_application = as_application(&rewritten);
    assert!(same_type_handle(
        &rewritten_application.arguments[0],
        &replacement
    ));
    assert_eq!(rewriter.saw_argument_binder_depth, Some(1));
    assert!(rewriter.saw_rewritten_argument);
}

// input: int32[int16[T0]], rewriting T0 to uint8 and int16[_] to bool
// premise: {}
// output: int32[bool], with the application callback observing uint8
#[tokio::test]
async fn application_rewriter_runs_after_rewriting_arguments() {
    let engine = create_test_engine().await;
    let target_constructor = Constructor::Primitive(Primitive::Int16);
    let replacement = Type2::new_primitive(Primitive::Bool, &engine);
    let argument_replacement = Type2::new_primitive(Primitive::Uint8, &engine);
    let nested = Type2::new_application(
        target_constructor.clone(),
        [Type2::new_generic_parameter(generic_parameter_id(0), &engine)],
        &engine,
    );
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [nested],
        &engine,
    );
    let mut rewriter = ApplicationRewriter {
        target_constructor,
        replacement: replacement.clone(),
        argument_replacement,
        visited_generic_parameters: 0,
        saw_rewritten_argument: false,
    };

    let rewritten = rewrite_type_or_clone(&ty, &mut rewriter, &engine);

    let rewritten_application = as_application(&rewritten);
    assert!(same_type_handle(
        &rewritten_application.arguments[0],
        &replacement
    ));
    assert_eq!(rewriter.visited_generic_parameters, 1);
    assert!(rewriter.saw_rewritten_argument);
}

struct FailingAsyncRewriter {
    target: GenericParameterID,
}

impl AsyncTypeRewriter for FailingAsyncRewriter {
    type Error = &'static str;

    async fn rewrite_generic_parameter(
        &mut self,
        id: GenericParameterID,
        _: RewriteContext,
    ) -> Result<Option<Interned<Type2>>, Self::Error> {
        if id == self.target { Err("stop") } else { Ok(None) }
    }
}

// input: int32[T0] rewritten by a callback that fails on T0
// premise: {}
// output: error "stop"
#[tokio::test]
async fn async_type_rewriter_returns_rewriter_error() {
    let engine = create_test_engine().await;
    let target = generic_parameter_id(0);
    let argument = Type2::new_generic_parameter(target, &engine);
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [argument],
        &engine,
    );

    let result =
        rewrite_type_async(&ty, &mut FailingAsyncRewriter { target }, &engine)
            .await;

    assert_eq!(result, Err("stop"));
}

// input: int32[T0, T1] substituted with { T0 = bool }
// premise: {}
// output: int32[bool, T1], with T1 preserved
#[tokio::test]
async fn instantiation_replaces_generic_parameter_and_leaves_missing_unchanged()
{
    let engine = create_test_engine().await;
    let target = generic_parameter_id(0);
    let missing = generic_parameter_id(1);
    let target_type = Type2::new_generic_parameter(target, &engine);
    let missing_type = Type2::new_generic_parameter(missing, &engine);
    let replacement = Type2::new_primitive(Primitive::Bool, &engine);
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [target_type, missing_type.clone()],
        &engine,
    );
    let mut instantiation = Substitution::default();
    instantiation.insert_generic(target, replacement.clone());

    let instantiated = ty.apply_or_clone(&instantiation, &engine);

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
    ) -> Option<Interned<Type2>> {
        self.records.push((id, ctx.binder_depth()));
        None
    }
}

// input: int32[T0, for<T>. fn(T1, for<U>. fn(T2))]
// premise: {}
// output: T0, T1, and T2 visited at binder depths 0, 1, and 2
#[tokio::test]
async fn binder_depth_tracks_function_pointer_nesting() {
    let engine = create_test_engine().await;
    let outside = generic_parameter_id(0);
    let inside_one = generic_parameter_id(1);
    let inside_two = generic_parameter_id(2);
    let nested_function_pointer = Type2::new_function_pointer_with_binder(
        Binder::new(
            engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type]),
        ),
        [],
        Type2::new_generic_parameter(inside_two, &engine),
        &engine,
    );
    let function_pointer = Type2::new_function_pointer_with_binder(
        Binder::new(
            engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type; 2]),
        ),
        [Type2::new_generic_parameter(inside_one, &engine)],
        nested_function_pointer,
        &engine,
    );
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [Type2::new_generic_parameter(outside, &engine), function_pointer],
        &engine,
    );
    let mut recorder = BinderDepthRecorder::default();

    let rewritten = rewrite_type_or_clone(&ty, &mut recorder, &engine);

    assert!(same_type_handle(&ty, &rewritten));
    assert_eq!(recorder.records, vec![
        (outside, 0),
        (inside_one, 1),
        (inside_two, 2),
    ]);
}

// input: for<T, U>. fn(T0, int16), rewriting T0 to bool
// premise: {}
// output: for<T, U>. fn(bool, int16), after visiting both arguments
#[tokio::test]
async fn rewrite_application_supports_async_failable_rewrite() {
    let engine = create_test_engine().await;
    let target = generic_parameter_id(0);
    let target_type = Type2::new_generic_parameter(target, &engine);
    let unchanged = Type2::new_primitive(Primitive::Int16, &engine);
    let replacement = Type2::new_primitive(Primitive::Bool, &engine);
    let function_pointer = Type2::new_function_pointer_with_binder(
        Binder::new(
            engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type; 2]),
        ),
        [target_type],
        unchanged.clone(),
        &engine,
    );
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

// input: int32[T0] rewritten by a failing argument callback
// premise: {}
// output: error "stop"
#[tokio::test]
async fn rewrite_application_returns_callback_error() {
    let engine = create_test_engine().await;
    let argument =
        Type2::new_generic_parameter(generic_parameter_id(0), &engine);
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [argument],
        &engine,
    );

    let result = rewrite_application(as_application(&ty), async |_| {
        Err::<Option<Interned<Type2>>, _>("stop")
    })
    .await;

    assert_eq!(result, Err("stop"));
}

// input: int32[^0.0, for<T>. fn(^0.0, ^1.0), ^0.1]
// premise: instantiate outer binder with [bool, int16]
// output: int32[bool, for<T>. fn(^0.0, bool), int16]
#[tokio::test]
async fn instantiate_respects_nested_binders() {
    let engine = create_test_engine().await;
    let replacement_zero = Type2::new_primitive(Primitive::Bool, &engine);
    let replacement_one = Type2::new_primitive(Primitive::Int16, &engine);
    let nested_inner_bound =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let nested_outer_bound =
        Type2::new_bound_variable(BoundVariable::new(1, 0), &engine);
    let nested_function_pointer = Type2::new_function_pointer_with_binder(
        Binder::new(
            engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type; 2]),
        ),
        [nested_inner_bound.clone()],
        nested_outer_bound,
        &engine,
    );
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [
            Type2::new_bound_variable(BoundVariable::new(0, 0), &engine),
            nested_function_pointer,
            Type2::new_bound_variable(BoundVariable::new(0, 1), &engine),
        ],
        &engine,
    );
    let binder = Binder::new(engine.intern_unsized([
        crate::r#type::kind::TyKind::Type,
        crate::r#type::kind::TyKind::Type,
    ]));

    let rewritten = binder.instantiate(
        &ty,
        &[replacement_zero.clone(), replacement_one.clone()],
        &engine,
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

// input: Symbolic[^0.0, ^1.0]
// premise: instantiate outer binder with [bool]
// output: Symbolic[^0.0, bool]
#[tokio::test]
async fn instantiate_respects_symbolic_binders() {
    let engine = create_test_engine().await;
    let replacement = Type2::new_primitive(Primitive::Bool, &engine);
    let inner_bound =
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine);
    let outer_bound =
        Type2::new_bound_variable(BoundVariable::new(1, 0), &engine);
    let nested_symbolic = Type2::new_symbolic_with_binder(
        SYMBOL_ID,
        Binder::new(
            engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type; 2]),
        ),
        [inner_bound.clone(), outer_bound],
        &engine,
    );
    let binder = Binder::new(
        engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type]),
    );

    let rewritten = binder.instantiate(
        &nested_symbolic,
        std::slice::from_ref(&replacement),
        &engine,
    );

    let rewritten = as_application(&rewritten);
    assert!(same_type_handle(&rewritten.arguments[0], &inner_bound));
    assert!(same_type_handle(&rewritten.arguments[1], &replacement));
}

// input: int32[^0.1] instantiated with no replacements
// premise: {}
// output: int32[^0.1], with the missing replacement preserved
#[tokio::test]
async fn instantiate_leaves_missing_replacement_unchanged() {
    let engine = create_test_engine().await;
    let missing = Type2::new_bound_variable(BoundVariable::new(0, 1), &engine);
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [missing.clone()],
        &engine,
    );

    let rewritten = ty.instantiate(&[], &engine);

    let rewritten_application = as_application(&rewritten);
    assert!(same_type_handle(&rewritten_application.arguments[0], &missing));
}

// input: [^0.0, int16]
// premise: instantiate with [bool]
// output: [bool, int16]
#[tokio::test]
async fn instantiate_interned_slice_of_types() {
    let engine = create_test_engine().await;
    let replacement = Type2::new_primitive(Primitive::Bool, &engine);
    let unchanged = Type2::new_primitive(Primitive::Int16, &engine);
    let arguments: Interned<[Interned<Type2>]> = engine.intern_unsized(vec![
        Type2::new_bound_variable(BoundVariable::new(0, 0), &engine),
        unchanged.clone(),
    ]);

    let instantiated =
        arguments.instantiate(std::slice::from_ref(&replacement), &engine);

    assert!(same_type_handle(&instantiated[0], &replacement));
    assert!(same_type_handle(&instantiated[1], &unchanged));
}

// input: int32[for<T>. fn(^1.0)]
// premise: instantiate ^0.0 with free variable ^0.7
// output: int32[for<T>. fn(^1.7)]
#[tokio::test]
async fn instantiate_shifts_free_bound_variables_in_replacement() {
    let engine = create_test_engine().await;
    let replacement =
        Type2::new_bound_variable(BoundVariable::new(0, 7), &engine);
    let nested_function_pointer = Type2::new_function_pointer_with_binder(
        Binder::new(
            engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type]),
        ),
        [],
        Type2::new_bound_variable(BoundVariable::new(1, 0), &engine),
        &engine,
    );
    let ty = Type2::new_application(
        Constructor::Primitive(Primitive::Int32),
        [nested_function_pointer],
        &engine,
    );
    let binder = Binder::new(
        engine.intern_unsized(vec![crate::r#type::kind::TyKind::Type]),
    );

    let rewritten =
        binder.instantiate(&ty, std::slice::from_ref(&replacement), &engine);

    let rewritten_application = as_application(&rewritten);
    let rewritten_nested = as_application(&rewritten_application.arguments[0]);
    let rewritten_variable = as_bound_variable(&rewritten_nested.arguments[0]);

    assert_eq!(rewritten_variable.depth(), 1);
    assert_eq!(rewritten_variable.index(), 7);
}
