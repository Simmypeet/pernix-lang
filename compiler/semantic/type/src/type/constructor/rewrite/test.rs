use pernixc_arena::ID;
use pernixc_qbice::{DuplicatingInterner, Interner};
use pernixc_symbol::{GlobalSymbolID, SymbolID};
use pernixc_target::TargetID;
use qbice::storage::intern::Interned;

use super::*;
use crate::{
    generic_parameters::{GenericParameter, GenericParameterID},
    instantiation::Instantiation,
    r#type::{
        Type,
        bound::Binder,
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
            binder: Binder::new_for_test(Vec::new()),
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

    let rewritten = rewrite_type(&ty, &mut NoopRewriter, &interner);

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

    let rewritten = rewrite_type(
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
    let mut instantiation = Instantiation::default();
    instantiation.insert(target, replacement.clone());

    let instantiated = instantiation.instantiate(&ty, &interner);

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

    let rewritten = rewrite_type(&ty, &mut recorder, &interner);

    assert!(same_type_handle(&ty, &rewritten));
    assert_eq!(recorder.records, vec![
        (outside, 0),
        (inside_one, 1),
        (inside_two, 2),
    ]);
}
