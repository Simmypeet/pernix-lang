//! Recursive post-order folding for interned terms and term-bearing
//! containers.

use std::{future::Future, pin::Pin};

use pernixc_qbice::TrackedEngine;
use qbice::{Identifiable, StableHash, storage::intern::Interned};

use crate::{
    constant::Constant, instance::Instance, lifetime::Lifetime, tuple,
    r#type::Type,
};

/// A short-circuit signal for recursive folding.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("folding aborted")]
pub struct Abort;

/// A boxed future used by recursive async folding entrypoints.
pub type FoldFuture<'a> =
    Pin<Box<dyn Future<Output = Result<(), Abort>> + Send + 'a>>;

/// A folder that may rewrite interned term handles in post-order.
pub trait Folder {
    /// Folds a lifetime handle after all of its children have been folded.
    fn fold_lifetime(
        &mut self,
        _: &mut Interned<Lifetime>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        Ok(())
    }

    /// Folds a type handle after all of its children have been folded.
    fn fold_type(
        &mut self,
        _: &mut Interned<Type>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        Ok(())
    }

    /// Folds a constant handle after all of its children have been folded.
    fn fold_constant(
        &mut self,
        _: &mut Interned<Constant>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        Ok(())
    }

    /// Folds an instance handle after all of its children have been folded.
    fn fold_instance(
        &mut self,
        _: &mut Interned<Instance>,
        _: &TrackedEngine,
    ) -> Result<(), Abort> {
        Ok(())
    }
}

/// An asynchronous folder that may rewrite interned term handles in
/// post-order.
pub trait FolderAsync: Send {
    /// Folds a lifetime handle after all of its children have been folded.
    fn fold_lifetime<'a>(
        &'a mut self,
        _: &'a mut Interned<Lifetime>,
        _: &'a TrackedEngine,
    ) -> impl Future<Output = Result<(), Abort>> + Send + 'a {
        async { Ok(()) }
    }

    /// Folds a type handle after all of its children have been folded.
    fn fold_type<'a>(
        &'a mut self,
        _: &'a mut Interned<Type>,
        _: &'a TrackedEngine,
    ) -> impl Future<Output = Result<(), Abort>> + Send + 'a {
        async { Ok(()) }
    }

    /// Folds a constant handle after all of its children have been folded.
    fn fold_constant<'a>(
        &'a mut self,
        _: &'a mut Interned<Constant>,
        _: &'a TrackedEngine,
    ) -> impl Future<Output = Result<(), Abort>> + Send + 'a {
        async { Ok(()) }
    }

    /// Folds an instance handle after all of its children have been folded.
    fn fold_instance<'a>(
        &'a mut self,
        _: &'a mut Interned<Instance>,
        _: &'a TrackedEngine,
    ) -> impl Future<Output = Result<(), Abort>> + Send + 'a {
        async { Ok(()) }
    }
}

/// A type that can be recursively folded through an interned handle.
pub trait Foldable: Sized {
    /// Recursively folds the interned handle in post-order.
    fn fold_with<F: Folder>(
        term: &mut Interned<Self>,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort>;
}

/// A type that can be recursively folded through an interned handle
/// asynchronously.
pub trait FoldableAsync: Sized {
    /// Recursively folds the interned handle in post-order.
    fn fold_with_async<'a, F: FolderAsync + 'a>(
        term: &'a mut Interned<Self>,
        folder: &'a mut F,
        engine: &'a TrackedEngine,
    ) -> FoldFuture<'a>;
}

/// Extension methods for folding interned handles.
pub trait FoldExt {
    /// The payload stored behind this interned handle.
    type Payload: Foldable;

    /// Recursively folds the handle in post-order.
    fn fold_with<F: Folder>(
        &mut self,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort>;
}

/// Extension methods for asynchronously folding interned handles.
pub trait FoldExtAsync {
    /// The payload stored behind this interned handle.
    type Payload: FoldableAsync;

    /// Recursively folds the handle in post-order.
    fn fold_with_async<'a, F: FolderAsync + 'a>(
        &'a mut self,
        folder: &'a mut F,
        engine: &'a TrackedEngine,
    ) -> FoldFuture<'a>;
}

impl<T: Foldable> FoldExt for Interned<T> {
    type Payload = T;

    fn fold_with<F: Folder>(
        &mut self,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort> {
        T::fold_with(self, folder, engine)
    }
}

impl<T: FoldableAsync> FoldExtAsync for Interned<T> {
    type Payload = T;

    fn fold_with_async<'a, F: FolderAsync + 'a>(
        &'a mut self,
        folder: &'a mut F,
        engine: &'a TrackedEngine,
    ) -> FoldFuture<'a> {
        T::fold_with_async(self, folder, engine)
    }
}

pub(crate) fn fold_interned<T, F, FoldChildren, FoldSelf>(
    term: &mut Interned<T>,
    folder: &mut F,
    engine: &TrackedEngine,
    fold_children: FoldChildren,
    fold_self: FoldSelf,
) -> Result<(), Abort>
where
    T: Clone + PartialEq + Identifiable + StableHash + Send + Sync + 'static,
    F: Folder,
    FoldChildren: FnOnce(&mut T, &mut F, &TrackedEngine) -> Result<(), Abort>,
    FoldSelf:
        FnOnce(&mut F, &mut Interned<T>, &TrackedEngine) -> Result<(), Abort>,
{
    let mut rebuilt_value = term.as_ref().clone();
    fold_children(&mut rebuilt_value, folder, engine)?;

    if term.as_ref() != &rebuilt_value {
        *term = engine.intern(rebuilt_value);
    }

    fold_self(folder, term, engine)
}

pub(crate) fn fold_term_slice<T: Foldable, F: Folder>(
    terms: &mut [Interned<T>],
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    for term in terms {
        T::fold_with(term, folder, engine)?;
    }

    Ok(())
}

pub(crate) fn fold_option_term<T: Foldable, F: Folder>(
    term: &mut Option<Interned<T>>,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    if let Some(term) = term {
        T::fold_with(term, folder, engine)?;
    }

    Ok(())
}

pub(crate) fn fold_tuple_terms<T: Foldable, F: Folder>(
    tuple: &mut tuple::Tuple<T>,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    for element in tuple.elements_mut() {
        T::fold_with(element.term_mut(), folder, engine)?;
    }

    Ok(())
}

pub(crate) async fn fold_term_slice_async<T: FoldableAsync, F: FolderAsync>(
    terms: &mut [Interned<T>],
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    for term in terms {
        T::fold_with_async(term, folder, engine).await?;
    }

    Ok(())
}

pub(crate) async fn fold_option_term_async<T: FoldableAsync, F: FolderAsync>(
    term: &mut Option<Interned<T>>,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    if let Some(term) = term {
        T::fold_with_async(term, folder, engine).await?;
    }

    Ok(())
}

pub(crate) async fn fold_tuple_terms_async<T: FoldableAsync, F: FolderAsync>(
    tuple: &mut tuple::Tuple<T>,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    for element in tuple.elements_mut() {
        T::fold_with_async(element.term_mut(), folder, engine).await?;
    }

    Ok(())
}

macro_rules! finish_fold_async {
    ($term:expr, $rebuilt_value:expr, $folder:expr, $engine:expr, $hook:ident) => {{
        if $term.as_ref() != &$rebuilt_value {
            *$term = $engine.intern($rebuilt_value);
        }

        $folder.$hook($term, $engine).await
    }};
}

pub(crate) use finish_fold_async;

#[cfg(test)]
mod tests {
    use pernixc_symbol::SymbolID;
    use pernixc_target::TargetID;

    use super::*;
    use crate::{
        constant,
        generic_arguments::{AssociatedSymbol, GenericArguments, Symbol},
        instance::{AnoymousTrait, InstanceAssociated},
        lifetime::Lifetime,
        test_support::create_test_engine,
        r#type::{Primitive, Qualifier, Reference},
    };

    struct NoOpFolder;

    impl Folder for NoOpFolder {}

    impl FolderAsync for NoOpFolder {}

    #[derive(Default)]
    struct RecordingFolder {
        saw_rebuilt_reference: bool,
        visited_types: Vec<String>,
    }

    impl Folder for RecordingFolder {
        fn fold_type(
            &mut self,
            r#type: &mut Interned<Type>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            self.visited_types.push(format!("{:?}", r#type.as_ref()));

            match r#type.as_ref() {
                Type::Primitive(Primitive::Bool) => {
                    *r#type = engine.intern(Type::Primitive(Primitive::Uint32));
                }

                Type::Reference(reference) => {
                    self.saw_rebuilt_reference = matches!(
                        reference.pointee().as_ref(),
                        Type::Primitive(Primitive::Uint32),
                    );
                }

                _ => {}
            }

            Ok(())
        }
    }

    impl FolderAsync for RecordingFolder {
        async fn fold_type(
            &mut self,
            r#type: &mut Interned<Type>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            self.visited_types.push(format!("{:?}", r#type.as_ref()));

            match r#type.as_ref() {
                Type::Primitive(Primitive::Bool) => {
                    *r#type = engine.intern(Type::Primitive(Primitive::Uint32));
                }

                Type::Reference(reference) => {
                    self.saw_rebuilt_reference = matches!(
                        reference.pointee().as_ref(),
                        Type::Primitive(Primitive::Uint32),
                    );
                }

                _ => {}
            }

            Ok(())
        }
    }

    struct RewriteAllTerms {
        replacement_instance_id: pernixc_target::Global<SymbolID>,
    }

    impl Folder for RewriteAllTerms {
        fn fold_lifetime(
            &mut self,
            lifetime: &mut Interned<Lifetime>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(lifetime.as_ref(), Lifetime::Static) {
                *lifetime = engine.intern(Lifetime::Erased);
            }

            Ok(())
        }

        fn fold_type(
            &mut self,
            r#type: &mut Interned<Type>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(r#type.as_ref(), Type::Primitive(Primitive::Bool)) {
                *r#type = engine.intern(Type::Primitive(Primitive::Uint32));
            }

            Ok(())
        }

        fn fold_constant(
            &mut self,
            constant: &mut Interned<Constant>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(
                constant.as_ref(),
                Constant::Primitive(constant::Primitive::Bool(true)),
            ) {
                *constant = engine.intern(Constant::Primitive(
                    constant::Primitive::Bool(false),
                ));
            }

            Ok(())
        }

        fn fold_instance(
            &mut self,
            instance: &mut Interned<Instance>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(instance.as_ref(), Instance::AnonymousTrait(_)) {
                *instance = engine.intern(Instance::AnonymousTrait(
                    AnoymousTrait::new(self.replacement_instance_id),
                ));
            }

            Ok(())
        }
    }

    impl FolderAsync for RewriteAllTerms {
        async fn fold_lifetime(
            &mut self,
            lifetime: &mut Interned<Lifetime>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(lifetime.as_ref(), Lifetime::Static) {
                *lifetime = engine.intern(Lifetime::Erased);
            }

            Ok(())
        }

        async fn fold_type(
            &mut self,
            r#type: &mut Interned<Type>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(r#type.as_ref(), Type::Primitive(Primitive::Bool)) {
                *r#type = engine.intern(Type::Primitive(Primitive::Uint32));
            }

            Ok(())
        }

        async fn fold_constant(
            &mut self,
            constant: &mut Interned<Constant>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(
                constant.as_ref(),
                Constant::Primitive(constant::Primitive::Bool(true)),
            ) {
                *constant = engine.intern(Constant::Primitive(
                    constant::Primitive::Bool(false),
                ));
            }

            Ok(())
        }

        async fn fold_instance(
            &mut self,
            instance: &mut Interned<Instance>,
            engine: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(instance.as_ref(), Instance::AnonymousTrait(_)) {
                *instance = engine.intern(Instance::AnonymousTrait(
                    AnoymousTrait::new(self.replacement_instance_id),
                ));
            }

            Ok(())
        }
    }

    struct AbortOnBool;

    impl Folder for AbortOnBool {
        fn fold_type(
            &mut self,
            r#type: &mut Interned<Type>,
            _: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(r#type.as_ref(), Type::Primitive(Primitive::Bool)) {
                return Err(Abort);
            }

            Ok(())
        }
    }

    impl FolderAsync for AbortOnBool {
        async fn fold_type(
            &mut self,
            r#type: &mut Interned<Type>,
            _: &TrackedEngine,
        ) -> Result<(), Abort> {
            if matches!(r#type.as_ref(), Type::Primitive(Primitive::Bool)) {
                return Err(Abort);
            }

            Ok(())
        }
    }

    #[tokio::test]
    async fn post_order_type_folding_sees_rebuilt_children() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let mut r#type = tracked.intern(Type::Reference(Reference::new(
            Qualifier::Immutable,
            tracked.intern(Lifetime::Static),
            tracked.intern(Type::Primitive(Primitive::Bool)),
        )));
        let mut folder = RecordingFolder::default();

        r#type.fold_with(&mut folder, &tracked).unwrap();

        assert!(folder.saw_rebuilt_reference);
        assert!(folder.visited_types[0].contains("Bool"));

        let Type::Reference(reference) = r#type.as_ref() else {
            panic!("expected reference type");
        };

        assert!(matches!(
            reference.pointee().as_ref(),
            Type::Primitive(Primitive::Uint32),
        ));
    }

    #[tokio::test]
    async fn no_op_folder_preserves_term_handle() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let original = tracked.intern(Type::Primitive(Primitive::Bool));
        let mut folded = original.clone();

        folded.fold_with(&mut NoOpFolder, &tracked).unwrap();

        assert_eq!(folded, original);
        assert!(std::ptr::eq(folded.as_ref(), original.as_ref()));
    }

    #[tokio::test]
    async fn abort_during_child_fold_leaves_root_unchanged() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let original = tracked.intern(Type::Reference(Reference::new(
            Qualifier::Immutable,
            tracked.intern(Lifetime::Static),
            tracked.intern(Type::Primitive(Primitive::Bool)),
        )));
        let mut folded = original.clone();

        assert_eq!(folded.fold_with(&mut AbortOnBool, &tracked), Err(Abort));
        assert_eq!(folded, original);
    }

    #[tokio::test]
    async fn generic_arguments_folding_updates_all_argument_lists() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;
        let replacement_instance_id =
            TargetID::TEST.make_global(SymbolID::from_u128(41));

        let mut generic_arguments = tracked.intern(GenericArguments::new(
            vec![tracked.intern(Lifetime::Static)],
            vec![tracked.intern(Type::Primitive(Primitive::Bool))],
            vec![
                tracked.intern(Constant::Primitive(constant::Primitive::Bool(
                    true,
                ))),
            ],
            vec![tracked.intern(Instance::AnonymousTrait(AnoymousTrait::new(
                TargetID::TEST.make_global(SymbolID::from_u128(40)),
            )))],
        ));

        generic_arguments
            .fold_with(
                &mut RewriteAllTerms { replacement_instance_id },
                &tracked,
            )
            .unwrap();

        assert!(matches!(
            generic_arguments.lifetimes()[0].as_ref(),
            Lifetime::Erased,
        ));
        assert!(matches!(
            generic_arguments.types()[0].as_ref(),
            Type::Primitive(Primitive::Uint32),
        ));
        assert!(matches!(
            generic_arguments.constants()[0].as_ref(),
            Constant::Primitive(constant::Primitive::Bool(false)),
        ));
        assert!(matches!(
            generic_arguments.instances()[0].as_ref(),
            Instance::AnonymousTrait(instance)
                if instance.trait_id() == replacement_instance_id,
        ));
    }

    #[tokio::test]
    async fn post_order_type_folding_async_sees_rebuilt_children() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let mut r#type = tracked.intern(Type::Reference(Reference::new(
            Qualifier::Immutable,
            tracked.intern(Lifetime::Static),
            tracked.intern(Type::Primitive(Primitive::Bool)),
        )));
        let mut folder = RecordingFolder::default();

        r#type.fold_with_async(&mut folder, &tracked).await.unwrap();

        assert!(folder.saw_rebuilt_reference);
        assert!(folder.visited_types[0].contains("Bool"));

        let Type::Reference(reference) = r#type.as_ref() else {
            panic!("expected reference type");
        };

        assert!(matches!(
            reference.pointee().as_ref(),
            Type::Primitive(Primitive::Uint32),
        ));
    }

    #[tokio::test]
    async fn generic_arguments_folding_async_updates_all_argument_lists() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;
        let replacement_instance_id =
            TargetID::TEST.make_global(SymbolID::from_u128(71));

        let mut generic_arguments = tracked.intern(GenericArguments::new(
            vec![tracked.intern(Lifetime::Static)],
            vec![tracked.intern(Type::Primitive(Primitive::Bool))],
            vec![
                tracked.intern(Constant::Primitive(constant::Primitive::Bool(
                    true,
                ))),
            ],
            vec![tracked.intern(Instance::AnonymousTrait(AnoymousTrait::new(
                TargetID::TEST.make_global(SymbolID::from_u128(70)),
            )))],
        ));

        generic_arguments
            .fold_with_async(
                &mut RewriteAllTerms { replacement_instance_id },
                &tracked,
            )
            .await
            .unwrap();

        assert!(matches!(
            generic_arguments.lifetimes()[0].as_ref(),
            Lifetime::Erased,
        ));
        assert!(matches!(
            generic_arguments.types()[0].as_ref(),
            Type::Primitive(Primitive::Uint32),
        ));
        assert!(matches!(
            generic_arguments.constants()[0].as_ref(),
            Constant::Primitive(constant::Primitive::Bool(false)),
        ));
        assert!(matches!(
            generic_arguments.instances()[0].as_ref(),
            Instance::AnonymousTrait(instance)
                if instance.trait_id() == replacement_instance_id,
        ));
    }

    #[tokio::test]
    async fn symbol_and_associated_symbol_folding_rebuild_generic_arguments() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;
        let replacement_instance_id =
            TargetID::TEST.make_global(SymbolID::from_u128(51));
        let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(52));

        let symbol_args = tracked.intern(GenericArguments::new(
            vec![tracked.intern(Lifetime::Static)],
            vec![tracked.intern(Type::Primitive(Primitive::Bool))],
            vec![],
            vec![],
        ));
        let mut symbol = tracked.intern(Symbol::new(symbol_id, symbol_args));

        symbol
            .fold_with(
                &mut RewriteAllTerms { replacement_instance_id },
                &tracked,
            )
            .unwrap();

        assert!(matches!(
            symbol.generic_arguments().lifetimes()[0].as_ref(),
            Lifetime::Erased,
        ));
        assert!(matches!(
            symbol.generic_arguments().types()[0].as_ref(),
            Type::Primitive(Primitive::Uint32),
        ));

        let parent_args = tracked.intern(GenericArguments::new(
            vec![tracked.intern(Lifetime::Static)],
            vec![],
            vec![],
            vec![],
        ));
        let member_args = tracked.intern(GenericArguments::new(
            vec![],
            vec![tracked.intern(Type::Primitive(Primitive::Bool))],
            vec![],
            vec![tracked.intern(Instance::AnonymousTrait(AnoymousTrait::new(
                TargetID::TEST.make_global(SymbolID::from_u128(50)),
            )))],
        ));
        let mut associated_symbol = tracked.intern(AssociatedSymbol::new(
            symbol_id,
            parent_args,
            member_args,
        ));

        associated_symbol
            .fold_with(
                &mut RewriteAllTerms { replacement_instance_id },
                &tracked,
            )
            .unwrap();

        assert!(matches!(
            associated_symbol.parent_generic_arguments().lifetimes()[0]
                .as_ref(),
            Lifetime::Erased,
        ));
        assert!(matches!(
            associated_symbol.member_generic_arguments().types()[0].as_ref(),
            Type::Primitive(Primitive::Uint32),
        ));
        assert!(matches!(
            associated_symbol.member_generic_arguments().instances()[0].as_ref(),
            Instance::AnonymousTrait(instance)
                if instance.trait_id() == replacement_instance_id,
        ));
    }

    #[tokio::test]
    async fn instance_associated_folding_rebuilds_parent_and_generic_arguments()
    {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;
        let symbol_id = TargetID::TEST.make_global(SymbolID::from_u128(60));
        let original_instance_id =
            TargetID::TEST.make_global(SymbolID::from_u128(61));
        let replacement_instance_id =
            TargetID::TEST.make_global(SymbolID::from_u128(62));

        let mut instance_associated = tracked.intern(InstanceAssociated::new(
            tracked.intern(Instance::AnonymousTrait(AnoymousTrait::new(
                original_instance_id,
            ))),
            symbol_id,
            tracked.intern(GenericArguments::new(
                vec![],
                vec![tracked.intern(Type::Primitive(Primitive::Bool))],
                vec![],
                vec![],
            )),
        ));

        instance_associated
            .fold_with(
                &mut RewriteAllTerms { replacement_instance_id },
                &tracked,
            )
            .unwrap();

        assert!(matches!(
            instance_associated.instance().as_ref(),
            Instance::AnonymousTrait(instance)
                if instance.trait_id() == replacement_instance_id,
        ));
        assert!(matches!(
            instance_associated.associated_instance_generic_arguments().types()
                [0]
            .as_ref(),
            Type::Primitive(Primitive::Uint32),
        ));
    }
}
