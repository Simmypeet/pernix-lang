use diagnostic::Diagnostic;
use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::kind::get_kind;
use pernixc_target::Global;
use pernixc_term::{
    constant::{Constant, Primitive as ConstantPrimitive},
    generic_arguments::Symbol,
    generic_parameters::get_generic_parameters,
    r#type::Type,
};
use pernixc_type_system::adt_fields::get_instantiated_adt_fields;
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
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
#[value(Interned<[Diagnostic]>)]
pub struct Key {
    pub symbol_id: Global<pernixc_symbol::SymbolID>,
}

#[executor(config = Config)]
pub async fn recursive_adt_check_executor(
    &Key { symbol_id }: &Key,
    engine: &TrackedEngine,
) -> Interned<[Diagnostic]> {
    if !engine.get_kind(symbol_id).await.is_adt() {
        return engine.intern_unsized([]);
    }

    let root_symbol = Symbol::new(
        symbol_id,
        engine
            .get_generic_parameters(symbol_id)
            .await
            .create_identity_generic_arguments(symbol_id),
    );

    let mut symbol_stack = vec![root_symbol.clone()];

    find_recursive_adt(&root_symbol, symbol_id, &mut symbol_stack, engine)
        .await
        .map_or_else(
            || engine.intern_unsized([]),
            |diagnostic| engine.intern_unsized([diagnostic]),
        )
}

async fn find_recursive_adt(
    symbol: &Symbol,
    root_adt_id: Global<pernixc_symbol::SymbolID>,
    symbol_stack: &mut Vec<Symbol>,
    engine: &TrackedEngine,
) -> Option<Diagnostic> {
    for field in engine.get_instantiated_adt_fields(symbol).await {
        if let Some(diagnostic) = Box::pin(find_recursive_in_type(
            field.r#type(),
            field.span().copied(),
            root_adt_id,
            symbol_stack,
            engine,
        ))
        .await
        {
            return Some(diagnostic);
        }
    }

    None
}

async fn find_recursive_in_type(
    ty: &Type,
    declaration_span: Option<pernixc_lexical::tree::RelativeSpan>,
    root_adt_id: Global<pernixc_symbol::SymbolID>,
    symbol_stack: &mut Vec<Symbol>,
    engine: &TrackedEngine,
) -> Option<Diagnostic> {
    match ty {
        Type::Error(_)
        | Type::Primitive(_)
        | Type::Parameter(_)
        | Type::FunctionSignature(_)
        | Type::AssociatedSymbol(_)
        | Type::InstanceAssociated(_)
        | Type::Inference(_)
        | Type::Pointer(_)
        | Type::Reference(_)
        | Type::Phantom(_) => None,

        Type::Array(array) => {
            if is_zero_sized_array(&array.length) {
                return None;
            }

            Box::pin(find_recursive_in_type(
                &array.r#type,
                declaration_span,
                root_adt_id,
                symbol_stack,
                engine,
            ))
            .await
        }

        Type::Tuple(tuple) => {
            for element in tuple.elements() {
                if let Some(diagnostic) = Box::pin(find_recursive_in_type(
                    element.term(),
                    declaration_span,
                    root_adt_id,
                    symbol_stack,
                    engine,
                ))
                .await
                {
                    return Some(diagnostic);
                }
            }

            None
        }

        Type::Symbol(symbol) => {
            if !engine.get_kind(symbol.id()).await.is_adt() {
                return None;
            }

            if let Some(cycle_start) = symbol_stack
                .iter()
                .position(|existing| existing.id() == symbol.id())
            {
                let mut cycle_path = symbol_stack[cycle_start..].to_vec();
                cycle_path.push(symbol.clone());

                return Some(Diagnostic::RecursiveAdt(
                    diagnostic::RecursiveAdt::new(
                        root_adt_id,
                        declaration_span,
                        cycle_path,
                    ),
                ));
            }

            symbol_stack.push(symbol.clone());
            let result = Box::pin(find_recursive_adt(
                symbol,
                root_adt_id,
                symbol_stack,
                engine,
            ))
            .await;
            symbol_stack.pop();

            result
        }
    }
}

const fn is_zero_sized_array(constant: &Constant) -> bool {
    matches!(constant, Constant::Primitive(ConstantPrimitive::Usize(0)))
}

#[distributed_slice(PERNIX_PROGRAM)]
static RECURSIVE_ADT_CHECK_EXECUTOR: Registration<Config> =
    Registration::new::<Key, RecursiveAdtCheckExecutor>();
