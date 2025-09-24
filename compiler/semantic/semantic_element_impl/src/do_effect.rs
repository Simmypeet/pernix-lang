use pernixc_handler::Storage;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_semantic_element::do_effect;
use pernixc_symbol::syntax::get_function_do_effect_syntax;
use pernixc_syntax::item::function::{EffectUnit, EffectUnitListKind};
use pernixc_term::effect;

use crate::build::{Build, Output};

pub mod diagnostic;

pub async fn build_do_effect(
    engine: &TrackedEngine,
    effect_unit_syntax: EffectUnit,
    effects: &mut Vec<effect::Unit>,
    handler: &Storage<diagnostic::Diagnostic>,
) -> Result<Output<do_effect::Key>, CyclicError> {
    todo!()
}

impl Build for do_effect::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, CyclicError> {
        let do_effect_syntax =
            engine.get_function_do_effect_syntax(key.0).await;

        let mut do_effects = Vec::new();

        // extract the effect units from the do effect syntax
        if let Some(do_effect_syntax) =
            do_effect_syntax.and_then(|x| x.effect_unit_list())
        {
            match do_effect_syntax {
                EffectUnitListKind::EffectUnitList(effect_unit_list) => {
                    for effect_unit in effect_unit_list.effect_units() {
                        do_effects.push(effect_unit);
                    }
                }
                EffectUnitListKind::ParenthesizedEffectUnitList(
                    parenthesized_effect_unit_list,
                ) => {
                    for effect_unit in
                        parenthesized_effect_unit_list.effect_units()
                    {
                        do_effects.push(effect_unit);
                    }
                }
            }
        }

        for effect in &do_effects {}

        todo!()
    }
}
