use std::fmt::Write;

use pernixc_extend::extend;
use pernixc_query::{TrackedEngine, runtime::executor::CyclicError};
use pernixc_semantic_element::{
    parameter::get_parameters, return_type::get_return_type,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    linkage::{C, Linkage, get_linkage},
    syntax::get_function_signature_syntax,
};
use pernixc_target::Global;

use crate::{
    formatter::{self, LinedFormatter, WriteSignatureOptions},
    hover::associate_symbols::format_associate_symbol,
};

#[extend]
pub async fn format_function_signature(
    self: &TrackedEngine,
    function_id: Global<pernixc_symbol::ID>,
    is_local: bool,
) -> Result<String, CyclicError> {
    self.format_associate_symbol(
        function_id,
        async |engine, formatter, function_id| {
            write_function_signature(engine, formatter, function_id, is_local)
                .await
        },
    )
    .await
}

async fn write_function_signature(
    engine: &TrackedEngine,
    formatter: &mut LinedFormatter<'_, '_, '_>,
    function_id: Global<pernixc_symbol::ID>,
    is_local: bool,
) -> Result<(), formatter::Error> {
    formatter
        .write_signature(
            function_id,
            &WriteSignatureOptions::builder()
                .signature_string("function")
                .build(),
        )
        .await?;

    let parameters_syn = if is_local {
        engine.get_function_signature_syntax(function_id).await.0
    } else {
        None
    };

    let kind = engine.get_kind(function_id).await;

    let parameters = engine.get_parameters(function_id).await?;

    let is_vargs = kind == Kind::ExternFunction
        && matches!(
            engine.get_linkage(function_id).await,
            Linkage::C(C { variadic: true })
        );

    let mut is_first = true;

    write!(formatter, "(")?;
    if let Some(parameters_syn) = parameters_syn {
        for (parameter, parameter_syn) in
            parameters.parameters_as_order().map(|x| x.1).zip(
                parameters_syn
                    .parameters()
                    .filter_map(|x| x.into_regular().ok())
                    .map(Some)
                    .chain(std::iter::repeat(None)),
            )
        {
            if !is_first {
                write!(formatter, ", ")?;
            }
            is_first = false;

            let pattern = parameter_syn.and_then(|x| x.irrefutable_pattern());
            formatter::format_pattern(formatter, pattern.as_ref())?;

            write!(formatter, ": ")?;

            formatter.write_type(&parameter.r#type).await?;
        }
    } else {
        for parameter in parameters.parameters_as_order().map(|x| x.1) {
            if !is_first {
                write!(formatter, ", ")?;
            }
            is_first = false;

            formatter.write_type(&parameter.r#type).await?;
        }
    }

    if is_vargs {
        if !is_first {
            write!(formatter, ", ")?;
        }

        write!(formatter, "...")?;
    }
    write!(formatter, ")")?;

    let return_type = engine.get_return_type(function_id).await?;

    write!(formatter, " -> ")?;
    formatter.write_type(&return_type).await?;

    Ok(())
}
