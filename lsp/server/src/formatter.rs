//! Provides functionality for formatting symbol signatures with proper
//! indentation.

use std::ops::{Deref, DerefMut};

use bon::Builder;
use pernixc_extend::extend;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_symbol::{kind::get_kind, name::get_name};
use pernixc_target::Global;
use pernixc_term::{
    display::Display, generic_parameters::get_generic_parameters,
};

use crate::formatter::{
    accessbility::get_accessiblity_str,
    generic_parameters::format_generic_parameters,
};

pub mod accessbility;
pub mod generic_parameters;

/// Represents errors that can occur during formatting.
#[derive(Debug, Clone, Copy, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Fmt(#[from] std::fmt::Error),

    #[error(transparent)]
    Cyclic(#[from] CyclicError),
}

/// Converts a Result that may contain a formatting error into one that
/// panics on formatting errors, propagating only cyclic errors.
#[extend]
pub fn assert_no_fmt_error<T>(
    self: Result<T, Error>,
) -> Result<T, CyclicError> {
    match self {
        Ok(value) => Ok(value),
        Err(Error::Cyclic(e)) => Err(e),
        Err(Error::Fmt(e)) => panic!("Unexpected formatting error: {e}"),
    }
}

/// Stores the internal buffer for formatting with indentation support.
pub struct Formatter<'x, 'y> {
    buffer: &'x mut (dyn std::fmt::Write + Send),
    engine: &'y pernixc_query::TrackedEngine,
    current_identiation_level: usize,
}

impl<'x, 'y> Formatter<'x, 'y> {
    /// Creates a new [`Formatter`] with the given buffer and engine.
    pub fn new(
        buffer: &'x mut (dyn std::fmt::Write + Send),
        engine: &'y pernixc_query::TrackedEngine,
    ) -> Self {
        Self { buffer, engine, current_identiation_level: 0 }
    }
}

impl std::fmt::Debug for Formatter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Formatter")
            .field("current_identiation_level", &self.current_identiation_level)
            .field("engine", &self.engine)
            .finish_non_exhaustive()
    }
}

/// Specifies whether to format accessibility when writing a symbol signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum FormatAccessiblity {
    Yes,
    No,
}

/// Specifies whether to format generic parameters when writing a symbol
/// signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum FormatGenericParameters {
    Yes,
    No,
}

/// Options for writing a symbol signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Builder)]
pub struct WriteSignatureOptions<'s> {
    /// The signature string to write. For example, "struct", "enum", etc.
    pub signature_string: &'s str,

    /// Whether to format accessibility.
    #[builder(default = FormatAccessiblity::Yes)]
    pub format_accessibility: FormatAccessiblity,

    /// Whether to format generic parameters.
    #[builder(default = FormatGenericParameters::Yes)]
    pub format_generic_parameters: FormatGenericParameters,
}

impl Formatter<'_, '_> {
    /// 4 spaces for indentation
    pub const TAB: &'static str = "    ";

    /// Starts a new line with the current indentation level, applies the given
    /// format function, and then adds a newline.
    pub async fn new_line(
        &mut self,
        format: impl AsyncFnOnce(LinedFormatter) -> Result<(), Error>,
    ) -> Result<(), Error> {
        for _ in 0..self.current_identiation_level {
            write!(self.buffer, "{}", Self::TAB).unwrap();
        }

        format(LinedFormatter { formatter: self }).await?;

        writeln!(self.buffer).unwrap();

        Ok(())
    }
}

/// A new type wrapper around [`Formatter`] that adds functionality for writing
/// symbol signatures with proper formatting.
#[derive(Debug)]
pub struct LinedFormatter<'x, 'y, 'z> {
    formatter: &'z mut Formatter<'x, 'y>,
}

impl<'x, 'y> Deref for LinedFormatter<'x, 'y, '_> {
    type Target = Formatter<'x, 'y>;

    fn deref(&self) -> &Self::Target { self.formatter }
}

impl DerefMut for LinedFormatter<'_, '_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target { self.formatter }
}

impl LinedFormatter<'_, '_, '_> {
    /// Writes the signature of the given symbol according to the provided
    /// options.
    pub async fn write_signature(
        &mut self,
        symbol: Global<pernixc_symbol::ID>,
        options: &WriteSignatureOptions<'_>,
    ) -> Result<(), Error> {
        if options.format_accessibility == FormatAccessiblity::Yes {
            let accessibility_str =
                self.engine.get_accessiblity_str(symbol).await;

            write!(self.buffer, "{accessibility_str} ").unwrap();
        }

        write!(self.buffer, "{}", options.signature_string).unwrap();
        let name = self.engine.get_name(symbol).await;

        write!(self.buffer, " {name}").unwrap();

        let kind = self.engine.get_kind(symbol).await;

        if kind.has_generic_parameters()
            && options.format_generic_parameters == FormatGenericParameters::Yes
        {
            let generic_parameters =
                self.engine.get_generic_parameters(symbol).await?;

            self.engine
                .format_generic_parameters(self.buffer, &generic_parameters)
                .await;
        }

        Ok(())
    }

    /// Indents the formatter for the duration of the given async function.
    /// After the function completes, the indentation level is restored.
    pub async fn indent(
        &mut self,
        f: impl AsyncFnOnce(&mut Formatter) -> Result<(), Error>,
    ) -> Result<(), Error> {
        writeln!(self.buffer, ":").unwrap();

        self.current_identiation_level += 1;
        f(self.formatter).await?;
        self.current_identiation_level -= 1;

        Ok(())
    }

    /// Writes the given type using the formatter's engine and the standard
    /// configuration.
    pub async fn write_type(
        &mut self,
        ty: &pernixc_term::r#type::Type,
    ) -> Result<(), std::fmt::Error> {
        let configuration = pernixc_term::display::Configuration::builder()
            .short_qualified_identifiers(true)
            .build();

        ty.write_async_with_configuration(
            self.engine,
            self.buffer,
            &configuration,
        )
        .await
    }
}

impl std::fmt::Write for LinedFormatter<'_, '_, '_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.formatter.buffer.write_str(s)
    }
}
