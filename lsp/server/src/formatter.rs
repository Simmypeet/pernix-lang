//! Provides functionality for formatting symbol signatures with proper
//! indentation.

use std::ops::{Deref, DerefMut};

use bon::Builder;
use pernixc_qbice::TrackedEngine;
use pernixc_semantic_element::{
    implements::get_implements, implements_arguments::get_implements_argument,
};
use pernixc_symbol::{
    accessibility::Accessibility, get_target_root_module_id, kind::get_kind,
    name::get_name, parent::get_closest_module_id,
};
use pernixc_target::Global;
use pernixc_term::{
    display::Display, generic_parameters::get_generic_parameters,
};

use crate::formatter::{
    accessbility::get_accessiblity_str,
    generic_parameters::format_generic_parameters,
};

pub mod accessbility;

mod generic_parameters;
mod irrefutable_pattern;
mod where_clause;

pub use irrefutable_pattern::format_pattern;

/// Stores the internal buffer for formatting with indentation support.
pub struct Formatter<'x, 'y> {
    buffer: &'x mut (dyn std::fmt::Write + Send),
    engine: &'y TrackedEngine,
    current_identiation_level: usize,
    first_line: bool,
}

impl<'x, 'y> Formatter<'x, 'y> {
    /// Creates a new [`Formatter`] with the given buffer and engine.
    pub fn new(
        buffer: &'x mut (dyn std::fmt::Write + Send),
        engine: &'y TrackedEngine,
    ) -> Self {
        Self { buffer, engine, current_identiation_level: 0, first_line: true }
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
    pub async fn new_line(&mut self, format: impl AsyncFnOnce(LinedFormatter)) {
        if !self.first_line {
            writeln!(self.buffer).unwrap();
        }

        self.first_line = false;

        for _ in 0..self.current_identiation_level {
            write!(self.buffer, "{}", Self::TAB).unwrap();
        }

        format(LinedFormatter { formatter: self }).await;
    }

    /// Immediately starts a new line without any indentation.
    pub fn immediate_line(&mut self) {
        if !self.first_line {
            writeln!(self.buffer).unwrap();
        }

        self.first_line = false;
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
    ) {
        if options.format_accessibility == FormatAccessiblity::Yes {
            let accessibility_str =
                self.engine.get_accessiblity_str(symbol).await;

            write!(self.buffer, "{accessibility_str} ").unwrap();
        }

        write!(self.buffer, "{}", options.signature_string).unwrap();
        let name = self.engine.get_name(symbol).await;

        write!(self.buffer, " {}", name.as_ref()).unwrap();

        let kind = self.engine.get_kind(symbol).await;

        if kind.has_generic_parameters()
            && options.format_generic_parameters == FormatGenericParameters::Yes
        {
            let generic_parameters =
                self.engine.get_generic_parameters(symbol).await;

            self.engine
                .format_generic_parameters(self.buffer, &generic_parameters)
                .await;
        }
    }

    /// Formats the accessibility to the buffer.
    pub async fn format_accessibility(
        &mut self,
        accessibility: Accessibility<pernixc_symbol::ID>,
        current_site: Global<pernixc_symbol::ID>,
    ) {
        let root_module_id =
            self.engine.get_target_root_module_id(current_site.target_id).await;
        let nearest_moodule_id =
            self.engine.get_closest_module_id(current_site).await;

        let accessibility_str = match accessibility {
            pernixc_symbol::accessibility::Accessibility::Public => "public",
            pernixc_symbol::accessibility::Accessibility::Scoped(id) => {
                if id == root_module_id {
                    "internal"
                } else if id == nearest_moodule_id {
                    "private"
                } else {
                    // should not happen
                    ""
                }
            }
        };

        write!(self.buffer, "{accessibility_str}").unwrap();
    }

    /// Writes the implements signature for the given implementation ID.
    pub async fn write_implements_signature(
        &mut self,
        implementation_id: Global<pernixc_symbol::ID>,
    ) -> bool {
        let (Some(implements_id), Some(implements_arguments)) = (
            self.engine.get_implements(implementation_id).await,
            self.engine.get_implements_argument(implementation_id).await,
        ) else {
            return false;
        };

        let implements_name = self.engine.get_name(implements_id).await;

        write!(self.buffer, "implements {}", implements_name.as_ref()).unwrap();

        if !implements_arguments.is_empty() {
            let configuration = pernixc_term::display::Configuration::builder()
                .short_qualified_identifiers(true)
                .build();

            implements_arguments
                .write_async_with_configuration(
                    self.engine,
                    self.buffer,
                    &configuration,
                )
                .await
                .unwrap();
        }

        true
    }

    /// Indents the formatter for the duration of the given async function.
    /// After the function completes, the indentation level is restored.
    pub async fn indent(&mut self, f: impl AsyncFnOnce(&mut Formatter)) {
        write!(self.buffer, ":").unwrap();

        self.current_identiation_level += 1;
        f(self.formatter).await;
        self.current_identiation_level -= 1;
    }

    /// Writes the given type using the formatter's engine and the standard
    /// configuration.
    pub async fn write_type(&mut self, ty: &pernixc_term::r#type::Type) {
        self.write_term(ty).await;
    }

    /// Writes the given lifetime using the formatter's engine and the standard
    /// configuration.
    pub async fn write_lifetime(
        &mut self,
        lifetime: &pernixc_term::lifetime::Lifetime,
    ) {
        self.write_term(lifetime).await;
    }

    /// Writes the given term using the formatter's engine and the standard
    /// configuration.
    pub async fn write_term<T: pernixc_term::display::Display>(
        &mut self,
        term: &T,
    ) {
        let configuration = pernixc_term::display::Configuration::builder()
            .short_qualified_identifiers(true)
            .build();

        term.write_async_with_configuration(
            self.engine,
            self.buffer,
            &configuration,
        )
        .await
        .unwrap();
    }
}

impl std::fmt::Write for LinedFormatter<'_, '_, '_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.formatter.buffer.write_str(s)
    }
}
