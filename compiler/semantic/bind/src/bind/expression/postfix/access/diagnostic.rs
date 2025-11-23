use std::fmt::Write;

use pernixc_diagnostic::{Highlight, Report, Severity};
use pernixc_ir::address::{self, Offset};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{TrackedEngine, runtime::executor};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::ByteIndex;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::source_map::to_absolute_span;
use pernixc_term::{
    constant::Constant,
    display::{Display, InferenceRenderingMap},
    r#type::Type,
};

pub use crate::pattern::bind::diagnostic::{
    FieldIsNotAccessible, FieldNotFound,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, StableHash, Serialize, Deserialize,
)]
pub enum ExpectedType {
    Struct,
    Tuple,
    Array,
}

/// Expected a particular kind of type to access a particular kind of field.
#[derive(Debug, Clone, PartialEq, Eq, StableHash, Serialize, Deserialize)]
pub struct UnexpectedTypeForAccess {
    /// The expected type.
    pub expected_type: ExpectedType,

    /// The span where the type check occurred.
    pub span: RelativeSpan,

    /// The expected struct type.
    pub found_type: Type,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,
}

impl Report for UnexpectedTypeForAccess {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let span = engine.to_absolute_span(&self.span).await;

        // Format the found type with inference rendering maps
        let mut found_type_str = String::new();
        self.found_type
            .write_async_with_mapping(
                engine,
                &mut found_type_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await
            .unwrap();

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message(format!(
                "expected a {} type to {}",
                match self.expected_type {
                    ExpectedType::Struct => "struct",
                    ExpectedType::Tuple => "tuple",
                    ExpectedType::Array => "array",
                },
                match self.expected_type {
                    ExpectedType::Struct => "access its field members",
                    ExpectedType::Tuple => "access its tuple element",
                    ExpectedType::Array => "index to its elements",
                }
            ))
            .primary_highlight(
                Highlight::builder()
                    .message(format!("found type '{found_type_str}'",))
                    .span(span)
                    .build(),
            )
            .build())
    }
}

/// The tuple index is too large.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct TooLargeTupleIndex {
    /// The span of the tuple index.
    pub access_span: RelativeSpan,
}

impl Report for TooLargeTupleIndex {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.access_span).await)
                    .build(),
            )
            .message("the tuple index is too large".to_string())
            .severity(Severity::Error)
            .build())
    }
}

/// The tuple index is out of bounds.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct TupleIndexOutOfBounds {
    /// The span of the tuple index.
    pub access_span: RelativeSpan,

    /// The number of elements in the tuple.
    pub element_count: usize,

    /// The index that was accessed.
    pub accessed_index: usize,
}

impl Report for TupleIndexOutOfBounds {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        Ok(pernixc_diagnostic::Rendered::builder()
            .primary_highlight(
                Highlight::builder()
                    .span(engine.to_absolute_span(&self.access_span).await)
                    .build(),
            )
            .message("the tuple index is out of bounds".to_string())
            .severity(Severity::Error)
            .help_message(format!(
                "the tuple has {} element(s) but the accessed index is {}",
                self.element_count, self.accessed_index
            ))
            .build())
    }
}

/// Indexing past the unpacked element in tuple is not allowed.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, StableHash)]
pub struct CannotIndexPastUnpackedTuple {
    /// The span of the index.
    pub index_span: RelativeSpan,

    /// The type of the tuple.
    pub tuple_type: pernixc_term::tuple::Tuple<Type>,

    /// The position of the unpacked element in the tuple.
    pub unpacked_position: usize,

    /// The index that was accessed.
    pub offset: address::Offset,

    /// The inference rendering map for types.
    pub type_inference_map: InferenceRenderingMap<Type>,

    /// The inference rendering map for constants.
    pub constant_inference_map: InferenceRenderingMap<Constant>,
}

impl Report for CannotIndexPastUnpackedTuple {
    async fn report(
        &self,
        engine: &TrackedEngine,
    ) -> Result<pernixc_diagnostic::Rendered<ByteIndex>, executor::CyclicError>
    {
        let index_span = engine.to_absolute_span(&self.index_span).await;

        // Format the tuple type with inference rendering maps
        let mut tuple_type_str = String::new();
        self.tuple_type
            .write_async_with_mapping(
                engine,
                &mut tuple_type_str,
                None,
                Some(&self.type_inference_map),
                Some(&self.constant_inference_map),
            )
            .await
            .unwrap();

        Ok(pernixc_diagnostic::Rendered::builder()
            .severity(Severity::Error)
            .message(match &self.offset {
                address::Offset::FromStart(_) | address::Offset::FromEnd(_) => {
                    "cannot index past unpacked tuple element".to_string()
                }
                address::Offset::Unpacked => {
                    "cannot index into unpacked tuple".to_string()
                }
            })
            .primary_highlight(
                Highlight::builder()
                    .message(format!("the tuple type is '{tuple_type_str}'",))
                    .span(index_span)
                    .build(),
            )
            .maybe_help_message(match self.offset {
                Offset::FromEnd(_) | Offset::FromStart(_) => {
                    let elem = self.offset.index(&self.tuple_type).unwrap();
                    let mut buffer = "if you try to access `".to_string();

                    elem.term
                        .write_async_with_mapping(
                            engine,
                            &mut buffer,
                            None,
                            Some(&self.type_inference_map),
                            Some(&self.constant_inference_map),
                        )
                        .await
                        .unwrap();

                    write!(buffer, "`, try using `{}` instead", match self
                        .offset
                        .flip(self.tuple_type.elements.len())
                    {
                        Offset::FromStart(i) => i.to_string(),
                        Offset::FromEnd(i) => format!("-{i}"),
                        Offset::Unpacked => todo!(),
                    })
                    .unwrap();

                    Some(buffer)
                }

                Offset::Unpacked => None,
            })
            .build())
    }
}
