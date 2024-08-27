//! Contains the code related to handling workspace configuration.

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use getset::Getters;
use parking_lot::RwLock;
use pernixc_base::{
    diagnostic::Handler,
    log::formatting,
    source_file::{SourceElement, SourceFile, Span},
};
use pernixc_lexical::{token, token_stream::TokenStream};
use pernixc_syntax::{parser::Parser, syntax_tree::json};
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Url};

use crate::span_ext::SpanExt;

/// Handles the workspace configuration.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Workspace {
    /// The root path of the workspace.
    ///
    /// The path is always an absolute path.
    #[get = "pub"]
    root_path: PathBuf,

    json_configuration_source_file: Arc<SourceFile>,
    configuration: Configuration,
}

/// Represents the parsed configuration of the `pernix.json` file.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Configuration {
    /// The name of the target.
    pub target_name: String,

    /// The root source file of the workspace.
    ///
    /// The path is always an absolute path.
    pub root_file: PathBuf,
}

/// An error occurs when reading the JSON configuration file.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum JsonConfigurationError {
    #[error("found lexcical error {0:?}")]
    Lexical(pernixc_lexical::error::Error),

    #[error("syntax error: {0:?}")]
    Syntax(pernixc_syntax::error::Error),

    #[error("expected a JSON map but found something else")]
    ExpectMap(json::Value),

    #[error("unknown key {}", .0.span.str())]
    UnknownKey(token::String),

    #[error("\"targetName\" key contains non a valid identifier name")]
    TargetNameIsNotIdentifier(token::String),

    #[error("\"rootFile\" key expects a string value")]
    RootFileExpectString(json::Value),

    #[error("\"{key}\" key expects a string value")]
    KeyExpectString {
        /// The key that expects a string value.
        key: &'static str,

        /// The value found.
        found: json::Value,
    },

    #[error("missing keys: {keys:?}")]
    MissingKeys {
        /// The missing keys.
        keys: Vec<&'static str>,

        /// The source file where the key is missing.
        map_span: Span,
    },

    #[error("\"rootFile\" path is invalid")]
    RootFilePathIsInvalid {
        /// The path that is invalid.
        path: Option<PathBuf>,

        /// The string token that represents the path.
        string: token::String,
    },

    #[error("\"rootFile\" path does not exist")]
    RootFilePathNotFound {
        /// The path that does not exist.
        path: PathBuf,

        /// The string token that represents the path.
        string: token::String,
    },

    #[error("\"rootFile\" path is not a file")]
    RootFilePathIsNotFile {
        /// The path that is not a file.
        path: PathBuf,

        /// The string token that represents the path.
        string: token::String,
    },

    #[error("\"rootFile\" path does not have `.pnx` file extension")]
    RootFilePathIsNotPnxFile {
        /// The path that is not a `.pnx` file.
        path: PathBuf,

        /// The string token that represents the path.
        string: token::String,
    },

    #[error("duplicated key found in map")]
    DuplicatedKey {
        /// The first appearance of the key.
        first_appearance: token::String,

        /// The duplicated key.
        duplicated: token::String,
    },
}

impl JsonConfigurationError {
    /// Converts the error to an LSP diagnostic.
    pub fn to_diagnostic(
        &self,
        skip_parising_errors: bool,
    ) -> Option<Diagnostic> {
        fn create_error_diagnostic(span: &Span, message: String) -> Diagnostic {
            Diagnostic {
                range: span.to_range(),
                severity: Some(DiagnosticSeverity::ERROR),
                message,
                ..Default::default()
            }
        }

        fn cut_message(message: &str, string_prefix: &str) -> String {
            let result = if let Some((first, _)) = message.split_once('\n') {
                first
            } else {
                message
            };

            let result = formatting::remove_vt100_codes(result);

            result.replace(string_prefix, "")
        }

        Some(match self {
            JsonConfigurationError::Lexical(error) => {
                if skip_parising_errors {
                    return None;
                }

                let error_message = error.to_string();
                let message = cut_message(&error_message, "[error]:");

                Diagnostic {
                    range: error.span().to_range(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message,
                    ..Default::default()
                }
            }
            JsonConfigurationError::Syntax(error) => {
                if skip_parising_errors {
                    return None;
                }

                let error_message = error.to_string();
                let message = cut_message(&error_message, "[error]:");

                Diagnostic {
                    range: error.found.span().to_range(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message,
                    ..Default::default()
                }
            }
            JsonConfigurationError::ExpectMap(e) => create_error_diagnostic(
                &e.span(),
                "expect JSON map".to_string(),
            ),
            JsonConfigurationError::UnknownKey(e) => create_error_diagnostic(
                &e.span,
                format!("key {} is unknown", e.span.str()),
            ),
            JsonConfigurationError::TargetNameIsNotIdentifier(e) => {
                create_error_diagnostic(
                    &e.span,
                    e.value
                        .as_ref()
                        .map(|x| {
                            format!(
                                "\"{}\" is not a valid identifier for target \
                                 name",
                                x
                            )
                        })
                        .unwrap_or_else(|| {
                            "\"targetName\" contains an invalid identifier \
                             value"
                                .to_string()
                        }),
                )
            }
            JsonConfigurationError::RootFileExpectString(e) => {
                create_error_diagnostic(
                    &e.span(),
                    "\"rootFile\" expects a string value".to_string(),
                )
            }
            JsonConfigurationError::KeyExpectString { key, found } => {
                create_error_diagnostic(
                    &found.span(),
                    format!("\"{}\" expects a string value", key),
                )
            }
            JsonConfigurationError::MissingKeys { keys, map_span } => {
                create_error_diagnostic(
                    map_span,
                    format!(
                        "the key(s) are missing: \"{}\"",
                        keys.join("\", \"")
                    ),
                )
            }
            JsonConfigurationError::RootFilePathIsInvalid { path, string } => {
                create_error_diagnostic(
                    &string.span,
                    path.as_ref().map_or_else(
                        || "invalid path".to_string(),
                        |path| {
                            format!(
                                "\"rootFile\" path \"{}\" is invalid",
                                path.display()
                            )
                        },
                    ),
                )
            }
            JsonConfigurationError::RootFilePathNotFound { path, string } => {
                create_error_diagnostic(
                    &string.span,
                    format!(
                        "\"rootFile\" path \"{}\" does not exist",
                        path.display()
                    ),
                )
            }
            JsonConfigurationError::RootFilePathIsNotFile { path, string } => {
                create_error_diagnostic(
                    &string.span,
                    format!(
                        "\"rootFile\" path \"{}\" is not a file",
                        path.display()
                    ),
                )
            }
            JsonConfigurationError::RootFilePathIsNotPnxFile {
                path,
                string,
            } => create_error_diagnostic(
                &string.span,
                format!(
                    "\"rootFile\" path \"{}\" does not have `.pnx` file \
                     extension",
                    path.display()
                ),
            ),
            JsonConfigurationError::DuplicatedKey { duplicated, .. } => {
                create_error_diagnostic(
                    &duplicated.span,
                    format!(
                        "duplicated key {} found in map",
                        duplicated.span.str()
                    ),
                )
            }
        })
    }
}

#[derive(Debug)]
struct JsonConfigurationErrorCollector(RwLock<Vec<JsonConfigurationError>>);

impl Handler<pernixc_lexical::error::Error>
    for JsonConfigurationErrorCollector
{
    fn receive(&self, error: pernixc_lexical::error::Error) {
        self.0.write().push(JsonConfigurationError::Lexical(error));
    }
}

impl Handler<pernixc_syntax::error::Error> for JsonConfigurationErrorCollector {
    fn receive(&self, error: pernixc_syntax::error::Error) {
        self.0.write().push(JsonConfigurationError::Syntax(error));
    }
}

/// An error occurs when creating a new workspace.
#[derive(Debug, thiserror::Error)]
#[allow(missing_docs)]
pub enum NewWorkspaceError {
    #[error("the given URL is not a local path")]
    NonLocalHostPath,

    #[error("the path {0} does not exist")]
    PathDoesNotExist(PathBuf),

    #[error("the path {0} is not a directory")]
    PathNotDirectory(PathBuf),

    #[error(
        "the path {0} does not contain `pernix.json` workspace configuration \
         file"
    )]
    PackageConfigurationNotFound(PathBuf),

    #[error("I/O error while reading the `pernix.json` file: {0}")]
    Io(#[from] std::io::Error),

    #[error("UTF-8 error while reading the `pernix.json` file: {0}")]
    Utf8(#[from] std::str::Utf8Error),

    #[error("the `pernix.json` file contains invalid JSON configuration")]
    JsonConfiguration(PathBuf, Vec<JsonConfigurationError>),
}

impl From<pernixc_base::source_file::Error> for NewWorkspaceError {
    fn from(error: pernixc_base::source_file::Error) -> Self {
        match error {
            pernixc_base::source_file::Error::Io(err) => {
                NewWorkspaceError::Io(err)
            }
            pernixc_base::source_file::Error::Utf8(err) => {
                NewWorkspaceError::Utf8(err)
            }
        }
    }
}

impl Workspace {
    /// Creates a new workspace from the given root URL.
    pub fn new(root_url: Url) -> Result<Self, NewWorkspaceError> {
        // the path
        let Some(abs_root_pathbuf) = root_url.to_file_path().ok() else {
            return Err(NewWorkspaceError::NonLocalHostPath);
        };

        // check if the folder exists
        if !abs_root_pathbuf.exists() {
            return Err(NewWorkspaceError::PathDoesNotExist(abs_root_pathbuf));
        }

        // check if the path is a directory
        if !abs_root_pathbuf.is_dir() {
            return Err(NewWorkspaceError::PathNotDirectory(abs_root_pathbuf));
        }

        // check if the path contains `pernix.json`
        let pernix_configuration_path = abs_root_pathbuf.join("pernix.json");
        let pernix_configuration_source_file = Arc::new(SourceFile::load(
            std::fs::File::open(&pernix_configuration_path)?,
            pernix_configuration_path.clone(),
        )?);

        let error_collector =
            JsonConfigurationErrorCollector(RwLock::new(Vec::new()));

        let token_stream = TokenStream::tokenize(
            &pernix_configuration_source_file,
            &error_collector,
        );

        let mut parser = Parser::new(
            &token_stream,
            pernix_configuration_source_file.clone(),
        );

        let Some(configuration_json) =
            parser.parse_json_value(&error_collector)
        else {
            return Err(NewWorkspaceError::JsonConfiguration(
                pernix_configuration_path,
                error_collector.0.into_inner(),
            ));
        };

        // map expected
        let json::Value::Map(configuration_map) = configuration_json else {
            error_collector
                .0
                .write()
                .push(JsonConfigurationError::ExpectMap(configuration_json));

            return Err(NewWorkspaceError::JsonConfiguration(
                pernix_configuration_path,
                error_collector.0.into_inner(),
            ));
        };

        let configuration = Self::parse_configuration(
            configuration_map,
            &mut error_collector.0.write(),
            &abs_root_pathbuf,
        );

        if let Some(configuration) = configuration {
            if !error_collector.0.read().is_empty() {
                return Err(NewWorkspaceError::JsonConfiguration(
                    pernix_configuration_path,
                    error_collector.0.into_inner(),
                ));
            }

            Ok(Self {
                root_path: abs_root_pathbuf,
                json_configuration_source_file:
                    pernix_configuration_source_file,
                configuration,
            })
        } else {
            Err(NewWorkspaceError::JsonConfiguration(
                pernix_configuration_path,
                error_collector.0.into_inner(),
            ))
        }
    }

    fn parse_configuration(
        configuration_map: json::Map,
        error_collector: &mut Vec<JsonConfigurationError>,
        root_path: &Path,
    ) -> Option<Configuration> {
        let map_span = configuration_map.span();
        let mut target_name_key_string: Option<token::String> = None;
        let mut root_file_key_string: Option<token::String> = None;

        let mut target_name: Option<String> = None;
        let mut root_file: Option<PathBuf> = None;

        for pair in configuration_map
            .dissolve()
            .1
            .into_iter()
            .flat_map(|x| x.into_elements())
        {
            let (key, _, value) = pair.dissolve();

            match key.value.as_deref() {
                Some("targetName") => {
                    // expect name string
                    let target_name_string = match *value {
                        json::Value::String(value) => value,

                        found => {
                            error_collector.push(
                                JsonConfigurationError::KeyExpectString {
                                    key: "targetName",
                                    found,
                                },
                            );
                            continue;
                        }
                    };

                    // duplicated key
                    if let Some(first_appearance) = &target_name_key_string {
                        error_collector.push(
                            JsonConfigurationError::DuplicatedKey {
                                first_appearance: first_appearance.clone(),
                                duplicated: key,
                            },
                        );
                        continue;
                    }

                    target_name_key_string = Some(key.clone());

                    // is not a valid identifier
                    let target_name_value = match target_name_string.value {
                        Some(string)
                        if token::Identifier::is_valid_identifier_string(&string)
                            => string,
                        _ => {
                            error_collector.push(
                                JsonConfigurationError::TargetNameIsNotIdentifier(
                                    target_name_string,
                                ),
                            );
                            continue;
                        }
                    };

                    target_name = Some(target_name_value);
                }

                Some("rootFile") => {
                    // expect name string
                    let path_string = match *value {
                        json::Value::String(value) => value,

                        found => {
                            error_collector.push(
                                JsonConfigurationError::RootFileExpectString(
                                    found,
                                ),
                            );
                            continue;
                        }
                    };

                    // duplicated key
                    if let Some(first_appearance) = &root_file_key_string {
                        error_collector.push(
                            JsonConfigurationError::DuplicatedKey {
                                first_appearance: first_appearance.clone(),
                                duplicated: key,
                            },
                        );
                        continue;
                    }

                    root_file_key_string = Some(key.clone());

                    // is not a valid identifier
                    let file_path = match &path_string.value {
                        Some(string) => {
                            // convert to absolute path
                            let path = root_path.join(string);
                            let Ok(path) = std::fs::canonicalize(&path) else {
                                error_collector.push(
                                    JsonConfigurationError::RootFilePathIsInvalid {
                                        path: Some(path),
                                        string: path_string,
                                    },
                                );
                                continue;
                            };

                            // file not found
                            if !path.exists() {
                                error_collector.push(
                                    JsonConfigurationError::RootFilePathNotFound {
                                        path,
                                        string: path_string,
                                    },
                                );
                                continue;
                            }

                            // not a file
                            if !path.is_file() {
                                error_collector.push(
                                    JsonConfigurationError::RootFilePathIsNotFile{
                                        path,
                                        string: path_string
                                    },
                                );
                                continue;
                            }

                            // not a `.pnx` file
                            if path.extension().and_then(|x| x.to_str())
                                != Some("pnx")
                            {
                                error_collector.push(
                                    JsonConfigurationError::RootFilePathIsNotPnxFile{
                                        path,
                                        string: path_string
                                    },
                                );
                                continue;
                            }

                            path
                        }

                        _ => {
                            error_collector.push(
                                JsonConfigurationError::RootFilePathIsInvalid {
                                    path: None,
                                    string: path_string,
                                },
                            );
                            continue;
                        }
                    };

                    root_file = Some(file_path);
                }

                _ => {
                    error_collector
                        .push(JsonConfigurationError::UnknownKey(key));
                }
            }
        }

        match (target_name, root_file) {
            (Some(target_name), Some(root_file)) => {
                Some(Configuration { target_name, root_file })
            }

            (_, _) => {
                let mut missing_keys = Vec::new();

                if target_name_key_string.is_none() {
                    missing_keys.push("targetName");
                }

                if root_file_key_string.is_none() {
                    missing_keys.push("rootFile");
                }

                if !missing_keys.is_empty() {
                    error_collector.push(JsonConfigurationError::MissingKeys {
                        keys: missing_keys,
                        map_span,
                    });
                }

                None
            }
        }
    }
}