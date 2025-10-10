//! Contains the code related to handling workspace configuration.

use std::path::{Path, PathBuf};

use getset::Getters;
use serde::Deserialize;
use tower_lsp::lsp_types::Url;

/// Handles the workspace configuration.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Workspace {
    /// The root path of the workspace.
    ///
    /// The path is always an absolute path.
    root_path: PathBuf,

    /// The parsed configuration of the `pernix.json` file.
    configuration: Configuration,
}

impl Workspace {
    /// Returns the root source file that the module tree starts from.
    #[must_use]
    pub fn root_source_file(&self) -> &Path { &self.configuration.root_file }

    /// Returns the name of the target specified in the `pernix.json` file.
    #[must_use]
    pub fn target_name(&self) -> &str { &self.configuration.target_name }
}

/// Represents the parsed configuration of the `pernix.json` file.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct Configuration {
    /// The name of the target.
    pub target_name: String,

    /// The root source file of the workspace.
    ///
    /// The path is always an absolute path.
    pub root_file: PathBuf,
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

    #[error(
        "the target name `{0}` specified in the `pernix.json` file is not a \
         valid identifier"
    )]
    TargetNameIsNotIdentifier(String),

    #[error("failed to canonicalize the root path {0}: {1}")]
    RootPathCanonicalizationFailed(PathBuf, std::io::Error),

    #[error("error(s) while parsing the `pernix.json` file: {1:?}")]
    JsonParsing(PathBuf, serde_json::Error),
}

impl Workspace {
    /// Creates a new workspace from the given root URL.
    ///
    /// # Errors
    ///
    /// See [`NewWorkspaceError`] for the possible errors that can occur.
    pub fn new(root_url: &Url) -> Result<Self, NewWorkspaceError> {
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

        let mut deserializer = serde_json::Deserializer::from_reader(
            std::fs::File::open(&pernix_configuration_path)?,
        );

        let mut configuration_obj =
            match Configuration::deserialize(&mut deserializer) {
                Ok(configuration) => configuration,
                Err(err) => {
                    return Err(NewWorkspaceError::JsonParsing(
                        pernix_configuration_path,
                        err,
                    ));
                }
            };

        if !pernixc_lexical::kind::Identifier::is_identifier_string(
            &configuration_obj.target_name,
        ) {
            return Err(NewWorkspaceError::TargetNameIsNotIdentifier(
                configuration_obj.target_name,
            ));
        }

        let canonicalized =
            match std::fs::canonicalize(&configuration_obj.root_file) {
                Ok(path) => path,
                Err(err) => {
                    return Err(
                        NewWorkspaceError::RootPathCanonicalizationFailed(
                            configuration_obj.root_file.clone(),
                            err,
                        ),
                    );
                }
            };

        configuration_obj.root_file = canonicalized;

        Ok(Workspace {
            root_path: abs_root_pathbuf,
            configuration: configuration_obj,
        })
    }
}
