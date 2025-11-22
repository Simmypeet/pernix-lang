//! Snapshot tests for LSP functionalities.

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use insta::assert_snapshot;
use pernix_server::{
    goto_definition::handle_goto_definition, hover::handle_hover,
};
use pernixc_query::TrackedEngine;
use pernixc_target::TargetID;
use tower_lsp::lsp_types::{
    GotoDefinitionParams, HoverParams, PartialResultParams,
    WorkDoneProgressParams,
};
use tracing_subscriber::{
    EnvFilter, Layer, layer::SubscriberExt, util::SubscriberInitExt,
};

use crate::fixture_with_cursor::FixtureWithCursor;

pub mod fixture_with_cursor;

#[test_generator::test_resources("lsp/integration_test/snapshot/**/main.pnx")]
fn main(resource: &str) {
    stacker::maybe_grow(3 * 1024 * 1024, 8 * 1024 * 1024, || {
        let _ = tracing_subscriber::registry()
            .with(
                tracing_subscriber::fmt::layer()
                    .with_thread_ids(true)
                    .with_thread_names(true)
                    .with_span_events(
                        tracing_subscriber::fmt::format::FmtSpan::CLOSE,
                    )
                    .with_filter(
                        EnvFilter::try_from_env("PERNIXC_LOG")
                            .unwrap_or_else(|_| "ERROR".into()),
                    ),
            )
            .try_init();

        let file_path = PathBuf::from(resource);
        std::env::set_current_dir(env!("PERNIXC_CARGO_WORKSPACE_DIR")).unwrap();

        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .thread_stack_size(
                // in the debug build, the stack size of the future is not
                // optimized well, so we need a larger stack size
                {
                    #[cfg(debug_assertions)]
                    {
                        8 * 1024 * 1024 // 8MB stack for debug build
                    }
                    #[cfg(not(debug_assertions))]
                    {
                        2 * 1024 * 1024 // 2MB stack default
                    }
                },
            )
            .build()
            .expect("Failed to create Tokio runtime")
            .block_on(test_file(file_path));
    });
}

/// Dispatches the test based on the file path.
pub async fn test_file(main_file: PathBuf) {
    let project_path = env!("PERNIXC_CARGO_WORKSPACE_DIR");
    let main_file = std::fs::canonicalize(main_file)
        .expect("Failed to canonicalize main file path");

    // NOTE: Define each directory to a specific functionality to test here
    let mut goto_path = PathBuf::from(project_path);
    goto_path.push("lsp");
    goto_path.push("integration_test");
    goto_path.push("snapshot");
    goto_path.push("goto_definition");

    let mut hover_path = PathBuf::from(project_path);
    hover_path.push("lsp");
    hover_path.push("integration_test");
    hover_path.push("snapshot");
    hover_path.push("hover");

    // NOTE: Match the prefix paths to the test functions here
    if main_file.starts_with(&goto_path) {
        test_goto_definition(main_file).await;
    } else if main_file.starts_with(&hover_path) {
        test_hover(main_file).await;
    } else {
        panic!(
            "No test defined for path: {}, available directories: {}, {}",
            main_file.display(),
            goto_path.display(),
            hover_path.display()
        );
    }
}

/// Tests the "hover" LSP functionality.
pub async fn test_hover(main_file: PathBuf) {
    let (tracked_engine, fixture, target_id) =
        create_engine_test_for_fixture_with_cursor(&main_file).await;

    let response = tracked_engine
        .handle_hover(target_id, HoverParams {
            text_document_position_params: fixture
                .cursor_text_document_position_params(),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        })
        .await
        .expect("encountered cyclic dependency");

    let snapshot_str = response.unwrap_or_else(|| "no hover found".to_string());

    test_snapshot_string(&main_file, &snapshot_str);
}

/// Tests the "go to definition" LSP functionality.
pub async fn test_goto_definition(main_file: PathBuf) {
    let (tracked_engine, fixture, target_id) =
        create_engine_test_for_fixture_with_cursor(&main_file).await;

    let response = tracked_engine
        .handle_goto_definition(target_id, GotoDefinitionParams {
            text_document_position_params: fixture
                .cursor_text_document_position_params(),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
        })
        .await
        .expect("encountered cyclic dependency");

    let snapshot_str = response.map_or_else(
        || "no definition found".to_string(),
        |response| {
            serde_json::to_string_pretty(&response)
                .expect("Failed to serialize goto definition response")
        },
    );

    dbg!(&snapshot_str);
    test_snapshot_string(&main_file, &snapshot_str);
}

/// Creates a tracked engine for testing with the given fixture file.
pub async fn create_engine_test_for_fixture_with_cursor(
    main_file: &Path,
) -> (TrackedEngine, Arc<FixtureWithCursor>, TargetID) {
    let parent_directory =
        main_file.parent().expect("Test file has no parent directory");

    // Load the fixture from the parent directory
    let fixture =
        Arc::new(fixture_with_cursor::FixtureWithCursor::from_directory(
            parent_directory,
        ));

    // Create the analyzer engine for the test
    let target_name = "lsp_test".to_string();
    let target_id = TargetID::from_target_name(&target_name);
    let engine = pernix_server::analyzer::Analyzer::create_engine(
        "lsp_test".to_string(),
        main_file.to_path_buf(),
        fixture.clone(),
    )
    .await;

    let tracked_engine = engine.tracked();

    (tracked_engine, fixture, target_id)
}

/// Tests the given string against a snapshot file located in the same
/// directory as `main_file`.
pub fn test_snapshot_string(main_file: &Path, string: &str) {
    let parent_directory =
        main_file.parent().expect("Test file has no parent directory");
    let mut settings = insta::Settings::clone_current();

    settings.set_snapshot_path(parent_directory);
    settings.set_prepend_module_to_snapshot(false);
    settings.remove_snapshot_suffix();

    let file_stem = env!("PERNIXC_CARGO_WORKSPACE_DIR");

    // Convert windows paths to Unix Paths.
    settings.add_filter(r"\\\\?([\w\d.])", "/$1");

    // Strip project directory from paths.
    settings.add_filter(file_stem, "/project/");

    let _guard = settings.bind_to_scope();
    assert_snapshot!("snapshot", string);
}
