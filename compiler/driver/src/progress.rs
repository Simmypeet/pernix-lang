//! Module for displaying compilation progress indicators.

use std::{io::Write as _, path::Path, time::Instant};

use codespan_reporting::term::termcolor::WriteColor;
use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};

/// Displays a compilation stage indicator with optional spinner.
pub struct ProgressIndicator {
    quiet: bool,
    start_time: Instant,
    spinner: Option<ProgressBar>,
}

impl ProgressIndicator {
    /// Creates a new progress indicator.
    #[must_use]
    pub fn new(quiet: bool) -> Self {
        Self { quiet, start_time: Instant::now(), spinner: None }
    }

    /// Shows a "Compiling" message with a spinner.
    pub fn show_compiling(
        &mut self,
        target_name: &str,
        file_path: &Path,
        err_writer: &mut dyn WriteColor,
    ) {
        if self.quiet {
            return;
        }

        let message =
            format!("Compiling {} ({})", target_name.bold(), file_path.display());

        // Write the message to stderr
        let _ = writeln!(err_writer, "{message}");

        // Create a spinner if we're writing to a terminal
        if atty::is(atty::Stream::Stderr) {
            let spinner = ProgressBar::new_spinner();
            spinner.set_style(
                ProgressStyle::default_spinner()
                    .template("{spinner:.green} {msg}")
                    .expect("valid template"),
            );
            spinner.set_message(format!(
                "Compiling {} ({})",
                target_name,
                file_path.display()
            ));
            spinner.enable_steady_tick(std::time::Duration::from_millis(100));
            self.spinner = Some(spinner);
        }
    }

    /// Shows a "Checking" message with a spinner.
    pub fn show_checking(
        &mut self,
        target_name: &str,
        file_path: &Path,
        err_writer: &mut dyn WriteColor,
    ) {
        if self.quiet {
            return;
        }

        let message =
            format!("Checking {} ({})", target_name.bold(), file_path.display());

        // Write the message to stderr
        let _ = writeln!(err_writer, "{message}");

        // Create a spinner if we're writing to a terminal
        if atty::is(atty::Stream::Stderr) {
            let spinner = ProgressBar::new_spinner();
            spinner.set_style(
                ProgressStyle::default_spinner()
                    .template("{spinner:.green} {msg}")
                    .expect("valid template"),
            );
            spinner.set_message(format!(
                "Checking {} ({})",
                target_name,
                file_path.display()
            ));
            spinner.enable_steady_tick(std::time::Duration::from_millis(100));
            self.spinner = Some(spinner);
        }
    }

    /// Shows a "Success" message with elapsed time.
    pub fn show_success(&mut self, err_writer: &mut dyn WriteColor) {
        if self.quiet {
            return;
        }

        // Clear the spinner
        if let Some(spinner) = self.spinner.take() {
            spinner.finish_and_clear();
        }

        let elapsed = self.start_time.elapsed();
        let elapsed_secs = elapsed.as_secs_f64();

        let message = format!("{} in {elapsed_secs:.2}s", "Success".green().bold());
        let _ = writeln!(err_writer, "{message}");
    }

    /// Shows a "Running" message for an executable.
    pub fn show_running(
        &mut self,
        executable_path: &Path,
        err_writer: &mut dyn WriteColor,
    ) {
        if self.quiet {
            return;
        }

        // Clear the spinner
        if let Some(spinner) = self.spinner.take() {
            spinner.finish_and_clear();
        }

        let message = format!("Running {}", executable_path.display());
        let _ = writeln!(err_writer, "{message}");
    }

    /// Shows an exit message for a successfully completed executable.
    pub fn show_exit_success(
        &mut self,
        executable_path: &Path,
        err_writer: &mut dyn WriteColor,
    ) {
        if self.quiet {
            return;
        }

        let message = format!(
            "Binary {} exited successfully with 0 exit code",
            executable_path.display()
        );
        let _ = writeln!(err_writer, "{message}");
    }
}

impl Drop for ProgressIndicator {
    fn drop(&mut self) {
        // Ensure spinner is cleared on drop
        if let Some(spinner) = self.spinner.take() {
            spinner.finish_and_clear();
        }
    }
}
