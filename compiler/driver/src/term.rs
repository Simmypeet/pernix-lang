//! Contains the function for retrieving the cconfiguration of terminal display

/// Gets the config used for `codespan_reporting::term::Config`
#[must_use]
pub fn get_coonfig() -> codespan_reporting::term::Config {
    let mut config = codespan_reporting::term::Config::default();

    config.chars.snippet_start = "╭─".to_string();

    config
}

/// Gets the styles used for `codespan_reporting::term::Styles`
#[must_use]
pub fn get_styles() -> codespan_reporting::term::Styles {
    let mut styles = codespan_reporting::term::Styles::default();

    styles.line_number.set_intense(true);
    styles.source_border.set_intense(true);
    styles.note_bullet.set_intense(true);
    styles.secondary_label.set_intense(true);

    styles.primary_label_error.set_bold(true);
    styles.primary_label_warning.set_bold(true);
    styles.primary_label_help.set_bold(true);
    styles.primary_label_note.set_intense(true);

    styles
}
