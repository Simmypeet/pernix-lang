//! Contains the function for retrieving the cconfiguration of terminal display

/// Gets the config used for `codespan_reporting::term::Config`
#[must_use]
pub fn get_coonfig() -> codespan_reporting::term::Config {
    let mut config = codespan_reporting::term::Config::default();

    config.styles.line_number.set_intense(true);
    config.styles.source_border.set_intense(true);
    config.styles.note_bullet.set_intense(true);
    config.styles.secondary_label.set_intense(true);

    config.styles.primary_label_error.set_bold(true);
    config.styles.primary_label_warning.set_bold(true);
    config.styles.primary_label_help.set_bold(true);
    config.styles.primary_label_note.set_intense(true);

    config
}
