.sanitizeAcronyms <- function(object) {
    assert(is.atomic(object))
    object %>%
        as.character() %>%
        # Sanitize "id" variants (e.g. "Id" to "ID").
        gsub(
            pattern = "\\b(id)\\b",
            replacement = "ID",
            x = .,
            ignore.case = TRUE
        ) %>%
        # Sanitize plurarlized acronyms (e.g. "UMIs" to "UMIS").
        gsub(
            pattern = "\\b([A-Z0-9]+)s\\b",
            replacement = "\\1S",
            x = .
        ) %>%
        # Sanitize mixed case concentrations (e.g. "10nM" to "10NM").
        gsub(
            pattern = "\\b([[:digit:]]+?[mnu]M)\\b",
            replacement = "\\U\\1",
            x = .,
            perl = TRUE
        ) %>%
        # Sanitize mixed case RNA types.
        gsub(
            pattern = "\\b([mi|nc|pi|r]RNA)\\b",
            replacement = "\\U\\1",
            x = .,
            perl = TRUE
        ) %>%
        # Handle RNA interference.
        gsub(
            pattern = "\\b(RNAi)\\b",
            replacement = "RNAI",
            x = .
        )
}
