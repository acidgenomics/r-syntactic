.sanitizeAcronyms <- function(object) {
    assert(is.atomic(object))
    object <- as.character(object)

    # Sanitize "id" variants (e.g. "Id" to "ID").
    object <- gsub(
        pattern = "\\b(id)\\b",
        replacement = "ID",
        x = object,
        ignore.case = TRUE
    )
    # Sanitize plurarlized acronyms (e.g. "UMIs" to "UMIS").
    object <- gsub(
        pattern = "\\b([A-Z0-9]+)s\\b",
        replacement = "\\1S",
        x = object
    )
    # Sanitize mixed case concentrations (e.g. "10nM" to "10NM").
    object <- gsub(
        pattern = "\\b([[:digit:]]+?[mnu]M)\\b",
        replacement = "\\U\\1",
        x = object,
        perl = TRUE
    )
    # Sanitize mixed case RNA types.
    object <- gsub(
        pattern = "\\b([mi|nc|pi|r]RNA)\\b",
        replacement = "\\U\\1",
        x = object,
        perl = TRUE
    )
    # Handle RNA interference.
    object <- gsub(
        pattern = "\\b(RNAi)\\b",
        replacement = "RNAI",
        x = object
    )

    object
}
