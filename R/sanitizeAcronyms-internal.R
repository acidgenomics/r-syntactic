# Updated 2019-07-19.
.sanitizeAcronyms <- function(object) {
    assert(is.atomic(object))
    object <- as.character(object)

    # Identifier variants (e.g. "Id" to "ID").
    object <- gsub(
        pattern = "\\b(id)\\b",
        replacement = "ID",
        x = object,
        ignore.case = TRUE
    )
    # Plurarlized acronyms (e.g. "UMIs" to "UMIS").
    object <- gsub(
        pattern = "\\b([A-Z0-9]+)s\\b",
        replacement = "\\1S",
        x = object
    )
    # Mixed case concentrations (e.g. "10nM" to "10NM").
    object <- gsub(
        pattern = "\\b([[:digit:]]+?[mnu]M)\\b",
        replacement = "\\U\\1",
        x = object,
        perl = TRUE
    )
    # Mixed case RNA types.
    object <- gsub(
        pattern = "\\b([mi|nc|pi|r]RNA)\\b",
        replacement = "\\U\\1",
        x = object,
        perl = TRUE
    )
    # RNA interference.
    object <- gsub(
        pattern = "\\b(RNAi)\\b",
        replacement = "RNAI",
        x = object
    )
    # Ethanol. EtOH splits into 2 words otherwise.
    # Consider spelling out "Ethanol" instead if this is too funky.
    object <- gsub(
        pattern = "\\b(EtOH)\\b",
        replacement = "Etoh",
        x = object
    )

    object
}
