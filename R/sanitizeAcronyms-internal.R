## Updated 2019-07-19.
.sanitizeAcronyms <- function(x) {
    assert(is.atomic(x))
    x <- as.character(x)
    ## Identifier variants (e.g. "Id" to "ID").
    x <- gsub(
        pattern = "\\b(id)\\b",
        replacement = "ID",
        x = x,
        ignore.case = TRUE
    )
    ## Molarity (e.g. "10nM" to "10nm").
    x <- gsub(
        pattern = "\\b([[:digit:]]+?[mnu]M)\\b",
        replacement = "\\L\\1",
        x = x,
        perl = TRUE
    )
    ## Plurarlized acronyms (e.g. "UMIs" to "UMIS").
    x <- gsub(
        pattern = "\\b([A-Z0-9]+)s\\b",
        replacement = "\\1S",
        x = x
    )
    ## Mixed case RNA types.
    x <- gsub(
        pattern = "\\b([mi|nc|pi|r]RNA)\\b",
        replacement = "\\U\\1",
        x = x,
        perl = TRUE
    )
    ## RNA interference.
    x <- gsub(
        pattern = "\\b(RNAi)\\b",
        replacement = "RNAI",
        x = x
    )
    ## Ethanol. EtOH splits into 2 words otherwise.
    ## Consider spelling out "Ethanol" instead if this is too funky.
    x <- gsub(
        pattern = "\\b(EtOH)\\b",
        replacement = "Etoh",
        x = x
    )
    ## Return.
    x
}
