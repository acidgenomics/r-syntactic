## Updated 2020-07-09.
.sanitizeAcronyms <- function(x) {
    assert(is.atomic(x))
    x <- as.character(x)
    ## Note that underscores are not considered a word boundary.
    x <- gsub(pattern = "_", replacement = ".", x = x)
    ## Identifier variants (e.g. "Id" to "ID").
    x <- gsub(
        pattern = "\\b(id)\\b",
        replacement = "ID",
        x = x,
        ignore.case = TRUE
    )
    ## Molarity (e.g. "10nM" to "10nm").
    x <- gsub(
        pattern = "\\b([mnu]M)\\b",
        replacement = "\\L\\1",
        x = x,
        perl = TRUE
    )
    ## Note that not including the front word boundary helps this work on
    ## examples such as "X10uM".
    x <- gsub(
        pattern = "([[:digit:]]+[mnu]M)\\b",
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
    x <- gsub(pattern = "\\.", replacement = "_", x = x)
    x
}



## Used internally to hand off to camel, dotted, and snake case.
## Updated 2023-02-06.
.syntactic <- function(x, smart = TRUE, prefix = TRUE) {
    assert(
        is.atomic(x),
        isFlag(smart),
        isFlag(prefix)
    )
    x <- makeNames(x, smart = smart, unique = FALSE)
    if (isTRUE(smart)) {
        ## Ignore single quotes.
        x <- gsub(pattern = "'", replacement = "", x = x)
        ## Standardize any mixed case acronyms.
        x <- .sanitizeAcronyms(x)
    }
    ## Include "X" prefix by default, but allowing manual disable, so we
    ## can pass to our shell scripts defined in koopa package.
    if (identical(prefix, FALSE)) {
        x <- gsub(
            pattern = "^X([^[:alpha:]])",
            replacement = "\\1",
            x = x,
            ignore.case = TRUE
        )
    }
    ## Establish word boundaries for camelCase acronyms
    ## (e.g. `worfdbHTMLRemap` -> `worfdb_HTML_remap`).
    ## Acronym following a word.
    x <- gsub(
        pattern = "([a-z])([A-Z])",
        replacement = "\\1_\\2",
        x = x
    )
    ## Word following an acronym.
    x <- gsub(
        pattern = "([A-Z0-9])([A-Z])([a-z])",
        replacement = "\\1_\\2\\3",
        x = x
    )
    ## Return.
    x
}
