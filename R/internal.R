#' Rename files and/or directories using a syntactic naming function
#'
#' @details
#' Intelligently deals with a case-insensitive file system, if necessary.
#' This is very useful for macOS and Windows.
#'
#' Our syntactic naming functions can result in changes that only differ in
#' case, which are problematic on case-insensitive mounts, and require movement
#' of the files into a temporary file name before the final rename.
#'
#' @note Updated 2019-10-22.
#' @noRd
#'
#' @examples
#' ## > .rename(x = "sample-1.fastq.gz", fun = "snakeCase")
.rename <- function(x, fun, ...) {
    assert(isString(fun))
    insensitive <- !isTRUE(isFileSystemCaseSensitive())
    FUN <- get(  # nolint
        x = fun,
        envir = asNamespace("syntactic"),
        inherits = FALSE
    )
    from <- realpath(x)
    to <- vapply(
        X = x,
        FUN = function(x) {
            from <- x
            dir <- dirname(from)
            stem <- FUN(basenameSansExt(from), ...)
            ext <- fileExt(from)
            ## Add back extension if necessary. Note that this handles both
            ## files without an extension and directories in the call.
            if (!is.na(ext)) {
                bn <- paste0(stem, ".", ext)
            } else {
                bn <- stem
            }
            to <- file.path(dir, bn)
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    if (identical(from, to)) {
        return(from)
    }
    if (isTRUE(insensitive)) {
        tmpTo <- file.path(dirname(from), paste0(".tmp.", basename(from)))
        ok <- file.rename(from = from, to = tmpTo)
        assert(all(ok))
        ok <- file.rename(from = tmpTo, to = to)
        assert(all(ok))
    } else {
        ok <- file.rename(from = from, to = to)
        assert(all(ok))
    }
    to
}



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
    ## Note that not including the front word boundary helps this work on
    ## examples such as "X10uM".
    x <- gsub(
        pattern = "([[:digit:]]+?[mnu]M)\\b",
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
