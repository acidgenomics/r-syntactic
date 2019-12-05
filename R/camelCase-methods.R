#' Camel case
#'
#' Format character strings to use (lower) camel-style formatting, where word
#' boundaries are defined by capitlization only (e.g. `thisIsCamelCase`).
#'
#' Camel case is recommended by Bioconductor for variable and function names.
#'
#' @name camelCase
#' @note Updated 2019-12-05.
#'
#' @inheritParams params
#'
#' @return Modified object.
#' Contains syntatically valid names. For objects supporting
#' [`names()`][base::names], the underlying data returns unchanged, except for
#' `character` or `vector` class.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' camelCase(object)
NULL



.camelCase <-  # nolint
    function(
        x,
        format = c("lower", "upper"),
        strict = FALSE,
        ...
    ) {
        x <- dotted(x, ...)
        assert(isFlag(strict))
        format <- match.arg(format)
        ## Simplify mixed case acronyms in strict mode.
        if (isTRUE(strict)) {
            x <- tolower(x)
        }
        ## lowerCamelCase or UpperCamelCase.
        if (identical(format, "lower")) {
            ## lowerCamelCase
            ## Coerce first word to lower.
            x <- gsub(
                pattern = "^(\\w+)\\b",
                replacement = "\\L\\1",
                x = x,
                perl = TRUE
            )
        } else if (identical(format, "upper")) {
            ## UpperCamelCase
            ## Capitalize the first letter.
            x <- gsub(
                pattern = "^([a-z])",
                replacement = "\\U\\1",
                x = x,
                perl = TRUE
            )
        }
        ## Remove dots in between numbers following a letter.
        x <- gsub("([[:alpha:]])\\.([[:digit:]])", "\\1\\2", x)
        ## First letter of second word must be capitalized.
        x <- gsub("\\.([[:alpha:]])", "\\U\\1", x, perl = TRUE)
        ## Remaining dots should be sanitized with "X" character.
        pattern <- "\\."
        if (any(grepl(pattern, x))) {
            if (identical(format, "lower")) {
                replacement <- "x"
            } else if (identical(format, "upper")) {
                replacement <- "X"
            }
            x <- gsub(pattern, replacement, x)
        }
        x
    }



`camelCase,character` <-  # nolint
    function(
        object,
        rename = FALSE,
        recursive = FALSE,
        smart = TRUE,
        strict = FALSE,
        names = !rename,
        prefix = !rename
    ) {
        assert(
            isCharacter(object),
            isFlag(rename),
            isFlag(recursive),
            isFlag(smart),
            isFlag(strict),
            isFlag(names),
            isFlag(prefix)
        )
        ## Rename mode ---------------------------------------------------------
        if (isTRUE(rename)) {
            path <- object
            if (isTRUE(recursive)) {
                path <- .recursive(path)
            }
            path <- .rename(
                path = path,
                fun = "camelCase",
                smart = smart,
                prefix = prefix
            )
            return(invisible(path))
        }
        ## String mode ---------------------------------------------------------
        if (isTRUE(names) && hasNames(object)) {
            names <- .camelCase(
                x = names(object),
                strict = strict,
                prefix = TRUE,
                smart = smart
            )
        } else {
            names <- names(object)
        }
        object <- .camelCase(
            x = object,
            strict = strict,
            prefix = prefix,
            smart = smart
        )
        names(object) <- names
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("character"),
    definition = `camelCase,character`
)



#' @rdname camelCase
#' @export
camel <- function(...) {
    camelCase(...)  # nocov
}
