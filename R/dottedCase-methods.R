#' Dotted case
#'
#' @name dottedCase
#' @note [dottedCase()] support is provided for matching against base R
#'   parameters. However, it is recommended to avoid using it for variable
#'   assignments into an `environment`, as that can introduce conflicts with
#'   base functions.
#' @note Updated 2019-10-07.
#'
#' @inheritParams params
#'
#' @return Modified object, with names formatted in dotted case.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' dottedCase(object)
NULL



## Dotted case formatting is used internally by other naming functions.
.dottedCase <-  # nolint
    function(x, prefix = TRUE) {
        assert(
            is.atomic(x),
            isFlag(prefix)
        )
        x <- as.character(x)
        assert(all(nzchar(x, keepNA = FALSE)))
        ## Handle "+" as a special case. Spell out as "plus".
        x <- gsub(
            pattern = "\\+",
            replacement = ".plus.",
            x = x
        )
        ## Handle "-" as a special case. Spell out as "minus".
        x <- gsub(
            pattern = "-[[:space:]]",
            replacement = ".minus.",
            x = x
        )
        x <- gsub(
            pattern = "^(.+)-$",
            replacement = "\\1.minus",
            x = x
        )
        ## Handle "/" as a special case. Spell out as "slash".
        x <- gsub(
            pattern = "/",
            replacement = ".slash.",
            x = x
        )
        ## Handle "%" as a special case. Spell out as "percent".
        x <- gsub(
            pattern = "%",
            replacement = ".percent.",
            x = x
        )
        ## Strip comma delims in between numbers (e.g. 1,000,000).
        x <- gsub(
            pattern = "(\\d),(\\d)",
            replacement = "\\1\\2",
            x = x
        )
        ## Ready to sanitize using base conventions.
        x <- make.names(
            names = x,
            unique = FALSE,
            allow_ = FALSE
        )
        ## Include "X" prefix by default, but allowing manual disable, so we
        ## can pass to our shell scripts defined in koopa package.
        if (identical(prefix, FALSE)) {
            x <- gsub(
                pattern = "^X(.+)$",
                replacement = "\\1",
                x = x,
                ignore.case = FALSE
            )
        }
        ## Ensure all non-alphanumeric characters get coerced to periods.
        x <- gsub(
            pattern = "[^[:alnum:]]",
            replacement = ".",
            x = x
        )
        ## Combine multiple dots.
        x <- gsub(
            pattern = "[\\.]+",
            replacement = ".",
            x = x
        )
        ## Strip leading or trailing dots.
        x <- gsub(
            pattern = "(^\\.|\\.$)",
            replacement = "",
            x = x
        )
        ## Coerce `"NA"` back to `NA` after `make.names()` call.
        x <- gsub(
            pattern = "^NA$",
            replacement = NA_character_,
            x = x
        )
        ## Standardize any mixed case acronyms.
        x <- .sanitizeAcronyms(x)
        ## Establish word boundaries for camelCase acronyms
        ## (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`).
        ## Acronym following a word.
        x <- gsub(
            pattern = "([a-z])([A-Z])",
            replacement = "\\1.\\2",
            x = x
        )
        ## Word following an acronym.
        x <- gsub(
            pattern = "([A-Z0-9])([A-Z])([a-z])",
            replacement = "\\1.\\2\\3",
            x = x
        )
        ## Return.
        x
    }



`dottedCase,character` <-  # nolint
    function(object, names = TRUE, prefix = TRUE) {
        assert(
            isFlag(names),
            isFlag(prefix)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .dottedCase(names(object))
        } else {
            names <- names(object)
        }
        object <- .dottedCase(object, prefix = prefix)
        names(object) <- names
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("character"),
    definition = `dottedCase,character`
)



#' @rdname dottedCase
#' @export
dotted <- function(...) {
    dottedCase(...)  # nocov
}
