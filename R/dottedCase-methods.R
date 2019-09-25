#' Dotted case
#'
#' @name dottedCase
#' @note [dottedCase()] support is provided for matching against base R
#'   parameters. However, it is recommended to avoid using it for variable
#'   assignments into an `environment`, as that can introduce conflicts with
#'   base functions.
#' @note Updated 2019-09-09.
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
    function(object) {
        assert(is.atomic(object))
        object <- as.character(object)
        ## Handle "+" as a special case. Spell out as "plus".
        object <- gsub(
            pattern = "\\+",
            replacement = ".plus.",
            x = object
        )
        ## Handle "-" as a special case. Spell out as "minus".
        object <- gsub(
            pattern = "-[[:space:]]",
            replacement = ".minus.",
            x = object
        )
        object <- gsub(
            pattern = "^(.+)-$",
            replacement = "\\1.minus",
            x = object
        )
        ## Handle "/" as a special case. Spell out as "slash".
        object <- gsub(
            pattern = "/",
            replacement = ".slash.",
            x = object
        )
        ## Handle "%" as a special case. Spell out as "percent".
        object <- gsub(
            pattern = "%",
            replacement = ".percent.",
            x = object
        )
        ## Strip comma delims in between numbers (e.g. 1,000,000).
        object <- gsub(
            pattern = "(\\d),(\\d)",
            replacement = "\\1\\2",
            x = object
        )
        ## Now we're ready to sanitize using base conventions.
        object <- make.names(
            names = object,
            unique = FALSE,
            allow_ = FALSE
        )
        ## Ensure all non-alphanumeric characters get coerced to periods.
        object <- gsub(
            pattern = "[^[:alnum:]]",
            replacement = ".",
            x = object
        )
        ## Combine multiple dots.
        object <- gsub(
            pattern = "[\\.]+",
            replacement = ".",
            x = object
        )
        ## Strip leading or trailing dots.
        object <- gsub(
            pattern = "(^\\.|\\.$)",
            replacement = "",
            x = object
        )
        ## Coerce `"NA"` back to `NA` after `make.names()` call.
        object <- gsub(
            pattern = "^NA$",
            replacement = NA_character_,
            x = object
        )
        ## Standardize any mixed case acronyms.
        object <- .sanitizeAcronyms(object)
        ## Establish word boundaries for camelCase acronyms
        ## (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`).
        ## Acronym following a word.
        object <- gsub(
            pattern = "([a-z])([A-Z])",
            replacement = "\\1.\\2",
            x = object
        )
        ## Word following an acronym.
        object <- gsub(
            pattern = "([A-Z0-9])([A-Z])([a-z])",
            replacement = "\\1.\\2\\3",
            x = object
        )
        ## Return.
        object
    }



`dottedCase,character` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- .dottedCase(names(object))
        } else {
            names <- names(object)
        }
        object <- .dottedCase(object)
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
