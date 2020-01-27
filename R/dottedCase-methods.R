#' @name dottedCase
#' @inherit acidgenerics::dottedCase
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' dottedCase(object)
NULL



#' @rdname dottedCase
#' @name dottedCase
#' @importFrom acidgenerics dottedCase
#' @usage dottedCase(object, ...)
#' @export
NULL



## Dotted case formatting is used internally by other naming functions.
.dottedCase <-  # nolint
    function(x, smart = TRUE, prefix = TRUE) {
        assert(
            is.atomic(x),
            isFlag(smart),
            isFlag(prefix)
        )
        x <- as.character(x)
        ## Error on empty strings, but allow passthrough of NA.
        assert(all(nzchar(x, keepNA = FALSE)))
        if (isTRUE(smart)) {
            ## Coerce accented characters to plain letter.
            x <- stri_trans_general(str = x, id = "Latin-ASCII")
            ## Handle "&" as a special case. Spell out as "and".
            x <- gsub(
                pattern = "\\&",
                replacement = ".and.",
                x = x
            )
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
        }
        ## Sanitize using base R conventions.
        x <- make.names(
            names = x,
            unique = FALSE,
            allow_ = FALSE
        )
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
        if (isTRUE(smart)) {
            ## Standardize any mixed case acronyms.
            x <- .sanitizeAcronyms(x)
        }
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
    function(
        object,
        smart = TRUE,
        names = TRUE,
        prefix = TRUE
    ) {
        assert(
            isCharacter(object),
            isFlag(smart),
            isFlag(names),
            isFlag(prefix)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .dottedCase(
                x = names(object),
                prefix = TRUE,
                smart = smart
            )
        } else {
            names <- names(object)
        }
        object <- .dottedCase(
            x = object,
            prefix = prefix,
            smart = smart
        )
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
