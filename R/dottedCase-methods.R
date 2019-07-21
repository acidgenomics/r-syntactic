#' @name dottedCase
#'
#' @note [dottedCase()] support is provided for matching against base R parameters.
#'   However, it is recommended to avoid using it for variable assignments into
#'   an `environment`, as that can introduce conflicts with base functions.
#'
#' @inherit bioverbs::dottedCase
#' @inherit camel return
#' @inheritParams params
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' lapply(syntactic, dottedCase)
NULL



#' @rdname dottedCase
#' @name dottedCase
#' @importFrom bioverbs dottedCase
#' @usage dottedCase(object, ...)
#' @export
NULL



## Dotted case formatting is used internally by camel, kebab, and snake.
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
        ## Handle "%" as a special case. Spell out as "percent".
        object <- gsub(
            pattern = "%",
            replacement = "percent",
            x = object
        )

        ## Strip comma delims in between numbers (e.g. 1,000,000).
        object <- gsub(
            pattern = "(\\d),(\\d)",
            replacement = "\\1\\2",
            x = object
        )

        ## Now we're ready to sanitize using base conventions.
        object <- make.names(object, unique = FALSE, allow_ = FALSE)

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

        object
    }



dottedCase.ANY <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names(object) <- dottedCase(names(object))
        }
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("ANY"),
    definition = dottedCase.ANY
)



dottedCase.character <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names <- .dottedCase(names(object))
        } else {
            names <- NULL
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
    definition = dottedCase.character
)



dottedCase.factor <-  # nolint
    function(object) {
        names <- names(object)
        object <- as.character(object)
        object <- dottedCase(object)
        object <- as.factor(object)
        names(object) <- dottedCase(names)
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("factor"),
    definition = dottedCase.factor
)



dottedCase.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- dottedCase(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dottedCase(colnames(object))
        }
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("matrix"),
    definition = dottedCase.matrix
)



dottedCase.Matrix <- dottedCase.matrix  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("Matrix"),
    definition = dottedCase.Matrix
)



dottedCase.data.frame <- dottedCase.matrix  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("data.frame"),
    definition = dottedCase.data.frame
)



dottedCase.DataFrame <- dottedCase.data.frame  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("DataFrame"),
    definition = dottedCase.DataFrame
)



dottedCase.GRanges <-  # nolint
    function(object) {
        colnames(mcols(object)) <- dottedCase(colnames(mcols(object)))
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("GRanges"),
    definition = dottedCase.GRanges
)



dottedCase.GRangesList <- dottedCase.GRanges  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("GRangesList"),
    definition = dottedCase.GRangesList
)



dottedCase.SummarizedExperiment <- dottedCase.matrix  # nolint



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("SummarizedExperiment"),
    definition = dottedCase.SummarizedExperiment
)



## Aliases =====================================================================
#' @rdname dottedCase
#' @export
dotted <- function(...) {
    dottedCase(...)
}
