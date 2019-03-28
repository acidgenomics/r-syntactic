#' @name dotted
#'
#' @note [dotted()] support is provided for matching against base R parameters.
#'   However, it is recommended to avoid using it for variable assignments into
#'   an `environment`, as that can introduce conflicts with base functions.
#'
#' @inherit bioverbs::dotted
#' @inherit camel return
#' @inheritParams params
#'
#' @examples
#' load(system.file("extdata", "mn.rda", package = "syntactic"))
#' lapply(mn, dotted)
NULL



#' @importFrom bioverbs dotted
#' @aliases NULL
#' @export
bioverbs::dotted



# Dotted case formatting is used internally by camel, kebab, and snake.
.dotted <-  # nolint
    function(object) {
        assert(is.atomic(object))
        object <- as.character(object)

        # Handle "+" as a special case. Spell out as "plus".
        object <- gsub(
            pattern = "\\+",
            replacement = ".plus.",
            x = object
        )
        # Handle "%" as a special case. Spell out as "percent".
        object <- gsub(
            pattern = "%",
            replacement = "percent",
            x = object
        )

        # Strip comma delims in between numbers (e.g. 1,000,000).
        object <- gsub(
            pattern = "(\\d),(\\d)",
            replacement = "\\1\\2",
            x = object
        )

        # Now we're ready to sanitize using base conventions.
        object <- make.names(object, unique = FALSE, allow_ = FALSE)

        # Ensure all non-alphanumeric characters get coerced to periods.
        object <- gsub(
            pattern = "[^[:alnum:]]",
            replacement = ".",
            x = object
        )

        # Combine multiple dots.
        object <- gsub(
            pattern = "[\\.]+",
            replacement = ".",
            x = object
        )
        # Strip leading or trailing dots.
        object <- gsub(
            pattern = "(^\\.|\\.$)",
            replacement = "",
            x = object
        )

        # Coerce `"NA"` back to `NA` after `make.names()` call.
        object <- gsub(
            pattern = "^NA$",
            replacement = NA_character_,
            x = object
        )

        # Standardize any mixed case acronyms.
        object <- .sanitizeAcronyms(object)

        # Establish word boundaries for camelCase acronyms
        # (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`).
        # Acronym following a word.
        object <- gsub(
            pattern = "([a-z])([A-Z])",
            replacement = "\\1.\\2",
            x = object
        )
        # Word following an acronym.
        object <- gsub(
            pattern = "([A-Z0-9])([A-Z])([a-z])",
            replacement = "\\1.\\2\\3",
            x = object
        )

        object
    }



dotted.ANY <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names(object) <- dotted(names(object))
        }
        object
    }



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("ANY"),
    definition = dotted.ANY
)



dotted.character <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names <- .dotted(names(object))
        } else {
            names <- NULL
        }
        object <- .dotted(object)
        names(object) <- names
        object
    }



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("character"),
    definition = dotted.character
)



dotted.factor <-  # nolint
    function(object) {
        names <- names(object)
        object <- as.character(object)
        object <- dotted(object)
        object <- as.factor(object)
        names(object) <- dotted(names)
        object
    }



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("factor"),
    definition = dotted.factor
)



dotted.matrix <-  # nolint
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
            rownames(object) <- dotted(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- dotted(colnames(object))
        }
        object
    }



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("matrix"),
    definition = dotted.matrix
)



dotted.Matrix <- dotted.matrix  # nolint



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("Matrix"),
    definition = dotted.Matrix
)



dotted.data.frame <- dotted.matrix  # nolint



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("data.frame"),
    definition = dotted.data.frame
)



dotted.DataFrame <- dotted.data.frame  # nolint



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("DataFrame"),
    definition = dotted.DataFrame
)



dotted.GRanges <-  # nolint
    function(object) {
        colnames(mcols(object)) <- dotted(colnames(mcols(object)))
        object
    }



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("GRanges"),
    definition = dotted.GRanges
)



dotted.GRangesList <- dotted.GRanges  # nolint



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("GRangesList"),
    definition = dotted.GRangesList
)



dotted.SummarizedExperiment <- dotted.matrix  # nolint



#' @rdname dotted
#' @export
setMethod(
    f = "dotted",
    signature = signature("SummarizedExperiment"),
    definition = dotted.SummarizedExperiment
)
