#' @name dotted
#' @inherit bioverbs::dotted
#' @note [dotted()] support is provided for matching against base R parameters.
#'   However, it is recommended to avoid using it for variable assignments into
#'   an `environment`, as that can introduce conflicts with base functions.
NULL



#' @importFrom bioverbs dotted
#' @aliases NULL
#' @export
bioverbs::dotted



# Dotted case formatting is used internally by camel and snake.
.dotted <-  # nolint
    function(object) {
        object %>%
            as.character() %>%
            # Handle "+" as a special case. Spell out as "plus".
            gsub("\\+", ".plus.", .) %>%
            # Handle "%" as a special case. Spell out as "percent".
            gsub("%", "percent", .) %>%
            # Strip comma delims in between numbers (e.g. 1,000,000).
            gsub("(\\d),(\\d)", "\\1\\2", .) %>%
            make.names(unique = FALSE, allow_ = FALSE) %>%
            # Ensure all non-alphanumeric characters get coerced to periods.
            gsub("[^[:alnum:]]", ".", .) %>%
            # Combine multiple dots.
            gsub("[\\.]+", ".", .) %>%
            # Strip leading or trailing dots.
            gsub("(^\\.|\\.$)", "", .) %>%
            # Coerce `"NA"` back to `NA` after `make.names()` call.
            gsub("^NA$", NA_character_, .) %>%
            # Standardize any mixed case acronyms.
            .sanitizeAcronyms() %>%
            # Establish word boundaries for camelCase acronyms
            # (e.g. `worfdbHTMLRemap` -> `worfdb.HTML.remap`).
            # Acronym following a word.
            gsub("([a-z])([A-Z])", "\\1.\\2", .) %>%
            # Word following an acronym.
            gsub("([A-Z0-9])([A-Z])([a-z])", "\\1.\\2\\3", .)
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
        object <- object %>%
            as.character() %>%
            dotted() %>%
            as.factor()
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
