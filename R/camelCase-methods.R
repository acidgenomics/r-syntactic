#' @name camelCase
#'
#' @inherit bioverbs::camelCase
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#' Contains syntatically valid names. For objects supporting
#' [`names()`][base::names], the underlying data returns unchanged, except for
#' `character` or `vector` class.
#'
#' @examples
#' data(mn, package = "acidtest")
#' lapply(mn, camelCase)
NULL



#' @rdname camelCase
#' @name camelCase
#' @importFrom bioverbs camelCase
#' @usage camelCase(object, ...)
#' @export
NULL



# Updated 2019-07-19.
.camelCase <-  # nolint
    function(
        object,
        format = c("lower", "upper"),
        strict = FALSE
    ) {
        object <- dotted(object)
        format <- match.arg(format)
        assert(isFlag(strict))

        # Simplify mixed case acronyms in strict mode.
        if (isTRUE(strict)) {
            object <- tolower(object)
        }

        # lowerCamelCase or UpperCamelCase.
        if (format == "lower") {
            # lowerCamelCase
            # Coerce first word to lower.
            object <- gsub(
                pattern = "^(\\w+)\\b",
                replacement = "\\L\\1",
                x = object,
                perl = TRUE
            )
        } else if (format == "upper") {
            # UpperCamelCase
            # Capitalize the first letter.
            object <- gsub(
                pattern = "^([a-z])",
                replacement = "\\U\\1",
                x = object,
                perl = TRUE
            )
        }

        # Remove dots in between numbers following a letter.
        object <- gsub("([[:alpha:]])\\.([[:digit:]])", "\\1\\2", object)

        # First letter of second word must be capitalized.
        object <- gsub("\\.([[:alpha:]])", "\\U\\1", object, perl = TRUE)

        # Remaining dots should be sanitized with "X" character.
        pattern <- "\\."
        if (any(grepl(pattern, object))) {
            if (format == "lower") {
                replacement <- "x"
            } else if (format == "upper") {
                replacement <- "X"
            }
            object <- gsub(pattern, replacement, object)
        }

        object
    }



# Updated 2019-07-19.
`camelCase,ANY` <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names(object) <- camelCase(names(object), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("ANY"),
    definition = `camelCase,ANY`
)



# Updated 2019-07-19.
`camelCase,character` <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names <- .camelCase(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .camelCase(object, strict = strict)
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



# Updated 2019-07-19.
`camelCase,factor` <-  # nolint
    function(object, strict = FALSE) {
        names <- names(object)
        object <- as.character(object)
        object <- camelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- camelCase(names, strict = strict)
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("factor"),
    definition = `camelCase,factor`
)



# Updated 2019-07-19.
`camelCase,matrix` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        strict = FALSE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- camelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camelCase(colnames(object), strict = strict)
        }
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("matrix"),
    definition = `camelCase,matrix`
)



# Updated 2019-07-19.
`camelCase,Matrix` <- `camelCase,matrix`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("Matrix"),
    definition = `camelCase,Matrix`
)



# Updated 2019-07-19.
`camelCase,data.frame` <- `camelCase,matrix`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("data.frame"),
    definition = `camelCase,data.frame`
)



# Updated 2019-07-19.
`camelCase,DataFrame` <- `camelCase,data.frame`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("DataFrame"),
    definition = `camelCase,DataFrame`
)



# Updated 2019-07-19.
`camelCase,GRanges` <-  # nolint
    function(object, strict = FALSE) {
        colnames(mcols(object)) <-
            camelCase(colnames(mcols(object)), strict = strict)
        object
    }



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("GRanges"),
    definition = `camelCase,GRanges`
)



# Updated 2019-07-19.
`camelCase,GRangesList` <- `camelCase,GRanges`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("GRangesList"),
    definition = `camelCase,GRangesList`
)



# Updated 2019-07-19.
`camelCase,SummarizedExperiment` <- `camelCase,matrix`  # nolint



#' @rdname camelCase
#' @export
setMethod(
    f = "camelCase",
    signature = signature("SummarizedExperiment"),
    definition = `camelCase,SummarizedExperiment`
)



# Aliases ======================================================================
#' @rdname camelCase
#' @usage NULL
#' @export
camel <- function(...) {
    camelCase(...)
}
