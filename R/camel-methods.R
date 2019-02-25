#' @name camel
#'
#' @inherit bioverbs::camel
#' @inheritParams params
#'
#' @return Modified object.
#' Contains syntatically valid names. For objects supporting
#' [`names()`][base::names], the underlying data returns unchanged, except for
#' `character` or `vector` class.
#'
#' @examples
#' load(system.file("extdata", "mn.rda", package = "syntactic"))
#' lapply(mn, camel)
NULL



#' @importFrom bioverbs camel
#' @aliases NULL
#' @export
bioverbs::camel



.camel <-  # nolint
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



camel.ANY <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names(object) <- camel(names(object), strict = strict)
        }
        object
    }



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("ANY"),
    definition = camel.ANY
)



camel.character <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names <- .camel(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .camel(object, strict = strict)
        names(object) <- names
        object
    }



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("character"),
    definition = camel.character
)



camel.factor <-  # nolint
    function(object, strict = FALSE) {
        names <- names(object)
        object <- as.character(object)
        object <- camel(object, strict = strict)
        object <- as.factor(object)
        names(object) <- camel(names, strict = strict)
        object
    }



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("factor"),
    definition = camel.factor
)



camel.matrix <-  # nolint
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
            rownames(object) <- camel(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- camel(colnames(object), strict = strict)
        }
        object
    }



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("matrix"),
    definition = camel.matrix
)



camel.Matrix <- camel.matrix  # nolint



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("Matrix"),
    definition = camel.Matrix
)



camel.data.frame <- camel.matrix  # nolint



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("data.frame"),
    definition = camel.data.frame
)



camel.DataFrame <- camel.data.frame  # nolint



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("DataFrame"),
    definition = camel.DataFrame
)



camel.GRanges <-  # nolint
    function(object, strict = FALSE) {
        colnames(mcols(object)) <-
            camel(colnames(mcols(object)), strict = strict)
        object
    }



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("GRanges"),
    definition = camel.GRanges
)



camel.GRangesList <- camel.GRanges  # nolint



#' @rdname camel
#' @export
setMethod(
    f = "camel",
    signature = signature("GRangesList"),
    definition = camel.GRangesList
)
