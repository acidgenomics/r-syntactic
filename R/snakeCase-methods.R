#' @name snakeCase
#'
#' @inherit bioverbs::snakeCase
#' @inherit camel return
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(mn, package = "acidtest")
#' lapply(mn, snakeCase)
NULL



#' @rdname snakeCase
#' @name snakeCase
#' @importFrom bioverbs snakeCase
#' @usage snakeCase(object, ...)
#' @export
NULL



# Updated 2019-07-19.
.snakeCase <-  # nolint
    function(object) {
        object <- dotted(object)
        object <- tolower(object)
        object <- gsub(pattern = "\\.", replacement = "_", x = object)
        object
    }



# Updated 2019-07-19.
`snakeCase,ANY` <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names(object) <- .snakeCase(names(object))
        }
        object
    }



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("ANY"),
    definition = `snakeCase,ANY`
)



# Updated 2019-07-19.
`snakeCase,character` <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names <- .snakeCase(names(object))
        } else {
            names <- NULL
        }
        object <- .snakeCase(object)
        names(object) <- names
        object
    }



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("character"),
    definition = `snakeCase,character`
)



# Updated 2019-07-19.
`snakeCase,factor` <-  # nolint
    function(object) {
        names <- names(object)
        object <- as.character(object)
        object <- snakeCase(object)
        object <- as.factor(object)
        names(object) <- snakeCase(names)
        object
    }



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("factor"),
    definition = `snakeCase,factor`
)



# Updated 2019-07-19.
`snakeCase,matrix` <-  # nolint
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
            rownames(object) <- snakeCase.character(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snakeCase.character(colnames(object))
        }
        object
    }



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("matrix"),
    definition = `snakeCase,matrix`
)



# Updated 2019-07-19.
`snakeCase,Matrix` <- `snakeCase,matrix`  # nolint



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("Matrix"),
    definition = `snakeCase,Matrix`
)



# Updated 2019-07-19.
`snakeCase,data.frame` <- `snakeCase,matrix`  # nolint



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("data.frame"),
    definition = `snakeCase,data.frame`
)



# Updated 2019-07-19.
`snakeCase,DataFrame` <- `snakeCase,data.frame`  # nolint



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("DataFrame"),
    definition = `snakeCase,DataFrame`
)



# Updated 2019-07-19.
`snakeCase,GRanges` <-  # nolint
    function(object) {
        colnames(mcols(object)) <- snakeCase(colnames(mcols(object)))
        object
    }



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("GRanges"),
    definition = `snakeCase,GRanges`
)



# Updated 2019-07-19.
`snakeCase,GRangesList` <- `snakeCase,GRanges`  # nolint



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("GRangesList"),
    definition = `snakeCase,GRangesList`
)



# Updated 2019-07-19.
`snakeCase,SummarizedExperiment` <- `snakeCase,matrix`  # nolint



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("SummarizedExperiment"),
    definition = `snakeCase,SummarizedExperiment`
)
