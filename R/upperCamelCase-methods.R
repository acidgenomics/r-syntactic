#' @name upperCamelCase
#'
#' @inherit bioverbs::upperCamelCase
#' @inherit camel return
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(mn, package = "acidtest")
#' lapply(mn, upperCamelCase)
NULL



#' @rdname upperCamelCase
#' @name upperCamelCase
#' @importFrom bioverbs upperCamelCase
#' @usage upperCamelCase(object, ...)
#' @export
NULL



## Updated 2019-07-19.
.upperCamelCase <-  # nolint
    function(object, strict = FALSE) {
        .camel(object, format = "upper", strict = strict)
    }



## Updated 2019-07-19.
`upperCamelCase,ANY` <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names(object) <- upperCamelCase(names(object), strict = strict)
        }
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("ANY"),
    definition = `upperCamelCase,ANY`
)



## Updated 2019-07-19.
`upperCamelCase,character` <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names <- .upperCamelCase(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .upperCamelCase(object, strict = strict)
        names(object) <- names
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("character"),
    definition = `upperCamelCase,character`
)



## Updated 2019-07-19.
`upperCamelCase,factor` <-  # nolint
    function(object, strict = FALSE) {
        names <- names(object)
        object <- as.character(object)
        object <- upperCamelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- upperCamelCase(names, strict = strict)
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("factor"),
    definition = `upperCamelCase,factor`
)



## Updated 2019-07-19.
`upperCamelCase,matrix` <-  # nolint
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
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("matrix"),
    definition = `upperCamelCase,matrix`
)



## Updated 2019-07-19.
`upperCamelCase,Matrix` <- `upperCamelCase,matrix`  # nolint



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Matrix"),
    definition = `upperCamelCase,Matrix`
)



## Updated 2019-07-19.
`upperCamelCase,data.frame` <- `upperCamelCase,matrix`  # nolint



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("data.frame"),
    definition = `upperCamelCase,data.frame`
)



## Updated 2019-07-19.
`upperCamelCase,DataFrame` <- `upperCamelCase,data.frame`  # nolint



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("DataFrame"),
    definition = `upperCamelCase,DataFrame`
)



## Updated 2019-07-19.
`upperCamelCase,GRanges` <-  # nolint
    function(object, strict = FALSE) {
        colnames(mcols(object)) <-
            upperCamelCase(colnames(mcols(object)), strict = strict)
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("GRanges"),
    definition = `upperCamelCase,GRanges`
)



## Updated 2019-07-19.
`upperCamelCase,GRangesList` <- `upperCamelCase,GRanges`  # nolint



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("GRangesList"),
    definition = `upperCamelCase,GRangesList`
)



## Updated 2019-07-19.
`upperCamelCase,SummarizedExperiment` <- `upperCamelCase,matrix`  # nolint



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("SummarizedExperiment"),
    definition = `upperCamelCase,SummarizedExperiment`
)



## Aliases ======================================================================
#' @rdname upperCamelCase
#' @usage NULL
#' @export
upperCamel <- function(...) {
    upperCamelCase(...)
}
