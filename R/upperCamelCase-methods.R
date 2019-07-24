#' @name upperCamelCase
#'
#' @inherit bioverbs::upperCamelCase
#' @inherit camel return
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' lapply(syntactic, upperCamelCase)
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
        .camelCase(object, format = "upper", strict = strict)
    }



## Base R classes ==============================================================
## Updated 2019-07-19.
`upperCamelCase,atomic` <-  # nolint
    function(object, names = TRUE, strict = FALSE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <- upperCamelCase(names(object), strict = strict)
        }
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("atomic"),
    definition = `upperCamelCase,atomic`
)



## Updated 2019-07-19.
`upperCamelCase,character` <-  # nolint
    function(object, names = TRUE, strict = FALSE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .upperCamelCase(names(object), strict = strict)
        } else {
            names <- names(object)
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
    function(object, names = TRUE, strict = FALSE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- upperCamelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- as.character(object)
        object <- upperCamelCase(object, strict = strict)
        object <- as.factor(object)
        names(object) <- names
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
`upperCamelCase,list` <- `upperCamelCase,atomic`  # nolint



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("list"),
    definition = `upperCamelCase,list`
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
            isFlag(rownames),
            isFlag(colnames),
            isFlag(strict)
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
`upperCamelCase,data.frame` <- `upperCamelCase,matrix`  # nolint



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("data.frame"),
    definition = `upperCamelCase,data.frame`
)



## S4 virtual classes ==========================================================
## Updated 2019-07-19.
`upperCamelCase,Vector` <-  # nolint
    function(
        object,
        names = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = FALSE
    ) {
        validObject(object)
        assert(
            isFlag(names),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names(object) <-
                upperCamelCase(names(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            mcolnames(object) <-
                upperCamelCase(mcolnames(object), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Vector"),
    definition = `upperCamelCase,Vector`
)



## Updated 2019-07-19.
## mcols metadata
`upperCamelCase,DataTable` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        mcols = TRUE,
        metadata = TRUE,
        strict = FALSE
    ) {
        validObject(object)
        assert(
            hasDimnames(object),
            isFlag(rownames),
            isFlag(colnames),
            isFlag(mcols),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        if (isTRUE(mcols) && hasNames(mcols(object))) {
            mcolnames(object) <-
                upperCamelCase(mcolnames(object), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object

    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("DataTable"),
    definition = `upperCamelCase,DataTable`
)



## Updated 2019-07-19.
`upperCamelCase,Ranges` <- `upperCamelCase,Vector`  # nolint
formals(`upperCamelCase,Ranges`)[c("mcols", "names")] <- c(TRUE, FALSE)



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("Ranges"),
    definition = `upperCamelCase,Ranges`
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



## S4 classes ==================================================================
## Updated 2019-07-19.
`upperCamelCase,SummarizedExperiment` <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE,
        assayNames = TRUE,
        rowData = TRUE,
        colData = TRUE,
        metadata = TRUE,
        strict = FALSE
    ) {
        validObject(object)
        assert(
            isFlag(rownames),
            isFlag(colnames),
            isFlag(assayNames),
            isFlag(rowData),
            isFlag(colData),
            isFlag(metadata),
            isFlag(strict)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <-
                upperCamelCase(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamelCase(colnames(object), strict = strict)
        }
        if (isTRUE(assayNames) && isCharacter(assayNames(object))) {
            ## `assayNames<-` assignment method doesn't work reliably.
            names(assays(object)) <-
                upperCamelCase(names(assays(object)), strict = strict)
        }
        if (isTRUE(rowData) && hasColnames(rowData(object))) {
            colnames(rowData(object)) <-
                upperCamelCase(colnames(rowData(object)), strict = strict)
        }
        if (isTRUE(colData) && hasColnames(colData(object))) {
            colnames(colData(object)) <-
                upperCamelCase(colnames(colData(object)), strict = strict)
        }
        if (isTRUE(metadata) && hasNames(metadata(object))) {
            names(metadata(object)) <-
                upperCamelCase(names(metadata(object)), strict = strict)
        }
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("SummarizedExperiment"),
    definition = `upperCamelCase,SummarizedExperiment`
)



## Aliases =====================================================================
#' @rdname upperCamelCase
#' @export
upperCamel <- function(...) {
    upperCamelCase(...)  # nocov
}
