#' @name upperCamel
#'
#' @inherit bioverbs::upperCamel
#' @inherit camel return
#' @inheritParams params
#'
#' @examples
#' data(mn, package = "acidtest")
#' lapply(mn, upperCamel)
NULL



#' @importFrom bioverbs upperCamel
#' @aliases NULL
#' @export
bioverbs::upperCamel



.upperCamel <-  # nolint
    function(object, strict = FALSE) {
        .camel(object, format = "upper", strict = strict)
    }



upperCamel.ANY <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names(object) <- upperCamel(names(object), strict = strict)
        }
        object
    }



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("ANY"),
    definition = upperCamel.ANY
)



upperCamel.character <-  # nolint
    function(object, strict = FALSE) {
        if (hasNames(object)) {
            names <- .upperCamel(names(object), strict = strict)
        } else {
            names <- NULL
        }
        object <- .upperCamel(object, strict = strict)
        names(object) <- names
        object
    }



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("character"),
    definition = upperCamel.character
)



upperCamel.factor <-  # nolint
    function(object, strict = FALSE) {
        names <- names(object)
        object <- as.character(object)
        object <- upperCamel(object, strict = strict)
        object <- as.factor(object)
        names(object) <- upperCamel(names, strict = strict)
        object
    }



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("factor"),
    definition = upperCamel.factor
)



upperCamel.matrix <-  # nolint
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
                upperCamel(rownames(object), strict = strict)
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <-
                upperCamel(colnames(object), strict = strict)
        }
        object
    }



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("matrix"),
    definition = upperCamel.matrix
)



upperCamel.Matrix <- upperCamel.matrix  # nolint



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("Matrix"),
    definition = upperCamel.Matrix
)



upperCamel.data.frame <- upperCamel.matrix  # nolint



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("data.frame"),
    definition = upperCamel.data.frame
)



upperCamel.DataFrame <- upperCamel.data.frame  # nolint



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("DataFrame"),
    definition = upperCamel.DataFrame
)



upperCamel.GRanges <-  # nolint
    function(object, strict = FALSE) {
        colnames(mcols(object)) <-
            upperCamel(colnames(mcols(object)), strict = strict)
        object
    }



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("GRanges"),
    definition = upperCamel.GRanges
)



upperCamel.GRangesList <- upperCamel.GRanges  # nolint



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("GRangesList"),
    definition = upperCamel.GRangesList
)



upperCamel.SummarizedExperiment <- upperCamel.matrix  # nolint



#' @rdname upperCamel
#' @export
setMethod(
    f = "upperCamel",
    signature = signature("SummarizedExperiment"),
    definition = upperCamel.SummarizedExperiment
)
