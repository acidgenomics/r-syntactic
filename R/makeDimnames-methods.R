#' @name makeDimnames
#' @inherit AcidGenerics::makeDimnames
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic$matrix
#' makeDimnames(object)
NULL



`makeDimnames,ANY` <- # nolint
    function(object) {
        assert(hasDimnames(object))
        ## Row names.
        if (hasRownames(object)) {
            rownames(object) <- makeNames(rownames(object), unique = TRUE)
        }
        ## Column names.
        if (hasColnames(object)) {
            colnames(object) <- makeNames(colnames(object), unique = TRUE)
        }
        object
    }



#' @rdname makeDimnames
#' @export
setMethod(
    f = "makeDimnames",
    signature = signature(object = "ANY"),
    definition = `makeDimnames,ANY`
)
