#' @name makeDimnames
#' @inherit acidgenerics::makeDimnames
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$matrix
#' makeDimnames(object)
NULL



#' @rdname makeDimnames
#' @name makeDimnames
#' @importFrom acidgenerics makeDimnames
#' @usage makeDimnames(object, ...)
#' @export
NULL



`makeDimnames,ANY` <-  # nolint
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
    signature = signature("ANY"),
    definition = `makeDimnames,ANY`
)
