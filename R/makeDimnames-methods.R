#' Make syntactically valid dimnames
#'
#' @name makeDimnames
#' @note Updated 2019-10-21.
#'
#' @inheritParams params
#'
#' @return Modified object.
#' Both row and columns names will be made syntactically valid.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$matrix
#' makeDimnames(object)
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
