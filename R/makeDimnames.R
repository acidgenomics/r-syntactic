#' Make syntactically valid dimnames
#'
#' @export
#' @inheritParams params
#'
#' @return Modified object.
#' Both row and columns names will be made syntactically valid.
#'
#' @examples
#' data(mn, package = "acidtest")
#' object <- mn$matrix
#' makeDimnames(object)

## Updated 2019-07-19.
makeDimnames <- function(object) {
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
