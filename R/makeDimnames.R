#' Make syntactically valid dimnames
#'
#' @export
#' @inheritParams params
makeDimnames <- function(object) {
    assert(hasDimnames(object))
    # Row names.
    if (hasRownames(object)) {
        rownames(object) <- makeNames(rownames(object), unique = TRUE)
    }
    # Column names.
    if (hasColnames(object)) {
        colnames(object) <- makeNames(colnames(object), unique = TRUE)
    }
    object
}
