#' @name makeNames
#' @inherit acidgenerics::makeNames
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param unique `logical(1)`.
#'   If `TRUE`, the resulting elements are unique. Recommended by default, for
#'   syntactically valid names (e.g. column, row names). Note that this is
#'   disabled by default for [`make.names()`][base::make.names].
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' makeNames(object)
NULL



#' @rdname makeNames
#' @name makeNames
#' @importFrom acidgenerics makeNames
#' @usage makeNames(object, ...)
#' @export
NULL



`makeNames,character` <-  # nolint
    function(object, unique = TRUE) {
    x <- make.names(names = object, unique = unique)
    x <- gsub(pattern = "\\.", replacement = "_", x = x)
    x
}



#' @rdname makeNames
#' @export
setMethod(
    f = "makeNames",
    signature = signature("character"),
    definition = `makeNames,character`
)



## This is needed for compatibility with bcbioRNASeq.
## Note that factor methods for other syntactic functions are in basejump.
`makeNames,factor` <- `makeNames,character`  # nolint



#' @rdname makeNames
#' @export
setMethod(
    f = "makeNames",
    signature = signature("factor"),
    definition = `makeNames,factor`
)
