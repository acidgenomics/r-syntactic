#' @name makeNames
#' @inherit acidgenerics::makeNames
#' @note Updated 2020-07-08.
#'
#' @inheritParams params
#' @param unique `logical(1)`.
#'   If `TRUE`, the resulting elements are unique. Recommended by default, for
#'   syntactically valid names (e.g. column, row names). Note that this is
#'   disabled by default for [`make.names()`][base::make.names].
#' @param ... Additional arguments.
#'
#' @seealso
#' - [ASCII table](https://cs.stanford.edu/people/miles/iso8859.html)
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



## Updated 2020-07-08.
`makeNames,character` <-  # nolint
    function(object, unique = TRUE) {
        x <- object
        x <- stri_trans_general(str = x, id = "Latin-ASCII")
        x <- gsub(pattern = "µ", replacement = "u", x = x)
        x <- make.names(names = x, unique = unique)
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
