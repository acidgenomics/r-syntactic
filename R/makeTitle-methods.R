#' @name makeTitle
#' @inherit acidgenerics::makeTitle
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' makeTitle("mitoVsCoding")
#' makeTitle("log10GenesPerUMI")
#' makeTitle("Already A Title")
#' makeTitle("NASA")
NULL



#' @rdname makeTitle
#' @name makeTitle
#' @importFrom acidgenerics makeTitle
#' @usage makeTitle(object, ...)
#' @export
NULL



`makeTitle,character` <-  # nolint
    function(object) {
        assert(isString(object))
        x <- makeWords(object)
        x <- sentenceCase(x)
        x
    }



#' @rdname makeTitle
#' @export
setMethod(
    f = "makeTitle",
    signature = signature("character"),
    definition = `makeTitle,character`
)
