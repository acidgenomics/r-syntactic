#' Make a title out of character string
#'
#' Doesn't modify strings already containing a space or multi-letter acronym.
#'
#' @name makeTitle
#' @note Updated 2019-10-21.
#'
#' @inheritParams params
#'
#' @return `character(1)`.
#'   British style sentence case.
#'
#' @seealso
#' - [makeWords()].
#' - [sentenceCase()].
#' - [stringr::str_to_sentence()].
#'
#' @examples
#' makeTitle("mitoVsCoding")
#' makeTitle("log10GenesPerUMI")
#' makeTitle("Already A Title")
#' makeTitle("NASA")
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
