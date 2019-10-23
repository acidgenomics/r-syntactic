#' Make a plot axis or legend label out of a character string
#'
#' Doesn't modify strings already containing a space or multi-letter acronym.
#'
#' @name makeLabel
#' @note Updated 2019-07-31.
#'
#' @inheritParams params
#'
#' @return `character(1)`.
#'   Lowercase.
#'
#' @seealso
#' - [makeWords()].
#' - [stringr::str_to_lower()]
#'
#' @examples
#' makeLabel("mitoVsCoding")
#' makeLabel("log10GenesPerUMI")
#' makeLabel("Already A Label")
#' makeLabel("NASA")
#' makeLabel("nGene")
NULL



`makeLabel,character` <-  # nolint
    function(object) {
        assert(isString(object))
        makeWords(object)
    }



#' @rdname makeLabel
#' @export
setMethod(
    f = "makeLabel",
    signature = signature("character"),
    definition = `makeLabel,character`
)
