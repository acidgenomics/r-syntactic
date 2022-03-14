#' @name makeLabel
#' @inherit AcidGenerics::makeLabel
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' makeLabel("mitoVsCoding")
#' makeLabel("log10GenesPerUMI")
#' makeLabel("Already A Label")
#' makeLabel("NASA")
#' makeLabel("nGene")
NULL



`makeLabel,character` <- # nolint
    function(object) {
        assert(isString(object))
        makeWords(object)
    }



#' @rdname makeLabel
#' @export
setMethod(
    f = "makeLabel",
    signature = signature(object = "character"),
    definition = `makeLabel,character`
)
