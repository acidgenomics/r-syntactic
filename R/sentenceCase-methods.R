#' @name sentenceCase
#' @inherit AcidGenerics::sentenceCase
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' x <- c("the quick Brown fox", "using AIC for model selection")
#' sentenceCase(x)
NULL



`sentenceCase,character` <- # nolint
    function(object) {
        assert(isCharacter(object))
        vapply(
            X = object,
            FUN = function(x) {
                if (!isTRUE(grepl(" ", x))) {
                    return(x)
                }
                x <- strsplit(x, split = " ")[[1L]]
                ## Only capitalize the first letter of the first word.
                firstWord <- paste0(
                    toupper(substring(x[[1L]], first = 1L, last = 1L)),
                    substring(x[[1L]], first = 2L)
                )
                ## Loop across the other words and look for acronyms.
                ## Convert to lower case otherwise.
                otherWords <- vapply(
                    X = x[seq(from = 2L, to = length(x))],
                    FUN = function(x) {
                        if (
                            !isTRUE(grepl("^[.A-Z0-9]+$", x)) &&
                                !isTRUE(grepl("[.a-z0-9][A-Z]", x)) &&
                                !isTRUE(grepl("[A-Z]{2}", x))
                        ) {
                            x <- tolower(x)
                        }
                        x
                    },
                    FUN.VALUE = character(1L),
                    USE.NAMES = FALSE
                )
                paste( # nolint
                    c(firstWord, otherWords),
                    sep = "",
                    collapse = " "
                )
            },
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        )
    }



#' @rdname sentenceCase
#' @export
setMethod(
    f = "sentenceCase",
    signature = signature(object = "character"),
    definition = `sentenceCase,character`
)
