#' @name makeWords
#' @inherit AcidGenerics::makeWords
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' makeWords(c(
#'     "killVMaim",
#'     "log10GenesPerUMI",
#'     "mitoVsCoding",
#'     "words already",
#'     "NASA"
#' ))
NULL



## Updated 2022-05-23.
`makeWords,character` <- # nolint
    function(object) {
        assert(isCharacter(object))
        ## Don't modify strings that already contain spaces.
        modify <- !grepl("\\s", object)
        out <- Map(
            x = object,
            modify = modify,
            f = function(x, modify) {
                if (isFALSE(modify)) {
                    return(x)
                }
                x <- .syntactic(x)
                x <- gsub("[_.]+", " ", x)
                ## Convert everything but multi-letter acronyms to lowercase.
                x <- gsub("\\b([A-Z])\\b", "\\L\\1", x, perl = TRUE)
                x <- gsub("\\b([A-Z][a-z0-9]+)\\b", "\\L\\1", x, perl = TRUE)
                ## Include period for versus.
                x <- gsub("\\b(v|vs)\\b", "\\1.", x)
                x
            }
        )
        out <- unlist(x = out, recursive = FALSE, use.names = FALSE)
        out
    }



#' @rdname makeWords
#' @export
setMethod(
    f = "makeWords",
    signature = signature(object = "character"),
    definition = `makeWords,character`
)
