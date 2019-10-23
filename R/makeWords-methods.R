#' Convert syntactic names to words separated by spaces
#'
#' @name makeWords
#' @note Updated 2019-10-21.
#'
#' @inheritParams params
#'
#' @return `character`.
#'   Words separated by spaces.
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



`makeWords,character` <-  # nolint
    function(object) {
        assert(isCharacter(object))
        ## Don't modify strings that already contain spaces.
        modify <- !grepl("\\s", object)
        mapply(
            x = object,
            modify = modify,
            FUN = function(x, modify) {
                if (!isTRUE(modify)) return(x)
                x <- dotted(x)
                ## Convert everything but multi-letter acronyms to lowercase.
                x <- gsub("\\b([A-Z])\\b", "\\L\\1", x, perl = TRUE)
                x <- gsub("\\b([A-Z][a-z0-9]+)\\b", "\\L\\1", x, perl = TRUE)
                x <- gsub("\\.", " ", x)
                ## Include period for versus.
                x <- gsub("\\b(v|vs)\\b", "\\1.", x)
                x
            },
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
        )
    }



#' @rdname makeWords
#' @export
setMethod(
    f = "makeWords",
    signature = signature("character"),
    definition = `makeWords,character`
)
