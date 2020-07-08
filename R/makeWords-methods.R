#' @name makeWords
#' @inherit acidgenerics::makeWords
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



#' @rdname makeWords
#' @name makeWords
#' @importFrom acidgenerics makeWords
#' @usage makeWords(object, ...)
#' @export
NULL



## Updated 2020-07-08.
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
                x <- .syntactic(x)
                x <- gsub("[_.]+", " ", x)
                ## Convert everything but multi-letter acronyms to lowercase.
                x <- gsub("\\b([A-Z])\\b", "\\L\\1", x, perl = TRUE)
                x <- gsub("\\b([A-Z][a-z0-9]+)\\b", "\\L\\1", x, perl = TRUE)
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
