#' Convert syntactic names to words separated by spaces
#'
#' @note Updated 2019-07-27.
#' @export
#'
#' @param names `character`.
#'
#' @return `character`.
#'   Words separated by spaces.
#'
#' @examples
#' makeWords(names = c(
#'     "killVMaim",
#'     "log10GenesPerUMI",
#'     "mitoVsCoding",
#'     "words already",
#'     "NASA"
#' ))
makeWords <- function(names) {
    assert(isCharacter(names))
    ## Don't modify strings that already contain spaces.
    modify <- !grepl("\\s", names)
    mapply(
        x = names,
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
