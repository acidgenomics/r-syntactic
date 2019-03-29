#' @name capitalize
#' @inherit bioverbs::capitalize
#' @inheritParams params
#' @examples
#' capitalize(c("hello", "world"))
NULL



#' @importFrom bioverbs capitalize
#' @aliases NULL
#' @export
bioverbs::capitalize



# Using `vapply()` call here to preserve `NA_character_`.
capitalize.character <- function(object) {
    vapply(
        X = object,
        FUN = function(x) {
            if (is.na(x)) return(x)
            first <- substring(x, first = 1L, last = 1L)
            first <- toupper(first)
            tail <- substring(x, first = 2L)
            paste(first, tail, sep = "")
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
}



#' @rdname capitalize
#' @export
setMethod(
    f = "capitalize",
    signature = signature("character"),
    definition = capitalize.character
)
