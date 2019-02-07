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



capitalize.character <- function(object) {
    first <- substring(object, first = 1L, last = 1L)
    first <- toupper(first)
    tail <- substring(object, first = 2L)
    paste(first, tail, sep = "")
}



#' @rdname capitalize
#' @export
setMethod(
    f = "capitalize",
    signature = signature("character"),
    definition = capitalize.character
)
