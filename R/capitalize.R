#' Capitalize
#'
#' Capitalize the first letter of each string in a `character` vector.
#'
#' @export
#' @inheritParams params
#'
#' @seealso `R.utils::capitalize()`.
#'
#' @return `character`.
#'
#' @examples
#' capitalize(c("hello", "world"))
capitalize <- function(x) {
    assert(isCharacter(x))
    first <- substring(x, first = 1L, last = 1L)
    first <- toupper(first)
    tail <- substring(x, first = 2L)
    paste(first, tail, sep = "")
}
