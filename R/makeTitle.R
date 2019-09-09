#' Make a title out of character string
#'
#' Doesn't modify strings already containing a space or multi-letter acronym.
#'
#' @export
#' @note Updated 2019-07-27.
#'
#' @inheritParams params
#'
#' @return `character(1)`.
#'   British style sentence case.
#'
#' @seealso
#' - [makeWords()].
#' - [capitalize()].
#' - [stringr::str_to_sentence()].
#'
#' @examples
#' makeTitle("mitoVsCoding")
#' makeTitle("log10GenesPerUMI")
#' makeTitle("Already A Title")
#' makeTitle("NASA")
makeTitle <- function(string) {
    assert(isString(string))
    x <- makeWords(string)
    x <- capitalize(x)
    x
}
