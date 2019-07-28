#' Make a title out of character string
#'
#' Doesn't modify strings already containing a space or multi-letter acronym.
#'
#' @note Updated 2019-07-27.
#' @export
#'
#' @param string `character(1)`.
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
#' title("mitoVsCoding")
#' title("log10GenesPerUMI")
#' title("Already A Title")
#' title("NASA")
title <- function(string) {
    assert(isString(string))
    x <- makeWords(string)
    x <- capitalize(x)
    x
}
