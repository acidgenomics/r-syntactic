#' Make a plot axis or legend label out of a character string
#'
#' Doesn't modify strings already containing a space or multi-letter acronym.
#'
#' @note Updated 2019-07-27.
#' @export
#'
#' @param string `character(1)`.
#'
#' @return `character(1)`.
#'   Lowercase.
#'
#' @seealso
#' - [makeWords()].
#' - [stringr::str_to_lower()]
#'
#' @examples
#' label("mitoVsCoding")
#' label("log10GenesPerUMI")
#' label("Already A Label")
#' label("NASA")
label <- function(string) {
    assert(isString(string))
    makeWords(string)
}
