#' Make an axis label out of a character string
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
#' axisLabel("mitoVsCoding")
#' axisLabel("log10GenesPerUMI")
#' axisLabel("Already A Label")
#' axisLabel("NASA")
axisLabel <- function(string) {
    assert(isString(string))
    makeWords(string)
}
