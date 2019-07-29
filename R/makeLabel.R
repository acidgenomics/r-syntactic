#' Make a plot axis or legend label out of a character string
#'
#' Doesn't modify strings already containing a space or multi-letter acronym.
#'
#' @note Updated 2019-07-29.
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
#' makeLabel("mitoVsCoding")
#' makeLabel("log10GenesPerUMI")
#' makeLabel("Already A Label")
#' makeLabel("NASA")
#' makeLabel("nGene")
makeLabel <- function(string) {
    assert(isString(string))
    x <- makeWords(string)
    ## Handle `n` prefixed strings (e.g. `nGene`).
    if (grepl("^n[[:space:]]", x)) {
        if (!grepl("s$", x)) {
            x <- paste0(x, "s")
        }
        x <- sub("^n[[:space:]]", "", x)
    }
    x
}
