## Don't use `title`, it will mask function in graphics package.



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
