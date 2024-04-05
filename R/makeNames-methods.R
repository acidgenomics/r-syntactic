#' @name makeNames
#' @inherit AcidGenerics::makeNames
#' @note Updated 2024-04-05.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso
#' - [ASCII table](https://cs.stanford.edu/people/miles/iso8859.html)
#' - `chartr` base R function for single character substitutions.
#' - `stringi::stri_trans_general` with `id = "Latin-ASCII"` for automatic
#'   decoding of characters with accent marks.
#' - https://stackoverflow.com/questions/17517319
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic[["character"]]
#' makeNames(object)
NULL



## Updated 2024-04-05.
`makeNames,character` <- # nolint
    function(object, unique = TRUE, smart = FALSE) {
        assert(
            isFlag(unique),
            isFlag(smart)
        )
        x <- as.character(object)
        assert(all(nzchar(x, keepNA = FALSE)))
        ## Single character substitutions.
        x <- chartr(
            old = c(
                "à",
                "á",
                "â",
                "ã",
                "ä",
                "å",
                "ç",
                "è",
                "é",
                "ê",
                "ë",
                "ì",
                "í",
                "î",
                "ï",
                "ð",
                "ñ",
                "ò",
                "ó",
                "ô",
                "õ",
                "ö",
                "÷",
                "ø",
                "ù",
                "ú",
                "û",
                "ü",
                "ý",
                "ÿ"
            ),
            new = c(
                "a",
                "a",
                "a",
                "a",
                "a",
                "a",
                "c",
                "e",
                "e",
                "e",
                "e",
                "i",
                "i",
                "i",
                "i",
                "d",
                "n",
                "o",
                "o",
                "o",
                "o",
                "o",
                "/",
                "o",
                "u",
                "u",
                "u",
                "u",
                "y",
                "y"
            ),
            x = x
        )
        ## Multi-character substitutions.
        x <- gsub(pattern = "æ", replacement = "ae", x = x)
        x <- gsub(pattern = "þ", replacement = "th", x = x)
        ## Ensure we convert pesky "micro" characters to "u".
        x <- gsub(pattern = "(\u00B5|\u03BC|&#181;)", replacement = "u", x = x)
        if (isTRUE(smart)) {
            x <- gsub(pattern = "'", replacement = "", x = x)
            x <- gsub(pattern = "\\&", replacement = "_and_", x = x)
            x <- gsub(pattern = "\\+", replacement = "_plus_", x = x)
            x <- gsub(
                pattern = "[[:space:]]-[[:space:]]",
                replacement = " ",
                x = x
            )
            x <- gsub(pattern = "-[[:space:]]", replacement = "_minus_", x = x)
            x <- gsub(pattern = "^-(.+)$", replacement = "minus_\\1", x = x)
            x <- gsub(pattern = "^(.+)-$", replacement = "\\1_minus", x = x)
            x <- gsub(pattern = "/", replacement = "_slash_", x = x)
            x <- gsub(pattern = "%", replacement = "_percent_", x = x)
            x <- gsub(pattern = "(\\d),(\\d)", replacement = "\\1\\2", x = x)
        }
        ## FIXME This returns TRUE for foreign characters, which we don't want.
        x <- gsub(pattern = "[^[:alnum:]]", replacement = "_", x = x)
        x <- gsub(pattern = "(^_|_$)", replacement = "", x = x)
        x <- make.names(names = x, unique = unique, allow_ = TRUE)
        x <- gsub(pattern = "\\.", replacement = "_", x = x)
        x <- gsub(pattern = "[_]+", replacement = "_", x = x)
        x <- gsub(pattern = "(^_|_$)", replacement = "", x = x)
        x <- gsub(pattern = "^NA$", replacement = NA_character_, x = x)
        if (isTRUE(unique)) {
            assert(hasNoDuplicates(x))
        }
        x
    }



## Updated 2023-04-12.
`makeNames,factor` <- # nolint
    `makeNames,character`



#' @rdname makeNames
#' @export
setMethod(
    f = "makeNames",
    signature = signature(object = "character"),
    definition = `makeNames,character`
)

#' @rdname makeNames
#' @export
setMethod(
    f = "makeNames",
    signature = signature(object = "factor"),
    definition = `makeNames,factor`
)
