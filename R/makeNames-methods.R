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
        ## FIXME Need to add support for greek characters.
        x <- chartr(old = "À", new = "A", x = x)
        x <- chartr(old = "Á", new = "A", x = x)
        x <- chartr(old = "Â", new = "A", x = x)
        x <- chartr(old = "Ã", new = "A", x = x)
        x <- chartr(old = "Ä", new = "A", x = x)
        x <- chartr(old = "Å", new = "A", x = x)
        x <- chartr(old = "Ç", new = "C", x = x)
        x <- chartr(old = "È", new = "E", x = x)
        x <- chartr(old = "É", new = "E", x = x)
        x <- chartr(old = "Ê", new = "E", x = x)
        x <- chartr(old = "Ë", new = "E", x = x)
        x <- chartr(old = "Ì", new = "I", x = x)
        x <- chartr(old = "Í", new = "I", x = x)
        x <- chartr(old = "Î", new = "I", x = x)
        x <- chartr(old = "Ï", new = "I", x = x)
        x <- chartr(old = "Ð", new = "D", x = x)
        x <- chartr(old = "Ñ", new = "N", x = x)
        x <- chartr(old = "Ò", new = "O", x = x)
        x <- chartr(old = "Ó", new = "O", x = x)
        x <- chartr(old = "Ô", new = "O", x = x)
        x <- chartr(old = "Õ", new = "O", x = x)
        x <- chartr(old = "Ö", new = "O", x = x)
        x <- chartr(old = "×", new = "*", x = x)
        x <- chartr(old = "Ø", new = "O", x = x)
        x <- chartr(old = "Ù", new = "U", x = x)
        x <- chartr(old = "Ú", new = "U", x = x)
        x <- chartr(old = "Û", new = "U", x = x)
        x <- chartr(old = "Ü", new = "U", x = x)
        x <- chartr(old = "Ý", new = "Y", x = x)
        x <- chartr(old = "à", new = "a", x = x)
        x <- chartr(old = "á", new = "a", x = x)
        x <- chartr(old = "â", new = "a", x = x)
        x <- chartr(old = "ã", new = "a", x = x)
        x <- chartr(old = "ä", new = "a", x = x)
        x <- chartr(old = "å", new = "a", x = x)
        x <- chartr(old = "ç", new = "c", x = x)
        x <- chartr(old = "è", new = "e", x = x)
        x <- chartr(old = "é", new = "e", x = x)
        x <- chartr(old = "ê", new = "e", x = x)
        x <- chartr(old = "ë", new = "e", x = x)
        x <- chartr(old = "ì", new = "i", x = x)
        x <- chartr(old = "í", new = "i", x = x)
        x <- chartr(old = "î", new = "i", x = x)
        x <- chartr(old = "ï", new = "i", x = x)
        x <- chartr(old = "ð", new = "d", x = x)
        x <- chartr(old = "ñ", new = "n", x = x)
        x <- chartr(old = "ò", new = "o", x = x)
        x <- chartr(old = "ó", new = "o", x = x)
        x <- chartr(old = "ô", new = "o", x = x)
        x <- chartr(old = "õ", new = "o", x = x)
        x <- chartr(old = "ö", new = "o", x = x)
        x <- chartr(old = "÷", new = "/", x = x)
        x <- chartr(old = "ø", new = "o", x = x)
        x <- chartr(old = "ù", new = "u", x = x)
        x <- chartr(old = "ú", new = "u", x = x)
        x <- chartr(old = "û", new = "u", x = x)
        x <- chartr(old = "ü", new = "u", x = x)
        x <- chartr(old = "ý", new = "y", x = x)
        x <- chartr(old = "ÿ", new = "y", x = x)
        ## Multi-character substitutions.
        x <- gsub(
            pattern = "Æ",
            replacement = "AE",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "Þ",
            replacement = "TH",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "ß",
            replacement = "ss",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "æ",
            replacement = "ae",
            x = x,
            fixed = TRUE
        )
        x <- gsub(
            pattern = "þ",
            replacement = "th",
            x = x,
            fixed = TRUE
        )
        ## Coerce pesky "micro" characters to "u".
        x <- gsub(
            pattern = "(\u00B5|\u03BC|&#181;)",
            replacement = "u",
            x = x,
            fixed = FALSE
        )
        if (isTRUE(smart)) {
            x <- gsub(
                pattern = "'",
                replacement = "",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "\\&",
                replacement = "_and_",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "\\+",
                replacement = "_plus_",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "[[:space:]]-[[:space:]]",
                replacement = " ",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "-[[:space:]]",
                replacement = "_minus_",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "^-(.+)$",
                replacement = "minus_\\1",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "^(.+)-$",
                replacement = "\\1_minus",
                x = x,
                fixed = FALSE
            )
            x <- gsub(
                pattern = "%",
                replacement = "_percent_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "*",
                replacement = "_times_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "/",
                replacement = "_slash_",
                x = x,
                fixed = TRUE
            )
            x <- gsub(
                pattern = "(\\d),(\\d)",
                replacement = "\\1\\2",
                x = x,
                fixed = FALSE
            )
        }
        ## FIXME This returns TRUE for foreign characters, which we don't want.
        x <- gsub(
            pattern = "[^[:alnum:]]",
            replacement = "_",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "(^_|_$)",
            replacement = "",
            x = x,
            fixed = FALSE
        )
        x <- make.names(names = x, unique = unique, allow_ = TRUE)
        x <- gsub(
            pattern = "\\.",
            replacement = "_",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "[_]+",
            replacement = "_",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "(^_|_$)",
            replacement = "",
            x = x,
            fixed = FALSE
        )
        x <- gsub(
            pattern = "^NA$",
            replacement = NA_character_,
            x = x,
            fixed = FALSE
        )
        if (isTRUE(unique)) {
            assert(hasNoDuplicates(x))
        }
        assert(all(nzchar(x, keepNA = FALSE)))
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
