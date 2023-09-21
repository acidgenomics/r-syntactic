## FIXME Don't require stringi here.



#' @name makeNames
#' @inherit AcidGenerics::makeNames
#' @note Updated 2023-04-12.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso
#' - [ASCII table](https://cs.stanford.edu/people/miles/iso8859.html)
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic[["character"]]
#' makeNames(object)
NULL



## Updated 2023-04-28.
`makeNames,character` <- # nolint
    function(object, unique = TRUE, smart = FALSE) {
        assert(
            requireNamespace("stringi", quietly = TRUE),
            isFlag(unique),
            isFlag(smart)
        )
        x <- as.character(object)
        assert(all(nzchar(x, keepNA = FALSE)))
        x <- stringi::stri_trans_general(str = x, id = "Latin-ASCII")
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
