#' @name makeNames
#' @inherit AcidGenerics::makeNames
#' @note Updated 2021-07-28.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @seealso
#' - [ASCII table](https://cs.stanford.edu/people/miles/iso8859.html)
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic$character
#' makeNames(object)
NULL



## Updated 2022-04-29.
`makeNames,character` <- # nolint
    function(object, unique = TRUE, smart = FALSE) {
        assert(
            requireNamespace("stringi", quietly = TRUE),
            isFlag(unique),
            isFlag(smart)
        )
        x <- as.character(object)
        ## Error on empty strings, but allow passthrough of NA.
        assert(all(nzchar(x, keepNA = FALSE)))
        ## Coerce accented characters to plain letter.
        x <- stringi::stri_trans_general(str = x, id = "Latin-ASCII")
        ## Use \uxxxx escapes for other characters.
        ## Lowercase mu (micro) is an edge case.
        x <- gsub(pattern = "(\u00B5|\u03BC)", replacement = "u", x = x)
        if (isTRUE(smart)) {
            ## Strip single quotation marks.
            x <- gsub(
                pattern = "'",
                replacement = "",
                x = x
            )
            ## Handle "&" as a special case. Spell out as "and".
            x <- gsub(
                pattern = "\\&",
                replacement = "_and_",
                x = x
            )
            ## Handle "+" as a special case. Spell out as "plus".
            x <- gsub(
                pattern = "\\+",
                replacement = "_plus_",
                x = x
            )
            ## Handle "-" as a special case. Spell out as "minus".
            x <- gsub(
                pattern = "-[[:space:]]",
                replacement = "_minus_",
                x = x
            )
            x <- gsub(
                pattern = "^-(.+)$",
                replacement = "minus_\\1",
                x = x
            )
            x <- gsub(
                pattern = "^(.+)-$",
                replacement = "\\1_minus",
                x = x
            )
            ## Handle "/" as a special case. Spell out as "slash".
            x <- gsub(
                pattern = "/",
                replacement = "_slash_",
                x = x
            )
            ## Handle "%" as a special case. Spell out as "percent".
            x <- gsub(
                pattern = "%",
                replacement = "_percent_",
                x = x
            )
            ## Strip comma delims in between numbers (e.g. 1,000,000).
            x <- gsub(
                pattern = "(\\d),(\\d)",
                replacement = "\\1\\2",
                x = x
            )
        }
        ## Ensure all non-alphanumeric characters get coerced to underscores.
        x <- gsub(
            pattern = "[^[:alnum:]]",
            replacement = "_",
            x = x
        )
        ## Strip leading or trailing underscores.
        x <- gsub(
            pattern = "(^_|_$)",
            replacement = "",
            x = x
        )
        ## Sanitize using base R conventions.
        x <- make.names(names = x, unique = unique, allow_ = TRUE)
        x <- gsub(pattern = "\\.", replacement = "_", x = x)
        ## Combine multiple underscores.
        x <- gsub(
            pattern = "[_]+",
            replacement = "_",
            x = x
        )
        ## Strip leading or trailing underscores.
        x <- gsub(
            pattern = "(^_|_$)",
            replacement = "",
            x = x
        )
        ## Coerce `"NA"` back to `NA` after `make.names()` call.
        x <- gsub(
            pattern = "^NA$",
            replacement = NA_character_,
            x = x
        )
        if (isTRUE(unique)) {
            assert(hasNoDuplicates(x))
        }
        x
    }



## This is needed for compatibility with bcbioRNASeq.
## Note that factor methods for other syntactic functions are in basejump.
`makeNames,factor` <- `makeNames,character` # nolint



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
