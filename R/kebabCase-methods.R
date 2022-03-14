#' @name kebabCase
#' @inherit AcidGenerics::kebabCase
#' @note Updated 2020-08-24.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic$character
#' kebabCase(object)
NULL



## Note that by calling `snakeCase()` internally, this will handle special words
## like "%" and "+", which we want. Refer to `dottedCase()` for this code.
.kebabCase <- # nolint
    function(x, ...) {
        x <- snakeCase(x, ...)
        x <- gsub(pattern = "_", replacement = "-", x = x)
        x
    }



## Updated 2021-08-24.
`kebabCase,character` <- # nolint
    function(object,
             smart = TRUE,
             prefix = TRUE) {
        assert(
            isCharacter(object),
            isFlag(smart),
            isFlag(prefix)
        )
        names <- names(object)
        object <- .kebabCase(object, smart = smart, prefix = prefix)
        names(object) <- names
        object
    }



#' @rdname kebabCase
#' @export
setMethod(
    f = "kebabCase",
    signature = signature(object = "character"),
    definition = `kebabCase,character`
)
