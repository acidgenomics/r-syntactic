#' @name snakeCase
#' @inherit AcidGenerics::snakeCase
#' @note Updated 2020-07-08.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic$character
#' snakeCase(object)
NULL



## Updated 2020-07-08.
.snakeCase <- # nolint
    function(...) {
        x <- .syntactic(...)
        x <- tolower(x)
        x
    }



## Updated 2021-08-24.
`snakeCase,character` <- # nolint
    function(object,
             smart = TRUE,
             names = TRUE,
             prefix = TRUE) {
        assert(
            isCharacter(object),
            isFlag(smart),
            isFlag(names),
            isFlag(prefix)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .snakeCase(
                x = names(object),
                smart = smart,
                prefix = TRUE
            )
        } else {
            names <- names(object)
        }
        object <- .snakeCase(
            x = object,
            smart = smart,
            prefix = prefix
        )
        names(object) <- names
        object
    }



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature(object = "character"),
    definition = `snakeCase,character`
)
