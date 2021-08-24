#' @name upperCamelCase
#' @inherit AcidGenerics::upperCamelCase
#' @note Updated 2021-08-24.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic$character
#' upperCamelCase(object)
NULL



.upperCamelCase <-  # nolint
    function(...) {
        .camelCase(..., format = "upper")
    }



## Updated 2021-08-24.
`upperCamelCase,character` <-  # nolint
    function(
        object,
        strict = TRUE,
        smart = TRUE,
        names = TRUE,
        prefix = TRUE
    ) {
        assert(
            isCharacter(object),
            isFlag(strict),
            isFlag(smart),
            isFlag(names),
            isFlag(prefix)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .upperCamelCase(
                x = names(object),
                strict = strict,
                smart = smart,
                prefix = TRUE
            )
        } else {
            names <- names(object)
        }
        object <- .upperCamelCase(
            x = object,
            strict = strict,
            smart = smart,
            prefix = prefix
        )
        names(object) <- names
        object
    }



#' @rdname upperCamelCase
#' @export
setMethod(
    f = "upperCamelCase",
    signature = signature("character"),
    definition = `upperCamelCase,character`
)
