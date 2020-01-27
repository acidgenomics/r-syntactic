#' @name upperCamelCase
#' @inherit acidgenerics::upperCamelCase
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' upperCamelCase(object)
NULL



#' @rdname upperCamelCase
#' @name upperCamelCase
#' @importFrom acidgenerics upperCamelCase
#' @usage upperCamelCase(object, ...)
#' @export
NULL



.upperCamelCase <-  # nolint
    function(...) {
        .camelCase(..., format = "upper")
    }



`upperCamelCase,character` <-  # nolint
    function(
        object,
        rename = FALSE,
        recursive = FALSE,
        smart = TRUE,
        strict = FALSE,
        names = !rename,
        prefix = !rename
    ) {
        assert(
            isCharacter(object),
            isFlag(rename),
            isFlag(recursive),
            isFlag(smart),
            isFlag(strict),
            isFlag(names),
            isFlag(prefix)
        )
        ## Rename mode ---------------------------------------------------------
        if (isTRUE(rename)) {
            path <- .rename(
                path = object,
                recursive = recursive,
                fun = "upperCamelCase",
                smart = smart,
                prefix = prefix
            )
            return(invisible(path))
        }
        ## String mode ---------------------------------------------------------
        if (isTRUE(names) && hasNames(object)) {
            names <- .upperCamelCase(
                x = names(object),
                strict = strict,
                prefix = TRUE,
                smart = smart
            )
        } else {
            names <- names(object)
        }
        object <- .upperCamelCase(
            x = object,
            strict = strict,
            prefix = prefix,
            smart = smart
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
