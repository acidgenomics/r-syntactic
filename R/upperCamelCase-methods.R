#' Upper camel case
#'
#' Format character strings to use upper camel-style formatting, where word
#' boundaries are defined by capitlization only (e.g. `ThisIsCamelCase`).
#'
#' Note that lower camel case is generally recommended in R over the use of
#' upper camel case. However, upper camel case is recommended by Bioconductor
#' for S4 class names and corresponding generators, but not variables or
#' functions.
#'
#' @name upperCamelCase
#' @note Updated 2019-12-05.
#'
#' @inherit camelCase return
#' @inheritParams params
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' upperCamelCase(object)
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



#' @rdname upperCamelCase
#' @export
upperCamel <- function(...) {
    upperCamelCase(...)  # nocov
}
