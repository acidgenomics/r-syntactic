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
#' @note Updated 2019-10-07.
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
    function(x, strict = FALSE, prefix = TRUE) {
        .camelCase(x, format = "upper", strict = strict, prefix = prefix)
    }



`upperCamelCase,character` <-  # nolint
    function(object, names = TRUE, strict = FALSE, prefix = TRUE) {
        assert(
            isFlag(names),
            isFlag(strict)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .upperCamelCase(names(object), strict = strict)
        } else {
            names <- names(object)
        }
        object <- .upperCamelCase(object, strict = strict, prefix = prefix)
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
