#' Kebab case
#'
#' Format character strings to use kebab-style formatting, where word boundaries
#' are defined by dashes/hyphens (e.g. `this-is-kebab-case`).
#'
#' Kebab case is recommended for URL slugs and file names. However, they should
#' not be used for names in R, since dashes are not valid, and should be
#' substituted with underscores or periods instead.
#'
#' @name kebabCase
#' @note We're not including the additional S4 methods that work on
#' [`names()`][base::names] and/or [`dimnames()`][base::dimnames] because dashes
#' are not syntactically valid for names in R.
#' @note Updated 2019-09-09.
#'
#' @inheritParams params
#'
#' @return Modified object.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' kebabCase(object)
NULL



## Note that by calling `snake()` internally, this will handle special words
## like "%" and "+", which we want. Refer to `dotted()` for this code.
## Updated 2019-07-21.
.kebabCase <-  # nolint
    function(object) {
        object <- snake(object)
        object <- gsub(pattern = "_", replacement = "-", x = object)
        object
    }



`kebabCase,character` <-  # nolint
    function(object) {
        names <- names(object)
        object <- .kebabCase(object)
        names(object) <- names
        object
    }



#' @rdname kebabCase
#' @export
setMethod(
    f = "kebabCase",
    signature = signature("character"),
    definition = `kebabCase,character`
)



#' @rdname kebabCase
#' @export
kebab <- function(...) {
    kebabCase(...)  # nocov
}
