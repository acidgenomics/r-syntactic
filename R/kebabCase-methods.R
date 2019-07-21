#' @name kebabCase
#'
#' @note We're not including the additional S4 methods that work on
#' [`names()`][base::names] and/or [`dimnames()`][base::dimnames] because dashes
#' are not syntactically valid for names in R.
#'
#' @inherit bioverbs::kebabCase
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @examples
#' kebabCase("this is kebabCase case")
NULL



#' @rdname kebabCase
#' @name kebabCase
#' @importFrom bioverbs kebabCase
#' @usage kebabCase(object, ...)
#' @export
NULL



## Note that by calling `snake()` internally, this will handle special words
## like "%" and "+", which we want. Refer to `dotted()` for this code.
## Updated 2019-07-19.
`kebabCase,character` <-  # nolint
    function(object) {
        names <- names(object)
        object <- snake(object)
        object <- gsub(pattern = "_", replacement = "-", x = object)
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



## Aliases =====================================================================
#' @rdname kebabCase
#' @export
kebab <- function(...) {
    kebabCase(...)
}
