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
#' @note Updated 2019-12-05.
#'
#' @inheritParams params
#'
#' @return Modified object.
#' Returns invisible modified file path when `rename = TRUE`.
#' Returns invisible `NULL` when `recursive = TRUE`.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' kebabCase(object)
NULL



## Note that by calling `snake()` internally, this will handle special words
## like "%" and "+", which we want. Refer to `dotted()` for this code.
.kebabCase <-  # nolint
    function(x, ...) {
        x <- snake(x, ...)
        x <- gsub(pattern = "_", replacement = "-", x = x)
        x
    }



`kebabCase,character` <-  # nolint
    function(
        object,
        rename = FALSE,
        recursive = FALSE,
        smart = TRUE,
        prefix = !rename
    ) {
        assert(
            isCharacter(object),
            isFlag(rename),
            isFlag(recursive),
            isFlag(smart),
            isFlag(prefix)
        )
        ## Rename mode ---------------------------------------------------------
        if (isTRUE(rename)) {
            path <- .rename(
                path = object,
                recursive = recursive,
                fun = "kebabCase",
                smart = smart,
                prefix = prefix
            )
            return(invisible(path))
        }
        ## String mode ---------------------------------------------------------
        names <- names(object)
        object <- .kebabCase(object, prefix = prefix, smart = smart)
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
