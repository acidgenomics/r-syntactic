#' @name kebabCase
#' @inherit AcidGenerics::kebabCase
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "AcidTest")
#' object <- syntactic$character
#' kebabCase(object)
NULL



#' @rdname kebabCase
#' @name kebabCase
#' @importFrom AcidGenerics kebabCase
#' @usage kebabCase(object, ...)
#' @export
NULL



## Note that by calling `snakeCase()` internally, this will handle special words
## like "%" and "+", which we want. Refer to `dottedCase()` for this code.
.kebabCase <-  # nolint
    function(x, ...) {
        x <- snakeCase(x, ...)
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
