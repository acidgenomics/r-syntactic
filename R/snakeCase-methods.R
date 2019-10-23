#' Snake case
#'
#' Format character strings to use snake-style formatting, where word boundaries
#' are defined by underscores (e.g. `this_is_snake_case`).
#'
#' @name snakeCase
#' @note Updated 2019-10-22.
#'
#' @inherit camelCase return
#' @inheritParams params
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' snakeCase(object)
NULL



.snakeCase <-  # nolint
    function(x, ...) {
        x <- dotted(x, ...)
        x <- tolower(x)
        x <- gsub(pattern = "\\.", replacement = "_", x = x)
        x
    }



`snakeCase,character` <-  # nolint
    function(
        object,
        rename = FALSE,
        smart = TRUE,
        names = !rename,
        prefix = !rename
    ) {
        assert(
            isCharacter(object),
            isFlag(rename),
            isFlag(smart),
            isFlag(names),
            isFlag(prefix)
        )
        ## File rename mode ----------------------------------------------------
        if (isTRUE(rename)) {
            files <- .rename(
                x = object,
                fun = "snakeCase",
                smart = smart,
                prefix = prefix
            )
            return(invisible(files))
        }
        ## String mode ---------------------------------------------------------
        if (isTRUE(names) && hasNames(object)) {
            names <- .snakeCase(
                x = names(object),
                prefix = TRUE,
                smart = smart
            )
        } else {
            names <- names(object)
        }
        object <- .snakeCase(
            x = object,
            prefix = prefix,
            smart = smart
        )
        names(object) <- names
        object
    }



#' @rdname snakeCase
#' @export
setMethod(
    f = "snakeCase",
    signature = signature("character"),
    definition = `snakeCase,character`
)



#' @rdname snakeCase
#' @export
snake <- function(...) {
    snakeCase(...)  # nocov
}
