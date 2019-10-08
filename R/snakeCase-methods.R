#' Snake case
#'
#' Format character strings to use snake-style formatting, where word boundaries
#' are defined by underscores (e.g. `this_is_snake_case`).
#'
#' @name snakeCase
#' @note Updated 2019-10-07.
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
    function(x, prefix = TRUE, smart = TRUE) {
        assert(
            all(nzchar(x, keepNA = FALSE)),
            isFlag(prefix),
            isFlag(smart)
        )
        x <- dotted(x, prefix = prefix, smart = smart)
        x <- tolower(x)
        x <- gsub(pattern = "\\.", replacement = "_", x = x)
        x
    }



`snakeCase,character` <-  # nolint
    function(object, names = TRUE, prefix = TRUE, smart = TRUE) {
        assert(
            isFlag(names),
            isFlag(prefix),
            isFlag(smart)
        )
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
