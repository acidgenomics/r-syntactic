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
    function(x, prefix = TRUE) {
        assert(
            all(nzchar(x, keepNA = FALSE)),
            isFlag(prefix)
        )
        x <- dotted(x, prefix = prefix)
        x <- tolower(x)
        x <- gsub(pattern = "\\.", replacement = "_", x = x)
        x
    }



`snakeCase,character` <-  # nolint
    function(object, names = TRUE, prefix = TRUE) {
        assert(
            isFlag(names),
            isFlag(prefix)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .snakeCase(names(object))
        } else {
            names <- names(object)
        }
        object <- .snakeCase(object, prefix = prefix)
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
