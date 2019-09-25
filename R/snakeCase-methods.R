#' Snake case
#'
#' Format character strings to use snake-style formatting, where word boundaries
#' are defined by underscores (e.g. `this_is_snake_case`).
#'
#' @name snakeCase
#' @note Updated 2019-09-09.
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
    function(object) {
        object <- dotted(object)
        object <- tolower(object)
        object <- gsub(pattern = "\\.", replacement = "_", x = object)
        object
    }



`snakeCase,character` <-  # nolint
    function(object, names = TRUE) {
        assert(isFlag(names))
        if (isTRUE(names) && hasNames(object)) {
            names <- .snakeCase(names(object))
        } else {
            names <- names(object)
        }
        object <- .snakeCase(object)
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
