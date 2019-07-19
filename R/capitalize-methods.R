#' @name capitalize
#' @inherit bioverbs::capitalize
#' @inheritParams params
#' @param ... Additional arguments.
#' @examples
#' capitalize(c("hello", "world"))
NULL



#' @rdname capitalize
#' @name capitalize
#' @importFrom bioverbs capitalize
#' @usage capitalize(object, ...)
#' @export
NULL



# Using `vapply()` call here to preserve `NA_character_`.
# Updated 2019-07-19.
`capitalize,character` <-  # nolint
    function(object) {
        vapply(
            X = object,
            FUN = function(x) {
                if (is.na(x)) return(x)
                first <- substring(x, first = 1L, last = 1L)
                first <- toupper(first)
                tail <- substring(x, first = 2L)
                paste(first, tail, sep = "")
            },
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        )
    }



#' @rdname capitalize
#' @export
setMethod(
    f = "capitalize",
    signature = signature("character"),
    definition = `capitalize,character`
)
