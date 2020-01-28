#' @name capitalize
#' @inherit acidgenerics::capitalize
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param strict `logical(1)`.
#'   Enforce strict renaming of words containing multiple capital letters,
#'   including acronyms.
#' @param ... Additional arguments.
#'
#' @examples
#' x <- c("the quick Brown fox", "using AIC for model selection")
#' capitalize(x, strict = FALSE)
#' capitalize(x, strict = TRUE)
NULL



#' @rdname capitalize
#' @name capitalize
#' @importFrom acidgenerics capitalize
#' @usage capitalize(object, ...)
#' @export
NULL



`capitalize,character` <-  # nolint
    function(object, strict = FALSE) {
        assert(isCharacter(object))
        vapply(
            X = strsplit(object, split = " "),
            FUN = function(x) {
                first <- toupper(substring(x, 1L, 1L))
                tail <- substring(x, 2L)
                if (isTRUE(strict)) {
                    tail <- tolower(tail)
                }
                paste(first, tail, sep = "", collapse = " ")
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
