#' Capitalize
#'
#' @name capitalize
#' @note Updated 2019-09-09.
#'
#' @inheritParams params
#' @param strict `logical(1)`.
#'   Enforce strict renaming of words containing multiple capital letters,
#'   including acronyms.
#'
#' @return Modified object.
#'
#' @examples
#' x <- c("the quick Brown fox", "using AIC for model selection")
#' capitalize(x, strict = FALSE)
#' capitalize(x, strict = TRUE)
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
