#' @name dottedCase
#' @inherit acidgenerics::dottedCase
#' @note Updated 2020-01-27.
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(syntactic, package = "acidtest")
#' object <- syntactic$character
#' dottedCase(object)
NULL



#' @rdname dottedCase
#' @name dottedCase
#' @importFrom acidgenerics dottedCase
#' @usage dottedCase(object, ...)
#' @export
NULL



## Updated 2020-07-08.
.dottedCase <-  # nolint
    function(...) {
        x <- .snakeCase(...)
        x <- gsub(pattern = "_", replacement = ".", x = x)
        x
    }



`dottedCase,character` <-  # nolint
    function(
        object,
        smart = TRUE,
        names = TRUE,
        prefix = TRUE
    ) {
        assert(
            isCharacter(object),
            isFlag(smart),
            isFlag(names),
            isFlag(prefix)
        )
        if (isTRUE(names) && hasNames(object)) {
            names <- .dottedCase(
                x = names(object),
                prefix = TRUE,
                smart = smart
            )
        } else {
            names <- names(object)
        }
        object <- .dottedCase(
            x = object,
            prefix = prefix,
            smart = smart
        )
        names(object) <- names
        object
    }



#' @rdname dottedCase
#' @export
setMethod(
    f = "dottedCase",
    signature = signature("character"),
    definition = `dottedCase,character`
)
