#' @name mcolnames
#' @inherit bioverbs::mcolnames
#'
#' @inheritParams params
#' @param ... Additional arguments.
#'
#' @examples
#' data(rse, package = "acidtest")
#' mcolnames(rse)
#'
#' ## Asignment method.
#' mcolnames(rse) <- toupper(mcolnames(rse))
#' mcolnames(rse)
NULL



#' @rdname mcolnames
#' @name mcolnames
#' @importFrom bioverbs mcolnames
#' @usage mcolnames(x, ...)
#' @export
NULL

#' @rdname mcolnames
#' @name mcolnames<-
#' @importFrom bioverbs mcolnames<-
#' @usage mcolnames(x, ...) <- value
#' @export
NULL



# Updated 2019-07-19.
`mcolnames,Vector` <-  # nolint
    function(x) {
        names(mcols(x))
    }



#' @rdname mcolnames
#' @export
setMethod(
    f = "mcolnames",
    signature = signature(x = "Vector"),
    definition = `mcolnames,Vector`
)



# Updated 2019-07-19.
`mcolnames<-,Vector,character` <-  # nolint
    function(x, value) {
        assert(
            isCharacter(value),
            areSameLength(names(mcols(x)), value)
        )
        names(mcols(x)) <- value
        x
    }



#' @rdname mcolnames
#' @export
setMethod(
    f = "mcolnames<-",
    signature = signature(
        x = "Vector",
        value = "character"
    ),
    definition = `mcolnames<-,Vector,character`
)



# Updated 2019-07-19.
`mcolnames<-,Vector,NULL` <-  # nolint
    function(x, value) {
        names(mcols(x)) <- NULL
        x
    }



#' @rdname mcolnames
#' @export
setMethod(
    f = "mcolnames<-",
    signature = signature(
        x = "Vector",
        value = "NULL"
    ),
    definition = `mcolnames<-,Vector,NULL`
)
