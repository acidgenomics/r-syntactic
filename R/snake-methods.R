#' @name snake
#'
#' @inherit bioverbs::snake
#' @inherit camel return
#' @inheritParams params
#'
#' @examples
#' load(system.file("extdata", "mn.rda", package = "syntactic"))
#' lapply(mn, snake)
NULL



#' @importFrom bioverbs snake
#' @aliases NULL
#' @export
bioverbs::snake



.snake <-  # nolint
    function(object) {
        object %>%
            dotted() %>%
            tolower() %>%
            gsub("\\.", "_", .)
    }



snake.ANY <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names(object) <- .snake(names(object))
        }
        object
    }



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("ANY"),
    definition = snake.ANY
)



snake.character <-  # nolint
    function(object) {
        if (hasNames(object)) {
            names <- .snake(names(object))
        } else {
            names <- NULL
        }
        object <- .snake(object)
        names(object) <- names
        object
    }



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("character"),
    definition = snake.character
)



snake.factor <-  # nolint
    function(object) {
        names <- names(object)
        object <- object %>%
            as.character() %>%
            snake() %>%
            as.factor()
        names(object) <- snake(names)
        object
    }



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("factor"),
    definition = snake.factor
)



snake.matrix <-  # nolint
    function(
        object,
        rownames = FALSE,
        colnames = TRUE
    ) {
        assert(
            hasDimnames(object),
            isFlag(rownames)
        )
        if (isTRUE(rownames) && hasRownames(object)) {
            rownames(object) <- snake.character(rownames(object))
        }
        if (isTRUE(colnames) && hasColnames(object)) {
            colnames(object) <- snake.character(colnames(object))
        }
        object
    }



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("matrix"),
    definition = snake.matrix
)



snake.Matrix <- snake.matrix  # nolint



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("Matrix"),
    definition = snake.Matrix
)



snake.data.frame <- snake.matrix  # nolint



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("data.frame"),
    definition = snake.data.frame
)



snake.DataFrame <- snake.data.frame  # nolint



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("DataFrame"),
    definition = snake.DataFrame
)



snake.GRanges <-  # nolint
    function(object) {
        colnames(mcols(object)) <- snake(colnames(mcols(object)))
        object
    }



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("GRanges"),
    definition = snake.GRanges
)



snake.GRangesList <- snake.GRanges  # nolint



#' @rdname snake
#' @export
setMethod(
    f = "snake",
    signature = signature("GRangesList"),
    definition = snake.GRangesList
)
